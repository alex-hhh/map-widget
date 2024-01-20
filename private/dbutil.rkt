#lang racket/base
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; dbutil.rkt -- utilities to open and create databases based on schema files.

;; This file is part of map-widget
;; Copyright (c) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/contract db)

;; Contract for the progress callback passed to db-open
(define progress-callback/c
  (-> string? exact-positive-integer? exact-positive-integer? any/c))

(provide (struct-out db-exn-bad-version)
         db-exn-bad-version-message)

(provide/contract
 [db-open (->*
           ((or/c 'memory path-string?))
           (#:schema-file (or/c #f path-string?)
            #:allow-higher-version boolean?
            #:expected-version (or/c #f exact-positive-integer?)
            #:progress-callback (or/c #f progress-callback/c))
           connection?)])

;; Read the next SQL statement from INPUT-PORT.  The statement is assumed to
;; be terminated by the #\; character.
(define (read-next-statement input-port)
  (let ((out (open-output-string))
        (in-string? #f))

    ;; Return the next character in the input stream PORT, collapsing all
    ;; whitespace to a single space and skipping all comments.  Comments start
    ;; with "--" and extend until the end of the line.  Strings are being
    ;; tracked for.
    (define (get-next-char)
      (let ((ch (read-char input-port)))

        (when (eqv? ch #\')
          (set! in-string? (not in-string?)))

        (cond ((eqv? ch eof) ch)

              ((and (char-whitespace? ch)
                    (let ((ch-next (peek-char input-port)))
                      (or (eqv? ch-next eof)
                          (char-whitespace? ch-next))))
               ;; Colapse all whitespace into one single space
               (get-next-char))

              ((and (not in-string?)
                    (eqv? ch #\-)
                    (eqv? (peek-char input-port) #\-))
               ;; This is a comment, skip it until end of line
               (for ((v (in-producer (lambda () (read-char input-port)) #\newline)))
                 (begin #f))
               #\ )

              ((char-whitespace? ch) #\ ) ; all whitespace converted to space
              (#t ch))))

    ;; read from the input stream using GET-NEXT-CHAR until a semi-colon (#\;)
    ;; is seen.  Intermediate read chars are written to OUT.  The full
    ;; statement is returned, or #f on EOF.
    (define (loop)
      (let ((ch (get-next-char)))
        (cond ((eqv? ch eof) ; incomplete statement
               #f)
              ((and (eqv? ch #\;) (not in-string?))
               (get-output-string out))
              (#t
               (write-char ch out)
               (loop)))))

    (loop)))

;; Read SQL statements from INPUT-PORT, and return them as a list
(define (collect-statements input-port)
  (let loop ((statements '()))
    (if (eqv? (peek-char input-port) eof)
        (reverse statements)
        (let ((stmt (read-next-statement input-port)))
          (loop (if stmt (cons stmt statements) statements))))))


;; Read SQL statements from INPUT-PORT and run them against DB.  Statements
;; are executed for side effects (e.g CREATE TABLE)
(define (execute-statements input-port db)
  (define (loop)
    (unless (eqv? (peek-char input-port) eof)
      (let ((stmt (read-next-statement input-port)))
        (when stmt
          (query-exec db stmt)))
      (loop)))
  (loop))


;; Create the initial schema in a database, if it does not already exist.  A
;; database that has no tables (no entries in SQLITE_MASTER) is considered to
;; be newly created.  In that case, we run the SCHEMA-FILE script on it to
;; create the initial database schema.  DATABASE-FILE is the name of the file
;; from which DB was opened.  If creating the schema fails, the file will be
;; removed.
(define (maybe-create-schema database-file schema-file db [progress-callback #f])
  (let ((new-database? (= 0 (query-value db "select count(*) from SQLITE_MASTER"))))
    (when new-database?
      (with-handlers
        ((exn:fail?
          (lambda (e)
            (disconnect db)
            ;; database-file can be 'memory for in memory databases
            (when (path-string? database-file)
              (delete-file database-file))
            (raise e))))
        (let* ((statements (call-with-input-file schema-file collect-statements))
               (statement-count (length statements)))
          (for ([stmt statements]
                [n statement-count])
            (query-exec db stmt)
            (when progress-callback
              (progress-callback "Executing SQL statement..." (+ n 1) statement-count))))))))

;; An exception used to indicate that the database was a incorrect schema
;; version
(struct db-exn-bad-version (file expected actual) #:transparent)
(define (db-exn-bad-version-message e)
  (format
   "bad schema version: expected ~a, actual ~a"
   (db-exn-bad-version-expected e)
   (db-exn-bad-version-actual e)))

(define (db-open database-file
                 #:schema-file [schema-file #f]
                 #:allow-higher-version [allow-higher-version #f]
                 #:expected-version [expected-version #f]
                 #:progress-callback [progress-callback #f])
  (let ((db (sqlite3-connect #:database database-file #:mode 'create #:use-place #t)))
    (when schema-file
      (maybe-create-schema database-file schema-file db progress-callback))
    (query-exec db "pragma foreign_keys = on")
    (query-exec db "pragma cache_size = 4000")
    (when expected-version
      (let ((actual-version (query-value db "select version from SCHEMA_VERSION")))
        (unless (and (number? actual-version)
                     (or (and allow-higher-version (>= actual-version expected-version))
                         (= actual-version expected-version)))
          (disconnect db)
          (raise (db-exn-bad-version database-file expected-version actual-version)))))
    db))
