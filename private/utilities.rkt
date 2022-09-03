#lang racket/base

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

(require racket/contract
         racket/port
         errortrace/errortrace-lib      ; for print-error-trace
         racket/file
         )

(provide/contract
 (log-exception (-> string? exn? any/c))
 (thread/log (->* ((-> any/c)) (#:name string?) any/c))
 (get-pref (-> symbol? (-> any/c) any/c))
 (put-pref (-> symbol? any/c any/c))
 (data-directory (-> path-string?)))

(provide map-widget-logger)

(provide dbg-printf)

;; Logger used by the map widget to send various notification messages.  The
;; application should define a log-receiver to receive and display the
;; messages.
(define map-widget-logger (make-logger 'map-widget (current-logger)))

;; When #t some debug messages are printed to the console (this is intended
;; for debugging this package and the value needs to be changed in the source
;; code.
(define print-dbg-messages #f)

;; Log a message to the console using printf, but only when
;; `print-dbg-messages` is #t
(define (dbg-printf . args)
  (when print-dbg-messages
    (apply printf args)))

;; Format an exception and log it to `map-widget-logger`.  `who` is a string
;; identifying who is responsible for that exception (this is also added to
;; the string that is sent to the logger)
(define (log-exception who e)
  ;; NOTE: 'print-error-trace' will only print a stack trace if the error
  ;; trace library is used.  To use it, remove all .zo files and run "racket
  ;; -l errortrace -t run.rkt"
  (let* ((message (if (exn? e) (exn-message e) e))
         (call-stack (if (exn? e)
                         (call-with-output-string
                          (lambda (o) (print-error-trace o e)))
                         "#<no call stack>"))
         (msg (format "~a: ~a ~a" who message call-stack)))
    (log-message map-widget-logger 'error #f msg)))

;; Start a thread for `thunk`, but install a toplevel handler which logs any
;; uncaught exceptions from `thunk`.
(define (thread/log thunk #:name [name "*unnamed*"])
  (thread
   (lambda ()
     (with-handlers
       ((exn:fail?
         (lambda (e) (log-message map-widget-logger
                                  'error #f (format "thread <~a>: ~a" name e)))))
       (thunk)))))

;; Return a stored preference named `name` -- this is just a wrapper around
;; `get-preference`
(define (get-pref name fail-thunk)
  (define application-get-pref
    (with-handlers
      ((exn:fail? (lambda (e) #f)))
      (dynamic-require 'the-application 'get-pref)))
  (if application-get-pref
      (application-get-pref name fail-thunk)
      (get-preference name fail-thunk 'timestamp)))

;; Store a preference value -- this is just a wrapper around put-preferences,
;; but handles one preference only.
(define (put-pref name value)
  (define application-put-pref
    (with-handlers
      ((exn:fail? (lambda (e) #f)))
      (dynamic-require 'the-application 'put-pref)))
  (if application-put-pref
      (application-put-pref name value)
      (put-preferences
       (list name) (list value)
       (lambda (p) (error 'lock-fail "Failed to get the pref file lock" p)))))

(define the-data-directory #f)

;; Return the default directory where the application will store its data
;; files.  This directory will be created if it does not exist.  This is used
;; to store the cache database for the map widget.
(define (data-directory)
  (unless the-data-directory
    (define application-data-directory
      (with-handlers
        ((exn:fail? (lambda (e) #f)))
        (dynamic-require 'the-application 'data-directory)))
    (if application-data-directory
        (set! the-data-directory (application-data-directory))
        (let ((dir (find-system-path 'pref-dir)))
          ;; dir might not exist, but make-directory* never fails
          (let ((pref-dir (build-path dir "map-widget")))
            (make-directory* pref-dir)
            (set! the-data-directory pref-dir)))))
  the-data-directory)
