#lang scribble/manual
@require[@for-label[map-widget
                    racket/class
                    racket/base]]

@title{map-widget}
@author{Alex Harsanyi}

@defmodule[map-widget]

@defclass[map-widget% object% ()]{

Display a map based on OpenStreetMap data, plus additional markers and GPS
tracks.  The widget allows zooming and paning with the mouse, as well as
programatically.

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
      (is-a?/c panel%) (is-a?/c pane%))])]{
Construct a new widget.
}

@defmethod[(set-zoom-level [level exact-nonnegative-integer?]) any/c]{ Set the
zoom level for the displayed map.  The zoom level is a value between 1 (zoomed
out) and 16 (zoomed in).  The zoom level can also be changed by the user using
the mousewheel, so once set, it is not guaranteed to remain at the same value}

@defmethod[(get-zoom-level) exact-nonnegative-integer?]{ Return the current
zoom level for the map.}

@defmethod[(clear-items) any/c]{ Remove all overlay items (tracks and markers)
from the widget}

@defmethod[(add-track (track sequence?) (group (or/c symbol? integer? #f)))
any/c]{ Add a new GPS track to the map.  The GPS track is a sequence of
points, each point is a vector of at least two elements: the latitude and
longitude of the point.

A track can be addded to a group, which is a symbol used to identify related
tracks.  The drawing color and z-order can be specified for a group.}

@defmethod[(add-marker (position (vector/c flonum? flonum?)) (name string?)
(direction (or/c -1 1)) (color (is-a?/c color%))) any/c]{Add a marker to the
map at the specified position.  "name" will be used for the label}

@defmethod[(set-current-location (location (or/c (vector/c flonum? flonum?)
#f))) any/c]{Display a cicle marker at the specified GPS point.  The marker
can be moved around by calling this method with a new position or it can be
cleared by specifying @racket[#f] as the location}

@defmethod[(set-track-current-location (flag boolean?)) any/c]{If flag is
@racket[#t], the map will be automatically panned so that the location
speficied by @racket[set-current-location] is always in the middle of the map,
when flag is @racket[#f], this functionality is disabled}

@defmethod[(set-group-pen (group (or/c #f integer? symbol?)) (pen (is-a?/c
pen%))) any/c]{Set the pen used to draw tracks in the specified track group.
When the track group is @racket[#f], the pen for the tracks with no track
group is updated.}

@defmethod[(set-group-zorder (group (or/c #f integer? symbol?)) (zorder
positive?))  any/c]{Set the order in which tracks in the specified trackgroup
will be drawn.}

@defmethod[(delete-group (group (or/c #f integer? symbol?))) any/c]{Delete all
GPS tracks within the specified group}

@defmethod[(center-map (group (or/c #f integer? symbol?))) any/c]{Center the
map around the specified track group, or around all tracks if group is
@racket[#f]}

@defmethod[(resize-to-fit (group (or/c #f symbol?))) any/c]{Center and set the
zoom level of the map such that the specified track group fits on the visible
part of the map canvas.  If group is @racket[#f], all the tracks will fit on
the map canvas}

}
