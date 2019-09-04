#lang scribble/manual
@require[@for-label[map-widget
                    racket/class
                    racket/base
                    racket/draw
                    racket/gui]]

@title{A Racket GUI Widget to display maps based on OpenStreetMap tiles }
@author{Alex Harsányi}

This module contains a widget to display a map based on OpenStreetMap data,
plus additional markers and GPS tracks.  The widget allows zooming and paning
with the mouse, as well as programatically.  Two interfaces are available,
@racket[map-widget%] is based on a @racket[canvas%] implementation and can be
added to other GUI elements, while @racket[map-snip%] implements the map as a
@racket[snip%] object which can be inserted into a @racket[pasteboard%].

There is a tutorial on how to use this widget, it is available
@hyperlink["https://alex-hhh.github.io/2018/06/a-racket-gui-widget-to-display-maps-based-on-openstreetmap-tiles.html"]{here}.

@section{Map Widget Concepts}

@subsection{Tile Servers}

The map widget displays a map as a collection of tiles, each tile is a square
bitmap 256 pixels in width and height.  These tiles are downloaded from tile
servers as needed and they are stored locally in a persistent cache on disk.
The Open Street Map tile server is always available, and it is used by the map
widget by default.  In addition tiles from the
@hyperlink["http://thunderforest.com/"]{Thunderforest} service can also be
used, but this requires an API key to be set.  If you have an API key, and you
can get a developer one for free, set the @tt{AL2TFAPIKEY} environment
variable to contain the API key.  The API key is compiled into the code and
the environment variable is not needed if you distribute a built application
using the map widget.

You can set the tile provider to use by calling
@racket[set-current-tile-provider] and you can get a list of available tile
providers by calling @racket[get-tile-provider-names].

@subsection{GPS Tracks and Groups}

The map widget can overlay GPS tracks onto the map.  These are sequences of
GPS points, which represent a continuous track: the map widget will draw aa
single line connecting all the points.  Multiple tracks can be added to the
map and all would be drawn independently.

GPS tracks can form groups.  A group is a number of symbol which is specified
to @racket[add-track] method.  Groups are useful because the map widget allows
specifying how the track is drawn (the @racket[pen%]) as well as the Z-order
at group level.  A group can be used to draw a GPS track that is discontinous.

@section{Map Widget Reference}

@defmodule[map-widget]

@defclass[map-widget% object% ()]{

A widget to display map plus GPS tracks and markers.

@defconstructor[([parent (or/c (is-a?/c frame%)
                               (is-a?/c dialog%)
                               (is-a?/c panel%)
                               (is-a?/c pane%))]
                [position (or/c #f (vector/c flonum? flonum?)) #f])]{

  Construct a new widget.  @racket[position] specifies the initial position
  shown in the center of the map, it is a vector containing the latitude and
  longitude.  If @racket[position] is @racket[#f], a defatult position will be
  used.

}

@defmethod*[([(zoom-level [level exact-nonnegative-integer?]) any/c]
             [(zoom-level) exact-nonnegative-integer?])]{

  Set or get the zoom level for the displayed map.  The zoom level is a value
  between 1 (zoomed out) and 16 (zoomed in).  The zoom level can also be
  changed by the user using the mousewheel, so once set, it is not guaranteed
  to remain at the same value.

}

@defmethod*[([(show-map-layer (show? boolean?)) any/c]
             [(show-map-layer) boolean?])]{

  Set or get whether to show the map tile layer not not.  If the map tile
  layer is hidden, only the traks and markers will be visible.

}

@defmethod[(clear) any/c]{
  Remove all overlay items (tracks and markers) from the widget
}

@defmethod[(add-track (track sequence?) (group (or/c symbol? integer? #f))) any/c]{

  Add a new GPS track to the map.  The GPS track is a sequence of points, each
  point is a vector of at least two elements: the latitude and longitude of
  the point.

  A track can be addded to a group, which is a symbol used to identify related
  tracks.  The @racket[pen%] and z-order can be specified for a group.

}

@defmethod[(add-marker (position (vector/c flonum? flonum?))
                       (name string?)
                       (direction (or/c -1 1))
                        (color (is-a?/c color%))) any/c]{

  Add a marker to the map at the specified position.  "name" will be used for
  the label.

}

@defmethod[(current-location (location (or/c (vector/c flonum? flonum?) #f))) any/c]{

  Display a cicle marker at the specified GPS point.  The marker can be moved
  around by calling this method with a new position or it can be cleared by
  specifying @racket[#f] as the location.

}

@defmethod[(track-current-location (flag boolean?)) any/c]{

  If flag is @racket[#t], the map will be automatically panned so that the
  location speficied by @racket[set-current-location] is always in the middle
  of the map, when flag is @racket[#f], this functionality is disabled

}

@defmethod[(set-group-pen (group (or/c #f integer? symbol?)) (pen (is-a?/c pen%))) any/c]{

  Set the pen used to draw tracks in the specified track group.  When the
  track group is @racket[#f], the default pen is updated, this is used for
  tracks that have no group defined (i.e. the group is @racket[#f])

}

@defmethod[(set-group-zorder (group (or/c #f integer? symbol?)) (zorder positive?)) any/c]{

  Set the order in which tracks in the specified trackgroup will be drawn.
  Tracks with a smaller Z-order will be drawn before tracks with a larger
  Z-order. The draw order for tracks with the same Z-order value is
  unspecified.

}

@defmethod[(delete-group (group (or/c #f integer? symbol?))) any/c]{

  Delete all GPS tracks within the specified group

}

@defmethod[(center-map (group (or/c #f integer? symbol?))) any/c]{

  Center the map around the specified track group, or around all tracks if
  group is @racket[#f]

}

@defmethod[(move-to (position (vector/c flonum? flonum?))) any/c]{

  Center the map around the specified @racket[position], a vector containing
  the latitude and longitude.

}

@defmethod[(resize-to-fit (group (or/c #f symbol?))) any/c]{

  Center and set the zoom level of the map such that the specified track group
  fits on the visible part of the map canvas.  If group is @racket[#f], all the
  tracks will fit on the map canvas.

}

}

@defclass[map-snip% snip% ()]{

A @racket[snip%] to display map plus GPS tracks and markers, which can be
inserted in a @racket[pasteboard%].

@defconstructor[([position (or/c #f (vector/c flonum? flonum?)) #f]
                [track sequence? #f]
                [width positive? 600]
                [height positive? 300])]{

  Construct a new map snip.  @racket[position] specifies the initial position
  shown in the center of the map, it is a vector containing the latitude and
  longitude.  If @racket[position] is @racket[#f], a defatult position will be
  used.

  @racket[track] specifies an initial track to add to the map, if @racket[#f],
  no tracks will be added.

  @racket[width] and @racket[height] represent the initial dimensions for the
  @racket[snip%].

}

@defmethod*[([(zoom-level [level exact-nonnegative-integer?]) any/c]
             [(zoom-level) exact-nonnegative-integer?])]{

  Set or get the zoom level for the displayed map.  The zoom level is a value
  between 1 (zoomed out) and 16 (zoomed in).  The zoom level can also be
  changed by the user using the mousewheel, so once set, it is not guaranteed
  to remain at the same value.

}

@defmethod*[([(show-map-layer (show? boolean?)) any/c]
             [(show-map-layer) boolean?])]{

  Set or get whether to show the map tile layer not not.  If the map tile
  layer is hidden, only the traks and markers will be visible.

}

@defmethod[(clear) any/c]{
  Remove all overlay items (tracks and markers) from the widget
}

@defmethod[(add-track (track sequence?) (group (or/c symbol? integer? #f))) any/c]{

  Add a new GPS track to the map.  The GPS track is a sequence of points, each
  point is a vector of at least two elements: the latitude and longitude of
  the point.

  A track can be addded to a group, which is a symbol used to identify related
  tracks.  The @racket[pen%] and z-order can be specified for a group.

}

@defmethod[(add-marker (position (vector/c flonum? flonum?))
                       (name string?)
                       (direction (or/c -1 1))
                        (color (is-a?/c color%))) any/c]{

  Add a marker to the map at the specified position.  "name" will be used for
  the label.

}

@defmethod[(current-location (location (or/c (vector/c flonum? flonum?) #f))) any/c]{

  Display a cicle marker at the specified GPS point.  The marker can be moved
  around by calling this method with a new position or it can be cleared by
  specifying @racket[#f] as the location.

}

@defmethod[(track-current-location (flag boolean?)) any/c]{

  If flag is @racket[#t], the map will be automatically panned so that the
  location speficied by @racket[set-current-location] is always in the middle
  of the map, when flag is @racket[#f], this functionality is disabled

}

@defmethod[(set-group-pen (group (or/c #f integer? symbol?)) (pen (is-a?/c pen%))) any/c]{

  Set the pen used to draw tracks in the specified track group.  When the
  track group is @racket[#f], the default pen is updated, this is used for
  tracks that have no group defined (i.e. the group is @racket[#f])

}

@defmethod[(set-group-zorder (group (or/c #f integer? symbol?)) (zorder positive?)) any/c]{

  Set the order in which tracks in the specified trackgroup will be drawn.
  Tracks with a smaller Z-order will be drawn before tracks with a larger
  Z-order. The draw order for tracks with the same Z-order value is
  unspecified.

}

@defmethod[(delete-group (group (or/c #f integer? symbol?))) any/c]{

  Delete all GPS tracks within the specified group

}

@defmethod[(center-map (group (or/c #f integer? symbol?))) any/c]{

  Center the map around the specified track group, or around all tracks if
  group is @racket[#f]

}

@defmethod[(move-to (position (vector/c flonum? flonum?))) any/c]{

  Center the map around the specified @racket[position], a vector containing
  the latitude and longitude.

}

@defmethod[(resize-to-fit (group (or/c #f symbol?))) any/c]{

  Center and set the zoom level of the map such that the specified track group
  fits on the visible part of the map canvas.  If group is @racket[#f], all the
  tracks will fit on the map canvas.

}

}


@defproc[(get-tile-provider-names) (listof string?)]{

  Returns a list of available tile providers.

  By default only the Open Street Map tile server is available.  To use
  @hyperlink["http://thunderforest.com/"]{Thunderforest} tiles, you will need
  to obtain an API key and set the @tt{AL2TFAPIKEY} environment variable with
  that API key.

}

@defproc[(current-tile-provider-name) string?]{

  Return the name of the current tile provider

}

@defproc[(set-current-tile-provider [name string?]) any/c]{

  Set the name of the tile provider to use.  All instances of
  @racket[map-widget%] will use the same tile provider.

  When this function is called, the selected tile provider is also stored int
  the racket preference file and this tile provider will be used each time the
  application is run, util this function is called with a different tile
  provider name

}
