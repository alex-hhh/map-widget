#lang scribble/manual
@require[@for-label[map-widget
                    geoid
                    pict
                    racket/class
                    racket/base
                    racket/draw
                    racket/gui]]

@title{A Racket GUI Widget to display maps based on OpenStreetMap tiles }
@author{Alex Harsányi}

This module contains a widget to display a map based on OpenStreetMap data,
plus additional markers and GPS tracks.  The widget allows zooming and paning
with the mouse, as well as programatically.  Two interfaces are available:
@racket[map-widget%] is based on a @racket[canvas%] implementation, and can be
added to other GUI elements, while @racket[map-snip%] implements the map as a
@racket[snip%] object which can be inserted into a @racket[pasteboard%].

The package was initially written to support the data visualisation needs for
@hyperlink["https://github.com/alex-hhh/ActivityLog2"]{ActivityLog2}. That
application uses all the features implemented by this package, and shows many
complex usage scenarios of the map widget.

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

@defmethod*[([(auto-resize-to-fit [flag boolean?]) any/c]
             [(auto-resize-to-fit) boolean?])]{

  Set a flag whether the map should be automatically resized to fit after
  adding a track, point cloud or marker.  This can be used in interactive
  programs which add several tracks in sequence.

  The flag will be automatically cleared when the user moves the map or zooms
  it.

}

@defmethod*[([(begin-edit-sequence) any/c]
            [(end-edit-sequence) any/c])]{

  These two methods can be used to group together several operations on the
  map widget, avoiding intermediate refresh calls which might cause
  flickering.

  The @racket[begin-edit-sequence] and @racket[end-edit-sequence] calls can be
  nested.

}

@defmethod*[([(add-layer (layer (is-a/c layer<%>))) any/c]
            [(remove-layer (layer-name (or/c symbol? integer?))) any/c])]{

  Add or remove a layer from the map, layers contain tracks (lists of
  waypoints), individual points, named locations or point clouds.

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


@defmethod*[([(auto-resize-to-fit [flag boolean?]) any/c]
             [(auto-resize-to-fit) boolean?])]{

  Set a flag whether the map should be automatically resized to fit after
  adding a track, point cloud or marker.  This can be used in interactive
  programs which add several tracks in sequence.

  The flag will be automatically cleared when the user moves the map or zooms
  it.

}

@defmethod*[([(begin-edit-sequence) any/c]
            [(end-edit-sequence) any/c])]{

  These two methods can be used to group together several operations on the
  map widget, avoiding intermediate refresh calls which might cause
  flickering.

  The @racket[begin-edit-sequence] and @racket[end-edit-sequence] calls can be
  nested.

}

@defmethod*[([(add-layer (layer (is-a/c layer<%>))) any/c]
            [(remove-layer (layer-name (or/c symbol? integer?))) any/c])]{

  Add or remove a layer from the map, layers contain tracks (lists of
  waypoints), individual points, named locations or point clouds.

}

}

@subsection{Layers}

@definterface[layer<%> ()]{

  Common interface provided by all map layers, layers are used to show lines,
  points, markers and point-clouds on the map.

  @defmethod[(get-name) (or/c symbol? string?)]{

    Return the name of this layer.

  }

  @defmethod*[([(get-zorder) (between/c 0 1)]
              [(set-zorder (zorder (between/c 0 1))) any/c])]{

    Set or get the drawing order of this layer.  Layers with lower
    @italic{z-order} values are drawn earlier, and this are "below" layers
    with higher @italic{z-order} values.

  }

}

@defclass[lines-layer% object% (layer<%>)]{

  A map layer that draws a sequence of waypoints as lines.  Use
  @racket[lines-layer] and @racket[line-layer] to create line layers.

  @defmethod[(set-pen (p (is-a/c pen%))) any/c]{

    Set the pen used to draw lines in this layer.

  }

}

@defproc[(line-layer (name (or/c symbol? integer?))
                     (waypoints (sequence/c (vector/c real? real?)))
                     (#:pen pen (is-a/c pen%) 'default-line-pen)
                     (#:zorder zorder (between/c 0 1) 0.2))
         (is-a/c lines-layer%)]{

  Creates a @racket[lines-layer%] object named @racket[name] from
  @racket[waypoints] which is a sequence of latitude and longitude
  coordinates.

}

@defproc[(lines-layer (name (or/c symbol? integer?))
                      (tracks (listof (sequence/c (vector/c real? real?))))
                      (#:pen pen (is-a/c pen%) 'default-line-pen)
                      (#:zorder zorder (between/c 0 1) 0.5))
         (is-a/c lines-layer%)]{

  Same as @racket[line-layer], but the layer each waypoint sequence in
  @racket[tracks] will be drawn as a separate line.  This can be used to
  create a single layer containing several and possibly disconnected lines.

}


@defclass[markers-layer% object% (layer<%>)]{

  A layer that can be used to display one or more labeled markers on the map.
  Use @racket[markers-layer] to create marker layers.

}

@defproc[(markers-layer (name (or/c symbol? integer?))
                        (markers (listof (list/c (vector/c real? real?)
                                                 string?
                                                 (or/c -1 1)
                                                 (or/c string? (is-a/c color%)))))
                        (#:zorder zorder (between/c 0 1) 0.5))
         (is-a/c markers-layer%)]{

  Create a @racket[markers-layer%] named @racket[name] from the list of
  @racket[markers].  Each marker is specified by a list of 4 elements: a
  latitude/longitude coordinate, the label to be shown on the map, the
  location of the label -1 to the left, 1 to the right and a color, specified
  either as a color name or a @racket[color%] object.

}

@defclass[points-layer% object% (layer<%>)]{

  Create a map layer which draws individual points.  Individual points can be
  highlighted when the user moves the mouse over them and a tooltip,
  represented as a @racket[pict] can be displayed for each point.  Use
  @racket[points-layer] to create a points layer.

}


@defproc[(points-layer (name (or/c symbol? integer?))
                       (points (lisfof (vector/c real? real?)))
                       (#:zorder zorder (between/c 0 1) 0.3)
                       (#:pen pen (is-a/c pen%) 'default-points-pen)
                       (#:brush brush (is-a/c brush%) 'default-points-brush)
                       (#:size size (>/c 0) 10)
                       (#:hlpen hlpen (is-a/c pen%) 'default-points-hlpen)
                       (#:hlbrush hlbrush (is-a/c brush%) 'default-points-hlbrush)
                       (#:hlsize hlsize (>/c 0) 15)
                       (#:hover-callback hover-callback (-> exact-nonnegative-integer? (or/c #f pict?)) 'default-hover-callback))
         (is-a/c points-layer%)]{

  Create a a @racket[points-layer%] named @racket[name] from a list of
  @racket[points] which are latitude/longitude positions.

  Each point is drawn asa a disk (circle) using @racket[pen] and
  @racket[brush] and it is of the specified @racket[size].  When the user
  moves the mouse over a point, it is highlighted, that is, drawn using
  @racket[hlpen] and @racket[hlbrush] and @racket[hlsize].

  @racket[hover-callback] represends an optional callback, whcih is invoked
  with the index of the point that is highlighted.  It can return either
  @racket[#f], in which case nothing is displayed, or a @racket[pict] object
  which is displayed next to the point -- this can be used to display
  additional information about the point.  The default callback always returns
  @racket[#f].

}

@defclass[point-cloud-layer% object% (layer<%>)]{

  A point-cloud layer shows a large amount (millions) of points on a map,
  grouped together and colored using a color map to show density of data
  around a location.

  @defmethod[(get-point-count) (values integer? integer?)]{

    Return the number of points in the point cloud as two values: the number
    of points that have been processed and available for drawing, and the
    total number of points that were added to the point cloud, this last value
    includes points that are not yet processed, since point processing happens
    in a separate OS thread (place).

  }

  @defmethod[(clear) any/c]{

    Clear all the points in the point cloud.

  }

  @defmethod[(add-points (points (or/c (listof? integer?)
                                       (listof? (list/c real? real?))
                                       (listof? (vector/c real? real?))))
            (#:format fmt (or/c 'lat-lng 'geoids 'ordered-geoids))) any/c]{

    Add some GPS points to the point cloud.  The points can be specified
    either as latitude/longitude pairs or as geoids (ordered or not).  This
    method can be called multiple times, allowing for streaminh in data
    points.

    @racket[fmt] specifies the format of input data: @racket['lat-lng] means
    the data is a sequence of latitude/longitude pairs, @racket['geoids] means
    the data is a list of geoids (possibly unordered), while
    @racket['ordered-geoids] specifies that the data is an ordered list of
    geoids.

    For large amonts of data, ordered geoids are the fastest to process, but
    it is only worthwhile using it if the data is already ordered (e.g stored
    as such in a database).  If you only have latitude/longitude pairs,
    converting them to geoids and sorting them will not make it faster.

    For mode information on geoids, see the @other-manual['geoid] package.

  }

  @defmethod[(set-color-map (cm (listof? (or/c (list real? real? real?)
                                               string?
                                               (is-a?/c color%)))))
             any/c]{

    Set the color map used for rendering point clouds, the color map is a list
    of colors, the first will be used for the least amount of points in a
    location, while the last color for coloring locations with most points.
    In-between colors will be used for locations of intermediate number of
    points.

  }
}

@defproc[(point-cloud-layer (name (or/c symbol? integer?))
                            (#:zorder zorder (between/c 0 1) 0.4)
                            (#:color-map color-map (or/c #f (listof (list/c real? real? real?))) #f))
         (is-a/c point-cloud-layer%)]{

  Create a new point cloud layer with a default color map.

}

@defclass[current-location-layer% object% (layer<%>)]{

  A map layer that displays a single location on the map, marked by a circle.
  The location can be updated by the user, and can be used to represent a way
  to associate some other data with a location on the map.  For example, an
  application might show an elevation plot and, when the user hovers over the
  plot, the location over the plot is shown on the map using this layer type.

  @defmethod[(current-location (location (or/c (vector/c real? real?) #f))) any/c]{

    Set the location to be shown on the map, or clears it @racket[#f] is used
    as the location.

  }

  @defmethod[(track-current-location (flag boolean?)) any/c]{

    If flag is @racket[#t], the map will be automatically panned so that the
    location speficied by @racket[set-current-location] is always in the
    middle of the map, when flag is @racket[#f], this functionality is
    disabled

  }

}

@defproc[(current-location-layer (name (or/c symbol? string))
                                 (#:track-current-location? track-current-location? boolean? #f)
                                 (#:zorder zorder (between/c 0 1) 0.6)
                                 (#:pen pen (is-a/c pen%) 'default-current-location-pen)
                                 (#:brush brush (is-a/c brush%) 'default-current-location-brush)
                                 (#:size size (>/c 0) 24))
         (is-a/c current-location-layer%)]{

  Create a new @racket[current-location-layer%] with the specified
  @racket[name] and drawing parameters.

  }

@subsection{Tile Providers}

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

@defproc[(get-tile-providers) (listof string?)]{

  Returns a list of available tile providers.

  By default only the Open Street Map tile server is available.  To use
  @hyperlink["http://thunderforest.com/"]{Thunderforest} tiles, you will need
  to obtain an API key and set the @tt{AL2TFAPIKEY} environment variable with
  that API key.

}

@defproc[(current-tile-provider) string?]{

  Return the name of the current tile provider.

}

@defproc[(set-current-tile-provider [name string?]) any/c]{

  Set the name of the tile provider to use.  All instances of
  @racket[map-widget%] will use the same tile provider.

  When this function is called, the selected tile provider is also stored int
  the racket preference file and this tile provider will be used each time the
  application is run, util this function is called with a different tile
  provider name.

}
