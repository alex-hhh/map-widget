-- SPDX-License-Identifier: LGPL-3.0-or-later
-- ostmtc-schema.sql -- database schema for caching OSM tile data
--
-- This file is part of map-widget -- A Racket GUI Widget to display maps
-- based on OpenStreetMap tiles
--
-- Copyright (c) 2018 Alex Harsanyi <AlexHarsanyi@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
-- License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

create table SCHEMA_VERSION(version integer);
insert into SCHEMA_VERSION(version) values(1);

create table TILE_CACHE (
  zoom_level integer not null,
  x_coord integer not null,
  y_coord integer not null,
  timestamp integer not null, -- unix timestamp when the tile was retrieved
  url text not null,          -- URL from which tile was retrieved
  data blob not null);

create unique index IX0_TILE_CACHE on TILE_CACHE(zoom_level, x_coord, y_coord);

-- Local Variables:
-- sql-product: sqlite
-- End:
