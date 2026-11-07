# Prepare map geometry for the dashboard ------------------------------------
#
# Input : data-raw/us_states_simplified/  (simplified U.S. Census cartographic
#         boundary file, 2018, 1:20m)
#         data/state_regions.csv          (state -> outreach region lookup)
# Output: data/us_states.geojson          (one feature per state + region)
#         data/regions.geojson            (states dissolved into 10 regions)
#
# Alaska, Hawaii and Puerto Rico are moved into insets below the lower 48 so the
# whole country fits in one view. The dashboard draws no basemap tiles, so
# the insets read naturally and no third-party tile API is needed.
#
# Run from the project root:  Rscript scripts/prepare_geography.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

sf_use_s2(FALSE)  # planar operations on lon/lat are fine for this layout

states <- st_read("data-raw/us_states_simplified", quiet = TRUE) |>
  st_transform(4326) |>
  select(state_abbr = STUSPS, state_name = NAME)

regions_lookup <- read_csv("data/state_regions.csv", show_col_types = FALSE)

states <- states |>
  inner_join(regions_lookup |> select(state_abbr, region), by = "state_abbr")

# Move a state's geometry: scale around its bbox centre, then translate
reposition <- function(geom, scale, new_centre) {
  bb <- st_bbox(geom)
  centre <- c(mean(bb[c("xmin", "xmax")]), mean(bb[c("ymin", "ymax")]))
  moved <- (geom - centre) * scale + new_centre
  st_set_crs(moved, 4326)
}

ak <- states$state_abbr == "AK"
hi <- states$state_abbr == "HI"

# Alaska crosses the antimeridian: shift to 0-360 longitudes before moving
ak_geom <- st_shift_longitude(st_geometry(states[ak, ]))
st_geometry(states)[ak] <- reposition(ak_geom, scale = 0.35, new_centre = c(-117, 25.5))
st_geometry(states)[hi] <- reposition(st_geometry(states[hi, ]), scale = 1, new_centre = c(-104, 25))

# Puerto Rico sits just below the default view; nudge it up beside Florida
pr <- states$state_abbr == "PR"
st_geometry(states)[pr] <- reposition(st_geometry(states[pr, ]), scale = 1.5, new_centre = c(-75, 24.5))

states <- st_make_valid(states)

regions <- states |>
  group_by(region) |>
  summarise(.groups = "drop") |>
  st_make_valid()

# Label point for each region: point-on-surface of its largest polygon
largest_part <- function(g) {
  parts <- st_cast(st_sfc(g, crs = 4326), "POLYGON")
  parts[which.max(st_area(st_set_crs(parts, NA)))]  # planar area is enough to pick the largest
}
label_pts <- suppressWarnings(  # lon/lat warning is fine for label placement
  do.call(c, lapply(st_geometry(regions), largest_part)) |> st_point_on_surface()
)
regions$label_lng <- st_coordinates(label_pts)[, 1]
regions$label_lat <- st_coordinates(label_pts)[, 2]

unlink(c("data/us_states.geojson", "data/regions.geojson"))
st_write(states, "data/us_states.geojson", quiet = TRUE)
st_write(regions, "data/regions.geojson", quiet = TRUE)

message("Wrote ", nrow(states), " states and ", nrow(regions), " regions.")
