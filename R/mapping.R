#' adjust center + scale for google map plotting
#'
#' @param x \code{sf} object
#' @param map \code{ggmap} object
#' @importFrom sf st_transform st_coordinates st_crs
#'
#' @return two-column matrix of locations from \code{x} projected to match \code{map}
googlemap_proj <- function(x, map){
  scale_min <- get_googlemap_min_scale(map)
  x_wm <- st_transform(x, crs = st_crs("EPSG:3857"))
  x_gm <- t(t(st_coordinates(x_wm)) - scale_min$min) * scale_min$scale
  return(x_gm)
}

#' Figure out scale and centering of google map by transforming reported lat long bounding box back to web mercator
#'
#' @param map \code{ggmap} object
#' @importFrom sf st_as_sf st_transform st_coordinates st_crs
#'
#' @return scale (factor by which web mercator has been shrunk) and min (leftmost, bottom most coordinate of rectangle)
get_googlemap_min_scale <- function(map){
  ## web mercator proj code
  # crs_wm <- st_crs(paste(
  #   "+proj=merc +a=6378137 +b=6378137",
  #   "+lat_ts=0.0 +lon_0=0 +x_0=0.0 +y_0=0",
  #   "+k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
  # ))
  crs_wm <- st_crs("EPSG:3857")
  ## get bb in long/lat from google map object  
  bb_map <- as.numeric(attr(map, "bb"))
  ## I think these checks are unnecessary 20220623HRS
  if (bb_map[1] < -90) {
    bb_map[1] <- -180 - bb_map[1]
  }
  if (bb_map[2] < -180) {
    bb_map[2] <- 360 + bb_map[2]
  }
  if (bb_map[3] > 90) {
    bb_map[3] <- 180 - bb_map[3]
  }
  if (bb_map[4] > 180) {
    bb_map[4] <- bb_map[4] - 360
  }
  ## make sf obj and transform to match web mercator    
  bb_sf <- st_as_sf(as.data.frame(t(matrix(bb_map, 2, 2))), coords = c("V2", "V1"), 
                    crs = "+proj=latlong")
  bb_wm <- st_transform(bb_sf, crs = crs_wm)
  bb <- st_coordinates(bb_wm)
  min <- bb[1, ]
  scale <- 1280 / (apply(bb, 2, diff))
  return(list(min = min, scale = scale))
}