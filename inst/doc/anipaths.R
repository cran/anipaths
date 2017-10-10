## ---- fig.show='hold', fig.cap="test", eval = F--------------------------
#  library(anipaths)
#  vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ]
#  delta.t <- "day"
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier")

## ----map_logical, eval = F-----------------------------------------------
#  delta.t <- "week"
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = TRUE)

## ----map_google, eval = F------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009:2010, ]
#  delta.t <- 3600*24*2 ## number of seconds in two days
#  background <- list(location = c(-90, 10),
#                     zoom = 3,
#                     maptype = "satellite")
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background)

## ----map_simple, eval = F------------------------------------------------
#  delta.t <- "week"
#  background <- rworldmap::getMap(resolution = "coarse")
#  sp::proj4string(background) ## matches default in animate_paths()
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background)

