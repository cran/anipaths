## ----setup, echo = F----------------------------------------------------------
knitr::opts_chunk$set(message = F)

## ----tails--------------------------------------------------------------------
library(anipaths)
vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
vultures_paths <- vultures[format(vultures$POSIX, "%Y")
                           %in% c(2009, 2010, 2011), ] ## limit attention to 2009, 2010, 2011
delta.t <- "week"
#tails
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier",
              uncertainty.type = 1)

## ----eval = F-----------------------------------------------------------------
#  ## remove files used to generate animation
#  system("rm -r index.html css js images")

## ----multiple_bg, eval = F----------------------------------------------------
#  background <- geodata::world(path = ".")
#  sf::st_crs(background)$proj4string ## matches default projection in animate_paths()
#  delta.t <- "week"
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                uncertainty.type = 5,
#                tail.colors = "unique",
#                tail.wd = 0.7,
#                background = background)
#  system("rm -r gadm") ## remove geodata map from machine

## ----eval = F-----------------------------------------------------------------
#  ## remove files used to generate animation
#  system("rm -r index.html css js images")

## ----multi_whales, eval = F---------------------------------------------------
#  whales$POSIX <- as.POSIXct(whales$timestamp)
#  whales_paths <- whales[format(whales$POSIX, "%Y") %in% c(2014),]
#  delta.t <- 3600*12
#  animate_paths(paths = whales_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                uncertainty.type = 5,
#                tail.colors = "unique",
#                tail.wd = 0.5)

## ----eval = F-----------------------------------------------------------------
#  ## remove files used to generate animation
#  system("rm -r index.html css js images")

## ----blur---------------------------------------------------------------------
#blur ellipses
delta.t <- "week"
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier",
              uncertainty.type = "blur",
              tail.colors = "red") 

## ----eval = F-----------------------------------------------------------------
#  ## remove files used to generate animation
#  system("rm -r index.html css js images")

## ----blue_whales--------------------------------------------------------------
library(ggmap)
whales$POSIX <- as.POSIXct(whales$timestamp, tz = "GMT")
whales_paths <- whales[format(whales$POSIX, "%Y") %in% 2015,]
delta.t <- 3600*12
animate_paths(paths = whales_paths,
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier",
              uncertainty.type = "blur",
              tail.colors = "green")

## -----------------------------------------------------------------------------
## remove files used to generate animation
system("rm -r index.html css js images")

