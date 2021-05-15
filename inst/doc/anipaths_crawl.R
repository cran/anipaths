## ----setup, include = FALSE---------------------------------------------------

options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  eval = F
)


## ----libraries, message=FALSE, warning=FALSE----------------------------------
#  library(anipaths)
#  library(tidyverse)
#  library(magrittr)

## ----subset-------------------------------------------------------------------
#  vultures %<>%
#    mutate(POSIX = as.POSIXct(timestamp, tz = "UTC"))
#  
#  vultures_spring11 <- vultures %>%
#    filter(POSIX > as.POSIXct("2011-04-05", origin = "1970-01-01") &
#             POSIX < as.POSIXct("2011-05-05", origin = "1970-01-01")
#           &
#             (individual.local.identifier %in%
#             c('Argentina', 'Domingo', 'La Pampa', 'Whitey', 'Young Luro'))
#    )

## ----crawl with tails---------------------------------------------------------
#  animate_paths(paths = vultures_spring11,
#                delta.t = "day",
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                interpolation_type = "crawl",
#                simulation = TRUE)

## ----crawl with tails hi res--------------------------------------------------
#  animate_paths(paths = vultures_spring11,
#                delta.t = 3600 * 4,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                interpolation_type = "crawl",
#                simulation = TRUE)

## ----blur---------------------------------------------------------------------
#  animate_paths(paths = vultures_spring11,
#                delta.t = 3600 * 6,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                interpolation_type = "crawl",
#                crawl.plot.type = "blur.tail")

## ----make background----------------------------------------------------------
#  background <- list(center = c(-70, -20),
#                     zoom = 4,
#                     maptype = "satellite")

## ----use background-----------------------------------------------------------
#  library(ggmap)
#  animate_paths(paths = vultures_spring11,
#                delta.t = 3600 * 6,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background,
#                interpolation_type = "crawl", simulation = TRUE)

## ----Whitney subset-----------------------------------------------------------
#  vultures_Whitey <- vultures_spring11 %>%
#    filter(individual.local.identifier == "Whitey")

## ----single path--------------------------------------------------------------
#  animate_paths(paths = vultures_Whitey,
#                delta.t = 3600 * 4,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                interpolation_type = "crawl", background = T,
#                simulation = TRUE, main = "Whitey")

## ----remove files, include = F------------------------------------------------
#  system("rm -r js; rm -r css; rm -r images; rm index.html")

