# Easy to Weast 2020
# Author: Cale Williams
# Last Updated: 05/12/2022

# ADMINISTRATIVE WORK -----------------------------------------------------

rm(list = ls())
set.seed(55)

setwd('C:/Users/caler/Documents/MyProjects/Fall50')

library(gpx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(mapview)
library(sf)
library(geosphere)

# aj1.raw <- read_gpx('./Data/EasyToWeast_2020/aj1.gpx')
# aj2.raw <- read_gpx('./Data/EasyToWeast_2020/aj2.gpx')
# cale1.raw <- read_gpx('./Data/EasyToWeast_2020/cale1.gpx')
# cale2.raw <- read_gpx('./Data/EasyToWeast_2020/cale2.gpx')
# hallie1.raw <- read_gpx('./Data/EasyToWeast_2020/hallie1.gpx')
# jake1.raw <- read_gpx('./Data/EasyToWeast_2020/jake1.gpx')
# karsten1.raw <- read_gpx('./Data/EasyToWeast_2020/karsten1.gpx')
# mark1.raw <- read_gpx('./Data/EasyToWeast_2020/mark1.gpx')
# sam1.raw <- read_gpx('./Data/EasyToWeast_2020/sam1.gpx')
# viet1.raw <- read_gpx('./Data/EasyToWeast_2020/viet1.gpx')
# zack1.raw <- read_gpx('./Data/EasyToWeast_2020/zack1.gpx')
# zack2.raw <- read_gpx('./Data/EasyToWeast_2020/zack2.gpx')

load('./Data/EasyToWeast_2020/data.RData')

# DATA PREP ---------------------------------------------------------------

## AJ 1 -------------------------------------------------------------------

aj1 <- aj1.raw$tracks[names(aj1.raw$tracks)][[1]]

aj1.time.total <- 43 * 60 + 52

aj1$Distance <- c(0,
                  cumsum(distHaversine(aj1[, c('Longitude',
                                               'Latitude')]))
                   * 3.28084 / 5280)

aj1.pace.avg <- aj1.time.total / aj1$Distance[nrow(aj1)] / 60

aj1$Elevation <- aj1$Elevation * 3.28084

aj1$Time.Cum <- cumsum(rep(aj1.time.total / nrow(aj1),
                           times = nrow(aj1)))

aj1.runner <- rep('AJ',
                   times = nrow(aj1))

## AJ 2 -------------------------------------------------------------------

aj2 <- aj2.raw$tracks[names(aj2.raw$tracks)][[1]]

aj2.time.total <- 45 * 60 + 12

aj2$Distance <- c(0,
                  cumsum(distHaversine(aj2[, c('Longitude',
                                               'Latitude')]))
                  * 3.28084 / 5280)

aj2.pace.avg <- aj2.time.total / aj2$Distance[nrow(aj2)] / 60

aj2$Elevation <- aj2$Elevation * 3.28084

aj2$Time.Cum <- cumsum(rep(aj2.time.total / nrow(aj2),
                           times = nrow(aj2)))

aj2.runner <- rep('AJ',
                  times = nrow(aj2))

## CALE 1 -----------------------------------------------------------------

cale1 <- cale1.raw$tracks[names(cale1.raw$tracks)][[1]]

cale1.time.total <- 45 * 60 + 48

cale1$Distance <- c(0,
                  cumsum(distHaversine(cale1[, c('Longitude',
                                               'Latitude')]))
                  * 3.28084 / 5280)

cale1.pace.avg <- cale1.time.total / cale1$Distance[nrow(cale1)] / 60

cale1$Elevation <- cale1$Elevation * 3.28084

cale1$Time.Cum <- cumsum(rep(cale1.time.total / nrow(cale1),
                           times = nrow(cale1)))

cale1.runner <- rep('Cale',
                  times = nrow(cale1))

## CALE 2 -----------------------------------------------------------------

cale2 <- cale2.raw$tracks[names(cale2.raw$tracks)][[1]]

cale2.time.total <- 46 * 60 + 17

cale2$Distance <- c(0,
                    cumsum(distHaversine(cale2[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

cale2.pace.avg <- cale2.time.total / cale2$Distance[nrow(cale2)] / 60

cale2$Elevation <- cale2$Elevation * 3.28084

cale2$Time.Cum <- cumsum(rep(cale2.time.total / nrow(cale2),
                             times = nrow(cale2)))

cale2.runner <- rep('Cale',
                    times = nrow(cale2))

## HALLIE 1 ---------------------------------------------------------------

hallie1 <- hallie1.raw$tracks[names(hallie1.raw$tracks)][[1]]

hallie1.time.total <- 95 * 60 + 14

hallie1$Distance <- c(0,
                  cumsum(distHaversine(hallie1[, c('Longitude',
                                               'Latitude')]))
                  * 3.28084 / 5280)

hallie1.pace.avg <- hallie1.time.total / hallie1$Distance[nrow(hallie1)] / 60

hallie1$Elevation <- hallie1$Elevation * 3.28084

hallie1$Time.Cum <- cumsum(rep(hallie1.time.total / nrow(hallie1),
                           times = nrow(hallie1)))

hallie1.runner <- rep('Hallie',
                  times = nrow(hallie1))

## JAKE 1 -----------------------------------------------------------------

jake1 <- jake1.raw$tracks[names(jake1.raw$tracks)][[1]]

jake1.time.total <- 40 * 60 + 23

jake1$Distance <- c(0,
                      cumsum(distHaversine(jake1[, c('Longitude',
                                                       'Latitude')]))
                      * 3.28084 / 5280)

jake1.pace.avg <- jake1.time.total / jake1$Distance[nrow(jake1)] / 60

jake1$Elevation <- jake1$Elevation * 3.28084

jake1$Time.Cum <- cumsum(rep(jake1.time.total / nrow(jake1),
                               times = nrow(jake1)))

jake1.runner <- rep('Jake',
                      times = nrow(jake1))

## KARSTEN 1 --------------------------------------------------------------

karsten1 <- karsten1.raw$tracks[names(karsten1.raw$tracks)][[1]]

karsten1.time.total <- 40 * 60 + 23

karsten1$Distance <- c(0,
                    cumsum(distHaversine(karsten1[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

karsten1.pace.avg <- karsten1.time.total / karsten1$Distance[nrow(karsten1)] / 60

karsten1$Elevation <- karsten1$Elevation * 3.28084

karsten1$Time.Cum <- cumsum(rep(karsten1.time.total / nrow(karsten1),
                             times = nrow(karsten1)))

karsten1.runner <- rep('Karsten',
                    times = nrow(karsten1))

## MARK 1 -----------------------------------------------------------------

mark1 <- mark1.raw$tracks[names(mark1.raw$tracks)][[1]]

mark1.time.total <- 32 * 60 + 16

mark1$Distance <- c(0,
                    cumsum(distHaversine(mark1[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

mark1.pace.avg <- mark1.time.total / mark1$Distance[nrow(mark1)] / 60

mark1$Elevation <- mark1$Elevation * 3.28084

mark1$Time.Cum <- cumsum(rep(mark1.time.total / nrow(mark1),
                             times = nrow(mark1)))

mark1.runner <- rep('Mark',
                    times = nrow(mark1))

## SAM 1 ------------------------------------------------------------------

sam1 <- sam1.raw$tracks[names(sam1.raw$tracks)][[1]]

sam1.time.total <- 41 * 60 + 1

sam1$Distance <- c(0,
                    cumsum(distHaversine(sam1[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

sam1.pace.avg <- sam1.time.total / sam1$Distance[nrow(sam1)] / 60

sam1$Elevation <- sam1$Elevation * 3.28084

sam1$Time.Cum <- cumsum(rep(sam1.time.total / nrow(sam1),
                             times = nrow(sam1)))

sam1.runner <- rep('Sam',
                    times = nrow(sam1))

## VIET 1 -----------------------------------------------------------------

viet1 <- viet1.raw$tracks[names(viet1.raw$tracks)][[1]]

viet1.time.total <- 45 * 60 + 1

viet1$Distance <- c(0,
                   cumsum(distHaversine(viet1[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

viet1.pace.avg <- viet1.time.total / viet1$Distance[nrow(viet1)] / 60

viet1$Elevation <- viet1$Elevation * 3.28084

viet1$Time.Cum <- cumsum(rep(viet1.time.total / nrow(viet1),
                            times = nrow(viet1)))

viet1.runner <- rep('Viet',
                   times = nrow(viet1))

## ZACK 1 -----------------------------------------------------------------

zack1 <- zack1.raw$tracks[names(zack1.raw$tracks)][[1]]

zack1.time.total <- 50 * 60 + 8

zack1$Distance <- c(0,
                    cumsum(distHaversine(zack1[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

zack1.pace.avg <- zack1.time.total / zack1$Distance[nrow(zack1)] / 60

zack1$Elevation <- zack1$Elevation * 3.28084

zack1$Time.Cum <- cumsum(rep(zack1.time.total / nrow(zack1),
                             times = nrow(zack1)))

zack1.runner <- rep('Zack',
                    times = nrow(zack1))

## ZACK 2 -----------------------------------------------------------------

zack2 <- zack2.raw$tracks[names(zack2.raw$tracks)][[1]]

zack2.time.total <- 36 * 60 + 40

zack2$Distance <- c(0,
                    cumsum(distHaversine(zack2[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

zack2.pace.avg <- zack2.time.total / zack2$Distance[nrow(zack2)] / 60

zack2$Elevation <- zack2$Elevation * 3.28084

zack2$Time.Cum <- cumsum(rep(zack2.time.total / nrow(zack2),
                             times = nrow(zack2)))

zack2.runner <- rep('Zack',
                    times = nrow(zack2))

## SUMMARIZE DATA ---------------------------------------------------------

etw.raw <- data.frame(Longitude = c(aj1$Longitude, aj2$Longitude,
                                       cale1$Longitude, cale2$Longitude,
                                       hallie1$Longitude, jake1$Longitude,
                                    karsten1$Longitude,
                                       mark1$Longitude, sam1$Longitude,
                                       viet1$Longitude, zack1$Longitude,
                                    zack2$Longitude),
                         Latitude = c(aj1$Latitude, aj2$Latitude,
                                      cale1$Latitude, cale2$Latitude,
                                      hallie1$Latitude, jake1$Latitude,
                                      karsten1$Latitude,
                                      mark1$Latitude, sam1$Latitude,
                                      viet1$Latitude, zack1$Latitude,
                                      zack2$Latitude),
                         Distance = c(aj1$Distance, aj2$Distance,
                                      cale1$Distance, cale2$Distance,
                                      hallie1$Distance, jake1$Distance,
                                      karsten1$Distance,
                                      mark1$Distance, sam1$Distance,
                                      viet1$Distance, zack1$Distance,
                                      zack2$Distance),
                         Time = c(aj1$Time.Cum, aj2$Time.Cum,
                                  cale1$Time.Cum, cale2$Time.Cum,
                                  hallie1$Time.Cum, jake1$Time.Cum,
                                  karsten1$Time.Cum,
                                  mark1$Time.Cum, sam1$Time.Cum,
                                  viet1$Time.Cum, zack1$Time.Cum,
                                  zack2$Time.Cum),
                         Elevation = c(aj1$Elevation, aj2$Elevation,
                                       cale1$Elevation, cale2$Elevation,
                                       hallie1$Elevation, jake1$Elevation,
                                       karsten1$Elevation,
                                       mark1$Elevation, sam1$Elevation,
                                       viet1$Elevation, zack1$Elevation,
                                       zack2$Elevation),
                         Runner = c(aj1.runner, aj2.runner, cale1.runner,
                                    cale2.runner, hallie1.runner, jake1.runner,
                                    karsten1.runner,
                                    mark1.runner, sam1.runner, viet1.runner,
                                    zack1.runner, zack2.runner))
 
aj.raw.ew <- data.frame(Longitude = c(aj1$Longitude, aj2$Longitude),
                       Latitude = c(aj1$Latitude, aj2$Latitude))
cale.raw.ew <- data.frame(Longitude = c(cale1$Longitude, cale2$Longitude),
                       Latitude = c(cale1$Latitude, cale2$Latitude))
hallie.raw.ew <- data.frame(Longitude = hallie1$Longitude,
                       Latitude = hallie1$Latitude)
jake.raw.ew <- data.frame(Longitude = jake1$Longitude,
                         Latitude = jake1$Latitude)
karsten.raw.ew <- data.frame(Longitude = karsten1$Longitude,
                       Latitude = karsten1$Latitude)
mark.raw.ew <- data.frame(Longitude = mark1$Longitude,
                       Latitude = mark1$Latitude)
sam.raw.ew <- data.frame(Longitude = sam1$Longitude,
                       Latitude = sam1$Latitude)
viet.raw.ew <- data.frame(Longitude = viet1$Longitude,
                      Latitude = viet1$Latitude)
zack.raw.ew <- data.frame(Longitude = c(zack1$Longitude, zack1$Longitude),
                       Latitude = c(zack1$Latitude, zack1$Latitude))

etw <- st_as_sf(etw.raw,
                   coords = c('Longitude', 'Latitude'),
                   crs = 4326)
aj.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'AJ', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
cale.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Cale', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
hallie.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Hallie', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
jake.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Jake', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
karsten.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Karsten', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
mark.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Mark', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
sam.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Sam', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
viet.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Viet', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
zack.ew <- st_as_sf(etw.raw[etw.raw$Runner == 'Zack', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)

# 
# # PLOT --------------------------------------------------------------------
# 
# ## GGPLOT -----------------------------------------------------------------
# 
# # ggplot(data = fall50.raw,
# #        aes(x = Longitude,
# #            y = Latitude,
# #            color = Runner)) +
# #   geom_point() +
# #   theme_solarized_2()
# 
# ## MAPVIEW ----------------------------------------------------------------
# 
# map.all.runners <- mapview(etw,
#                            xcol = 'Longitude',
#                            ycol = 'Latitude',
#                            # crs = 4326,
#                            grid = FALSE,
#                            cex = 1,
#                            lwd = 0,
#                            col.regions = '#fcffa4',
#                            map.types = 'Esri.WorldImagery')
# map.all.runners
# 
map.aj.ew <- mapview(aj.ew,
                    xcol = 'Longitude',
                    ycol = 'Latitude',
                    # crs = 4326,
                    grid = FALSE,
                    cex = 2,
                    lwd = 0,
                    col.regions = 'red',
                    map.types = 'Esri.WorldImagery')
map.cale.ew <- mapview(cale.ew,
                  xcol = 'Longitude',
                  ycol = 'Latitude',
                  # crs = 4326,
                  grid = FALSE,
                  cex = 2,
                  lwd = 0,
                  col.regions = 'blue',
                  map.types = 'Esri.WorldImagery')
map.hallie.ew <- mapview(hallie.ew,
                    xcol = 'Longitude',
                    ycol = 'Latitude',
                    # crs = 4326,
                    grid = FALSE,
                    cex = 2,
                    lwd = 0,
                    col.regions = 'yellow',
                    map.types = 'Esri.WorldImagery')
map.jake.ew <- mapview(jake.ew,
                      xcol = 'Longitude',
                      ycol = 'Latitude',
                      # crs = 4326,
                      grid = FALSE,
                      cex = 2,
                      lwd = 0,
                      col.regions = 'green',
                      map.types = 'Esri.WorldImagery')
map.karsten.ew <- mapview(karsten.ew,
                      xcol = 'Longitude',
                      ycol = 'Latitude',
                      # crs = 4326,
                      grid = FALSE,
                      cex = 2,
                      lwd = 0,
                      col.regions = 'purple',
                      map.types = 'Esri.WorldImagery')
map.mark.ew <- mapview(mark.ew,
                      xcol = 'Longitude',
                      ycol = 'Latitude',
                      # crs = 4326,
                      grid = FALSE,
                      cex = 2,
                      lwd = 0,
                      col.regions = 'pink',
                      map.types = 'Esri.WorldImagery')
map.sam.ew <- mapview(sam.ew,
                    xcol = 'Longitude',
                    ycol = 'Latitude',
                    # crs = 4326,
                    grid = FALSE,
                    cex = 2,
                    lwd = 0,
                    col.regions = 'orange',
                    map.types = 'Esri.WorldImagery')
map.viet.ew <- mapview(viet.ew,
                    xcol = 'Longitude',
                    ycol = 'Latitude',
                    # crs = 4326,
                    grid = FALSE,
                    cex = 2,
                    lwd = 0,
                    col.regions = 'cyan',
                    map.types = 'Esri.WorldImagery')
map.zack.ew <- mapview(zack.ew,
                    xcol = 'Longitude',
                    ycol = 'Latitude',
                    # crs = 4326,
                    grid = FALSE,
                    cex = 2,
                    lwd = 0,
                    col.regions = 'violet',
                    map.types = 'Esri.WorldImagery')

map.all.ew <- map.aj.ew + map.cale.ew + map.hallie.ew + map.jake.ew + map.karsten.ew +
  map.mark.ew + map.sam.ew + map.viet.ew + map.zack.ew
# map.all
# 
# # ELEVATION ---------------------------------------------------------------

aj.dist.ew <- c(aj1$Distance, aj2$Distance + aj1$Distance[nrow(aj1)])
cale.dist.ew <- c(cale1$Distance, cale2$Distance + cale1$Distance[nrow(cale1)])
zack.dist.ew <- c(zack1$Distance, zack2$Distance + zack1$Distance[nrow(zack1)])

flat.elev <- data.frame(Distance = c(aj.dist.ew, cale.dist.ew,
                                     hallie1$Distance, jake1$Distance,
                                     karsten1$Distance, mark1$Distance, sam1$Distance,
                                     viet1$Distance, zack.dist.ew),
                        Elevation = c(aj1$Elevation - aj1$Elevation[1],
                                      aj2$Elevation - aj2$Elevation[1],
                                      cale1$Elevation - cale1$Elevation[1],
                                      cale2$Elevation - cale2$Elevation[1],
                                      hallie1$Elevation - hallie1$Elevation[1],
                                      jake1$Elevation - jake1$Elevation[1],
                                      karsten1$Elevation - karsten1$Elevation[1],
                                      mark1$Elevation - mark1$Elevation[1],
                                      sam1$Elevation - sam1$Elevation[1],
                                      viet1$Elevation - viet1$Elevation[1],
                                      zack1$Elevation - zack1$Elevation[1],
                                      zack2$Elevation - zack2$Elevation[1]),
                        Runner = c(aj1.runner, aj2.runner, cale1.runner,
                                   cale2.runner, hallie1.runner, jake1.runner,
                                   karsten1.runner, mark1.runner, sam1.runner,
                                   viet1.runner, zack1.runner, zack2.runner))

ggplot(data = flat.elev,
       aes(x = Distance,
           y = Elevation,
           color = Runner)) +
  geom_path(size = 2) +
  xlab('Distance') +
  ylab('Elevation Change') +
  theme_solarized_2()

