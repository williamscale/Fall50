# Fall 50 2021
# Author: Cale Williams
# Last Updated: 05/18/2022

# ADMINISTRATIVE WORK -----------------------------------------------------

rm(list = ls())
set.seed(55)

setwd('C:/Users/caler/Documents/MyProjects/Fall50')

library(shiny)
library(bslib)

library(gpx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(mapview)
library(sf)
library(geosphere)

library(shinythemes)

library(lubridate)
library(leaflet)
library(purrr)

# leg1.raw <- read_gpx('./Data/Fall50_2021/leg1.gpx')
# leg23.raw <- read_gpx('./Data/Fall50_2021/leg2-3.gpx')
# leg4.raw <- read_gpx('./Data/Fall50_2021/leg4.gpx')
# leg5.raw <- read_gpx('./Data/Fall50_2021/leg5.gpx')
# leg6.raw <- read_gpx('./Data/Fall50_2021/leg6.gpx')
# leg7.raw <- read_gpx('./Data/Fall50_2021/leg7.gpx')
# leg8.raw <- read_gpx('./Data/Fall50_2021/leg8.gpx')
# leg9.raw <- read_gpx('./Data/Fall50_2021/leg9.gpx')
# leg10.raw <- read_gpx('./Data/Fall50_2021/leg10.gpx')

load('./Data/Fall50_2021/data.RData')

# DATA PREP ---------------------------------------------------------------

## LEG 1 ------------------------------------------------------------------

leg1 <- leg1.raw$tracks[names(leg1.raw$tracks)][[1]]

leg1.time.total <- 49 * 60 + 7

leg1$Distance <- c(0,
                   cumsum(distHaversine(leg1[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg1.pace.avg <- leg1.time.total / leg1$Distance[nrow(leg1)] / 60

leg1$Elevation <- leg1$Elevation * 3.28084

leg1$Time.Cum <- cumsum(rep(leg1.time.total / nrow(leg1),
                            times = nrow(leg1)))

leg1.runner <- rep('Viet',
                   times = nrow(leg1))

## LEG 2-3 ----------------------------------------------------------------

leg23 <- leg23.raw$tracks[names(leg23.raw$tracks)][[1]]

leg23$Time <- as.numeric(leg23$Time)
leg23$Time <- leg23$Time - leg23$Time[1]

leg23.time.total <- leg23[nrow(leg23), 'Time']

leg23$Distance <- c(0,
                    cumsum(distHaversine(leg23[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

leg23.pace.avg <- leg23.time.total / leg23$Distance[nrow(leg23)] / 60

leg23$Elevation <- leg23$Elevation * 3.28084

leg23.runner <- rep('Cale',
                    times = nrow(leg23))

## LEG 4 ------------------------------------------------------------------

leg4 <- leg4.raw$tracks[names(leg4.raw$tracks)][[1]]

leg4.time.total <- 32 * 60 + 26

leg4$Distance <- c(0,
                   cumsum(distHaversine(leg4[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg4.pace.avg <- leg4.time.total / leg4$Distance[nrow(leg4)] / 60

leg4$Elevation <- leg4$Elevation * 3.28084

leg4$Time.Cum <- cumsum(rep(leg4.time.total / nrow(leg4),
                            times = nrow(leg4)))

leg4.runner <- rep('Zack',
                   times = nrow(leg4))

## LEG 5 ------------------------------------------------------------------

leg5 <- leg5.raw$tracks[names(leg5.raw$tracks)][[1]]

leg5$Time <- as.numeric(leg5$Time)
leg5$Time <- leg5$Time - leg5$Time[1]

leg5.time.total <- leg5[nrow(leg5), 'Time']

leg5$Distance <- c(0,
                   cumsum(distHaversine(leg5[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg5.pace.avg <- leg5.time.total / leg5$Distance[nrow(leg5)] / 60

leg5$Elevation <- leg5$Elevation * 3.28084

leg5.runner <- rep('Laurence',
                   times = nrow(leg5))

## LEG 6 ------------------------------------------------------------------

leg6 <- leg6.raw$tracks[names(leg6.raw$tracks)][[1]]

leg6.time.total <- 24 * 60 + 38

leg6$Distance <- c(0,
                   cumsum(distHaversine(leg6[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg6.pace.avg <- leg6.time.total / leg6$Distance[nrow(leg6)] / 60

leg6$Elevation <- leg6$Elevation * 3.28084

leg6$Time.Cum <- cumsum(rep(leg6.time.total / nrow(leg6),
                            times = nrow(leg6)))

leg6.runner <- rep('Zack',
                   times = nrow(leg6))

## LEG 7 ------------------------------------------------------------------

leg7 <- leg7.raw$tracks[names(leg7.raw$tracks)][[1]]

leg7.time.total <- 33 * 60 + 28

leg7$Distance <- c(0,
                   cumsum(distHaversine(leg7[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg7.pace.avg <- leg7.time.total / leg7$Distance[nrow(leg7)] / 60

leg7$Elevation <- leg7$Elevation * 3.28084

leg7$Time.Cum <- cumsum(rep(leg7.time.total / nrow(leg7),
                            times = nrow(leg7)))

leg7.runner <- rep('Viet',
                   times = nrow(leg7))

## LEG 8 ------------------------------------------------------------------

leg8 <- leg8.raw$tracks[names(leg8.raw$tracks)][[1]]

leg8.time.total <- 44 * 60 + 33

leg8$Distance <- c(0,
                   cumsum(distHaversine(leg8[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg8.pace.avg <- leg8.time.total / leg8$Distance[nrow(leg8)] / 60

leg8$Elevation <- leg8$Elevation * 3.28084

leg8$Time.Cum <- cumsum(rep(leg8.time.total / nrow(leg8),
                            times = nrow(leg8)))

leg8.runner <- rep('Zack',
                   times = nrow(leg8))

## LEG 9 ------------------------------------------------------------------

leg9 <- leg9.raw$tracks[names(leg9.raw$tracks)][[1]]

leg9$Time <- as.numeric(leg9$Time)
leg9$Time <- leg9$Time - leg9$Time[1]

leg9.time.total <- leg9[nrow(leg9), 'Time']

leg9$Distance <- c(0,
                   cumsum(distHaversine(leg9[, c('Longitude',
                                                 'Latitude')]))
                   * 3.28084 / 5280)

leg9.pace.avg <- leg9.time.total / leg9$Distance[nrow(leg9)] / 60

leg9$Elevation <- leg9$Elevation * 3.28084

leg9.runner <- rep('Laurence',
                   times = nrow(leg9))

## LEG 10 -----------------------------------------------------------------

leg10 <- leg10.raw$tracks[names(leg10.raw$tracks)][[1]]

leg10.time.total <- 35 * 60 + 42

leg10$Distance <- c(0,
                    cumsum(distHaversine(leg10[, c('Longitude',
                                                   'Latitude')]))
                    * 3.28084 / 5280)

leg10.pace.avg <- leg10.time.total / leg10$Distance[nrow(leg10)] / 60

leg10$Elevation <- leg10$Elevation * 3.28084

leg10$Time.Cum <- cumsum(rep(leg10.time.total / nrow(leg10),
                             times = nrow(leg10)))

leg10.runner <- rep('Hallie',
                    times = nrow(leg10))

## SUMMARIZE DATA ---------------------------------------------------------

fall50.raw <- data.frame(Longitude = c(leg1$Longitude, leg23$Longitude,
                                       leg4$Longitude, leg5$Longitude,
                                       leg6$Longitude, leg7$Longitude,
                                       leg8$Longitude, leg9$Longitude,
                                       leg10$Longitude),
                         Latitude = c(leg1$Latitude, leg23$Latitude,
                                      leg4$Latitude, leg5$Latitude,
                                      leg6$Latitude, leg7$Latitude,
                                      leg8$Latitude, leg9$Latitude,
                                      leg10$Latitude),
                         Distance = c(leg1$Distance, leg23$Distance,
                                      leg4$Distance, leg5$Distance,
                                      leg6$Distance, leg7$Distance,
                                      leg8$Distance, leg9$Distance,
                                      leg10$Distance),
                         Time = c(leg1$Time.Cum, leg23$Time, leg4$Time.Cum,
                                  leg5$Time, leg6$Time.Cum, leg7$Time.Cum,
                                  leg8$Time.Cum, leg9$Time, leg10$Time.Cum),
                         Elevation = c(leg1$Elevation, leg23$Elevation,
                                       leg4$Elevation, leg5$Elevation,
                                       leg6$Elevation, leg7$Elevation,
                                       leg8$Elevation, leg9$Elevation,
                                       leg10$Elevation),
                         Runner = c(leg1.runner, leg23.runner, leg4.runner,
                                    leg5.runner, leg6.runner, leg7.runner,
                                    leg8.runner, leg9.runner, leg10.runner),
                         Leg = c(rep('Leg 1', length(leg1.runner)),
                                 rep('Leg 2-3', length(leg23.runner)),
                                 rep('Leg 4', length(leg4.runner)),
                                 rep('Leg 5', length(leg5.runner)),
                                 rep('Leg 6', length(leg6.runner)),
                                 rep('Leg 7', length(leg7.runner)),
                                 rep('Leg 8', length(leg8.runner)),
                                 rep('Leg 9', length(leg9.runner)),
                                 rep('Leg 10', length(leg10.runner))))

viet.raw <- data.frame(Longitude = c(leg1$Longitude, leg7$Longitude),
                       Latitude = c(leg1$Latitude, leg7$Latitude))
cale.raw <- data.frame(Longitude = leg23$Longitude,
                       Latitude = leg23$Latitude)
zack.raw <- data.frame(Longitude = c(leg4$Longitude, leg6$Longitude,
                                     leg8$Longitude),
                       Latitude = c(leg4$Latitude, leg6$Latitude,
                                    leg8$Latitude))
laurence.raw <- data.frame(Longitude = c(leg5$Longitude, leg9$Longitude),
                           Latitude = c(leg5$Latitude, leg9$Latitude))
hallie.raw <- data.frame(Longitude = leg10$Longitude,
                         Latitude = leg10$Latitude)


fall50 <- st_as_sf(fall50.raw,
                   coords = c('Longitude', 'Latitude'),
                   crs = 4326)
viet <- st_as_sf(fall50.raw[fall50.raw$Runner == 'Viet', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
cale <- st_as_sf(fall50.raw[fall50.raw$Runner == 'Cale', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
zack <- st_as_sf(fall50.raw[fall50.raw$Runner == 'Zack', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
laurence <- st_as_sf(fall50.raw[fall50.raw$Runner == 'Laurence', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)
hallie <- st_as_sf(fall50.raw[fall50.raw$Runner == 'Hallie', ],
                 coords = c('Longitude', 'Latitude'),
                 crs = 4326)

# PLOT --------------------------------------------------------------------

## GGPLOT -----------------------------------------------------------------

# ggplot(data = fall50.raw,
#        aes(x = Longitude,
#            y = Latitude,
#            color = Runner)) +
#   geom_point() +
#   theme_solarized_2()

## MAPVIEW ----------------------------------------------------------------

# map.all.runners <- mapview(fall50,
#                            xcol = 'Longitude',
#                            ycol = 'Latitude',
#                            # crs = 4326,
#                            grid = FALSE,
#                            cex = 1,
#                            lwd = 0,
#                            col.regions = '#fcffa4',
#                            map.types = 'Esri.WorldImagery')
# map.all.runners

# map.viet <- mapview(viet,
#                     xcol = 'Longitude',
#                     ycol = 'Latitude',
#                     # crs = 4326,
#                     grid = FALSE,
#                     cex = 2,
#                     lwd = 0,
#                     col.regions = '#227c9d',
#                     map.types = 'Esri.WorldImagery')
# 
# map.cale <- mapview(cale,
#                     xcol = 'Longitude',
#                     ycol = 'Latitude',
#                     # crs = 4326,
#                     grid = FALSE,
#                     cex = 2,
#                     lwd = 0,
#                     col.regions = '#17c3b2',
#                     map.types = 'Esri.WorldImagery')
# 
# map.zack <- mapview(zack,
#                     xcol = 'Longitude',
#                     ycol = 'Latitude',
#                     # crs = 4326,
#                     grid = FALSE,
#                     cex = 2,
#                     lwd = 0,
#                     col.regions = '#ffcb77',
#                     map.types = 'Esri.WorldImagery')
# 
# map.laurence <- mapview(laurence,
#                         xcol = 'Longitude',
#                         ycol = 'Latitude',
#                         # crs = 4326,
#                         grid = FALSE,
#                         cex = 2,
#                         lwd = 0,
#                         col.regions = '#fef9ef',
#                         map.types = 'Esri.WorldImagery')
# 
# map.hallie <- mapview(hallie,
#                       xcol = 'Longitude',
#                       ycol = 'Latitude',
#                       # crs = 4326,
#                       grid = FALSE,
#                       cex = 2,
#                       lwd = 0,
#                       col.regions = '#fe6d73',
#                       map.types = 'Esri.WorldImagery')
# 
# map.all <- map.viet + map.cale + map.zack + map.laurence + map.hallie

# ELEVATION ---------------------------------------------------------------

fall50.raw$Distance.Cum <- cumsum(c(0, distHaversine(p1 = fall50.raw[1:2]) * 3.28084 / 5280))

elevation.plot <- ggplot(data = fall50.raw,
       aes(x = Distance.Cum,
           y = Elevation)) +
  geom_line(aes(color = Runner,
                group = 1),
            size = 1.5) +
  ylim(550, 950) +
  xlab('Mileage') +
  ylab('Elevation') +
  theme_solarized_2()

# TIME --------------------------------------------------------------------

leg23.time.cum <- leg1.time.total + leg23$Time
leg4.time.cum <- leg23.time.cum[length(leg23.time.cum)] + leg4$Time.Cum
leg5.time.cum <- leg4.time.cum[length(leg4.time.cum)] + leg5$Time
leg6.time.cum <- leg5.time.cum[length(leg5.time.cum)] + leg6$Time.Cum
leg7.time.cum <- leg6.time.cum[length(leg6.time.cum)] + leg7$Time.Cum
leg8.time.cum <- leg7.time.cum[length(leg7.time.cum)] + leg8$Time.Cum
leg9.time.cum <- leg8.time.cum[length(leg8.time.cum)] + leg9$Time
leg10.time.cum <- leg9.time.cum[length(leg9.time.cum)] + leg10$Time.Cum

fall50.raw$Time.Cum <- c(leg1$Time.Cum, leg23.time.cum, leg4.time.cum,
                         leg5.time.cum, leg6.time.cum, leg7.time.cum,
                         leg8.time.cum, leg9.time.cum, leg10.time.cum)

time.plot <- ggplot(data = fall50.raw,
       aes(x = Time.Cum / 60 / 60,
           y = Distance.Cum)) +
  geom_line(aes(color = Runner,
                group = 1),
            size = 1.5) +
  geom_hline(yintercept = 50,
             color = '#002b36',
             linetype = 'dashed',
             size = 1) +
  xlab('Time [Hour]') +
  ylab('Mileage') +
  theme_solarized_2()

# SHOES -------------------------------------------------------------------


