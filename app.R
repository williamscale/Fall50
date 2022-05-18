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
library(slickR)
library(jpeg)

# setwd('C:/Users/caler/Documents/MyProjects/Fall50')
# LoadToEnvironment <- function(RData, env=new.env()) {
#   load('./Data/fall50_2021.RData', env)
#   load('./Data/easytoweast_2020.RData', env)
#   return(env)
# }

load('./Data/fall50_2021.RData')

# User Interface ----------------------------------------------------------

ui <- navbarPage(

  title = 'Fall 50',
  theme = bs_theme(version = 4,
                   bootswatch = 'solar'),
  

## 2021 --------------------------------------------------------------------


tabPanel(title = '2021',
  tabsetPanel(type = 'tabs',
              tabPanel('Results',
                       fluidRow(column(2,
                                       h1('Harambe\'s Angels'),
                                       h2('October 23, 2021'),
                                       h5(HTML(paste0('31',
                                                      tags$sup('st'),
                                                      ' of 164 Coed Teams'))),
                                       p(strong('Logged Time:'),
                                         textOutput('TeamTime2021'),
                                         style = 'display: inline')),
                                tags$style('#TeamTime2021{display: inline}'),
                                column(4,
                                       br(),
                                       slickROutput('Images2021'))),
                       br(),
                       fluidRow(column(6,
                                       br(),
                                       leafletOutput('RaceMap2021'))),
                       br()),
              tabPanel('Team',
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = 'runner2021',
                                       label = 'Athlete',
                                       choices = list('Cale', 'Hallie',
                                                      'Katie', 'Laurence',
                                                      'Viet', 'Zack')),
                           width = 2),
                       mainPanel(textOutput('RunnerName2021'),
                                 br(),
                                 fluidRow(column(width = 4,
                                                 imageOutput('RunnerImg2021')),
                                          column(width = 4,
                                                 p(strong('Legs Run:'),
                                                   textOutput('RunnerLegs2021'),
                                                   style = 'display: inline; text-align: left'),
                                                 p(strong('Total Mileage:'),
                                                   textOutput('RunnerMileage2021'),
                                                   style = 'display: inline; text-align: left'),
                                                 p(strong('Average Pace:'),
                                                   textOutput('RunnerPace2021'),
                                                   style = 'display: inline; text-align: left'),
                                                 p(strong('Shoes:'),
                                                   textOutput('RunnerShoes2021'),
                                                   style = 'display: inline; text-align: left')
                                                 )),
                                 br(),
                                 fluidRow(column(width = 8,
                                                 plotOutput('Elevation2021'))),
                                 br(),
                                 fluidRow(column(width = 8,
                                                 leafletOutput('RunnerMap2021'))),
                                 tags$style('#RunnerName2021{font-size: 40px; 
                                            font-style: bold}',
                                            '#RunnerLegs2021{display: inline}',
                                            '#RunnerMileage2021{display: inline}',
                                            '#RunnerPace2021{display: inline}',
                                            '#RunnerShoes2021{display: inline}'
                                            )))))),

tabPanel(title = '2022')

)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # library(shiny)
  # library(bslib)
  # 
  # library(gpx)
  # library(dplyr)
  # library(ggplot2)
  # library(ggthemes)
  # library(mapview)
  # library(sf)
  # library(geosphere)
  # 
  # library(shinythemes)
  # 
  # library(lubridate)
  # library(leaflet)
  # library(purrr)
  
  # load('./Data/fall50_2021.RData')
  # load('./Data/easytoweast_2020.RData')

  runner.elevation.2021 <- reactive({
    re2021 <- subset(fall50.raw,
                     Runner %in% input$runner2021)
    return(re2021)
  })
  
  output$Elevation2021 <- renderPlot({
    
    if (input$runner2021 != 'Katie'){
    
    ggplot(data = runner.elevation.2021(),
            aes(x = Distance,
                y = Elevation)) +
        geom_point(aes(color = Leg,
                      group = 1),
                  size = 1.5) +
        ylim(550, 950) +
        xlab('Mileage') +
        ylab('Elevation') +
        theme_solarized_2()}
    else {
      ggplot(data = fall50.raw,
             aes(x = Distance.Cum,
                 y = Elevation)) +
        annotation_raster(pacifica,
                          xmin = -Inf,
                          xmax = Inf,
                          ymin = -Inf,
                          ymax = Inf) +
        geom_point(color = 'goldenrod',
                   size = 1.5) +
        ylim(550, 950) +
        xlab('Mileage') +
        ylab('Elevation') +
        theme_solarized_2()
    }
    
  })



  output$RaceMap2021 <- renderLeaflet({
    map.all <- mapview(cale,
                       xcol = 'Longitude',
                       ycol = 'Latitude',
                       # crs = 4326,
                       grid = FALSE,
                       cex = 2,
                       lwd = 0,
                       col.regions = '#17c3b2',
                       map.types = 'Esri.WorldImagery') +
      mapview(hallie,
              xcol = 'Longitude',
              ycol = 'Latitude',
              # crs = 4326,
              grid = FALSE,
              cex = 2,
              lwd = 0,
              col.regions = '#fe6d73',
              map.types = 'Esri.WorldImagery') +
      mapview(laurence,
              xcol = 'Longitude',
              ycol = 'Latitude',
              # crs = 4326,
              grid = FALSE,
              cex = 2,
              lwd = 0,
              col.regions = '#fef9ef',
              map.types = 'Esri.WorldImagery') +
      mapview(viet,
              xcol = 'Longitude',
              ycol = 'Latitude',
              # crs = 4326,
              grid = FALSE,
              cex = 2,
              lwd = 0,
              col.regions = '#227c9d',
              map.types = 'Esri.WorldImagery') +
      mapview(zack,
              xcol = 'Longitude',
              ycol = 'Latitude',
              # crs = 4326,
              grid = FALSE,
              cex = 2,
              lwd = 0,
              col.regions = '#ffcb77',
              map.types = 'Esri.WorldImagery')
    map.all@map
  })

  output$Images2021 <-
    renderSlickR({
      #change to explicit list?
      images2021 <- list.files('./Images/2021',
                         pattern = '.jpeg',
                         full.names = TRUE)
      # images2021 <- c('./Images/2021/ff_1.jpeg',
      #                 './Images/2021/ff_3.jpeg',
      #                 './Images/2021/ff_4.jpeg')
      slickR(images2021,
             height = '400px'
             # ,settings(dots = TRUE)
             )
    })
  
  output$TeamTime2021 <-
    renderText({
      as.character(round(seconds_to_period(fall50.raw$Time.Cum[nrow(fall50.raw)]), 0))
    })
  
  output$RunnerName2021 <-
    renderText({
      
      input$runner2021
      

    })
  
  output$RunnerLegs2021 <-
    renderText({
      
      if (input$runner2021 == 'Cale')
        {'2, 3'}
      
      else if (input$runner2021 == 'Hallie')
        {'10'}
      
      else if (input$runner2021 == 'Katie')
        {'-'}
      
      else if (input$runner2021 == 'Laurence')
        {'5, 9'}
      
      else if (input$runner2021 == 'Viet')
        {'1, 7'}
      
      else if (input$runner2021 == 'Zack')
        {'4, 6, 8'}
      
    })
  
  output$RunnerMileage2021 <-
    renderText({
      
      if (input$runner2021 == 'Cale')
        {round(leg23$Distance[nrow(leg23)], 1)}
      
      else if (input$runner2021 == 'Hallie')
        {round(leg10$Distance[nrow(leg10)], 1)}
      
      else if (input$runner2021 == 'Katie')
        {'~50'}
      
      else if (input$runner2021 == 'Laurence')
        {round(sum(leg5$Distance[nrow(leg5)],
                   leg9$Distance[nrow(leg9)]), 1)}
      
      else if (input$runner2021 == 'Viet')
        {round(sum(leg1$Distance[nrow(leg1)],
                   leg7$Distance[nrow(leg7)]), 1)}
      
      else if (input$runner2021 == 'Zack')
        {round(sum(leg4$Distance[nrow(leg4)],
                   leg6$Distance[nrow(leg6)],
                   leg8$Distance[nrow(leg8)]), 1)}
      
    })
  
  output$RunnerPace2021 <-
    renderText({
      
      if (input$runner2021 == 'Cale')
      {paste0(floor(leg23$Time[nrow(leg23)] / 60 /
                      leg23$Distance[nrow(leg23)]),
              ':',
              round(60 * (leg23$Time[nrow(leg23)]/ 60 /
                            leg23$Distance[nrow(leg23)] - floor(leg23$Time[nrow(leg23)] / 60 /
                      leg23$Distance[nrow(leg23)])), 0))
        }
      
      else if (input$runner2021 == 'Hallie')
      {paste0(floor(leg10$Time.Cum[nrow(leg10)] / 60 /
                      leg10$Distance[nrow(leg10)]),
              ':',
              round(60 * (leg10$Time.Cum[nrow(leg10)]/ 60 /
                            leg10$Distance[nrow(leg10)] - floor(leg10$Time.Cum[nrow(leg10)] / 60 /
                                                                  leg10$Distance[nrow(leg10)])), 0))}
      
      else if (input$runner2021 == 'Katie')
      {'~2:00'}
      
      else if (input$runner2021 == 'Laurence')
      {paste0(floor(sum(leg5$Time[nrow(leg5)],
                        leg9$Time[nrow(leg9)]) / 60 /
                      sum(leg5$Distance[nrow(leg5)],
                          leg9$Distance[nrow(leg9)])),
              ':',
              round(60 * (sum(leg5$Time[nrow(leg5)],
                              leg9$Time[nrow(leg9)]) / 60 /
                            sum(leg5$Distance[nrow(leg5)],
                                leg9$Distance[nrow(leg9)]) - floor(sum(leg5$Time[nrow(leg5)],
                                                                       leg9$Time[nrow(leg9)]) / 60 /
                                                                     sum(leg5$Distance[nrow(leg5)],
                                                                         leg9$Distance[nrow(leg9)]))), 0))}
      
      else if (input$runner2021 == 'Viet')
      {paste0(floor(sum(leg1$Time.Cum[nrow(leg1)],
                        leg7$Time.Cum[nrow(leg7)]) / 60 /
                      sum(leg1$Distance[nrow(leg1)],
                          leg7$Distance[nrow(leg7)])),
              ':',
              round(60 * (sum(leg1$Time.Cum[nrow(leg1)],
                              leg7$Time.Cum[nrow(leg7)]) / 60 /
                            sum(leg1$Distance[nrow(leg1)],
                                leg7$Distance[nrow(leg7)]) - floor(sum(leg1$Time.Cum[nrow(leg1)],
                                                                       leg7$Time.Cum[nrow(leg7)]) / 60 /
                                                                     sum(leg1$Distance[nrow(leg1)],
                                                                         leg7$Distance[nrow(leg7)]))), 0))}
      
      else if (input$runner2021 == 'Zack')
      {paste0(floor(sum(leg4$Time.Cum[nrow(leg4)],
           leg6$Time.Cum[nrow(leg6)],
           leg8$Time.Cum[nrow(leg8)]) / 60 /
          sum(leg4$Distance[nrow(leg4)],
              leg6$Distance[nrow(leg6)],
              leg8$Distance[nrow(leg8)])),
          ':',
          round(60 * (sum(leg4$Time.Cum[nrow(leg4)],
              leg6$Time.Cum[nrow(leg6)],
              leg8$Time.Cum[nrow(leg8)]) / 60 /
            sum(leg4$Distance[nrow(leg4)],
                leg6$Distance[nrow(leg6)],
                leg8$Distance[nrow(leg8)]) - floor(sum(leg4$Time.Cum[nrow(leg4)],
                                                 leg6$Time.Cum[nrow(leg6)],
                                                 leg8$Time.Cum[nrow(leg8)]) / 60 /
            sum(leg4$Distance[nrow(leg4)],
                leg6$Distance[nrow(leg6)],
                leg8$Distance[nrow(leg8)]))), 0))
          }
      
    })
  
  output$RunnerShoes2021 <-
    renderText({
      
      if (input$runner2021 == 'Cale')
        {'New Balance 880'}
      
      else if (input$runner2021 == 'Hallie')
        {'Saucony Endorphin Pro'}
      
      else if (input$runner2021 == 'Katie')
        {'Kodiak Boots'}
      
      else if (input$runner2021 == 'Laurence')
        {'Saucony'}
      
      else if (input$runner2021 == 'Viet')
        {'Brooks Ghost 12'}
      
      else if (input$runner2021 == 'Zack')
        {'New Balance 880'}
      
      })
  

  
  output$RunnerImg2021 <-
    renderImage({
      
      if (input$runner2021 == 'Cale')
        
        {runnerimg2021 = 'www/cale_ff_1.jpeg'}
      
      else if (input$runner2021 == 'Hallie')
        
        {runnerimg2021 = 'www/hallie_ff_1.jpeg'}
      
      else if (input$runner2021 == 'Katie')
        
        {runnerimg2021 = 'www/katie_ff_1.jpeg'}
      
      else if (input$runner2021 == 'Laurence')
        
        {runnerimg2021 = 'www/laurence_ff_2.jpeg'}
      
      else if (input$runner2021 == 'Viet')
        
        {runnerimg2021 = 'www/viet_ff_1.jpeg'}
      
      else if (input$runner2021 == 'Zack')
        
        {runnerimg2021 = 'www/zack_ff_3.jpeg'}
      
      list(src = runnerimg2021,
           # width = '60%',
           height = '100%')},
      deleteFile = FALSE)

  
  output$RunnerMap2021 <- 
    renderLeaflet({
      
      if (input$runner2021 == 'Cale')
        
        {mapview(cale,
                 xcol = 'Longitude',
                 ycol = 'Latitude',
                 grid = FALSE,
                 cex = 2,
                 lwd = 0,
                 col.regions = '#17c3b2',
                 map.types = 'Esri.WorldImagery')@map}
      
      else if (input$runner2021 == 'Hallie')
        
        {mapview(hallie,
                 xcol = 'Longitude',
                 ycol = 'Latitude',
                 grid = FALSE,
                 cex = 2,
                 lwd = 0,
                 col.regions = '#fe6d73',
                 map.types = 'Esri.WorldImagery')@map}
      
      else if (input$runner2021 == 'Laurence')
        
      {mapview(laurence,
               xcol = 'Longitude',
               ycol = 'Latitude',
               grid = FALSE,
               cex = 2,
               lwd = 0,
               col.regions = '#fef9ef',
               map.types = 'Esri.WorldImagery')@map}
      
      else if (input$runner2021 == 'Viet')
        
      {mapview(viet,
               xcol = 'Longitude',
               ycol = 'Latitude',
               grid = FALSE,
               cex = 2,
               lwd = 0,
               col.regions = '#227c9d',
               map.types = 'Esri.WorldImagery')@map}
      
      else if (input$runner2021 == 'Zack')
        
      {mapview(zack,
               xcol = 'Longitude',
               ycol = 'Latitude',
               grid = FALSE,
               cex = 2,
               lwd = 0,
               col.regions = '#ffcb77',
               map.types = 'Esri.WorldImagery')@map}
      
    })
}


shinyApp(ui = ui, server = server)
