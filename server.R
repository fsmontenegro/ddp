library(shiny)
library(ggmap)
library(leaflet)
library(rCharts)
library(stringr)
library(dplyr)
library(tidyr)

# Load Programs from CSV and dataprep - adjust dates, string formatting, ...
# load up 't1' data frame with all programs
source('./dataprep.R')


# Data for Community Centers - created by hand based on OpenMap data
commcenters <- read.csv("./commcenters.csv",stringsAsFactors = FALSE)

# From http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
# Convert Degrees to Radians
deg2rad <- function(deg) return(deg*pi/180)

# From http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
# Use Haversine formula for distance calculation
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  #print(paste("in gcd",a))
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

# Adjust table to add radian coordinates for distance calculations
commcenters$lat.r <- deg2rad(commcenters$lat)
commcenters$lon.r <- deg2rad(commcenters$lon)




#
# Main Shiny loop
#
shinyServer(
    function(input,output) {


# reactive input - get coordinates for home address
      home_coord <- reactive({
        suppressMessages(geocode(input$address,source = "google"))
      })


# Reactive function 1 - calculate distance between home and each community center and filter on distance
      filtered_locations <- reactive({
        home_lat <- as.double(deg2rad(home_coord()[2]))
        home_lon <- as.double(deg2rad(home_coord()[1]))

        for(i in 1:nrow(commcenters)) {
          commcenters$dist[i] <- gcd.hf(home_lon,home_lat,commcenters$lon.r[i],commcenters$lat.r[i])
        }
        filter(commcenters,dist<{input$distance})
      })
#
# Reactive function - filter programs by child age, day of week, distance to home
      filtered_programs <- reactive({

        # Figure out start and end times

        timestart <- paste({input$timeofday[1]},":00",sep="")
        timeend <- paste({input$timeofday[2]},":00",sep="")
        starttime <- as.POSIXct(timestart,format="%H:%M",origin="2015-01-01")
        endtime <- as.POSIXct(timeend,format="%H:%M",origin="2015-01-01")

        t1 %>%
        filter(((F_Start.Date-{input$age})/30) >= F_AgeStart) %>%
        filter(((F_Start.Date-{input$age})/30) <= F_AgeEnd) %>%
        filter(Location %in% filtered_locations()$Name) %>%
        filter(F_TimeStart>=starttime & F_TimeEnd<=endtime) %>%
        filter(F_Day %in% {input$days}) %>%
          select(-starts_with("F_"))    # Don't display columns used in calculations
      })

# High level Map with Community Centers highlighted
      map <- leaflet(commcenters) %>%
        addTiles() %>%
        addMarkers(lng=~long,lat=~lat,popup=~Name)

# Summary text
# for a child born on <date> (aprox. <x> months old at start of programs) these are the viable
# courses at community centres with a <> km radius of <address>

      message <- reactive({
        paste("For a child born on ",{input$age}," (approximately",sep = '')
      })

# Shiny Output blocks
      output$oaddress <- renderPrint({input$address})
      output$odistance <- renderPrint({input$distance})
      output$odob <- renderPrint({input$age})
      output$odays <- renderPrint({input$days})
      output$otimeofday <- renderPrint({input$timeofday})
      output$ocoords <- renderPrint(home_coord())            # DEBUG: my coordinates
      output$oqueries <- renderPrint(as.numeric(geocodeQueryCheck()))
      output$omap <- renderLeaflet(map)                      # High-level map
      output$ocomm <- renderDataTable(filtered_locations())  # Locations less than distance
      output$ofprog <- renderDataTable(filtered_programs())  # Programs within all constraints
      output$ot1 <- renderDataTable(t1)

# Observer for drawing circle around chosen address
      observe({
        leafletProxy("omap",data=home_coord()) %>%
          clearShapes() %>%
          addCircles(lng=home_coord()$lon,lat=home_coord()$lat,radius=(input$distance)*1000,weight=2)
      })
  }
)

