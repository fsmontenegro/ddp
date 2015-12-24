library(shiny)
library(leaflet)

appdetails <- paste(h4("About this app"),
                    'This app was written by <a href="http://en.gravatar.com/fsmontenegro">Fernando Montenegro</a> as the class project for the JHU',
                    '/Coursera Developing Data Products class',
                    h4("Caveats:"),
                    "<li> Very limited error checking, please don't break the app :-)",
                    "<li> App uses free Google service for geocoding, limited to 2500 queries per day.",
                    h4("Future Enhancements"),
                    "<li>Better Error checking on inputs",
                    "<li>Change time of day slider to reflect hours/minutes (requires JS modifications)",
                    "<li>Include specific program details (link or text)",
                    sep = "")
appinstructions <- paste(h3("Instructions"),
                         "<li>Choose your home address",
                         "<li>Select maximum distance to consider",
                         "<li>Choose your child's birthday",
                         "<li>Choose which days for programs",
                         "<li>Choose time window for activity start/end",
                         "<br><br>",
                         "The table below the map is the list of programs that meet your criteria.",
                         'You can then register for them at the <a href="https://econnect.markham.ca/Start/Start.asp">EZReg</a> site',
                         sep = "")

today_date <- Sys.Date()

shinyUI(
  bootstrapPage (
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    headerPanel("Choosing Kids' Activities - Markham, ON"),
    sidebarPanel(
      textInput('address',"Address",value = "101 Town Centre Boulevard, Markham, ON"),
      sliderInput('distance',"Maximum Distance (km)",0,25,5,step=0.5),
      dateInput('age',"Child's Birthday",max=today_date, value = "2015-01-01", startview = "year"),
      checkboxGroupInput('days',"Desired Days of the Week",
                         choices = c("Sunday"="sun","Monday"="mon","Tuesday"="tue",
                           "Wednesday"="wed", "Thursday"="thu","Friday"="fri","Saturday"="sat",
                           "Week Long (Mon-Fri)"="mon-fri"),
                          selected = c("sun","mon","tue","wed","thu","fri","sat")),
      sliderInput('timeofday',"Between hours of ",5,24,range(8,20),step=1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map&Programs", leafletOutput('omap'), dataTableOutput('ofprog')),
        tabPanel("App Instructions",HTML(appinstructions)),
        tabPanel("App Details",HTML(appdetails))
      )
    )
  )
)
