library(shiny)
library(shinyBS)
library(DT)
library(lubridate)
library(dplyr)

source("00-Functions.R")

WUTZdata <- WU.time.zones(as.df = TRUE)
destCampuseschoices <- c("Select All", unique(WUTZdata$Location))

shinyServer(function(input, output, session) {

#####URL PARAMETER PROCESSING#####      
      observe({
            query <- parseQueryString(session$clientData$url_search)

            if (!is.null(query[["h"]])) {
                  updateSelectInput(session, "hour", selected = query[["h"]])
            }
            
            if (!is.null(query[["m"]])) {
                  updateSelectInput(session, "minute", selected = query[["m"]])
            }
            
            if (!is.null(query[["d"]])) {
                  updateSelectInput(session, "date", selected = query[["d"]])
            }
            
            if (!is.null(query[["c"]])) {
                  updateSelectInput(session, "campus", selected = query[["c"]])
            }
            
            if (!is.null(query[["s"]])) {
                  updateSelectInput(session, "destCampuses", selected = query[["s"]])
            }
      })

#####CREATE DATETIME OBJECT FROM INPUT#####      
      eventDT <- reactive({
            
            zone <- input$tz
            
            if(input$tzSelectMethod == "Campus") {
                  zone <- WUTZdata$TZ[WUTZdata$Location == input$campus]
            }
            
            time <- paste(input$hour, input$minute, "00", sep = ":")
            eventDT <- as.POSIXct(paste(input$date, time), tz = zone)
            return(eventDT)
      })

#####LOAD REACTIVE DATASET BASED ON INPUT CONDITIONS#####      
      datasetInput <- reactive({
            DTX <- WUTZdata
            
            if("Select All" %in% input$destCampuses) {
                  selected_destCampuseschoices <- setdiff(destCampuseschoices, "Select All")
                  updateSelectInput(session, "destCampuses", selected = selected_destCampuseschoices)
            }
            
            
            DTX <- DTX[DTX$Location %in% unique(c(input$campus, input$destCampuses)), ]

            return(DTX)
      })

#####START MAIN BODY ######   
      output$mainbody <- renderUI({
            fluidPage(
                  
                  ## Get geolocation information
                  tags$script('
                              $(document).ready(function () {
                                    navigator.geolocation.getCurrentPosition(onSuccess, onError);
                              
                                    function onError (err) {
                                          Shiny.onInputChange("geolocation", false);
                                    }
                              
                                    function onSuccess (position) {
                                          setTimeout(function () {
                                                var coords = position.coords;
                                                console.log(coords.latitude + ", " + coords.longitude);
                                                Shiny.onInputChange("geolocation", true);
                                                Shiny.onInputChange("lat", coords.latitude);
                                                Shiny.onInputChange("long", coords.longitude);
                                          }, 1100)
                                    }
                              });
                              '),
                  
                  ## Custom Webster branded theme
                  theme = "mystyle.css",
                  #img(src = "DSC-1256.jpg", alt = "Webster Hall by Inocencio Boc"),
                  br(), br(),
                  
                  titlePanel("Webster World Clock"),
                  br(), br(),
                  
                  ## Application Inputs
                  sidebarLayout(
                        sidebarPanel(
                              dateInput("date", "Date:", 
                                        width = 120
                                        ),
                              selectInput("hour", "Hour:",
                                          choices = c(paste(0, c(0:9), sep = ""), 10:23), 
                                          selected = "08",
                                          width = 60
                              ),
                              selectInput("minute", "Minute:",
                                          choices = c(paste(0, c(0:9), sep = ""), 10:59),
                                          width = 60
                                          ),
                              radioButtons(inputId = "tzSelectMethod", 
                                           label = "Select Time Zone by:",
                                           choices = c("Campus", "Time Zone"),
                                           selected = "Campus",
                                           inline = TRUE),
                              conditionalPanel(condition = "input.tzSelectMethod == 'Time Zone'",
                                               selectInput(inputId = "tz", 
                                                           label = "Time Zone:", 
                                                           choices = unique(WUTZdata$TZ),
                                                           selected = "America/Chicago"
                                               )
                              ),
                              conditionalPanel(condition = "input.tzSelectMethod == 'Campus'",
                                               selectInput(inputId = "campus", 
                                                           label = "Host Campus:", 
                                                           choices = unique(WUTZdata$Location),
                                                           selected = "WEBG")
                              ),
                              selectInput(inputId = "destCampuses", 
                                          label = "Satelite Campuses:",
                                          choices = destCampuseschoices,
                                          multiple = TRUE),
                              br(), br(),
                              img(src="401px-Webster_University_Logo.svg.png", alt = "Webster University Logo")
                        ),
                        
                        ## Render the data table
                        mainPanel(
                              DT::dataTableOutput("timesDT")
                        )
                  )
            )
      })

#####DATA TABLE CREATION AND TIME ZONE PROCESSING#####
      output$timesDT <- DT::renderDataTable({
            
            ##Make a copy of the campus and time zone data frame
            table <- datasetInput()
            
            ##Make a list of the supplied time in each time zone
            ##Must be a list and not a vector b/c time zone is an attribute of the entire vector
            local <- lapply(as.list(table$TZ), function(x) with_tz(eventDT(), x))
            
            ##Make a chracter vector from the timezone specific stamp
            table$LocalTime <- unlist(lapply(local, function(x) as.character(format(x, "%Y-%m-%d %H:%M"))))
            
            ##Make an object that determines if the local day is Yesterday, Today, or Tomorrow
            table$DiffD <- day(table$LocalTime) - day(eventDT())
            
            ##Use DiffD to provide specific positive or negative hours
            table$DiffH <- NA
            table$DiffH[table$DiffD == 1] <- hour(table$LocalTime[table$DiffD == 1]) - hour(eventDT()) + 24
            table$DiffH[table$DiffD == 0] <- hour(table$LocalTime[table$DiffD == 0]) - hour(eventDT())
            table$DiffH[table$DiffD == -1] <- hour(table$LocalTime[table$DiffD == -1]) - hour(eventDT()) - 24
            
            table$DiffDesc <- as.character(table$DiffH)
            table$DiffDesc[table$DiffH >= 0] <- paste0("+", table$DiffDesc[table$DiffH >= 0])
            table$DiffDesc[table$DiffH == 1] <- paste0(table$DiffDesc[table$DiffH == 1], " hour")
            table$DiffDesc[!table$DiffH == 1] <- paste0(table$DiffDesc[!table$DiffH == 1], " hours")
            
            table$DiffD <- NULL
            table$DiffH <- NULL

            DT::datatable(table, rownames = FALSE,
                          colnames = c("Time Difference" = "DiffDesc",
                                       "Local Time" = "LocalTime"),
                          extensions = c('Buttons', 'ColReorder', 'Responsive'),
                          options = list(dom = 'Brtip',
                                         buttons = c('colvis', 'copy', 'print'),
                                         colReorder = list(realtime = FALSE))
                          )
      })
})
