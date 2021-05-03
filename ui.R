library(DT)
library(config)
library(ggplot2)
library(httr)
library(jsonlite)
library(patchwork)
library(plyr)
library(readxl)
library(shinyjs)
library(tibble)
library(tidyr)
library(stringr)
library(comprehenr)
library(data.table)
library(grid)
library(gridExtra)
library(gridtext)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(mapview)
library(measurements)
library(purrr)
library(shinythemes)
library(shinyWidgets)
detach(package:plyr)



enums <- list(NUMBER = 1:8, TYPE = c(
                                     "1. Map with buffers",
                                     "2. Map with district names"))


ui <- fluidPage(theme = shinytheme("united"),
                shinyjs::useShinyjs(),

                # Sidebarpanel ----
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "comboBox",
                                label = "Map types",
                                selected = "1. Map with buffers",
                                choices = enums$TYPE),



                    conditionalPanel(
                      condition = paste0("input.comboBox=='", enums$TYPE[1], "'"),
                      selectizeInput("regk1", label = "Region", choices = NULL, multiple = FALSE),
                      actionButton(inputId = "clicksk1", label = "Submit"),
                      br(),br(),br(),
                      uiOutput('mapchoice1'),
                      uiOutput('radius1'),
                      uiOutput('checkbox')
                    ),

                    conditionalPanel(
                      condition = paste0("input.comboBox=='", enums$TYPE[2], "'"),
                      selectizeInput("regk2", label = "Region", choices = NULL, multiple = FALSE),
                      actionButton(inputId = "clicksk2", label = "Submit"),
                      br(),br(),br(),
                      uiOutput('mapchoice2'),
                      uiOutput('bezirkliste'),
                      uiOutput('applybuttonk2')

                    ),


                    width = 3),

                  # Mainpanel ----
                  mainPanel(

                    ### Karte 1 ----
                    conditionalPanel(
                      condition = paste0("input.comboBox=='", enums$TYPE[1], "'"),
                      leafletOutput("map1", height = 800),

                      uiOutput('quellek1'),

                      #h5("Quelle: empirica regio (© Statistik der Bundesagentur für Arbeit)"),
                      #textInput("quellek1", label = "Quelle", value = "Quelle: empirica regio (© Statistik der Bundesagentur für Arbeit)", width = '60%'),
                    ),

                    ### Karte 2 ----
                    conditionalPanel(
                      condition = paste0("input.comboBox=='", enums$TYPE[2], "'"),
                      leafletOutput("map2", height = 800),
                      uiOutput('quellek2'),
                      #textInput("quellek2", label = "Quelle", value = "Quelle: empirica regio (© Statistik der Bundesagentur für Arbeit)", width = '60%'),

                    ),

                    width = 9
                  )
                )
)
