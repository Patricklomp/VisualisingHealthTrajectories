# Load R packages
library(shiny)
library(shinysky)
library(shinythemes)
# https://www.htmlwidgets.org/showcase_networkD3.html
library(networkD3)
library("data.table")
library(DT)
library(visNetwork)

# Define UI
ui <- shinyUI(fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Trajectories",
                  tabPanel("Network view",
                           absolutePanel(
                             top = 200, left = 10, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px; background-color: rgba(44, 62, 80, 0.2); padding: 20px",
                             tags$h2("Search:"),
                             uiOutput("icd_select2input"),
                             uiOutput("weight_radiobox"),
                             sliderInput("effect_value",
                                        "effect",
                                        min = 1,
                                        max = 100,
                                        value = 1.5),
                             checkboxInput("active", "Use filter", FALSE),
                             uiOutput("condition_checkbox")
                           ), # sidebarPanel
                            visNetworkOutput("network", width = "100%", height = "90vh")
                  ),
                  tabPanel("Linear view",
                           sidebarPanel(
                             tags$h3("Search:"),
                           ), # sidebarPanel
                           mainPanel(
                             h1("Result"),
                             sankeyNetworkOutput(outputId = "sankeyNet")
                           ) # mainPanel

                  ),
                  tabPanel("Raw data",
                           mainPanel(
                             h1("Data table"),
                             DT::dataTableOutput("table")
                           ) # mainPanel

                  ),
                  tabPanel("Guide", "This panel is intentionally left blank"),
                  tabPanel("About", "This panel is intentionally left blank")

                ) # navbarPage
  )) # fluidPage

