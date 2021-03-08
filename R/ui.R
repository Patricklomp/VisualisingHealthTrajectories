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
                             tags$h4("Use network or sankey view"),
                             switchInput(inputId = "network_view_switch",
                                         value = TRUE,
                                         onLabel = "Network view",
                                         offLabel = "Sankey view",),
                             uiOutput("icd_selectinput"),
                             uiOutput("weight_radiobox"),
                             uiOutput("weight_slider"),
                             uiOutput("importance_slider"),
                             checkboxInput("active", "Use filter", FALSE),
                             uiOutput("condition_checkbox")
                           ), # sidebarPanel
                           conditionalPanel(
                             condition = "input.network_view_switch == 1",
                             visNetworkOutput("network", width = "100%", height = "90vh")
                           ),
                           conditionalPanel(
                             condition = "input.network_view_switch == 0",
                             plotlyOutput("sankeyNet", width = "100%", height = "90vh")
                           )
                  ),
                  tabPanel("Raw data",
                           mainPanel(
                             h1("Data table"),
                             DT::dataTableOutput("table")
                           ) # mainPanel

                  ),
                  tabPanel("Guide", "This panel is intentionally left blank")
                ) # navbarPage
  )) # fluidPage

