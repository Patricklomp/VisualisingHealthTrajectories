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
                           sidebarPanel(
                             tags$h2("Search:"),
                             tags$h4("Use network or sankey view"),
                             switchInput(inputId = "network_view_switch",
                                         value = TRUE,
                                         onLabel = "Network view",
                                         offLabel = "Sankey view",),
                             uiOutput("icd_selectinput"),
                             uiOutput("weight_radiobox"),
                             uiOutput('weight_slider'),
                             uiOutput("importance_slider"),
                             prettyCheckbox(inputId = "active", label = "Enable filter", icon = icon("check"), value = FALSE),
                             label = "Filter"
                           ), # sidebarPanel
                           mainPanel(
                           conditionalPanel(
                             condition = "input.network_view_switch == 1",
                             visNetworkOutput("network", width = "100%", height = "90vh")
                           ),
                           conditionalPanel(
                             condition = "input.network_view_switch == 0",
                             plotlyOutput("sankeyNet", width = "100%", height = "90vh")
                           )),

                           status = 'primary'
                  ),
                  tabPanel("Raw data",
                           mainPanel(
                             h1("Data table"),
                             DT::dataTableOutput("table")
                           ) # mainPanel

                  ),
                  tabPanel("Guide", get_guide_page())
                ) # navbarPage
  )) # fluidPage

