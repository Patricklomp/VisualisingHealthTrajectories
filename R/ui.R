# Load R packages
library(shiny)
library(shinythemes)
# https://www.htmlwidgets.org/showcase_networkD3.html
library(networkD3)
library("data.table")
library(DT)


make_ui <- function() {
  # Define UI
  ui <- fluidPage(theme = shinytheme("flatly"),
                  navbarPage(
                    "Trajectories",
                    tabPanel("Network view",
                             sidebarPanel(
                               tags$h3("Search:"),
                               sliderInput("opacity",
                                           "Opacity",
                                           min = 0.1,
                                           max = 1,
                                           value = 0.4),
                               textInput("code", "Search for code:", "")
                             ), # sidebarPanel
                             mainPanel(
                               h1("Result"),
                               forceNetworkOutput(outputId = "forceNet")

                             ) # mainPanel

                    ),
                    tabPanel("Linear view",
                             sidebarPanel(
                               tags$h3("Search:"),
                               textInput("code", "Search for code:", "")
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
  ) # fluidPage

  return(ui)
}
