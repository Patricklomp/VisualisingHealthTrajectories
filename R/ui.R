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
                               textInput("code", "Search for code:", "")
                             ), # sidebarPanel
                             mainPanel(
                               h1("Result"),
                               forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
                                            Target = "target", Value = "value", NodeID = "name",
                                            Group = "group", opacity = 1)

                             ) # mainPanel

                    ),
                    tabPanel("Linear view",
                             sidebarPanel(
                               tags$h3("Search:"),
                               textInput("code", "Search for code:", "")
                             ), # sidebarPanel
                             mainPanel(
                               h1("Result")
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
