
# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Load R packages
library(shiny)
library(shinythemes)
library(networkD3)

data(MisLinks, MisNodes)


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

                  ), # Network view
                  tabPanel("Linear view",
                           sidebarPanel(
                             tags$h3("Search:"),
                             textInput("code", "Search for code:", "")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Result")
                           ) # mainPanel

                  ), # Linear view
                  tabPanel("Guide", "This panel is intentionally left blank"),
                  tabPanel("About", "This panel is intentionally left blank")

                ) # navbarPage
) # fluidPage


# Define server function
server <- function(input, output) {

  output$txtout <- renderText({
    paste( input$txt1, "test", sep = " " )
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
