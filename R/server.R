# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Load R packages
library(shiny)
library(shinythemes)
# https://www.htmlwidgets.org/showcase_networkD3.html
library(networkD3)
library("data.table")
library(DT)

data(MisLinks, MisNodes)

get_test_data <- function() {
  return(as.data.frame(fread("../sample-data/event_pairs_1.tsv")))
}

make_server <- function(data) {
  # Define server function
  server <- function(input, output) {

    output$txtout <- renderText({
      paste( input$txt1, "test", sep = " " )
    })

    output$table <- DT::renderDataTable({
      data
    })
  } # server

  return(server)
}

visualize_data_pairs <- function(data) {
  require(shiny)
  require(shinythemes)
  require(networkD3)
  require(data.table)
  #data validation

  #process data

  #making shiny server
  shinyApp(ui = make_ui(), server = make_server(data))
}
