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
  return(as.data.frame(fread("sample-data/event_pairs_1.tsv")))
}

make_links_from_data <- function(data) {
    data2 <- subset(data, select=c("E1_NAME", "E2_NAME", "EVENT_PAIR_EFFECT"))
    #MisLinks
    #return(as.data.frame(data2))
    return(as.data.frame(MisLinks))
}

make_nodes_from_data <- function(data) {
    return(as.data.frame(MisNodes))
}


make_server <- function(data) {
  # Define server function
  server <- function(input, output) {


    #create nodes dataframe
    nodes <- make_nodes_from_data(data)
    #create links dataframe
    links <- make_links_from_data(data)

    output$txtout <- renderText({
      paste( input$txt1, "test", sep = " " )
    })

    output$table <- DT::renderDataTable({
      data
    })

    output$forceNet <- renderForceNetwork(forceNetwork(
      Links  = links, Nodes   = nodes,
      Source = "source", Target  = "target",
      Value  = "value",  NodeID  = "name",
      Group  = "group",  opacity = input$opacity
    ))

    output$sankeyNet <- renderSankeyNetwork(sankeyNetwork(
      Links = links, Nodes = nodes, Source = "source",
      Target = "target", Value = "value", NodeID = "name",
      units = "TWh", fontSize = 12, nodeWidth = 30
    ))

  }

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
