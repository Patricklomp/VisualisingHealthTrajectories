# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Load R packages
library(DT)
library(shiny)
library(shinythemes)
# https://www.htmlwidgets.org/showcase_networkD3.html
library(networkD3)
library("data.table")
library(icd)
library(tidyverse)
library(tidygraph)
library(readxl)
library(futile.logger)

library(visNetwork)
library(geomnet)
library(igraph)

load("Data/icd10cm2019.rda", verbose = T) # From icd R package
test_data_source = "sample-data/sign_pairs_combined.xlsx"
icd = icd10cm2019 %>%
  map(~ as.character(.x)) %>%
  as_tibble()


make_server <- function(data) {
  # Define server function
  server <- function(input, output) {
    flog.info("Loading Shiny server")

    graph_filter <- reactive({new("GraphFilter",
        use_for_weight = input$use_for_weight,
        effect_value = input$effect_value,
        active = input$active)
    })

    #create nodes dataframe
    nodes <- make_nodes_from_data(data)
    #create links dataframe
    edges <- make_links_from_data(data)

    #Get filtered nodes and edges
    tg = tbl_graph(nodes, edges)
    nodesandedges <- reactive({filter_nodes_and_edges(tg, graph_filter())
    })

    output$table <- DT::renderDataTable({
      data
    })

    output$network <- renderVisNetwork({

      visNetwork(nodesandedges()$nodes, nodesandedges()$edges, width = "100%", height = "90vh") %>%
        visIgraphLayout() %>%
        visNodes(
          shape = "dot",
          color = list(
            highlight = "#FF8000"
          ),
          shadow = list(enabled = TRUE, size = 10)
        ) %>%
        visEdges(
          arrows ="to",
          color = list(highlight = "#C62F4B")
        ) %>%
        visLayout(randomSeed = 11) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(width = 0.3)
    })

    output$sankeyNet <- renderSankeyNetwork(sankeyNetwork(
      Links  = nodesandedges()$edges, Nodes   = nodesandedges()$nodes,
      Source = "from", Target  = "to",
      Value  = "value",  NodeID  = "id",
      fontSize = 12, nodeWidth = 30
    ))

    output$weight_radiobox <- renderUI({
      radioButtons("use_for_weight", "Use for weight:",
                   colnames(data%>%select(-D1, -D2))
                   )
    })

    output$icd_select2input <- renderUI({
      select2Input("select2Input1",
                   "Select icd codes",
                   choices=c("a","b","c"),
                   selected="")
    })

  }

  return(server)
}

visualize_data_pairs <- function(data) {
  #data validation
  if(length(colnames(data))<3){
    flog.error("Dataset does not contain enough columns")
    stop()
  }
  if(!('D1' %in% colnames(data)) | !('D2' %in% colnames(data))){
    flog.error("Dataset does not contain needed column D1 or D2")
    stop()
  }
  #process data

  #making shiny server
  shinyApp(ui = ui, server = make_server(data))
}
