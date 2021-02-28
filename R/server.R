# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Load R packages
library(DT)
library(shiny)
library(shinythemes)
# https://www.htmlwidgets.org/showcase_networkD3.html
library(plotly)
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
test_data_source = "sample-data/event_pairs_tested.xlsx"
icd = icd10cm2019 %>%
  map( ~ as.character(.x)) %>%
  as_tibble()


make_server <- function(data) {
  # Define server function
  server <- function(input, output) {
    flog.info("Loading Shiny server")

    graph_filter <- reactive({
      new(
        "GraphFilter",
        active = input$active,
        use_for_weight = ifelse(!is.null(input$use_for_weight),input$use_for_weight,"RR"),
        effect_value = ifelse(!is.null(input$effect_value),input$effect_value,0),
        selected_icd_codes = ifelse(!is.null(input$input$selected_icd_codes),input$input$selected_icd_codes,"")
      )
    })

    #create nodes dataframe
    nodes <- make_nodes_from_data(data)
    #create links dataframe
    edges <- make_links_from_data(data)

    #Get filtered nodes and edges
    tg = tbl_graph(nodes, edges)
    nodesandedges <-
      reactive({
        filter_nodes_and_edges(tg, graph_filter())
      })

    output$table <- DT::renderDataTable({
      data
    })

    output$network <- renderVisNetwork({
      visNetwork(
        nodesandedges()$nodes,
        nodesandedges()$edges,
        width = "100%",
        height = "90vh"
      ) %>%
        visIgraphLayout() %>%
        visNodes(
          shape = "dot",
          color = list(highlight = "#FF8000"),
          shadow = list(enabled = TRUE, size = 10)
        ) %>%
        visEdges(arrows = "to",
                 color = list(highlight = "#C62F4B")) %>%
        visOptions(highlightNearest = list(enabled = T, degree = 2)) %>%
        visLayout(randomSeed = 11) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(width = 0.3)
    })

    output$sankeyNet <- renderPlotly({
      plot_ly(
        type = "sankey",
        orientation = "h",

        node = list(
          label = select(nodesandedges()$nodes, id),
          pad = 15,
          thickness = 20,
          line = list(
            color = "black",
            width = 0.5
          )
        ),

        link = list(
          source = select(nodesandedges()$edges, from),
          target = select(nodesandedges()$edges, to),
          value =  select(nodesandedges()$edges, value)
        )
      )
    })

    output$weight_slider <- renderUI({
      sliderInput("effect_value",
                  "effect",
                  min = min(edges %>% select(!!as.symbol(graph_filter()@use_for_weight)), na.rm = TRUE),
                  max = max(edges %>% select(!!as.symbol(graph_filter()@use_for_weight)), na.rm = TRUE),
                  value = graph_filter()@effect_value)
    })

    output$weight_radiobox <- renderUI({
      radioButtons(
        "use_for_weight",
        "Use for weight:",
        colnames(
          data %>% select_if(is.numeric) %>% select(RR, E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)
        )
      )
    })

    output$condition_checkbox <- renderUI({
      checkboxGroupInput(
        "use_conditions",
        "Add conditions:",
        choices = colnames(
          data %>% select_if(is.logical)
        )
      )
    })

    output$icd_select2input <- renderUI({
      selectInput(
        "selected_icd_codes",
        label = h3("Select icd codes"),
        choices =  nodes$id,
        selected = list(NULL),
        multiple = TRUE
      )
    })

  }

  return(server)
}

visualize_data_pairs <- function(data) {
  #data validation
  if (length(colnames(data)) < 3) {
    flog.error("Dataset does not contain enough columns")
    stop()
  }
  if (!('E1_CONCEPT_ID' %in% colnames(data)) |
      !('E2_CONCEPT_ID' %in% colnames(data))) {
    flog.error("Dataset does not contain needed column E1_CONCEPT_ID or E2_CONCEPT_ID")
    flog.info("Column names: ", colnames(data))
    stop()
  }
  #process data

  #making shiny server
  shinyApp(ui = ui, server = make_server(data))
}
