# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Load R packages
library(DT)
library(shiny)
library(shinythemes)
library(shinyWidgets)
# https://www.htmlwidgets.org/showcase_networkD3.html
library(plotly)
library("data.table")
library(icd)
library(tidyverse)
library(tidygraph)
library(readxl)
library(futile.logger)
library(shinydashboard)
library(visNetwork)
library(geomnet)
library(igraph)

DEFAULT_COLUMNS_FOR_WEIGHT = c("RR", "E1_AND_E2_TOGETHER_COUNT_IN_EVENTS")

load("Data/icd10cm2019.rda", verbose = T) # From icd R package
test_data_source = "sample-data/event_pairs_tested.xlsx"
icd = icd10cm2019 %>%
  map(~ as.character(.x)) %>%
  as_tibble()


make_server <- function(data) {
  # Define server function
  server <- function(input, output) {
    flog.info("Loading Shiny server")

    #create nodes dataframe
    nodes <- make_nodes_from_data(data)
    #create links dataframe
    edges <- make_links_from_data(data)

    #Get filtered nodes and edges
    tg = tbl_graph(nodes, edges)

    nodesandedges <-
      reactive({
        icd_codes = data.frame(input$selected_icd_codes)
        graph_filter <-
          new(
            "GraphFilter",
            use_for_weight = ifelse(
              !is.null(input$use_for_weight),
              input$use_for_weight,
              "RR"
            ),
            RR_effect_value = ifelse(!is.null(input$RR_effect_value), input$RR_effect_value, 0),
            E1E2Together_effect_value = ifelse(!is.null(input$E1E2Together_effect_value), input$E1E2Together_effect_value, 0),
            importance_value = ifelse(!is.null(input$importance_value), input$importance_value, 0),
            use_network_view = ifelse(
              !is.null(input$network_view_switch),
              input$network_view_switch,
              TRUE
            )
          )

        return(filter_nodes_and_edges(tg, graph_filter, input$selected_icd_codes))
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
        visInteraction(navigationButtons = TRUE) %>%
        visLegend(width = 0.3) %>%
        visExport(type = "jpeg", name = "export-network",
                    float = "left", label = "Save network", background = "purple", style= "") %>%
        visLayout(randomSeed = 11)
    })

    output$sankeyNet <- renderPlotly({
      plot_ly(
        type = "sankey",
        orientation = "h",

        node = list(
          label = as.list(select(nodesandedges()$nodes, title))$title,
          pad = 15,
          thickness = 20,
          line = list(color = "black",
                      width = 0.5)
        ),

        link = list(
          source = as.list(select(nodesandedges()$edges, from_row))$from_row - 1,
          #R counts indexes from 1, whereas plotly from 0
          target = as.list(select(nodesandedges()$edges, to_row))$to_row - 1,
          value =  as.list(select(nodesandedges()$edges, value))$value
        )
      )
    })

    output$weight_slider <- renderUI({
      div(
        sliderInput(
          "RR_effect_value",
          "Relative risk",
          min = min(edges %>% select(
             RR
          ), na.rm = TRUE),
          max = max(edges %>% select(
             RR
          ), na.rm = TRUE),
          value = 1
        ),

        sliderInput(
          "E1E2Together_effect_value",
          "Events together count",
          min = min(edges %>% select(
             E1_AND_E2_TOGETHER_COUNT_IN_EVENTS
          ), na.rm = TRUE),
          max = max(edges %>% select(
            E1_AND_E2_TOGETHER_COUNT_IN_EVENTS
          ), na.rm = TRUE),
          value = 1
        )
      )


    })



    output$importance_slider <- renderUI({
      sliderInput(
        "importance_value",
        "Importance value",
        min = 1,
        max = 5,
        #max(edges %>% select(!!as.symbol(input$use_for_weight)), na.rm = TRUE)
        value = 1
      )
    })

    output$weight_radiobox <- renderUI({
      radioButtons("use_for_weight",
                   "Use for weight:",
                   colnames(
                     data %>% select_if(is.numeric) %>% select(RR, E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)
                   ))
    })



    output$icd_selectinput <- renderUI({
      pickerInput(
        inputId = "selected_icd_codes",
        label = h3("Select icd codes"),
        choices =  nodes$id,
        options = list(`actions-box` = TRUE, `liveSearch` = TRUE),
        multiple = T
      )
    })

  }

  return(server)
}

#' Visualize data pairs using Shiny application
#'
#' @param data Data used for visualization. Has to contain columns E1_CONCEPT_ID and E2_CONCEPT_ID
#' @examples
#' visualize_data_pairs(data)
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
