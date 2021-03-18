ui <- dashboardPage(
  dashboardHeader(
    title = "Trajectories",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Network", tabName = "network", icon = icon("project-diagram")),
      menuItem("Raw Data", tabName = "raw_data", icon = icon("table")),
      menuItem("Guide", tabName = "guide", icon = icon("info-circle"))
    ),
    switchInput(inputId = "network_view_switch",
                value = TRUE,
                onLabel = "Network view",
                offLabel = "Sankey view",),
    uiOutput("icd_selectinput"),
    uiOutput("weight_radiobox"),
    uiOutput('weight_slider'),
    uiOutput("importance_slider")
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "network",
            conditionalPanel(
              condition = "input.network_view_switch == 1",
              visNetworkOutput("network", width = "100%", height = "90vh")
            ),
            conditionalPanel(
              condition = "input.network_view_switch == 0",
              plotlyOutput("sankeyNet", width = "100%", height = "90vh")
            )
    ),
    tabItem(tabName = "raw_data",
            h1("Data table"),
            DT::dataTableOutput("table")
    ),
    tabItem(tabName = "guide",
            get_guide_page()
    )
  ))
)


