create_nodes_and_edges = function(tg) {
  #Base filtering and remove isolated nodes
  tg = tg %>%
    activate(edges) %>%
    filter(!is.na(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)) %>%
    filter(value > 1.5) %>%
    activate(nodes) %>%
    filter(!node_is_isolated())

  # Separate out edges and node data frames
  tg_nodes <-
    tg %>%
    activate(nodes) %>%
    data.frame() %>%
    tibble::rowid_to_column("rowid")

  tg_edges <-
    tg %>%
    activate(edges) %>%
    data.frame()

  named_edge_list <-
    tg_edges %>%
    # Rename from nodes
    left_join(tg_nodes, by = c("from" = "rowid")) %>%
    select(-from) %>%
    rename(from = id) %>%

    # Rename to nodes
    left_join(tg_nodes, by = c("to" = "rowid")) %>%
    select(-to) %>%
    rename(to = id) %>%
    select(from, to, value)


  tg_nodes <- select(tg_nodes, -rowid) %>%
    mutate(title = CodeDescription)

  print(head(tg_nodes))
  print(head(named_edge_list))
  return(list("nodes" = tg_nodes, "edges" = named_edge_list))
}

filter_nodes_and_edges = function(tg, filter) {
  flog.info("Filtering dataset")
  active <- filter@active
  use_for_weight <- filter@use_for_weight
  effect <- filter@effect_value
  selected_icd_codes <- filter@selected_icd_codes

  #Check if filtering is active
  if (!active) {
    tg = tg %>%
      activate(edges) %>%
      mutate(value = RR)
    return(create_nodes_and_edges(tg))
  }

  #Filter by weight
  tg = tg %>%
    activate(edges) %>%
    filter(!!as.symbol(use_for_weight) > effect) %>%
    mutate(value = !!as.symbol(use_for_weight))

  #Filter by icd codes

  chosen_nodes = tg %>%
    activate(nodes) %>%
    filter(id %in% selected_icd_codes)

  print(chosen_nodes)

  #tg = tg %>%
    #activate(nodes) %>%
   # mutate(dist_to_center = node_distance_to(node_is_center())) %>%
   # filter(dist_to_center < 4000)

  return(create_nodes_and_edges(tg))
}
