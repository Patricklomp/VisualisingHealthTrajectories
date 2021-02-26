create_nodes_and_edges = function(tg){
  #Base filtering and remove isolated nodes
  tg = tg %>%
    activate(edges) %>%
    filter(!is.na(COUNT_IN_DENMARK)) %>%
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
    mutate(title = id)

  print(head(tg_nodes))
  print(head(named_edge_list))
  return(list("nodes" = tg_nodes, "edges" = named_edge_list))
}

filter_nodes_and_edges = function(tg, filter){
  flog.info("Filtering dataset")
  active <- filter@active
  use_for_weight <- filter@use_for_weight
  effect <- filter@effect_value

  #Check if filtering is active
  if(!active){
    tg = tg %>%
      activate(edges) %>%
      mutate(value = EFFECT_IN_ESTONIA)
    return(create_nodes_and_edges(tg))
  }

  tg = tg %>%
    activate(edges) %>%
    filter(!!as.symbol(use_for_weight) > effect) %>%
    mutate(value = !!as.symbol(use_for_weight))


  return(create_nodes_and_edges(tg))
}
