GraphFilter <- setClass(
  "GraphFilter",
  slots = c(
    use_for_weight = "character",
    effect_value = "numeric",
    active = "logical"
  )
)

get_test_data = function() {
  test_data = as.data.frame(read_xlsx(test_data_source))

  flog.info("Got test data from: %s", test_data_source)
  return(test_data)
}

make_links_from_data = function(data) {
  flog.info("Making edges from data")

  # Prepare edges
  edges = data %>%
    rename(from = D1, to = D2) %>%
    mutate(EFFECT_IN_ESTONIA = coalesce(EFFECT_IN_ESTONIA, 0)) %>%
    mutate(EFFECT_IN_DENMARK = coalesce(EFFECT_IN_DENMARK, 0)) %>%
    mutate(COUNT_IN_ESTONIA = coalesce(COUNT_IN_ESTONIA, 0)) %>%
    mutate(COUNT_IN_DENMARK = coalesce(COUNT_IN_DENMARK, 0))

  return(as.data.frame(edges))
}

make_nodes_from_data = function(data) {
  flog.info("Making nodes from data")

  nodes = tibble(
    name = unique(c(data$D1, data$D2))
  )

  nodes = nodes %>%
    left_join(icd, by = c("name" = "code")) %>%
    select(name, Description = short_desc, Chapter = chapter) %>%
    mutate(CodeDescription = str_c(name, " - ", Description)) %>%
    group_by(Chapter) %>%
    mutate(ChapterNew = str_c(min(name), " - ", max(name), ": ", Chapter)) %>%
    ungroup() %>%
    mutate(Chapter = ChapterNew) %>%
    select(-ChapterNew) %>%
    rename(id = name) %>%
    rename(group = Chapter)

  return(as.data.frame(nodes))
}
