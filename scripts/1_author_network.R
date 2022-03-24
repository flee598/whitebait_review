#Author network

library(tidyverse)
library(igraph)
library(ggraph)

db <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")

char_vec <- db$Author
cnt_vec <- db$Countries

  # nwk_data <- full_with_excl$keywords[1:50]
  char_vec <- str_replace_all(char_vec, pattern = fixed("; "), replacement = ";")
  
  nwk_data <- data.frame(txt = str_trim(char_vec)) 
  
  # Get the frequency table for each individual string
  nwk_index <- nwk_data$txt %>%
    str_split(";") %>%
    unlist %>%
    table %>%
    data.frame %>%
    arrange(-Freq)
  
  names(nwk_index)[1] = "txt"
  
  # Edges between the strings
  edges <- nwk_data$txt %>%
    str_split(";") %>%
    lapply(function(x) {
      expand.grid(x, x, stringsAsFactors = FALSE)
    }) %>%
    bind_rows(.id = "ID")
  
  edges <- edges %>% 
    group_by(ID) %>%
    mutate(key = paste0(pmin(Var1, Var2), ";", pmax(Var1, Var2))) %>% 
    distinct(key) %>%
    separate(key, c("from", "to"), sep = ";")
  
  # get number of collabs/pubs per author
  edges <- edges %>%
    group_by(from, to) %>%
    summarise(collabs = n()) %>%
    arrange(-collabs)
  
# get authors with > x papers
  auth_cnt <- edges %>%
    filter(from == to) %>%
    filter(collabs > 2)
    
  # filter edges to only the authors above
  df2 <- edges %>%
    filter(from %in% auth_cnt$from) %>%
    filter(to %in% auth_cnt$to)
  
  # Names ro title case
  df2 <- df2 %>%
    mutate(from = paste(
      stringr::str_to_title(stringr::word(tolower(from), 1)), word(from, 2)),
      to = paste(
        stringr::str_to_title(stringr::word(tolower(to), 1)), word(to, 2)))
  
  # convert to igraph object
  graph <- graph_from_data_frame(df2)
  graph <- simplify(graph, edge.attr.comb = "max")
  
  
  # Get author country combos, if author has multiple countries, use most common
  cnt_data <- data.frame(txt =str_trim(cnt_vec)) 
  cnt_df <- data.frame(
    name = nwk_data$txt %>%
      str_split(";") %>%
      unlist,
   cnt = cnt_data$txt %>%
    str_split(";") %>%
    unlist
  ) %>%
    mutate(cnt = replace_na(cnt, "NA"),
           name = replace_na(name, "NA"))%>%
    group_by(name, cnt) %>%
    tally() %>%
    group_by(name,) %>%
    filter(n == max(n)) %>%
    select(name, cnt) %>%
    arrange(name) %>%
    mutate(name = paste(
      stringr::str_to_title(stringr::word(tolower(name), 1)), word(name, 2)))
  
  # join author countries and number of papers
  cnt_df <- df2 %>%
    filter(from == to) %>%
    select(from, collabs) %>%
    rename(name = from) %>%
    left_join(cnt_df)
  
  # adda ttribute to graph
  V(graph)$country <- cnt_df$cnt
  V(graph)$papers <- cnt_df$collabs
  
  # plot
  lay <- create_layout(graph, layout = "fr")
  my_col <- c("#7D9D33", "#BC8E7D", "#FAE093", "#D04E59", "#2C3B75", "#C582B2")
  
  p1 <- ggraph(lay) +
    geom_edge_link(aes(edge_width = collabs ), edge_colour = "grey") +
    geom_node_point(aes(colour = country, size = papers), shape = 16,
                    alpha = 0.9) +
    scale_size_continuous(range = c(3, 15)) +
    scale_colour_manual(values = my_col) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_graph(base_family="sans") +
    labs(colour = "Country", size = "Papers", edge_width = "Collabs") +
    guides(color = guide_legend(override.aes = list(size = 5)))
  
ggsave("figures/auth_net.pdf", p1, width = 30, heigh = 20, units = "cm")
ggsave("figures/auth_net.png", p1, width = 30, heigh = 20, units = "cm")

# ------------------------------------------------------------------------------  
  

  
  
  