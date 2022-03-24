

fun_auth_network <- function(db, min_yr, mx_yr){
  
  db <- db %>%
    filter(YearPublished >= min_yr & YearPublished <= mx_yr)


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
    filter(from == to) 
  
  
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
  
  return(graph)
}
#----------------------------------------------------------


db <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")


# graph gor each decade
min_yr = c(1968, seq(1981, 2011, by = 10))
mx_yr = seq(1980, 2020, by = 10)

nets <- mapply(fun_auth_network, min_yr = min_yr, mx_yr = mx_yr,
       MoreArgs = list(db = db), SIMPLIFY = F)

# clusters and cluster sizes
g_size <- setNames(sapply(nets, function(x) igraph::clusters(x)$csize), c("1970s", "1980s",
                                                                "1990s", "2000s",
                                                                "2010s"))

g_size <- setNames(stack(g_size)[2:1], c('decade', 'size'))


g_size <- g_size %>%
  arrange(decade, size) %>%
  group_by(decade) %>%
  mutate(id = 1:n())

p1 <- ggplot(g_size, aes(id, size, colour = decade)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_line() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Cluster", y = "Cluster size", colour = "Decade") +
  theme_bw()

# create a random graph
g <- erdos.renyi.game(10, 1/5)
plot(g)

V(g)$clust <- as.character(igraph::clusters(g)$membership)

# layout and plot
lay2 <- create_layout(g, layout = "fr")
g_col <- c("#7D9D33", "#BC8E7D", "#FAE093")


# inset plot
p2 <- ggraph(lay2) +
  geom_edge_link(edge_width = 1, edge_colour = "grey") +
  geom_node_point(aes(fill = clust),size = 2, shape = 21, alpha = 0.9) +
 # scale_fill_manual(values = g_col) +
  theme_graph(base_family="sans") + 
  theme(legend.position = "none",
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

p3 <- p1 + inset_element(p2, left = 0.01, bottom = 0.5, right = 0.5, top = 0.99)

p3

ggsave("figures/author_clusters.png", p3, width = 9, heigh = 6, units = "cm", dpi = 300)
ggsave("figures/author_clusters.pdf", p3, width = 9, heigh = 6, units = "cm")


