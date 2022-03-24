
# keyowrd network 

# required packages
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(tidygraph)

df <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")

df_keyw <- df %>%
  filter(!is.na(Abstract)) 


# custom stopwords
sp_stp_wrds <-  c("maculatus", "banded", "kokopu", "giant", "fasciatus",
                  "brevipinnis","argenteus", "truttaceus", "taylor", "francis",
                  "society", "british", "isles", "rights" ,"reserved", 
                  "shortjaw","jenyns", "inanga", "koaro", "gray", 
                  "significant", "difference", "results", "suggest",
                  "galaxiid", "galaxias", "galaxiids")




  
## Most common word pairs in abstracts 
tidy_abs_ngrams <- df_keyw |>
  select(ID, YearPublished, Abstract) |> 
  unnest_tokens(bigram, Abstract, token = "ngrams", n = 2) |> 
  separate(bigram, into = c("from", "to"), sep = " ") |>
  filter(!from %in% c(stop_words$word, sp_stp_wrds, NA),
         !to %in% c(stop_words$word,sp_stp_wrds, NA),
         !(str_detect(from, "\\d")),
         !(str_detect(to, "\\d"))) |>
  mutate(from = SnowballC::wordStem(from),
         to = SnowballC::wordStem(to)) |>
  count(from, to, sort = T) 

# get top bigrams
bigram_graph <- tidy_abs_ngrams %>% 
  top_n(100) 

# get total mentios for each word
wrd_cnts <- tidy_abs_ngrams %>% 
  pivot_longer(cols = from:to) %>%
  group_by(value) %>%
  summarise(tot = sum(n)) %>%
  arrange(-tot) %>%
  dplyr::filter(value %in% bigram_graph$from | value %in% bigram_graph$to) 

# convert ot igraph
bigram_graph2 <- bigram_graph %>%
  graph_from_data_frame(directed = TRUE, vertices = wrd_cnts)

# custom arrow
arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# plot
p1 <- ggraph(bigram_graph2, layout = "fr") + 
  geom_node_point(aes(size = tot), shape = 16, colour = "grey") +
  geom_edge_link(aes(edge_colour = log(n)), arrow = arrow, end_cap = circle(0.05, "inches"), edge_width = 1) + 
  scale_size_continuous(range = c(3, 15)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_colour_viridis() +
  theme_graph(base_family="sans") +
  labs(edge_colour = "log(\nbigram \ncount)", size = "word \ncount") +
  guides(
    size = guide_legend(reverse = TRUE))

p1

ggsave("figures/bigram_net.pdf", p1, width = 30, heigh = 20, units = "cm")



