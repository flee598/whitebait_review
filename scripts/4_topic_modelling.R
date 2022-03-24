# Topic modelling (biterm and LDA)


# LDA TOPIC MODELLING ---------------------------------------------------------

library(tidyverse)
library(purrr)
library(patchwork)

biblioDF <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")

# get relevant cols
lda_data <- biblioDF %>%
  select(YearPublished, Title, Abstract) %>%
  filter(!is.na(Abstract))


lda_data <- lda_data %>%
  tidytext::unnest_tokens(word, Abstract)

# pre defined stop words
stpWrds <- rbind(tidytext::get_stopwords(source = "snowball"), 
                 tidytext::get_stopwords(source = "smart"))
stpWrds <- unique(stpWrds[ , 1])


# custom stopwords
stpWrds2 <- data.frame(word =  c(
  "mm", "g", "i.e", "s1", "km", "taylor", 
  "francis", "karo", "kkopu", "blackwell", "publisher", "publishing",
  "wiley", "son", "sons", "john", "royal", "society", "rights", "reserved",
  "galaxias", "kokopu", "banded", "jenyns", "shortjaw", "inanga", "koaro","galaxiid",
  "kokopu", "banded", "galaxias","giant", "karo", "kkopu",
  "fasciatus", "maculatus", "brevipinnis", "argenteus", "truttaceus", "species", "fish"))


# combine all stop words
stpWrds3 <- rbind(stpWrds, stpWrds2)


#  stemm words & remove stop words 
lda_data_2 <- lda_data %>%
  filter(!(str_detect(word, "\\d"))) %>%
  filter(!word %in% stpWrds3$word) %>%
  mutate(word = tm::stemDocument(.$word)) %>%
  filter(!word %in% stpWrds3$word) %>%
  group_by(Title) %>%
  count(word) %>%
  tidytext::cast_dtm(Title, word, n)


# run  LDA topic model 
n_topics <- 8
ap_lda <- topicmodels::LDA(lda_data_2, k = n_topics, control = list(seed = 1234))


# word-topics 
ap_topics <- tidytext::tidy(ap_lda, matrix = "beta")

# topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# name of topics - place holder
topic2 <- data.frame(topic = 1:n_topics, 
                     topic2 = c("Spawning", "Exotic spp. / food-webs", "Parasites", "Fish passage",
                       "Life-history", "Habitat", "Biogeography", "Landlocked") )



ap_top_terms <- left_join(ap_top_terms, topic2)

# plot topic beta scores
p1 <- ap_top_terms %>%
  mutate(term = tidytext::reorder_within(term, beta, topic2)) %>%
  ggplot(aes(beta, term, fill = factor(topic2))) +
  scale_fill_viridis_d(direction = 1, option = "E") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic2, scales = "free_y", nrow = 2) +
  tidytext::scale_y_reordered() +
  scale_x_continuous(n.breaks = 3) +
  labs(x = "Beta", y = "Term") +
  theme_bw() +
  theme(text=element_text(size=14))


################################################
# document-topics through time  and by country #
################################################


dat4a <- biblioDF %>%
  select(YearPublished, Title, study_country)

colnames(ap_documents)[1] <- "Title" 

ap_documents <- ap_documents %>% inner_join(dat4a)
ap_documents <- left_join(ap_documents, topic2)

ap_document_time <- ap_documents %>%
  group_by(YearPublished, topic2) %>%
  summarise(MnGamma = mean(gamma))

# plot change in topic trends over time
p2 <- ggplot(ap_document_time, aes(YearPublished, MnGamma)) +
  geom_point(size = 1, alpha = 0.5, colour = "grey27") +
  geom_line(colour = "grey27") +
  geom_smooth(colour = "grey27", fill = "grey27", alpha = 0.2) +
  facet_wrap(~topic2, nrow = 2) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Year", y = "Gamma") +
  theme_bw() +
  theme(text=element_text(size=14))


# topics by country - data tidying
ap_document_country <- ap_documents %>%
  mutate(study_country = str_trim(study_country),
         study_country = recode(study_country,
                                "GHANA" = "New Zealand",
                                "ARGENTINA" = "Argetina",
                                "AUSTRALIA" = "Australia",
                                "NZ" = "New Zealand",
                                "NEW ZEALAND" = "New Zealand",
                                "CHILE" = "Chile",
                                "SOUTHERN HEMISPERE" = "Multiple",
                                )) %>%
  filter(study_country %in% c("Argetina", "Australia", "Chile", "New Zealand", "Multiple")) %>%
  group_by(study_country, topic2) %>%
  summarise(MnGamma = mean(gamma)) %>%
  group_by(topic2) %>%
  mutate(MnGamma_rel = MnGamma/sum(MnGamma))

# adding country insets to facets - helper functions ---------------------------

# formatting for single inste plot
get_inset <- function(ap_document_country){
  
  country_cols <- c("#6d7cb5", "#bc8e7d", "#d04e59", "#7d9d33", "#fae093")
  names(country_cols) <- c("New Zealand", "Australia", "Chile", "Argetina", "Multiple")
  
  p <- ggplot(ap_document_country, aes(x= "", y = MnGamma_rel, fill = study_country)) +
    geom_bar(stat = "identity", width = 1, color='black') +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = country_cols) +
    labs(fill = "Country") +
    theme_void() +
    theme(legend.position = "none")
  return(p)
}


# placing a single inset plot
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = F, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}


# split data by faceting variable and create insets
insets <- ap_document_country %>% 
  split(f = .$topic2) %>%
  purrr::map(~annotation_custom2(
    grob = ggplotGrob(get_inset(.)), 
    data = data.frame(topic2=unique(.$topic2)),
    ymin = 0.7, ymax=1.05, xmin=1998, xmax=2030)
  )


p3 <- p2 + insets


ggsave("./figures/5_topic_model_topics_time_and_country.pdf",
       width = 30, height = 15, units = "cm")


# number of articles associated with each topic
ap_doc_n <- ap_documents %>%
  select(Title, topic2, gamma) %>%
  group_by(Title) %>%
  filter(gamma == max(gamma)) %>%
  ungroup() %>%
  count(topic2)

p1 <- p1 +
  geom_text(data = ap_doc_n, aes(x = 0.027,
                                 y = 1,
                                 label = paste("n = ", n, sep = "")), hjust = 0)



ggsave("./figures/5_topic_model_topics.png", plot = p1,
       width = 30, height = 15, units = "cm", dpi = 300)

