
#library(bibliometrix)
library(tidyverse)
library(patchwork)

df <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")

# meta data 
df2 <- df %>%
  select(ID, Author, ArticleType,  Title, PublicationName, 
         lead_author_institution_type,  YearPublished,
         study_country, koaro:method_other, stream:landlocked) %>%
  rename("author_inst" = "lead_author_institution_type",
         "period_freq" = "preiod_frq") %>%
  mutate(author_inst = ifelse(
  author_inst == "TERTIARY", "University", "Other"),
  study_country = stringr::str_trim(study_country, side = "both"))


# get number of unique authors 
n_auth <- length(unique(unlist(str_split(df2$Author, ";")))) - 1


# number authors through time
datTime <- df2 %>%
  mutate(NumAuth = lengths(gregexpr(";", df$Author)) + 1,
         TitleLength = lengths(gregexpr(" ", df$Title)) + 1) %>%
  group_by(YearPublished) %>%
  summarise(numArticles = n(),
            NumAuth = mean(NumAuth),
            TitleLength = mean(TitleLength))

datTime2 <- pivot_longer(datTime, names_to = "var", values_to = "value", -YearPublished)

datTime2 <- datTime2 %>%
  filter(var %in% c("numArticles", "NumAuth"))

# recode
datTime2[datTime2$var == "numArticles", 2] <- "Articles"
datTime2[datTime2$var == "NumAuth", 2] <- "Authors"

alph <- 0.7
ptSz <- 2

p1 <- datTime2 %>%
  filter(var == "Authors") %>%
  ggplot(aes(YearPublished, value)) +
  geom_point(size = ptSz, alpha = alph, colour = "grey27") +
  geom_line(colour = "grey27") +
  labs(y = "Authors/article", x = "Year") +
  geom_smooth(colour = "grey27", fill = "grey27", alpha = 0.2) +
  scale_y_continuous(breaks = seq(0, 7, by = 2), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.1, 7.5)) +
  annotate("text", x = 1972, y = 7, label = paste("n =", n_auth)) +
  theme_bw()  +
  theme(text=element_text(size=14))



# Author institution through time ----------------------------------------------

dat_inst <- df2 %>%
  group_by(YearPublished) %>%
  count(author_inst) %>%
  filter(author_inst != "na")

dat_inst3 <- data.frame(
  expand.grid(YearPublished = min(dat_inst$YearPublished):
                max(dat_inst$YearPublished), 
              author_inst = unique(dat_inst$author_inst))
) %>%
  left_join(dat_inst)
dat_inst3[is.na(dat_inst3)] <- 0

dat_inst3$author_inst <- factor(dat_inst3$author_inst, 
                                levels = c("University", "Other"))


p2 <- ggplot(dat_inst3, aes(YearPublished, n, colour = author_inst,
                            fill = author_inst, group = author_inst)) +
  geom_point(size = ptSz, alpha = alph) +
  geom_line() +
  geom_smooth(alpha = 0.1) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 12, by = 3), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.1,13)) +
  labs(colour = "Lead author institution", fill = "Lead author institution") +
  guides(fill=guide_legend(title.position="top"), colour = guide_legend(title.position="top")) +
  xlab("Year") +
  ylab("") +
  annotate("text", x = 1972, y = 12, label = paste("n =", nrow(df))) +
  theme_bw() +
  theme(legend.position = "bottom")  +
  theme(text=element_text(size=14))


## Where are people publishing -------------------------------------------------

df2$PublicationName <- str_to_title(df2$PublicationName)
df2$PublicationName <- ifelse(df2$PublicationName == "Australian Journal Of Marine And Freshwater Research",
                    "Marine And Freshwater Research", df2$PublicationName)

# recode to reduce name length
df2$PublicationName  <- gsub("-Marine And Freshwater Ecosystems", "", df2$PublicationName)
df2$PublicationName  <- gsub("Marine", "Mar.", df2$PublicationName)
df2$PublicationName  <- gsub("New Zealand", "NZ", df2$PublicationName)
df2$PublicationName  <- gsub("Journal", "J.", df2$PublicationName)
df2$PublicationName  <- gsub("Research", "Res.", df2$PublicationName)
df2$PublicationName  <- gsub("Freshwater", "Fresh.", df2$PublicationName)
df2$PublicationName  <- gsub("Biology", "Bio.", df2$PublicationName)
df2$PublicationName  <- gsub("Ecology", "Ecol.", df2$PublicationName)
df2$PublicationName  <- gsub("Environmental", "Env.", df2$PublicationName)
df2$PublicationName  <- gsub("Society", "Soc.", df2$PublicationName)
df2$PublicationName  <- gsub("Progress", "Prog.", df2$PublicationName)
df2$PublicationName  <- gsub("Experimental", "Exper.", df2$PublicationName)
df2$PublicationName  <- gsub("Reviews", "Revs.", df2$PublicationName)
df2$PublicationName  <- gsub("And", "&", df2$PublicationName)
df2$PublicationName  <- gsub("Transactions", "Trans.", df2$PublicationName)
df2$PublicationName  <- gsub("Fisheries", "Fish.", df2$PublicationName)
df2$PublicationName  <- gsub("American", "Am.", df2$PublicationName)
df2$PublicationName  <- gsub("Of", "of", df2$PublicationName)
df2$PublicationName  <- gsub("of ", "", df2$PublicationName)
df2$PublicationName  <- gsub("Nz", "NZ", df2$PublicationName)

colnames(df2)

# get top n
df_journals <- df2 %>%
  count(PublicationName) %>%
  arrange(-n) %>%
  top_n(10)


p3 <- ggplot(df_journals, aes(fct_inorder(PublicationName), n)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  xlab("Journal") +
  ylab("Articles") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  +
  theme(text=element_text(size=14),
        plot.margin = unit(c(0.5,0.5,0.5,1.5), "cm"))


df_n_year <- df2 %>%
  group_by(YearPublished, PublicationName) %>%
  summarise(tot = n()) %>%
  group_by(YearPublished) %>%
  summarise(tot2 = n())

# add years without pubs
df_n_year <- data.frame(YearPublished = min(df_n_year$YearPublished):
                          max(df_n_year$YearPublished)) %>%
  left_join(df_n_year, keep = F) %>%
  mutate(tot2 = ifelse(is.na(tot2), 0, tot2))


n_jour <- length(unique(df2$PublicationName))


p4 <- ggplot(df_n_year, aes(YearPublished, tot2)) +
  geom_point(size = ptSz, alpha = alph, colour = "grey27") +
  geom_line(colour = "grey27") +
  geom_smooth(colour = "grey27", fill = "grey27", alpha = 0.2) +
  scale_y_continuous(limits = c(0,14)) +
  annotate("text", x = 1972, y = 12.5, label = paste("n =", n_jour)) +
  xlab("Year") +
  ylab("Distinct journals") +
  theme_bw() 


## number of studies on each species -------------------------------------------

dat_count <- df2 %>%
  select(study_country , koaro, inanga, giant_kokopu, 
         shortjaw_kokopu, banded_kokopu, spotted_galaxias) %>%
  pivot_longer(cols = -study_country, names_to = "species", values_to = "value") %>%
  group_by(study_country, species) %>%
  count(value) %>%
  filter(value == "Y") 

dat_count <- dat_count %>%
  filter(study_country != "GHANA") %>%
  filter(study_country != "CANADA") %>%
  mutate(study_country = ifelse(study_country %in% c("CHILE, ARGENTINA", 
                                                     "SOUTHERN HEMISPERE"), 
                                "Multiple", study_country))


dat_count$study_country <-  recode(dat_count$study_country, 
                                   ARGENTINA = "Arg",
                                   CHILE = "Chile",
                                   AUSTRALIA = "Australia",
                                   NZ = "New Zealand")


dat_count$study_country <- factor(dat_count$study_country,
                                  levels = c('New Zealand','Arg','Australia',
                                             'Chile', 'Multiple'))

dat_count$species <- factor(dat_count$species, 
                            levels = c('inanga',"spotted_galaxias",
                                       'banded_kokopu','koaro',
                                       'giant_kokopu', 'shortjaw_kokopu'))


fish_cols <- RColorBrewer::brewer.pal(6, "Dark2")
names(fish_cols) <- c("G. maculatus", "G. brevipinnis",
                      "G. fasciatus", "G. postvectis",
                      "G. argenteus", "G. truttaceus")

dat_count$species <-  recode(dat_count$species,
                             inanga = "G. maculatus",
                             spotted_galaxias = "G. truttaceus",
                             banded_kokopu = "G. fasciatus",
                             giant_kokopu = "G. argenteus",
                             shortjaw_kokopu = "G. postvectis",
                             koaro = "G. brevipinnis")

# for talk
# names(fish_cols) <- c("\u012Ananga", "K\u014Daro",
#                       "Banded k\u014Dkopu", "Shortjaw k\u014Dkopu",
#                       "Giant k\u014Dkopu", "Spotted galaxias")
# 
# dat_count$species <-  recode(dat_count$species, 
#                              inanga = "\u012Ananga",
#                              spotted_galaxias = "Spotted galaxias",
#                              banded_kokopu = "Banded k\u014Dkopu",
#                              giant_kokopu = "Giant k\u014Dkopu",
#                              shortjaw_kokopu = "Shortjaw k\u014Dkopu",
#                              koaro = "K\u014Daro")

dat_count <- dat_count %>%
  filter(study_country != "Multiple")


p5 <- ggplot(dat_count, aes(species, n, fill = species)) +
  geom_bar(position = "dodge",  stat = "identity", alpha = 0.8) +
  xlab("Country") +
  ylab("Articles") +
  scale_fill_manual(values = fish_cols) +
    facet_grid(~study_country, scales = "free_x", space = "free_x", switch = "x") +
  guides(fill=guide_legend(title.position="top")) +
  theme_bw() +
  theme(text=element_text(size=14),
          axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "italic")) 


####################
## Spatial extent ##
####################

dat_space <- df2 %>%
  select(spatial_extent, spatial_extent_km)

dat_space2 <- dat_space %>%
  count(spatial_extent) %>%
  filter(spatial_extent != "na") %>%
  mutate(spatial_extent = tolower(spatial_extent))



dat_space2$spatial_extent <- factor(dat_space2$spatial_extent, 
                                    levels = c('lab','reach','basin',
                                               'regional', 'international'))

p6 <- ggplot(dat_space2, aes(spatial_extent, n)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  xlab("Spatial extent") +
  ylab("Articles") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))  +
  theme(text=element_text(size=14))



dat_space3 <- dat_space %>%
  filter(!spatial_extent_km %in% c("na", "NA")) %>%
  mutate(spatial_extent_km = as.numeric(spatial_extent_km)) %>%
  filter(!is.na(spatial_extent_km)) %>%
  filter(spatial_extent != "lab") %>%
  arrange(spatial_extent_km) %>%
  mutate(id = 1:nrow(.))


dat_space3
quantile(sort(dat_space3$spatial_extent_km), 0.25)
quantile(sort(dat_space3$spatial_extent_km), 0.5)
quantile(sort(dat_space3$spatial_extent_km), 0.75)

max(sort(dat_space3$spatial_extent_km))


p7 <- ggplot(dat_space3, aes(id, spatial_extent_km)) +
  geom_vline(xintercept = quantile(1:nrow(dat_space3), 0.25),
             colour = "grey", size = 1.5, alpha = 0.5) + 
  geom_vline(xintercept = quantile(1:nrow(dat_space3), 0.5),
             colour = "grey", size = 1.5, alpha = 0.5) + 
  geom_vline(xintercept = quantile(1:nrow(dat_space3), 0.75),
             colour = "grey", size = 1.5, alpha = 0.5) + 
  geom_point(alpha = 0.8) +
  scale_y_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                     labels = function(x) sprintf("%g", x)) +
  xlab("Studies") +
  ylab("Spatial extent (km)") +
  theme_bw()  +
  theme(text=element_text(size=14))



###################
## Temporal scale # 
###################

# ignore warning about NA's, it is text NA being converted to NA
dat_time <- df2 %>%
  select(temporal, period_yrs, period_freq, koaro, inanga, giant_kokopu, shortjaw_kokopu, banded_kokopu, spotted_galaxias) %>%
  filter(temporal == "Y") %>%
  mutate_at(c("period_yrs", "period_freq"), as.numeric) %>%
  na.omit() %>%
  pivot_longer(cols = koaro:spotted_galaxias, names_to = "species", values_to = "value") %>%
  filter(value == "Y")


# order species so colour match the pervious plot 
dat_time$species <- factor(dat_time$species, 
                           levels = c('inanga', "spotted_galaxias", 'banded_kokopu','koaro',
                                      'giant_kokopu', 'shortjaw_kokopu'))


dat_time$species <-  recode(dat_time$species,
                             inanga = "G. maculatus",
                             spotted_galaxias = "G. truttaceus",
                             banded_kokopu = "G. fasciatus",
                             giant_kokopu = "G. argenteus",
                             shortjaw_kokopu = "G. postvectis",
                             koaro = "G. brevipinnis")

# talk
# dat_time$species <-  recode(dat_time$species, 
#                              inanga = "\u012Ananga",
#                              spotted_galaxias = "Spotted galaxias",
#                              banded_kokopu = "Banded k\u014Dkopu",
#                              giant_kokopu = "Giant k\u014Dkopu",
#                              shortjaw_kokopu = "Shortjaw k\u014Dkopu",
#                              koaro = "K\u014Daro")
# 

# lines represent max age of each species - need to find this info! 
p8 <- ggplot(dat_time, aes(period_yrs, period_freq, colour = species)) +
  geom_vline(xintercept = 4, colour = fish_cols[1], size = 1, linetype = "dashed") + # inanga
  geom_vline(xintercept = 7, colour = fish_cols[6], size = 1, linetype = "dashed") + # spotted_galaxias
  geom_vline(xintercept = 12, colour = fish_cols[3], size = 1, linetype = "dashed") + # banded
  geom_vline(xintercept = 8, colour = fish_cols[2], size = 1, linetype = "dashed") + # koaro
  geom_vline(xintercept = 27, colour = fish_cols[5], size = 1, linetype = "dashed") + # giant
  geom_vline(xintercept = 9, colour = fish_cols[4], size = 1, linetype = "dashed") + # shortjaw
  geom_point(size = 2, alpha = 0.6) +
  scale_colour_manual(values = fish_cols) +
  scale_y_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                     labels = function(x) sprintf("%g", x)) +  xlab("Study temporal range (years)") +
  ylab("Samples/observations") +
  theme_bw() +
  guides(colour=guide_legend(title.position="top", title.hjust = 0.5, nrow = 3)) +
  theme(legend.position = "bottom")  +
  theme(text=element_text(size=14),
        legend.text = element_text(face = "italic"))


#############################
## Temporal * Spatial scale # 
#############################

dat_time <- df2 %>%
  select(temporal, period_yrs, spatial_extent_km, koaro, inanga, giant_kokopu, shortjaw_kokopu,
         banded_kokopu, spotted_galaxias) %>%
  mutate_at(c("period_yrs"), as.numeric) %>%
  mutate(period_yrs = ifelse(is.na(period_yrs), 0, period_yrs)) %>%
  filter(!is.na(spatial_extent_km)) %>%
  pivot_longer(cols = koaro:spotted_galaxias, names_to = "species", values_to = "value") %>%
  filter(value == "Y")


# dat_time$species <-  recode(dat_time$species, 
#                             inanga = "\u012Ananga",
#                             spotted_galaxias = "Spotted galaxias",
#                             banded_kokopu = "Banded k\u014Dkopu",
#                             giant_kokopu = "Giant k\u014Dkopu",
#                             shortjaw_kokopu = "Shortjaw k\u014Dkopu",
#                             koaro = "K\u014Daro")


dat_time$species <-  recode(dat_time$species,
                            inanga = "G. maculatus",
                            spotted_galaxias = "G. truttaceus",
                            banded_kokopu = "G. fasciatus",
                            giant_kokopu = "G. argenteus",
                            shortjaw_kokopu = "G. postvectis",
                            koaro = "G. brevipinnis")


# lines represent max age of each species - need to find this info! 
p8b <- ggplot(dat_time, aes(period_yrs, spatial_extent_km, colour = species, label = species)) +
  geom_vline(xintercept = 4, colour = fish_cols[1], size = 1, linetype = "dashed") + # inanga
  geom_vline(xintercept = 7, colour = fish_cols[6], size = 1, linetype = "dashed") + # spotted_galaxias
  geom_vline(xintercept = 12, colour = fish_cols[3], size = 1, linetype = "dashed") + # banded
  geom_vline(xintercept = 8, colour = fish_cols[2], size = 1, linetype = "dashed") + # koaro
  geom_vline(xintercept = 27, colour = fish_cols[5], size = 1, linetype = "dashed") + # giant
  geom_vline(xintercept = 9, colour = fish_cols[4], size = 1, linetype = "dashed") + # shortjaw
  geom_point(size = 2, alpha = 0.6) +
  scale_colour_manual(values = fish_cols) +
  scale_y_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                     labels = function(x) sprintf("%g", x)) +
  labs(x = "Study temporal range (years)", y = "", colour = "Species") +
  theme_bw() +
  guides(colour=guide_legend(title.position="top", title.hjust = 0.5, nrow = 3)) +
  theme(legend.position = "bottom",
        legend.text = element_text(face = "italic"))  +
  theme(text=element_text(size=14),
        plot.margin = unit(c(1,0.5,0.5,0), "cm"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())




#####################
## analysis method ##
#####################

dat_meth <-  df2 %>%
  select(survey, model, experiment, ex_field, ex_lab, review) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "value") %>%
  group_by(Method) %>%
  count(value) %>%
  filter(value == "Y")

dat_meth <- rbind(dat_meth, data.frame(Method = "modelling", value = "Y", n = 0))

dat_meth <- dat_meth %>%
  filter(Method != "experiment")

dat_meth$Method <-  recode(dat_meth$Method, ex_lab = "lab experiment", 
                           ex_field = "field experiment")

p9 <- ggplot(dat_meth, aes(reorder(Method, -n), n)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_x_discrete(na.translate = FALSE, guide = guide_axis(n.dodge = 2)) +
  xlab("Method") +
  ylab("Articles") +
  theme_bw() +
  theme(text=element_text(size=14))

################################
## system (stream/lake/ocean) ##
################################

dat_sys <-  df2 %>%
  select(stream, lake, ocean, landlocked) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "value") %>%
  group_by(Method) %>%
  count(value) %>%
  filter(value == "Y") %>%
  filter(Method != "landlocked")

dat_sys$Method[dat_sys$Method == "lake"] <- "lentic"
dat_sys$Method[dat_sys$Method == "stream"] <- "lotic"
dat_sys$Method[dat_sys$Method == "ocean"] <- "marine"


p10 <- ggplot(dat_sys, aes(reorder(Method, -n), n)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  xlab("System") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(size=14))

#############
## Methods ##
#############

df2$method_other <- tolower(df2$method_other)

dat_meth <-  df2 %>%
  mutate(method_other  = 
           ifelse(method_other == "metabolic rates", "respirometry", 
                  ifelse(method_other == "mark recapture", "tagging", 
                         ifelse(method_other %in% c("prey removal", "predation", "interaction"), 
                                "species interactions",
                                ifelse(method_other %in% c("dna", "genetics"), "genetic analysis",
                                       method_other))))) %>%
  select(method_other) %>%
  filter(method_other != "na") %>%
  count(method_other) %>%
  arrange(-n) %>%
  top_n(10)


p11 <- ggplot(dat_meth, aes(reorder(method_other, -n), n)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  xlab("Methods") +
  ylab("Number of articles") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##################
## Save figures ##
##################


p2a <- p2 + theme(legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 10))

p5a <- p5 + theme(legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 10))



cowplot::plot_grid(p5a, p2a, p1, p3, align = "h", axis = "b", nrow = 2, labels="AUTO")

ggsave(filename = "figures/2_publishing_journal.png", width = 22,
       height = 20, dpi = 300, units = "cm")


p5
ggsave(filename = "figures/3_species_country.png", width = 18,
       height = 8, dpi = 300, units = "cm")



combined <- p7 + p8b  + p6 + guide_area() + 
  plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A')

# annotate function
fun_annnotate <- function(plt, x, y, colr, text){
  cowplot::ggdraw(plt) + cowplot::draw_label(text, x = x, y = y, color = colr,
                                             size = 8)
}

# annotate above plot
plt_c2 <- fun_annnotate(combined, x = 0.2, y = 0.92, colr = "black", text = expression(paste("25"^"th")))
plt_c2 <- fun_annnotate(plt_c2, x = 0.29, y = 0.92, colr = "black", text = expression(paste("50"^"th")))
plt_c2 <- fun_annnotate(plt_c2, x = 0.38, y = 0.92, colr = "black", text = expression(paste("75"^"th")))
plt_c2

ggsave("figures/4_spatiotemporal.png", width = 20, height = 20, units = "cm", dpi = 300)


(p9 + p10) + plot_annotation(tag_levels = 'A')
ggsave("figures/5_method_sytem_keyword.pdf", width = 18, height = 7, units = "cm")
ggsave("figures/5_method_sytem_keyword.png", width = 18, height = 7, units = "cm", dpi = 300)




##################
## TALK Save figures ##
##################


cowplot::plot_grid(p2, p1, p3, align = "h", axis = "b", nrow = 1)

ggsave(filename = "figures/2_publishing_journal.png", width = 30,
       height = 14, dpi = 300, units = "cm")


cowplot::plot_grid(p3, p9, align = "h", axis = "b", nrow = 1)

ggsave(filename = "figures/2_publishing_methods.png", width = 30,
       height = 14, dpi = 300, units = "cm")


combined <-  (p2 | guide_area())/ p5 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")


ggsave(filename = "figures/3_species_country.png", width = 18, height = 8, dpi = 300, units = "cm")


combined <-  p7 + p8b + plot_layout(widths = c(1, 1.5)) & theme(legend.position = "bottom") 



plt_c2 <- fun_annnotate(plt_c2, x = 0.53, y = 0.94, colr = fish_cols[1], text = "\u012Ananga")

plt_c2 <- fun_annnotate(plt_c2, x = 0.56, y = 0.97, colr = fish_cols[6], text = "Spotted")
plt_c2 <- fun_annnotate(plt_c2, x = 0.58, y = 0.94, colr = fish_cols[2], text = "K\u014Daro")
plt_c2 <- fun_annnotate(plt_c2, x = 0.60, y = 0.97, colr = fish_cols[4], text = "Shortjaw")
plt_c2 <- fun_annnotate(plt_c2, x = 0.62, y = 0.94, colr = fish_cols[3], text = "Banded")
plt_c2 <- fun_annnotate(plt_c2, x = 0.79, y = 0.94, colr = fish_cols[5], text = "Giant")
plt_c2


ggsave("figures/4_spatiotemporal.png", width = 18, height = 18, units = "cm", dpi = 300)
ggsave("figures/4_spatiotemporal.pdf", width = 18, height = 18, units = "cm")


p2 + p5
ggsave("figures/3_studies_by_species.png", width = 33, height = 13, units = "cm", dpi = 300)



  