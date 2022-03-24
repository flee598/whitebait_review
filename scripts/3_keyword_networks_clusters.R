
# keyowrd network - doesn't work very well as everything is stringly connected
# to everything else. 

# required packages
library(tidyverse)
library(igraph)
library(ggraph)

min_yr <- 1960
mx_yr <- 1980
db <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")


fun_auth_network <- function(db, min_yr, mx_yr){
  
  df_keyw <- db %>%
    filter(!is.na(AuthorKeywords)) %>%
    filter(YearPublished >= min_yr & YearPublished <= mx_yr)

  if (nrow(df_keyw) == 0) graph <- NA

  if (nrow(df_keyw) > 0) {
  
df_keyw2 <- df_keyw %>%
  pull(AuthorKeywords) %>%
  str_split(";") %>%
  lapply(function(x){
    z <- str_trim(x, side = "both")
    gsub("[^[:alnum:][:space:]]", "", z)
  })

# recode keywords
df_keyw2 <- lapply(df_keyw2, function(x){
  
  x <- recode(x, "GIANT KOKOPU" = "GALAXIAS ARGENTEUS")
  x <- recode(x, "GIANT KKOPU" = "GALAXIAS ARGENTEUS")
  
  x <- recode(x,"BANDED KOKOPU" = "GALAXIAS FASCIATUS")
  x <- recode(x,"GALAXIASFASCIATUS" = "GALAXIAS FASCIATUS")
  x <- recode(x,"BANDED KKOPU" = "GALAXIAS FASCIATUS")
  
  x <- recode(x,"KOARO" = "GALAXIAS BREVIPINNIS")
  x <- recode(x,"GBREVIPINNIS" = "GALAXIAS BREVIPINNIS")
  
  x <- recode(x,"GULAXIAS MACULOTUS" = "GALAXIAS MACULATUS")
  x <- recode(x,"GMACULATUS" = "GALAXIAS MACULATUS")
  x <- recode(x,"INANGA" = "GALAXIAS MACULATUS")
  x <- recode(x,"GALAXIASMACULATUS" = "GALAXIAS MACULATUS")
  x <- recode(x,"COMMON GALAXIAS" = "GALAXIAS MACULATUS")
  
  x <- recode(x,"GALAXIIDS" = "GALAXIIDAE")
  x <- recode(x,"GALAXIID" = "GALAXIIDAE")
  x <- recode(x,"GALAXID" = "GALAXIIDAE")
  
  x <- recode(x,"GALAXIAS SPP" = "GALAXIAS")
  
  x <- recode(x,"DIADROMOUS" = "DIADROMY")
  x <- recode(x,"DIADROMOUS FISH" = "DIADROMY")
  
  x <- recode(x,"STREAMS" = "STREAM")
  x <- recode(x,"RIVERS" = "STREAM")
  x <- recode(x,"LAKES" = "LAKE")
  
  x <- recode(x,"AMPHIDROMOUS" = "AMPHIDROMY")
  x <- recode(x,"AMPHIDROMOUS SPECIES" = "AMPHIDROMY")
  x <- recode(x,"FISH DISTRIBUTION" = "DISTRIBUTION")
  x <- recode(x,"FISH RAMP" = "RAMP")
  
  x <- recode(x,"GENETIC DIVERSITY" = "GENETICS")
  x <- recode(x,"LIFE" = "LIFE-HISTORY")
  x <- recode(x,"ANDEAN LAKES AND RESERVOIRS" = "ANDEAN LAKES")
  x <- recode(x,"AUSTRALIAN GRAYLING" = "PROTOTROCTES MARAENA")
  x <- recode(x,"BEFORE AFTER CONTROL IMPACT" = "BACI")
  x <- recode(x,"BEHAVIOUR" = "BEHAVIOURAL")
  
  x <- recode(x,"BROWN TROUT" = "SALMO TRUTTA")
  x <- recode(x,"CHOICE EXPERIMENT" = "CHOICE")
  x <- recode(x,"CLADOCERANS" = "CLADOCERA")
  x <- recode(x,"COMMON BULLIES" = "GOBIOMORPHUS COTIDIANUS")
  x <- recode(x,"COMMON BULLY" = "GOBIOMORPHUS COTIDIANUS")
  
  x <- recode(x,"CONSERVATION PLANNING" = "CONSERVATION")
  x <- recode(x,"DETECTION ISSUES" = "DETECTION")
  x <- recode(x,"DIADROMOUS FISHES" = "DIADROMY")
  x <- recode(x,"DIADROMOUS MIGRATION" = "DIADROMY")
  x <- recode(x,"DIEL ACTIVITY" = "DIEL")
  x <- recode(x,"DIEL HABITAT USE" = "DIEL")
  x <- recode(x,"DOMINANCE HIERARCHY" = "DOMINANCE")
  x <- recode(x,"EGG PREDATION" = "EGG MORTALITY")
  x <- recode(x,"EXPERIMENTAL ECOLOGY" = "EXPERIMENT")
  x <- recode(x,"EXPERIMENTS" = "EXPERIMENT")
  x <- recode(x,"FEEDING DIEL ACTIVITY" = "FEEDING")
  x <- recode(x,"FEEDING HABITAT" = "FEEDING")
  x <- recode(x,"FEEDING SELECTIVITY" = "FEEDING")
  x <- recode(x,"FEEDING TACTICS" = "FEEDING")
  x <- recode(x,"FERTILISATION SUCCESS" = "FERTILISATION")
  x <- recode(x,"FISH ABUNDANCE" = "ABUNDANCE")
  x <- recode(x,"FISH BARRIER" = "BARRIER")
  x <- recode(x,"FISH BEHAVIOUR" = "BEHAVIOUR")
  x <- recode(x,"FISH CULTURE" = "CULTURE")
  x <- recode(x,"FISH DENSITY" = "DENSITY")
  x <- recode(x,"FISH DISTRIBUTION" = "DISTRIBUTION")
  x <- recode(x,"FISH EGG INCUBATION" = "EGG INCUBATION")
  x <- recode(x,"FISH GROWTH" = "GROWTH")
  x <- recode(x,"FISH HABITAT" = "HABITAT")
  x <- recode(x,"FISH INTRODUCTION" = "INTRODUCTION")
  x <- recode(x,"FISH INTRODUCTIONS" = "INTRODUCTION")
  x <- recode(x,"FISH LARVAE" = "LARVAE")
  x <- recode(x,"FISH MIGRATION" = "MIGRATION")
  x <- recode(x,"FISH MORPHOMETRY" = "MORPHOMETRY")
  x <- recode(x,"FISH NUMBERS" = "NUMBERS")
  x <- recode(x,"FISH ONTOGENY" = "ONTOGENY")
  x <- recode(x,"FISH PASSAGE" = "PASSAGE")
  x <- recode(x,"FISH PASSES" = "PASSAGE")
  x <- recode(x,"FISH PREDATION" = "PREDATION")
  x <- recode(x,"FISH RAMP" = "RAMP")
  x <- recode(x,"FISHES" = "FISH")
  x <- recode(x,"FLUVIAL FORM" = "FLUVIAL MORPHOLOGY")
  x <- recode(x,"FOOD WEB THEORY" = "FOOD WEBS")
  x <- recode(x,"FRESHWATER FISHES" = "FRESHWATER FISH")
  x <- recode(x,"GENETIC DIFFERENTIATION AND STRUCTURING" = "GENETICS")
  x <- recode(x,"GENETIC DISTANCE" = "GENETICS")
  x <- recode(x,"GENETIC DIVERSITY" = "GENETICS")
  x <- recode(x,"GENETIC STOCK" = "GENETICS")
  x <- recode(x,"GENETIC STRUCTURE" = "GENETICS")
  x <- recode(x,"GENETIC VARIATION" = "GENETICS")
  x <- recode(x,"HABITAT EDGES" = "HABITAT FRAGMENTATION")
  x <- recode(x,"HABITAT QUALITY" = "HABITAT USE")
  x <- recode(x,"HABITAT SUITABILITY" = "HABITAT USE")
  x <- recode(x,"HABITAT PREFERENCE" = "HABITAT USE")
  x <- recode(x,"HABITAT SHIFT" = "HABITAT USE")
  x <- recode(x,"HABITAT SHIFTS" = "HABITAT USE")
  x <- recode(x,"HYDROELECTRIC POWER" = "HYDROPOWER")
  x <- recode(x,"INSTREAM BARRIERS" = "BARRIER")
  x <- recode(x,"INSTREAM COVER" = "COVER")
  x <- recode(x,"INSTREAM FLOWS" = "FLOW")
  x <- recode(x,"INTERACTIONS" = "INTERACTION")
  x <- recode(x,"INTRODUCED SALMONIDS" = "INTRODUCED SPECIES")
  x <- recode(x,"INTRODUCED TROUT" = "INTRODUCED SPECIES")
  x <- recode(x,"INVASIVE SPECIES" = "INTRODUCED SPECIES")
  x <- recode(x,"INVASION BIOLOGY" = "INTRODUCED SPECIES")
  x <- recode(x,"LANDLOCKED FISH" = "LANDLOCKED")
  x <- recode(x,"LANDLOCKED FORM" = "LANDLOCKED")
  x <- recode(x,"LANDLOCKED OR DIADROMOUS LIFE STYLE" = "LANDLOCKED")
  x <- recode(x,"LANDLOCKED POPULATION" = "LANDLOCKED")
  x <- recode(x,"LANDLOCKING" = "LANDLOCKED")
  x <- recode(x,"LARVA" = "LARVAE")
  x <- recode(x,"LARVAL" = "LARVAE")
  x <- recode(x,"LARVAL FISH" = "LARVAE")
  x <- recode(x,"LIFE" = "LIFE HISTORY")
  x <- recode(x,"LIFE HISTORY THEORY" = "LIFE HISTORY")
  x <- recode(x,"LIFE STRATEGIES" = "LIFE HISTORY")
  x <- recode(x,"LIFEHISTORY PLASTICITY" = "LIFE HISTORY")
  x <- recode(x,"NEWZEALAND" = "NEW ZEALAND")
  x <- recode(x,"LONGFINNED EELS" = "ANGUILLA AUSTRALIS")
  x <- recode(x,"MATERNAL EFFECTS" = "MATERNAL EFFECT")
  x <- recode(x,"MATERNAL PROVISIONING" = "MATERNAL INVESTMENT")
  x <- recode(x,"MERISTIC COUNTS" = "MERISTIC")
  x <- recode(x,"MERISTIC VARIATION" = "MERISTIC")
  x <- recode(x,"METABOLIC" = "METABOLISM")
  x <- recode(x,"METABOLIC RATE" = "METABOLISM")
  x <- recode(x,"MICROHABITAT SELECTION" = "MICROHABITAT")
  x <- recode(x,"MICROSATELLITE MARKERS" = "MICROSATELLITE")
  x <- recode(x,"MICROSATELLITES" = "MICROSATELLITE")
  x <- recode(x,"MIGRATORY" = "MIGRATION")
  x <- recode(x,"MIGRATORY SPECIES" = "MIGRATION")
  x <- recode(x,"MORPHOMETRICS" = "MORPHOLOGY")
  x <- recode(x,"MORPHOMETRY" = "MORPHOLOGY")
  x <- recode(x,"NAHUEL HUAPI" = "NAHUEL")
  x <- recode(x,"NATIVE FRESH WATER FISH" = "NATIVE FISH")
  x <- recode(x,"NATIVE FRESHWATER FISH" = "NATIVE FISH")
  x <- recode(x,"NATURALLY ACIDIC WATERS" = "NATURALLY ACIDIC")
  x <- recode(x,"NONNATIVE PREDATORY FISH" = "INTRODUCED SPECIES")
  x <- recode(x,"NONNATIVE SALMONIDS" = "INTRODUCED SPECIES")
  x <- recode(x,"NONNATIVE SPECIES" = "INTRODUCED SPECIES")
  x <- recode(x,"NONVISUAL FEEDING" = "INTRODUCED SPECIES")
  x <- recode(x,"OFFSPRING FITNESS" = "OFFSPRING")
  x <- recode(x,"OFFSPRING SIZE" = "OFFSPRING")
  x <- recode(x,"OLFACTION GALAXIID" = "OLFACTION")
  x <- recode(x,"OLFACTORY" = "OLFACTION")
  x <- recode(x,"OLFACTORY CUES" = "OLFACTION")
  x <- recode(x,"OLIGOTROPHIC LAKE" = "OLIGOTROPHIC")
  x <- recode(x,"OLIGOTROPHIC LAKES" = "OLIGOTROPHIC")
  x <- recode(x,"OTOLITH CHEMISTRY" = "OTOLITH")
  x <- recode(x,"OTOLITH MICROCHEMISTRY" = "OTOLITH")
  x <- recode(x,"OTOLITH MICROSTRUCTURE" = "OTOLITH")
  x <- recode(x,"OTOLITHS" = "OTOLITH")
  x <- recode(x,"OXYGEN CONSUMPTION" = "OXYGEN")
  x <- recode(x,"OXYGEN TOLERANCE" = "OXYGEN")
  x <- recode(x,"PARASITEINDUCED" = "PARASITISM")
  x <- recode(x,"PARASITEINDUCED MORTALITY" = "PARASITISM")
  x <- recode(x,"PARTITIONED RESPIROMETRY" = "RESPIROMETRY")
  x <- recode(x,"PATAGONIAN" = "PATAGONIA")
  x <- recode(x,"PELAGIC FISH" = "PELAGIC")
  x <- recode(x,"PELAGIC LARVAL DURATION" = "PELAGIC")
  x <- recode(x,"PERCHED" = "PERCHED CULVERTS")
  x <- recode(x,"PHENOTYPIC VARIATION" = "PHENOTYPIC PLASTICITY")
  x <- recode(x,"PHEROMONES" = "PHEROMONE")
  x <- recode(x,"PHYLOGENETICS" = "PHYLOGENY")
  x <- recode(x,"PINE FOREST" = "PINE")
  x <- recode(x,"PINE TREES" = "PINE")
  x <- recode(x,"PIT TAGS" = "PIT TELEMETRY")
  x <- recode(x,"PORTABLE PIT TECHNOLOGY" = "PIT TELEMETRY")
  x <- recode(x,"PREDATOR AVOIDANCE" = "PREDATION")
  x <- recode(x,"PREDATORPREY INTERACTIONS" = "PREDATION")
  x <- recode(x,"PREDATORPREY REGULATION" = "PREDATION")
  x <- recode(x,"PREDATORPREY RELATIONSHIPS" = "PREDATION")
  x <- recode(x,"PUYEN" = "PUYE")
  x <- recode(x,"RAINBOW TROUT" = "ONCORHYNCHUS MYKISS")
  x <- recode(x,"RATES" = "RATE")
  x <- recode(x,"RECOLONIZATION" = "RECOLONISATION")
  x <- recode(x,"RECRUITMENT SOURCE" = "RECRUITMENT")
  x <- recode(x,"REDFIN BULLY" = "GOBIOMORPHUS HUTTONI")
  x <- recode(x,"REPRODUCTIVE" = "REPRODUCTION")
  x <- recode(x,"REPRODUCTIVE BEHAVIOUR" = "REPRODUCTION")
  x <- recode(x,"REPRODUCTIVE BIOLOGY" = "REPRODUCTION")
  x <- recode(x,"REPRODUCTIVE STRATEGY" = "REPRODUCTION")
  x <- recode(x,"REPRODUCTIVE TRAIT" = "REPRODUCTION")
  x <- recode(x,"RESERVOIRS" = "RESERVOIR")
  x <- recode(x,"RESOURCE" = "RESOURCE PARTITIONING")
  x <- recode(x,"RESOURCE ALLOCATION" = "RESOURCE PARTITIONING")
  x <- recode(x,"RETROPINNA" = "RETROPINNIDAE")
  x <- recode(x,"RIPARIAN FOREST COVER" = "RIPARIAN ZONE")
  x <- recode(x,"RIPARIAN ZONES" = "RIPARIAN ZONE")
  x <- recode(x,"RIPARIAN MANAGEMENT" = "RIPARIAN ZONE")
  x <- recode(x,"RIPARIAN VEGETATION" = "RIPARIAN ZONE")
  x <- recode(x,"RIVER MOUTH SELECTION" = "RIVER MOUTH")
  x <- recode(x,"RIVER MOUTHS" = "RIVER MOUTH")
  x <- recode(x,"RIVERS" = "STREAM")
  x <- recode(x,"SALINITY TOLERANCE" = "SALINITY")
  x <- recode(x,"SALMONIFORMES" = "SALMONID")
  x <- recode(x,"SALMONIDS" = "SALMONID")
  x <- recode(x,"SEASONAL VARIATIONS" = "SEASONAL VARIATION")
  x <- recode(x,"SEMICLOSED RESPIROMETRY" = "RESPIROMETRY")
  x <- recode(x,"SETTLEMENT AGE" = "SETTLEMENT")
  x <- recode(x,"SETTLEMENT MARK" = "SETTLEMENT")
  x <- recode(x,"SETTLEMENT SIZE" = "SETTLEMENT")
  x <- recode(x,"SHORTFIN EEL" = "ANGUILLA DIEFFENBACHII")
  x <- recode(x,"SHORTFINNED EELS" = "ANGUILLA DIEFFENBACHII")
  x <- recode(x,"SHORTJAW KOKOPU" = "GALAXIAS POSTVECTIS")
  x <- recode(x,"SHORTJAWED KOKOPU" = "GALAXIAS POSTVECTIS")
  x <- recode(x,"SIZE AT FIRST" = "SIZE")
  x <- recode(x,"SIZEATAGE" = "SIZE")
  x <- recode(x,"SIZE AT MATURITY" = "SIZE")
  x <- recode(x,"SOURCESINK" = "SOURCESINK DYNAMICS")
  x <- recode(x,"SOUTHERN SOUTH AMERICA" = "SOUTH AMERICA")
  x <- recode(x,"SOUTHEASTERN AUSTRALIA" = "AUSTRALIA")
  x <- recode(x,"SOUTHWESTERN AUSTRALIA" = "AUSTRALIA")
  x <- recode(x,"SPATIAL DISTRIBUTION" = "SPATIAL")
  x <- recode(x,"SPATIAL PATTERNS" = "SPATIAL")
  x <- recode(x,"SPATIAL PREDICTION" = "SPATIAL")
  x <- recode(x,"SPATIAL USE" = "SPATIAL")
  x <- recode(x,"SPAWNING BEHAVIOR" = "SPAWNING")
  x <- recode(x,"SPAWNING GROUNDS" = "SPAWNING")
  x <- recode(x,"SPAWNING MIGRATION" = "SPAWNING")
  x <- recode(x,"SPAWNING PERIOD" = "SPAWNING")
  x <- recode(x,"SPAWNING SITE" = "SPAWNING")
  x <- recode(x,"SPECIES DIVERSITY" = "DIVERSITY")
  x <- recode(x,"SPECIES EXTINCTION" = "EXTINCTION")
  x <- recode(x,"SPOTTED GALAXIAS" = "GALAXIAS TRUTTACEUS")
  x <- recode(x,"STABLE CARBON ISOTOPES" = "STABLE ISOTOPES")
  x <- recode(x,"STREAMS" = "STREAM")
  x <- recode(x,"STRONTIUM ISOTOPES" = "STRONTIUM")
  x <- recode(x,"SUPPLY LIMITED RECRUITMENT" = "RECRUITMENT")
  x <- recode(x,"SWIMMING MODE" = "SWIMMING")
  x <- recode(x,"SWIMMING PERFORMANCE" = "SWIMMING")
  x <- recode(x,"SWIMMING VELOCITY" = "SWIMMING")
  x <- recode(x,"TAXON RICHNESS" = "RICHNESS")
  x <- recode(x,"TEMPERATURE PREFERENCE" = "TEMPERATURE")
  x <- recode(x,"TEMPERATURE REFUGIA" = "TEMPERATURE")
  x <- recode(x,"TEMPORAL SEGREGATION" = "TEMPORAL ACTIVITY")
  x <- recode(x,"TERRESTRIAL EGG" = "TERRESTRIAL")
  x <- recode(x,"TERRESTRIAL FOODS" = "TERRESTRIAL")
  x <- recode(x,"TERRESTRIAL INVERTEBRATES" = "TERRESTRIAL")
  x <- recode(x,"THREATENED FISH" = "THREATENED")
  x <- recode(x,"THREATENED NATIVE GALAXIIDS" = "THREATENED")
  x <- recode(x,"TNANGA" = "GALAXIAS MACULATUS")
  x <- recode(x,"TOLERANT SPECIES" = "TOLERANCE")
  x <- recode(x,"TOXICITY TESTING" = "TOXICITY")
  x <- recode(x,"TROUT INVASION" = "INTRODUCED SPECIES")
  x <- recode(x,"TROUT MINNOW" = "GALAXIAS TRUTTACEUS")
  x <- recode(x,"TURBIDITY GUIDELINES" = "TURBIDITY")
  x <- recode(x,"URBAN DEVELOPMENT" = "URBAN")
  x <- recode(x,"URBAN STREAMS" = "URBAN")
  x <- recode(x,"VULGARIS" = "GALAXIAS VULGARIS")
  x <- recode(x,"WATER QUALITY REMEDIATION" = "WATER QUALITY")
  x <- recode(x,"WOOD MATERIAL" = "WOOD")
  x <- recode(x,"ZEALAND" = "NEW ZEALAND")
  x <- recode(x,"KOKOPU" = "GALAXIAS")
  x <- recode(x,"FRESHWATER FISH" = "FISH")
  x <- recode(x,"HABITAT USE" = "HABITAT")
  x <- recode(x,"PUYE" = "GALAXIAS MACULATUS")
  return(x)
})

# Edges between the strings
edges <- df_keyw2 %>%
  lapply(function(x) {
    expand.grid(x, x, stringsAsFactors = FALSE)
  }) %>%
  bind_rows() %>%
  filter(Var1 != "") %>%
  filter(Var2 != "") %>%
  group_by(Var1, Var2) %>%
  summarise(tot = n()) %>%
  arrange(-tot) 

# # try removing dominant key words to clarrify - didn't help much
# rmv <- c("GALAXIAS MACULATUS", "GALAXIIDAE", "GALAXIAS BREVIPINNIS",
#          "GALAXIAS FASCIATUS", "GALAXIAS ARGENTEUS", "GALAXIAS", 
#          "GALAXIAS POSTVECTIS", "GALAXIAS TRUTTACEUS",
#          "NEW ZEALAND", "PATAGONIA")
# 
# edges <- edges %>%
#   filter(!Var1 %in% rmv) %>%
#   filter(!Var2 %in% rmv)

# # get authors with > x papers
# key_cnt <- edges %>%
#   filter(Var1 == Var2) %>%
#   head(50)
# 
# # filter edges to only the authors above
# edges <- edges %>%
#   filter(Var1 %in% key_cnt$Var1) %>%
#   filter(Var2 %in% key_cnt$Var1)

# convert to igraph object
graph <- graph_from_data_frame(edges)
graph <- as.undirected(graph)

}

return(graph)
}


db <- read.csv(file = "data/created/biblio_db_manual_updates5.csv")


fun_auth_network(db, min_yr = 1960, mx_yr = 1980)

# graph gor each decade
min_yr = c(1968, seq(1981, 2011, by = 10))
mx_yr = seq(1980, 2020, by = 10)

nets <- mapply(fun_auth_network, min_yr = min_yr, mx_yr = mx_yr,
               MoreArgs = list(db = db), SIMPLIFY = F)

nets <- nets[2:5]

# clusters and cluster sizes
g_size <- setNames(sapply(nets, function(x) igraph::clusters(x)$csize),
                   c("1980s", "1990s", "2000s", "2010s"))

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