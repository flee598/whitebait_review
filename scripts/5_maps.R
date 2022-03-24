
library(compmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgbif)
library(patchwork)

# functions -------------------------------------------------------------------
fun_prop_region_studies <- function(fish_records, country_map) {
 
  fish_records <- fish_records %>% 
    filter_at(vars(decimalLatitude, decimalLongitude),all_vars(!is.na(.)))
  
  fish_records <- fish_records %>%
    sf::st_as_sf(coords = c("decimalLongitude", 
                            "decimalLatitude"), 
                 crs = 4326) %>%
    select(-year, -scientificName)
  
  colnames(fish_records)[2] <- "points_region"
  
  
  colnames(country_map)[2] <- "map_region"
  country_map <- country_map %>%
    sf::st_as_sf(crs = 4326)
  
  # find points inside polygon (i.e. not at sea etc.)
  mat <- sf::st_intersects(fish_records, country_map)
  fish_records <- fish_records[apply(mat, 1, any) == T, ]
  
  
  # pull index of point regions
  zz <- sf::st_intersects(fish_records, country_map)
  
  # for some reason some SA points pull regions twice so the vector is the 
  # wrong length, this only takes the first index per point
  zz <- unlist(purrr::map(zz, 1))
  
  # add map_region to fish_records
  fish_records$map_region <- country_map$map_region[zz]
  
  # relevant columns
  xx <- fish_records %>%
    select(country, map_region)
  
  # remove geometry
  sf::st_geometry(xx) = NULL
  
  xx %>%
    count(map_region)
  
  # calc prop fish records per region
  xx <- xx %>%
    select(country, map_region) %>%
    count(country, map_region) %>%
    group_by(country) %>%
    mutate(tot = sum(n)) %>%
    group_by(country, map_region) %>%
    mutate(PropRecords = n/tot) %>%
    select(country, map_region, PropRecords)
  
  # add fish records to map
  country_map <- left_join(country_map, xx, by = "map_region")
  
  # calc difference between fish records and study records
  country_map <- country_map %>%
    mutate_at(vars(prop), ~tidyr::replace_na(., 0)) %>%
    mutate(propDifference = prop - PropRecords)
  return(country_map)
}



fun_plot_density_map <- function(country_map, fish_points){
  
  # get country outline
  aus_whole <- sf::st_union(country_map$geometry)
  aus_whole <- sf::st_make_valid(aus_whole)
  
  # get bounding box + extra padding
  aus_bbox <- sf::st_bbox(aus_whole) 
  aus_bbox[1] <- aus_bbox[1] - aus_bbox[1] * 0.01 # left
  aus_bbox[3] <- aus_bbox[3] + aus_bbox[3] * 0.01 # right
  aus_bbox[2] <- aus_bbox[2] + aus_bbox[2] * 0.03 # bottom
  aus_bbox[4] <- aus_bbox[4] - aus_bbox[4] * 0.1  # top
  
  if (aus_bbox[3] > 180) {
    aus_bbox[3] <- 179.99
  }
  
  # as sf
  aus_bbox <- aus_bbox %>%
    sf::st_as_sfc() %>% 
    sf::st_as_sf()
  
  # mask box with aus outline
  aus_mask <- sf::st_difference(aus_bbox, aus_whole)
  
  ggplot() +
    geom_sf(data = country_map, colour = "black") +
    theme_void() +
    stat_density2d(aes(x = decimalLongitude, y = decimalLatitude, 
                       fill = ..level..,alpha = ..level..),
                   bins = 100, 
                   geom = "polygon",
                   data = fish_points) +
    scale_fill_viridis_c() +
    geom_sf(data = aus_mask, fill = 'white', colour = NA) + # mask sea with white
    geom_sf(data = aus_whole, colour = "black", fill = NA) + # return outline
    theme(legend.position = "none")  
}

# Map preprocessing -----------------------------------------------------------
# calculate summary of records by country and region, download maps of 
# countries, get GBIF and Atlas of Aus data. 

# study summary
df1 <- read.csv("data/created/biblio_db_manual_updates5.csv")


# meta data 
df2 <- df1 %>%
  select(ID, study_country, study_region) %>%
  filter(study_country != "GHANA")

df3 <- df2 %>%
  separate(study_region, sep = ",", into = letters[1:13]) %>%
  pivot_longer(cols = a:m, names_to = "names", values_to = "region") %>%
  na.omit() %>%
  select(-names) %>%
  filter(region != "na") %>%
  mutate(region = stringr::str_trim(region, side = "both"),
         study_country = stringr::str_trim(study_country, side = "both"))

# manually enter double country study
df3 %>%
  filter(study_country == "CHILE, ARGENTINA")

df3b <- data.frame(
  ref = rep(93, 5),
  study_country = c("ARGENTINA", "ARGENTINA", "CHILE", "CHILE", "CHILE"),
  region = c("Patagonia", "Magallanes", "O'Higgins", "Ñuble", "Los Ríos")
)

df3 <- rbind(df3, df3b)

df4 <- df3 %>%
  filter(study_country != "Chile, Argentina") %>%
  mutate(region = recode(region,
                         "BoP" = "Bay of Plenty",
                         "Victoriua" = "Victoria",
                         "Nelson" = "Nelson-Tasman",
                         "Ñuble" = "Southern Chile",
                         "Biobío" = "Southern Chile",
                         "Araucanía" = "Southern Chile",
                         "La Araucanía" = "Southern Chile",
                         "Los Ríos" = "Southern Chile",
                         "Los Lagos" = "Los Lagos",
                         "Aisén" = "Aisén",
                         "Magallanes" = "Magallanes",
                         "O'Higgins" = "Central Chile",
                         "Atacama" = "Norte Chico",
                         "Coquimbo" = "Norte Chico",
                         "Maule" = "Central Chile",
                         "Valparaíso" = "Central Chile")) %>%
  group_by(study_country) %>%
  count(region) %>%
  group_by(study_country) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(study_country, -prop)


# save study summary
saveRDS(df4, file = "data/created/map_study_summary.rds")

df4 <- readRDS(file = "data/created/map_study_summary.rds")


## GET MAPS -------------------------------------------------------------------

# Get country codes for countries of interest
raster::getData('ISO3') %>%
  filter(NAME %in% c("New Zealand", "Australia", "Argentina", "Chile"))

# get small file size map 
nz <- fun_get_map(cntry = "NZ", kp = 0.02, min_area = 1500, pth = "maps")
aus <- fun_get_map(cntry = "AUS", kp = 0.02, min_area = 1000, pth = "maps")
arg1 <- fun_get_map(cntry = "ARG", kp = 0.02, min_area = 1000, pth = "maps")
chl <- fun_get_map(cntry = "CHL", kp = 0.02, min_area = 1000, pth = "maps")

my_maps <- rbind(nz, aus, arg1, chl) %>%
  mutate(NAME_1 = recode(NAME_1, 
                       'Manawatu-Wanganui' = "Manawatu-Whanganui",
                       'New South Wales' = "NSW",
                       "Tierra del Fuego" = "Patagonia", 
                       "Santa Cruz"  = "Patagonia",
                       "Chubut" = "Patagonia",
                       "Río Negro" = "Patagonia",
                       "Neuquén" = "Patagonia",
                       "Mendoza" = "Cuyo",
                       "San Luis" = "Cuyo",
                       "San Juan" = "Cuyo",
                       "Buenos Aires" = "Pampas",
                       "La Pampa" = "Pampas",
                       "Córdoba" = "Pampas",
                       "Santa Fe" = "Pampas",
                       "Ciudad de Buenos Aires" = "Pampas",
                       "Entre Ríos" = "Mesopotamia",
                       "Corrientes" = "Mesopotamia",
                       "Misiones" = "Mesopotamia",
                       "Chaco" = "Chaco",
                       "Formosa" = "Chaco",
                       "La Rioja" = "Northwest",
                       "Catamarca" = "Northwest",
                       "Santiago del Estero" = "Northwest",
                       "Salta" = "Northwest",
                       "Jujuy" = "Northwest",
                       "Tucumán" = "Northwest",
                       "Arica y Parinacota" = "Norte Grande", 
                       "Tarapacá"  = "Norte Grande",
                       "Antofagasta" = "Norte Grande",
                       "Atacama" = "Norte Chico",
                       "Coquimbo" = "Norte Chico",
                       "Valparaíso" = "Central Chile",
                       "Libertador General Bernardo O'Higgins" =
                         "Central Chile",
                       "Maule" = "Central Chile",
                       "Región Metropolitana de Santiago" = "Central Chile",
                       "Bío-Bío" = "Southern Chile",
                       "Araucanía" = "Southern Chile",
                       "Los Ríos" = "Southern Chile",
                       "Ñuble" = "Southern Chile",
                       "Los Lagos" = "Los Lagos",
                       "Aisén del General Carlos Ibáñez del Campo" = "Aisén",
                       "Magallanes y Antártica Chilena" = "Magallanes")) %>%
  mutate( 
         region = ifelse(NAME_1 == "Nelson" | NAME_1 == "Tasman", 
                         "Nelson-Tasman", NAME_1)) %>%
  group_by(NAME_0, region) %>% 
  summarise(do_union = TRUE) %>%
  left_join(., df4, by = "region", keep = FALSE) %>%
  select(-study_country) %>%
  rename(country = NAME_0)


# get rgbif observations for whitebait ----------------------------------------
sp <- c('Galaxias maculatus', "Galaxias brevipinnis", "Galaxias truttaceus",
        "Galaxias	fasciatus", "Galaxias argenteus", "Galaxias postvectis")

gbif_fish <- lapply(sp, function(x) occ_search(scientificName = x,
                                               limit = 100000))

gbif_fish2 <- bind_rows(unlist(lapply(gbif_fish,"[", 3), recursive = F)) %>% 
  select(scientificName, decimalLatitude, decimalLongitude, occurrenceStatus,
         basisOfRecord, lifeStage, stateProvince, year, geodeticDatum, country)

# get living atlas Australia (ala) observations for whitebait -----------------
ala <- read.csv("data/raw/aus_living_atlas/records-2021-06-17.csv")

# select relevant records
ala2 <- ala %>%
  select(basisOfRecord, year, country, stateProvince,
        decimalLatitude, decimalLongitude, scientificName)


# combine and save fish occurences ---------------------------------------------
# countries to keep - throw away a few islands and dodgy records
cntry <- c("Argentina", "Australia", "Chile", "New Zealand")

# join Living altas and GBIF records
all_fish <- plyr::rbind.fill(ala2, gbif_fish2) %>%
  filter( basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN") & 
         occurrenceStatus != "ABSENT" & country %in% cntry) %>%
  select(!occurrenceStatus:geodeticDatum, -basisOfRecord) %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) 


# save data and maps
saveRDS(all_fish, "data/created/all_fish.rds")

saveRDS(my_maps, file = "data/created/my_maps.rds")

# End of data prep 

# MAPPING ---------------------------------------------------------------------

# load maps and species observations
# stud_sum <- readRDS(file = "data/created/map_study_summary.rds")
my_maps <- readRDS("data/created/my_maps.rds")
all_fish <- readRDS("data/created/all_fish.rds")

# country maps
nz <- filter(my_maps, country == "New Zealand")
aus <- filter(my_maps, country == "Australia")           
arg1 <- filter(my_maps, country == "Argentina")                            
chl <- filter(my_maps, country == "Chile")

sth_am <- filter(my_maps, country %in% c("Chile", "Argentina"))


# add common names
all_fish <- all_fish %>%
  filter(scientificName %in% c("Galaxias maculatus (Jenyns, 1842)",
                               "Galaxias brevipinnis Günther, 1866",
                               "Galaxias truttaceus Valenciennes, 1846",
                               "Galaxias fasciatus Gray, 1842",
                               "Galaxias postvectis Clarke, 1899",
                               "Galaxias argenteus (Gmelin, 1789)",
                               "Galaxias truttaceus Valenciennes, 1846"))

all_fish <- all_fish %>%
  mutate(common_name = ifelse(scientificName == "Galaxias maculatus (Jenyns, 1842)", "G. maculatus",
                       ifelse(scientificName == "Galaxias brevipinnis Günther, 1866", "G. brevipinnis",
                       ifelse(scientificName == "Galaxias fasciatus Gray, 1842", "G. fasciatus",
                       ifelse(scientificName == "Galaxias postvectis Clarke, 1899", "G. postvectis",
                       ifelse(scientificName == "Galaxias argenteus (Gmelin, 1789)", "G. argenteus",
                              "G. truttaceus")))))) %>%
  filter(!is.na(common_name))





# split occurrences by out countries
fish_nz <- filter(all_fish, country == "New Zealand")
fish_arg <- filter(all_fish, country == "Argentina")
fish_chl <- filter(all_fish, country == "Chile")
fish_aus <- filter(all_fish, country == "Australia")

# colour scale
my_cols <- cartography::carto.pal(pal1 = "green.pal", n1 = 20)
RColorBrewer::display.brewer.all()

myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "GnBu"))
myPalette <- colorRampPalette(cartography::carto.pal(pal1 = "green.pal", n1 = 20))


sc <- scale_fill_gradientn(colours = myPalette(100), 
                           limits = c(0, max(c(nz$n, aus$n, sth_am$n), na.rm = T)))

## NZ -------------------------------------------------------------------------

# drop Chatham Islands records
fish_nz <- fish_nz %>%
  filter(decimalLongitude > 0)

# add variable: propDifference, the difference in the number of records 
# relative to the number of studies
nz2 <- fun_prop_region_studies(fish_nz, nz)

p1nz <- ggplot() +
  geom_sf(data = nz2, aes(fill = propDifference), colour = "black") +
  colorspace::scale_fill_continuous_divergingx(palette = 'BrBG', mid = 0) + 
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Studies:Records")

nz2 <- nz2 %>%
  mutate_at(vars(n), ~tidyr::replace_na(., 0))



p2nz <- ggplot() +
  geom_sf(data = nz2, aes(fill = n), colour = "black") +
  annotate("text", x = 172, y = -38, label = paste("n = ", sum(nz2$n, na.rm = T), sep = ""), size = 5) +
  theme(legend.position = "bottom") +
  labs(fill = "Studies") +
  theme_void() +
  sc  +
  theme(text=element_text(size=14))




plt_all <- p2nz + p1nz & theme(legend.position = "bottom")

## Aus -------------------------------------------------------------------------

# add variable: propDifference, the difference in the number of records 
# relative to the number of studies
aus2 <- fun_prop_region_studies(fish_aus, aus)

# cahnge in range states from NA to 0
aus2$n[1] <- 0
aus2$n[5] <- 0

p1aus <- ggplot() +
  geom_sf(data = aus2, aes(fill = propDifference), colour = "black") +
  colorspace::scale_fill_continuous_divergingx(palette = 'BrBG', mid = 0) + 
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Studies:Records")




p2aus <- ggplot() +
  geom_sf(data = aus2, aes(fill = n), colour = "black") +
  annotate("text", x = 125, y = -12, label = paste("n = ", sum(aus2$n, na.rm = T), sep = ""), size = 5) +
  sc +
  labs(fill = "Studies") + 
  theme_void() +
  theme(legend.position = "bottom")  +
  theme(text=element_text(size=14))


plt_all <- p2aus + p1aus & theme(legend.position = "bottom")

# South America  --------------------------------------------------------------


# combine Chl and Arg
fish_st_am <- rbind(fish_chl, fish_arg)

sth_am2 <- fun_prop_region_studies(fish_st_am, sth_am)

sth_am2$n[8] <- 0
sth_am2$n[14] <- 0


p1sa <- ggplot() +
  geom_sf(data = sth_am2, aes(fill = propDifference), colour = "black") +
  colorspace::scale_fill_continuous_divergingx(palette = 'BrBG', mid = 0) + 
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Studies:Records")

# total number of papers for each country
sth_n <- sth_am2 %>%
  group_by(country.x) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  pull(n)

p2sa <- ggplot() +
  geom_sf(data = sth_am2, aes(fill = n), colour = "black") +
  annotate("text", x = -74, y = -20, label = paste("n = ", sth_n[2], sep = ""), size = 5) +
  annotate("text", x = -60, y = -20, label = paste("n = ", sth_n[1], sep = ""), size = 5) +
  scale_x_continuous(limits = c(-80, -50)) +
  sc +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Studies")  +
  theme(text=element_text(size=14))


plt_all <- p2sa + p1sa & theme(legend.position = "bottom")

library(patchwork)

# number of studies per region
plt_all_studies2 <- p2aus + p2nz + p2sa & theme(legend.position = "bottom")
plt_all_studies2 <- plt_all_studies2  + 
  plot_layout(widths = c(1.5, 1.5, 1)) + 
  plot_layout(guides = "collect")

plt_all_studies2


ggsave(filename = "figures/map_studies_per_region.png", width = 30, height = 20, 
       units = "cm", dpi = 300)


# relative occurance of studies to observations
plt_all_studies <- p1aus + p1nz + p1sa & theme(legend.position = "bottom")
plt_all_studies <- plt_all_studies  + 
  plot_layout(widths = c(1.5, 1.5, 1)) +
  plot_layout(guides = "collect")

plt_all_studies

plt_all_studies2 / plt_all_studies




# talk map ---------------------------------------------------------------------

fish_cols <-RColorBrewer::brewer.pal(6, "Dark2")
names(fish_cols) <- c("G. maculatus", "G. brevipinnis",
                      "G. fasciatus", "G. postvectis",
                      "G. argenteus", "G. truttaceus")



# South America
p1_fish <- ggplot() +
  geom_sf(data = sth_am, colour = "black") +
  geom_point(data = fish_st_am, aes(x = decimalLongitude, y = decimalLatitude, colour = common_name), alpha = 0.5) +
  scale_colour_manual(values = fish_cols) +
  theme_void() +
  labs(colour = "Species") +
  theme(text=element_text(size=14))




# aus -------------------------------------------------------------------------
fish_aus <- fish_aus %>%
  filter(decimalLongitude < 155 & decimalLatitude < -15) %>%
  filter(common_name != "Giant k\u014Dkopu")


p2_fish <- ggplot() +
  geom_sf(data = aus, colour = "black") +
  geom_point(data = fish_aus, aes(x = decimalLongitude, y = decimalLatitude, colour = common_name), alpha = 0.5) +
  scale_colour_manual(values = fish_cols) +
  theme_void() +
  labs(colour = "Species") +
  theme(text=element_text(size=14))



# NZ -------------------------------------------------------------------------
fish_nz <- fish_nz %>%
  filter(decimalLatitude > -50)

p3_fish <- ggplot() +
  geom_sf(data = nz, colour = "black") +
  geom_point(data = fish_nz, aes(x = decimalLongitude, y = decimalLatitude, colour = common_name), alpha = 0.5) +
  scale_colour_manual(values = fish_cols) +
  theme_void() +
  labs(colour = "Species") +
  theme(text=element_text(size=14))



# number of studies per region
plt_all_studies2 <- p2_fish + p3_fish + p1_fish & theme(legend.position = "bottom",
                                                        legend.text = element_text(face = "italic"))
plt_all_studies2 <- plt_all_studies2  + 
  plot_layout(widths = c(1.5, 1.5, 1)) + 
  plot_layout(guides = "collect")


plt_all_studies2



ggsave(filename = "figures/map_studies_per_region_gbif.png", width = 30, height = 20, 
       units = "cm", dpi = 300)




