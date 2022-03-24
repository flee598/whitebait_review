
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# function ---------------------------------------------------------------------

# takes map file and fish data and combines and calculates prop studies per region
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


# MAPPING ---------------------------------------------------------------------

# load maps and species observations
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
myPalette <- colorRampPalette(cartography::carto.pal(pal1 = "green.pal", n1 = 20))

sc <- scale_fill_gradientn(colours = myPalette(100), 
                           limits = c(0,
                                      max(c(nz$n, aus$n, sth_am$n),
                                          na.rm = T)))

## NZ -------------------------------------------------------------------------

# drop Chatham Islands records
fish_nz <- fish_nz %>%
  filter(decimalLongitude > 0)

# add proprtion of studies by region
nz2 <- fun_prop_region_studies(fish_nz, nz)

# replace NA with 0 in the n column
nz2 <- nz2 %>%
  mutate_at(vars(n), ~tidyr::replace_na(., 0))

# plot
p2nz <- ggplot() +
  geom_sf(data = nz2, aes(fill = n), colour = "black") +
  annotate("text", x = 172, y = -38, label = paste("n = ", sum(nz2$n, na.rm = T), sep = ""), size = 5) +
  theme(legend.position = "bottom") +
  labs(fill = "Studies") +
  theme_void() +
  sc  +
  theme(text=element_text(size=14))

## Aus -------------------------------------------------------------------------

# add proportion of studies by region
aus2 <- fun_prop_region_studies(fish_aus, aus)

# cahnge in range states from NA to 0
aus2$n[1] <- 0
aus2$n[5] <- 0

p2aus <- ggplot() +
  geom_sf(data = aus2, aes(fill = n), colour = "black") +
  annotate("text", x = 125, y = -12, label = paste("n = ", sum(aus2$n, na.rm = T), sep = ""), size = 5) +
  sc +
  labs(fill = "Studies") + 
  theme_void() +
  theme(legend.position = "bottom")  +
  theme(text=element_text(size=14))


# South America  --------------------------------------------------------------
# combine Chl and Arg
fish_st_am <- rbind(fish_chl, fish_arg)

# add proportion of studies by region
sth_am2 <- fun_prop_region_studies(fish_st_am, sth_am)

sth_am2$n[8] <- 0
sth_am2$n[14] <- 0

# total number of papers for each country
sth_n <- sth_am2 %>%
  group_by(country.x) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  pull(n)

# plot
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


# number of studies per region
plt_all_studies2 <- p2aus + p2nz + p2sa & theme(legend.position = "bottom")
plt_all_studies2 <- plt_all_studies2  + 
  plot_layout(widths = c(1.5, 1.5, 1)) + 
  plot_layout(guides = "collect")

plt_all_studies2

ggsave(filename = "figures/map_studies_per_region.png", width = 30, height = 20, 
       units = "cm", dpi = 300)


# ------------------------------------------------------------------------------
# maps of all known observations -----------------------------------------------
# ------------------------------------------------------------------------------

# colour palette
fish_cols <-RColorBrewer::brewer.pal(6, "Dark2")

#  rename
names(fish_cols) <- c("G. maculatus", "G. brevipinnis",
                      "G. fasciatus", "G. postvectis",
                      "G. argenteus", "G. truttaceus")

# South America
p1_fish <- ggplot() +
  geom_sf(data = sth_am, colour = "black") +
  geom_point(data = fish_st_am, aes(x = decimalLongitude, y = decimalLatitude, colour = common_name), alpha = 0.5) +
  scale_colour_manual(values = fish_cols) +
  theme_void() +
  labs(colour = "Species")


# aus -------------------------------------------------------------------------
fish_aus <- fish_aus %>%
  filter(decimalLongitude < 155 & decimalLatitude < -15) %>%
  filter(common_name != "Giant k\u014Dkopu")


p2_fish <- ggplot() +
  geom_sf(data = aus, colour = "black") +
  geom_point(data = fish_aus, aes(x = decimalLongitude, y = decimalLatitude, colour = common_name), alpha = 0.5) +
  scale_colour_manual(values = fish_cols) +
  theme_void() +
  labs(colour = "Species") 

# NZ -------------------------------------------------------------------------
fish_nz <- fish_nz %>%
  filter(decimalLatitude > -50)

p3_fish <- ggplot() +
  geom_sf(data = nz, colour = "black") +
  geom_point(data = fish_nz, aes(x = decimalLongitude, y = decimalLatitude, colour = common_name), alpha = 0.5) +
  scale_colour_manual(values = fish_cols) +
  theme_void() +
  labs(colour = "Species")


# number of studies per region
plt_all_studies2 <- p2_fish + p3_fish + p1_fish & theme(legend.position = "bottom",
                                                        legend.text = element_text(face = "italic"))
plt_all_studies2 <- plt_all_studies2  + 
  plot_layout(widths = c(1.5, 1.5, 1)) + 
  plot_layout(guides = "collect")


plt_all_studies2