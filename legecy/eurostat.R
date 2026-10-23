

# https://ec.europa.eu/eurostat/comext/newxtweb/

# EU Trade Since 1988 By CN8

# EU trade since 1988 by HS2-4-6 and CN8 

https://ec.europa.eu/eurostat/comext/newxtweb/setupdimselection.do

# The "reporter" is the country providing the data about the trade flow??? essentially the country from whose perspective the trade is being reported.
# The "partner" is the country or territory to which goods are exported or from which goods are imported.

  
# If Country A (reporter) exports goods to Country B, Country B is the "partner" in this export data.
# Conversely, if Country A (reporter) imports goods from Country B, Country B is still the "partner," but now in the import data.




# -----------------------------------------------------------------------------------------------------------------------------------------------

#library(remotes)
#remotes::install_github("ropengov/eurostat")

library(eurostat)
library(tidyverse)


# Get Eurostat data listing
toc <- get_eurostat_toc()

# info about plants
kable(head(search_eurostat("plant")))

kable(head(search_eurostat("trees")))

options()
Sys.setlocale("LC_ALL","English")

dat <- get_eurostat("ef_lac_flower", time_format = "num")

dat_l <- label_eurostat(map_data, fix_duplicated = TRUE)
dat_l


# label_eurostat_vars(names(dat_l))
dat_l %>%  distinct(so_eur)
dat_l %>%  distinct(croparea)
dat_l %>%  distinct(crops)
dat_l %>%  distinct(geo)

dat_l$geo_2 <- dat_l$geo
dat_l$geo <- dat$geo


dat_l <- dat_l %>% 
  group_by(geo, geo_2) %>% 
  summarise(sum=sum(values)) %>% 
  na.omit()

geodata <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = 2,
  year = 2013
)


# Fix / remove some broken entries for the demo purpose
geodata <- sf::st_make_valid(geodata)
geodata <- geodata[sf::st_is_valid(geodata), ]


geodata

map_data <- inner_join(geodata, dat_l)
head(map_data)


map_data %>% filter(sum > 10000) %>% 
ggplot() +
  geom_sf(data=geodata, col="black") +
  geom_sf( aes(fill = sum)) +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  theme_bw()


