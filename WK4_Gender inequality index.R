#date frame
#worldcities: World_Countries_Generalized.geojson
#worldcities_all: Composite_indices_1990_2022.csv
#worldcities_gii: iso3 and gii from 1990 to 2022
#worldcities_gii_diff: iso2 and gii difference 

#Import all libraries
library(tmap)
library(tmaptools)
library(sf)
library(here)
library(dplyr)
library(countrycode) #install_packages('') if needed
library(janitor) #install_packages('') if needed
library(ggplot2)

# read geojson file, quick map 
worldcities <- st_read("Data/World_Countries_Generalized.geojson")
qtm(worldcities)

# open csv. file
here::here()
worldcities_all <-read.csv(here::here("Data/Composite_indices_1990_2022.csv"), 
                           header = TRUE, sep = ",", 
                           encoding = "UTF-8")

# select gii data from 1990 to 2021
worldcities_gii <- worldcities_all %>%
  select(iso3, country, matches("^gii_19[9][0-9]$|^gii_20[0-2][0-9]$"))

#create a new column for iso2; new function countrycode()
worldcities_gii$iso2 <- countrycode(worldcities_gii$iso3, origin = 'iso3c', destination = 'iso2c')
head(worldcities_gii)#check it

# move iso2 to the first column
worldcities_gii <- worldcities_gii[, c("iso2", setdiff(names(worldcities_gii), "iso2"))]
head(worldcities_gii)#check it again

# calculate the difference between 1990 and 2022
worldcities_gii <- worldcities_gii %>%
  mutate(diff = gii_2022 - gii_1990)

# new df for diff and iso2
worldcities_gii_diff <- worldcities_gii %>%
  select(iso2, diff)
head(worldcities_gii_diff)

# Merge the map and csv df; left join
worldcities_gii_diff_clean <- clean_names(worldcities_gii_diff)
Gii_Map <- worldcities %>%
  clean_names() %>%
  left_join(gii_diff, by = c("iso" = "iso2")) %>%
  distinct(iso, .keep_all = TRUE)

#Make a map
ggplot(Gii_Map) +
  geom_sf(aes(fill = diff)) + 
  scale_fill_gradient2(low = "blue", mid = "lightblue", high = "red", 
                       midpoint = 0, na.value = "grey") 
theme_minimal() +
  ggtitle("GII difference between 1990 and 2022")