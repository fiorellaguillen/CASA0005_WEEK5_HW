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
worldcities <- st_read("World_Countries_Generalized.geojson")
qtm(worldcities)

# open csv. file
here::here()
worldcities_all <-read.csv(here::here("Data/Composite_indices_1990_2022.csv"), 
                           header = TRUE, sep = ",", 
                           encoding = "UTF-8")

# select gii data from 1990 to 2021
worldcities_gii <- worldcities_all %>%
  select(iso3, country, matches("^gii_19[9][0-9]$|^gii_20[0-2][0-9]$"))

#create a new column for iso2
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

# Merge the map and csv df
gii_diff <- clean_names(worldcities_gii_diff)
worldcities_map <- worldcities %>%
  clean_names() %>%
  merge(.,
        gii_diff,
        by.x="iso", 
        by.y="iso2",
        no.dups = TRUE) %>%
  distinct(., iso, 
           .keep_all = TRUE)

#left join
gii_diff <- worldcities_gii

#