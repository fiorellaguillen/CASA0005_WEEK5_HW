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
  dplyr::select("iso3","gii_2010","gii_2019") #selected only columns for country code, and gii in 2010 and 2019

#create a new column for iso2; new function countrycode()
worldcities_gii$iso2 <- countrycode(worldcities_gii$iso3, origin = 'iso3c', destination = 'iso2c')
head(worldcities_gii)#check it

# move iso2 to the first column
worldcities_gii <- worldcities_gii[, c("iso2", setdiff(names(worldcities_gii), "iso2"))]
head(worldcities_gii)#check it again

# calculate the difference between 1990 and 2022
worldcities_gii <- worldcities_gii %>%
  mutate(diff = gii_2019 - gii_2010) #calculate the difference between 2019 and 2010

# new df for diff and iso2
worldcities_gii_diff <- worldcities_gii %>%
  select(iso2, diff)
head(worldcities_gii_diff)

# Merge the map and csv df; left join
worldcities_gii_diff_clean <- clean_names(worldcities_gii_diff)

Gii_Map <- worldcities %>%
  clean_names() %>%
  left_join(.,worldcities_gii_diff_clean, by = c("iso" = "iso2"))

#Make a map
#ggplot(Gii_Map) +
#  geom_sf(aes(fill = diff)) + 
#  scale_fill_gradient2(low = "blue", mid = "lightblue", high = "red", 
#                       midpoint = 0, na.value = "grey") 
#theme_minimal() +
#  ggtitle("GII difference between 2010 and 2019")

#---------------
# map making(plots in the Viewer)
library(leaflet)
library(leafpop)
library(RColorBrewer)

#set breaks for legend
breaks <- c(-0.5,-0.3,-0.15, -0.1,-0.05, 0, 0.05, 0.1, 0.15, 0.2)


#select data to display in pop up info windows
popupdiff <-Gii_Map %>%
  st_drop_geometry()%>%
  dplyr::select("diff", "country")%>%
  dplyr::rename(`Gender Inequality Index Diff` = diff) %>%
  popupTable()

#select color palettes 

pal_colors <- rev(brewer.pal(n=length(breaks), name = "RdBu"))
pal1 <- Gii_Map %>%
  colorBin(palette = pal_colors, domain=.$`diff`, bins=breaks)

#create map
map<- leaflet(Gii_Map)%>%
  addPolygons(color="gray", 
              weight = .8,
              opacity = 1,
              dashArray = "3",
              popup = popupdiff,
              fillOpacity = 0.8,
              fillColor = ~pal1(`diff`),
              group = "Gender Inequality Index")%>%
  #add base map
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  
  #add legend
  addLegend(pal = pal1, values = ~`diff`, group = c("Gender Inequality Index"), 
            position ="bottomleft", title = "Gender Inequality Index Diff") %>%
  
  # specify layers control
  addLayersControl(
    baseGroups = c("CartoDB"),
    overlayGroups = c("Gender Inequality Index"),
    options = layersControlOptions(collapsed = FALSE)
  )

#plot map
map