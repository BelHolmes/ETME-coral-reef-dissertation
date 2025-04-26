#R script to seperate data into reefs and then regions

library(divDyn)
data(stages)
View(stages)
library(ggplot2)
library(palaeoverse)
library(sf)
library(egg)

#Step 1 - separate into reefs
#Get coral formations:
load('Data/TJ_cleaned_binned_final-14-11-2024.RData')
just_corals <- subset(TJ_data, order=='Scleractinia')
just_corals <- subset(just_corals, !(formation==''))
coral_formations <- unique(just_corals$formation)
coral_ecosystems <- subset(TJ_data, formation %in% coral_formations) #Get all data in coral formations as final coral reef ecosystem dataset 
#Rotate into palaeocoordinates
coral_ecosystems <- palaeorotate(coral_ecosystems, lng='lng', lat='lat', age='min_ma', model='PALEOMAP') #use palaeoverse package's palaeorotate function to reconstruction Triassic-Jurassic age coordinates 
save(coral_ecosystems, file='Data/coral_ecosystems_data.RData')

#Seperate out into Triassic and Jurassic reefs
triassic_stages <- c('Carnian', 'Norian', 'Rhaetian')
jurassic_stages <- c('Hettangian', 'Sinemurian', 'Pliensbachian', 'Toarcian', 'Aalenian')
triassic_coral_ecosystems <- subset(coral_ecosystems, stage %in% triassic_stages)
jurassic_coral_ecosystems <- subset(coral_ecosystems, stage %in% jurassic_stages)


#Step 2 - separate into regions
#Plot data:
#load and transform PTr palaeogeogrpahy shapefile 
shapefile_path <- "Data/GPlates/reconstructed_201.00Ma/reconstructed_201.00Ma_polygon.shp"

#read shape file
continents_shape <- st_read(shapefile_path)
continents_proj <- st_transform(continents_shape)

triassicmap <- ggplot(triassic_coral_ecosystems) +
  geom_sf(data=continents_proj, fill='gray', col='gray', linewidth=1.5) +
  geom_point(aes(x=p_lng, y=p_lat)) +
  theme_bw()
  
jurassicmap <- ggplot(jurassic_coral_ecosystems) +
  geom_sf(data=continents_proj, fill='gray', col='gray', linewidth=1.5) +
  geom_point(aes(x=p_lng, y=p_lat)) +
  theme_bw()

ggarrange(triassicmap, jurassicmap, ncol=1)


# Subsetting into 4 lat-long boxes ~ BH
# Triassic boxes
triassic_plot <- ggplot(triassic_coral_ecosystems) +
  geom_sf(data = continents_proj, fill = 'gray', col = 'gray', linewidth = 1.5) +
  geom_point(aes(x = p_lng, y = p_lat)) +
  
  geom_rect(aes(xmin = -65, xmax = -40, ymin = 0, ymax = 50), color = "black", fill = NA, linewidth = 1) +
  geom_rect(aes(xmin = -50, xmax = -30, ymin = -30, ymax = -10), color = "black", fill = NA, linewidth = 1) +
  geom_rect(aes(xmin = 0, xmax = 25, ymin = 0, ymax = 55), color = "black", fill = NA, linewidth = 1) +
  geom_rect(aes(xmin = 35, xmax = 110, ymin = 0, ymax = 55), color = "black", fill = NA, linewidth = 1) +
  
  theme_bw()

# Jurassic boxes
jurassic_plot <- ggplot(jurassic_coral_ecosystems) +
  geom_sf(data = continents_proj, fill = 'gray', col = 'gray', linewidth = 1.5) +
  geom_point(aes(x = p_lng, y = p_lat)) +
  
  geom_rect(aes(xmin = -65, xmax = -40, ymin = 5, ymax = 45), color = "black", fill = NA, linewidth = 1) +
  geom_rect(aes(xmin = -50, xmax = -20, ymin = -50, ymax = -10), color = "black", fill = NA, linewidth = 1) +
  geom_rect(aes(xmin = -10, xmax = 30, ymin = 20, ymax = 60), color = "black", fill = NA, linewidth = 1) +
  geom_rect(aes(xmin = 60, xmax = 110, ymin = 5, ymax = 60), color = "black", fill = NA, linewidth = 1) +
  
  theme_bw()

library(gridExtra)
grid.arrange(triassic_plot, jurassic_plot, ncol = 1)

# save subsetted data

triassic_region_one <- subset(triassic_coral_ecosystems, p_lng > -65 & p_lng < -40 & p_lat > 0 & p_lat < 50)
triassic_region_two <- subset(triassic_coral_ecosystems, p_lng > -50 & p_lng < -30 & p_lat > -30 & p_lat < -10)
triassic_region_three <- subset(triassic_coral_ecosystems, p_lng > 0 & p_lng < 25 & p_lat > 0 & p_lat < 55)
triassic_region_four <- subset(triassic_coral_ecosystems, p_lng > 35 & p_lng < 110 & p_lat > 0 & p_lat < 55)

jurassic_region_one <- subset(jurassic_coral_ecosystems, p_lng > -65 & p_lng < -40 & p_lat > 5 & p_lat < 45)
jurassic_region_two <- subset(jurassic_coral_ecosystems, p_lng > -50 & p_lng < -20 & p_lat > -50 & p_lat < -10)
jurassic_region_three <- subset(jurassic_coral_ecosystems, p_lng > -10 & p_lng < 30 & p_lat > 20 & p_lat < 60)
jurassic_region_four <- subset(jurassic_coral_ecosystems, p_lng > 60 & p_lng < 110 & p_lat > 5 & p_lat < 60)

save(triassic_region_one, file = "Data/triassic_region_one_data.RData")
save(triassic_region_two, file = "Data/triassic_region_two_data.RData")
save(triassic_region_three, file = "Data/triassic_region_three_data.RData")
save(triassic_region_four, file = "Data/triassic_region_four_data.RData")

save(jurassic_region_one, file = "Data/jurassic_region_one_data.RData")
save(jurassic_region_two, file = "Data/jurassic_region_two_data.RData")
save(jurassic_region_three, file = "Data/jurassic_region_three_data.RData")
save(jurassic_region_four, file = "Data/jurassic_region_four_data.RData")

write.csv(coral_ecosystems, "coral_ecosystems.csv")
