install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("JakobMie/nightlightstats")
install.packages("sf")
library(tidyverse)
library(nightlightstats)
library(sf)

getwd()
buffer_coastline <- sf::st_read("C:\\Users\\aburt\\OneDrive\\aburto_lab_r_projects\\nightlightstats/sampling_grid_mangroves_oned.gpkg") 
setwd("D:/aburto_lab_datacenter/nightlights/")
#%>% 
  #select(SOVEREIGNT, SOV_A3, ADMIN, GEOUNIT, ECONOMY, INCOME_GRP, CONTINENT, REGION_UN, SUBREGION, REGION_WB, geom)

## World Mollweide [equal area]	Mollweide	WGS84	meters	54009

sf::sf_use_s2(FALSE)



target_crs <- st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133")


offset <- 180 - 133


polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
buffer_coastline2 <- buffer_coastline %>% st_difference(polygon)

# perform transformation on modified version of world dataset
buffer_coastline_equal_area <- st_transform(buffer_coastline2, 
                               crs = '+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133')


ggplot(buffer_coastline_equal_area) +
  geom_sf()



area <- st_area(buffer_coastline_equal_area)

# nightlight_calculate(
#   area_names = buffer_coastline$SOVEREIGNT,
#   time = c("1992", "1995"),
#   shapefile_location = "data/buffer_to_extract.gpkg",
#   light_location = "D:/aburto_lab_datacenter/nightlights/",
#   admlevel = 1)

# nightlight_download(
#   time = c("1992", "2022"),
#   light_location = "D:/aburto_lab_datacenter/nightlights/")





file_list <- list.files("D:/aburto_lab_datacenter/nightlights/", pattern = ".avg_vis.tif", full.names = T, recursive = F)


extract_lights <- function(path_to_raster) {
  
  r <- raster::raster(path_to_raster)
  #r <- raster::raster(file_list[1])
 
  
  lights <- as.data.frame(raster::extract(r, buffer_coastline, fun = mean))


  names(lights) <- "light_mean"
  
  lights$year <- str_extract_all(path_to_raster,"\\(?[0-9]+\\)?")
  #lights$year <- str_extract_all(file_list[1],"\\(?[0-9]+\\)?")
  
 write_rds(x = lights , file = paste0("extracted/", stringr::str_remove_all(basename(path_to_raster), ".tif"), ".Rds"))
 #write_rds(x = lights , file = paste0("extracted/", stringr::str_remove_all(basename(file_list[1]), ".tif"), ".Rds"))

}



lapply(file_list, extract_lights)

for(i in 1:length(file_list)){
  
  r <- raster::raster(file_list[i])
 
  
  lights <- as.data.frame(raster::extract(r, buffer_coastline, fun = mean))


  names(lights) <- "light_mean"
  
  lights$year <- str_extract_all(file_list[1],"\\(?[0-9]+\\)?")
  
  print(stringr::str_remove_all(basename(file_list[i]), ".tif"))
  write_rds(x = lights , file = paste0("extracted/", stringr::str_remove_all(basename(file_list[i]), ".tif"), ".Rds"))
  print(i)

}




light_list <- list.files(path = "extracted/", pattern = ".Rds", full.names = T)




load_and_set <- function(x) {
  
  year <- str_extract_all(x, "\\(?[0-9]+\\)?")
  
  out <- read_rds(x) %>% 
    select(-year) %>% 
    mutate(id = 1:1533) %>% 
    mutate(year = year) %>% 
    pivot_wider(names_from = year, values_from = light_mean)
  
}


lights_to_merge <- lapply(light_list, load_and_set) %>% 
  reduce(left_join, by = "id") %>% 
  janitor::clean_names()


lights_final <- cbind(buffer_coastline, lights_to_merge)
glimpse(lights_final)

lights_final2 <- lights_final %>% 
  pivot_longer(cols = c_101992_4:c_182012_4, names_to = "year", values_to = "lights_mean") %>% 
  mutate(year = as.numeric(substr(year, 5, 8))) %>% 
  as.data.frame() 
  
  lights_final2%>% 
  select(-geom) %>% 
  group_by(year) %>% 
  summarise(lights_mean = mean(lights_mean)) %>% 
  ggplot(aes(x=year, y=lights_mean)) +
  geom_point() +
  geom_smooth()

getwd()
glimpse(lights_final2)
lights_final2 %>% 
  #select(-geom) %>% 
  write.csv("C:\\Users\\aburt\\OneDrive\\aburto_lab_r_projects\\nightlightstats\\outputs\\nightlights_mangroves.csv")

lights_final %>% 
  pivot_longer(cols = x1992:x2012, names_to = "year", values_to = "lights_mean") %>% 
  mutate(year = as.numeric(str_remove_all(year, "x"))) %>% 
  as.data.frame() %>% 
  select(-geom) %>% 
  write_rds("outputs/nightlights_dataframe.Rds")






lights <- read_rds("outputs/nightlights_dataframe.Rds")

lights %>%
  filter(SOVEREIGNT == "Mexico") %>% 
  group_by(year) %>% 
  summarise(lights_mean = mean(lights_mean)) %>% 
  ggplot(aes(x=year, y=lights_mean)) +
  geom_point() +
  geom_smooth()

lights$area <- as.numeric(area)/1000000



write_rds(lights, "outputs/nightlights_dataframe.Rds")



