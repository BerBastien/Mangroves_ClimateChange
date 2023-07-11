library(tidyverse)
library(sf)
library(raster)
library(landscapemetrics)
library(exactextractr)
library(terra)
library(ncdf4)


grid <- st_read("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\Grid\\sampling_grid_mangroves_oned.gpkg")
temp_files <- list.files(path = "C:\\Users\\basti\\Box\\Data\\Climate\\CRU\\temp", pattern = "*.nc", full.names = TRUE)
precip_files <- list.files(path = "C:\\Users\\basti\\Box\\Data\\Climate\\GPCC\\precip", pattern = "*.nc", full.names = TRUE)

output <- data.frame(ID = integer(), Year = integer(),  Mean_Precipitation = numeric())
years <- c(2000:2021)
for (i in seq_along(years)) {
  
  # Load the temperature and precipitation data
  #temp <- brick(temp_files[i])
  precip <- brick(precip_files[i])
  
  # Calculate the annual mean
  #annual_mean_temp <- mean(temp)
  annual_mean_precip <- mean(precip)
  #plot(annual_mean_precip)
  #plot(grid,add=TRUE)
  #ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Precip_sampling.png")
  
  if(i==1){
    raster_crs <- crs(annual_mean_precip)
    grid <- st_transform(grid, raster_crs)
  }
    

    mean_precip <- exact_extract(annual_mean_precip, grid,'mean')
    mean_precip <- unlist(mean_precip)
    na_precip <- which(is.na(mean_precip))
    sp_buffer <-st_buffer(grid[na_precip,],100000) 
    mean_precip[na_precip] <- exact_extract(annual_mean_precip,sp_buffer ,'mean')


  # Create a dataframe
  #data <- data.frame(ID = grid$ID, Year = as.integer(years[i]), Mean_Temperature = mean_temp, Mean_Precipitation = mean_precip)
  data <- data.frame(ID = grid$ID, Year = as.integer(years[i]), Mean_Precipitation = unlist(mean_precip))
  
  # Append to the output dataframe
  output <- rbind(output, data)
  print(years[i])
}

write.csv(output,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\precip.csv")
glimpse(data)
glimpse(output)
ggplot(output,aes(x=Year,y=Mean_Precipitation,group=ID))+
geom_line()
# Mean temperature data


temp_files <- list.files(path = "C:\\Users\\basti\\Box\\Data\\Climate\\BerkeleyEarth", pattern = "*.nc", full.names = TRUE)
temp_files
temp_brick <- brick(temp_files[1])
glimpse(temp_brick)


# Calculate the annual mean temperature for each year
# Split the RasterBrick into a list of RasterStacks, each containing 12 layers (i.e., one for each month of a year)
raster_list <- lapply(seq(1, (nlayers(temp_brick)-12), 12), function(i) {
  temp_brick[[i:(i+11)]]
})

plot(mean(raster_list[[150]],na.rm=TRUE))

# Calculate the annual mean temperature for each RasterStack (i.e., each year)
annual_mean_temp_list <- lapply(raster_list, function(r) {
  mean((r), na.rm = TRUE)
})

# Combine the RasterLayers back into a RasterBrick
annual_mean_temp <- brick(annual_mean_temp_list)
1850+173
annual_mean_temp <- annual_mean_temp[[150:173]]
plot(annual_mean_temp)
dim(annual_mean_temp)
output <- data.frame(ID = integer(), Year = integer(),  Mean_Precipitation = numeric())
years <- c(2000:2023)
for(i in 1:dim(annual_mean_temp)[3]){
    mean_temp <- extract(subset(annual_mean_temp,i), grid)
    # glimpse(mean_temp)
    # class(mean_temp)
    mean_temp <- unlist(mean_temp)
    mean_temp <- as.data.frame(mean_temp)
    
    data <- data.frame(ID = grid$ID, Year = as.integer(years[i]), Mean_Temperature_anom = mean_temp)
    
    # Append to the output dataframe
    output <- rbind(output, data)
    print(years[i])

}
mean_temp <- extract(annual_mean_temp, grid)
glimpse(mean_temp)

# Create a data frame
df <- data.frame(
  grid_id = grid$ID,
  year = rep(2000:2021, each = nrow(grid)),
  mean_temp = mean_temp
)
glimpse(df)
