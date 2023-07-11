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

p <- nc_open(precip_files[1])
print(p)

output <- data.frame(ID = integer(), Year = integer(),  Mean_Precipitation = numeric())
years <- c(2000:2021)
for (i in seq_along(years)) {
  precip <- brick(precip_files[i])
  #plot(precip)
  annual_mean_precip <- mean(precip)
  plot(annual_mean_precip)  
  if(i==1){
    raster_crs <- crs(annual_mean_precip)
    grid <- st_transform(grid, raster_crs)
  }
    

    mean_precip <- exact_extract(annual_mean_precip, grid,'mean')
    mean_precip <- unlist(mean_precip)
    na_precip <- which(is.na(mean_precip))
    sp_buffer <-st_buffer(grid[na_precip,],100000) 
    mean_precip[na_precip] <- exact_extract(annual_mean_precip,sp_buffer ,'mean')

    data <- data.frame(ID = grid$ID, Year = as.integer(years[i]), Mean_Precipitation = unlist(mean_precip))
  
    
  output <- rbind(output, data)
  print(years[i])
}
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


## SPEI


spei_files <- list.files(path = "C:\\Users\\basti\\Box\\Data\\Climate\\CRU\\spei\\", pattern = "*.nc", full.names = TRUE)
spei_b <- brick(spei_files[1])
spei_b

# Calculate the annual mean temperature for each year
# Split the RasterBrick into a list of RasterStacks, each containing 12 layers (i.e., one for each month of a year)
raster_list <- lapply(seq(1, (nlayers(spei_b)-12), 12), function(i) {
  spei_b[[i:(i+11)]]
})

plot(raster_list[[90]])
mean(raster_list[[90]])
plot(mean(raster_list[[90]]))

raster_list <- raster_list[100:120]
# Calculate the annual mean temperature for each RasterStack (i.e., each year)
annual_mean_spei_list <- lapply(raster_list, function(r) {
  mean((r), na.rm = TRUE)
})  #here


# Combine the RasterLayers back into a RasterBrick
annual_mean_spei <- brick(annual_mean_spei_list)
dim(annual_mean_spei)

output <- data.frame(ID = integer(), Year = integer(),  spei = numeric())
years <- c(2000:2020)
for(i in 1:dim(annual_mean_spei)[3]){
    mean_spei <- extract(subset(annual_mean_spei,i), grid)
    mean_spei2 <- exact_extract(subset(annual_mean_spei,i),grid ,'mean')
    glimpse(mean_spei)
    glimpse(mean_spei2)
    na_spei <- which(is.na(mean_spei2))
    sp_buffer <-st_buffer(grid[na_spei,],10000) 
    mean_spei2[na_spei] <- exact_extract(subset(annual_mean_spei,i),sp_buffer ,'mean')
    mean_spei2 <- as.data.frame(mean_spei2)

    data <- data.frame(ID = grid$ID, Year = as.integer(years[i]), Mean_spei = mean_spei2)
    output <- rbind(output, data)
    print(years[i])

}

glimpse(output)
sum(is.na(output$mean_spei))
write.csv(output,'C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\spei.csv')
















raster_list
temp_files <- list.files(path = "C:\\Users\\basti\\Box\\Data\\Climate\\BerkeleyEarth", pattern = "*.nc", full.names = TRUE)
temp_files
temp_brick <- brick(temp_files[1])
clim_temp_brick <- brick(temp_files[1],varname="climatology")
temp_brick
clim_temp_brick
raster_list
library(SPEI)
install.packages("scPDSI")
library(scPDSI)
base_year_precip <- 1
for(i in 1:10){
  precip_brick <- raster_list[[i+base_year_precip]]
  year <- 2010
  plot(temp_brick)
  temp_brick
  years_temp <- which(grepl(paste0("X",year),names(temp_brick)))
  tempsub <- temp_brick[[min(years_temp):max(years_temp)]]
  tempsub_nonas <- mask(tempsub, is.finite(tempsub))

  celsius_temp <- tempsub_nonas + clim_temp_brick
  temp_brick_kelvin <- celsius_temp + 273.15

    
  # Ensure the bricks are compatible
  if (!compareRaster(temp_brick_kelvin, precip_brick, stopiffalse = FALSE)) {
    stop("The temperature and precipitation bricks do not match.")
  }

  precip <- precip_brick
  temp <- temp_brick_kelvin

  # Create a function to calculate PDSI
  calc_pdsi <- function(precip, temp) {
    # Create a data frame with precipitation and temperature
    data <- data.frame(precip[[1]], temp[[1]])

    # Calculate PDSI
    pdsi <- SPEI::pdsi(data)
    SPEI::pdsi(258,23)
    # Return the annual PDSI
    return(mean(pdsi, na.rm = TRUE))
  }

    # Apply the function to each pixel's data
    pdsi_brick <- overlay(precip_brick, temp_brick, fun = calc_pdsi)

  # First, create a raster of latitudes
  latitude_raster <- raster(nrow = nrow(temp_brick_kelvin), ncol = ncol(temp_brick_kelvin),
                            ext = extent(temp_brick_kelvin), crs = crs(temp_brick_kelvin))
  latitude_raster[] <- yFromCell(latitude_raster, 1:ncell(latitude_raster))
  latitude_raster
  plot(latitude_raster)
  lat_vec <- getValues(latitude_raster)
  glimpse(lat_vec)
  
  # Convert the temp_brick to a vector
  temp_vector <- getValues(temp_brick_kelvin)
  glimpse(temp_vector[,1])
  glimpse(lat_vec)
  
  SPEI::thornthwaite(temp_vector[,1], lat_vec)




  # Create a function that calculates Thornthwaite's PET for each pixel
  temp <- temp_brick[[i]]
  calc_PET <- function(temp, lat) {
    # If temperature is NA, return NA
    if (is.na(temp)) {
      return(NA)
    }

    # Otherwise, calculate PET
    PET <- SPEI::thornthwaite(temp + 273.15, lat)  # Convert temperature to Kelvin
    return(PET)
  }

  # Create a raster stack for PET
  PET_stack <- stack()

  # Apply the function to each layer of your temperature brick
  for (i in 1:nlayers(celsius_temp)) {
    PET_layer <- calc_PET(getValues(temp_brick[[i]]), getValues(latitude_raster))
    PET_stack <- stack(PET_stack, PET_layer)
  }

  # Now, PET_stack should be a RasterStack of the same dimensions as temp_brick,
  # but with PET values instead of temperatures


  # Now you can plot
  plot(tempsub_nonas)
  print(tempsub)
  glimpse(tempsub)
  plot(tempsub)
}