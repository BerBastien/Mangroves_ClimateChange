library(tidyverse)
library(sf)
library(raster)
library(landscapemetrics)
library(exactextractr)
library(terra)
library(ncdf4)


grid <- st_read("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\Grid\\sampling_grid_mangroves_oned.gpkg")
salinity_files <- list.files(path = "C:\\Users\\basti\\Box\\Data\\Oceans\\salinity\\OISSS_L4_multimission_monthly_v1\\2012_2021", pattern = "*.nc", full.names = TRUE)

nc <- nc_open(salinity_files[1])
# Get the data
lon <- ncvar_get(nc, "longitude")  # assuming "lon" and "lat" are the names of the longitude and latitude variables
lat <- ncvar_get(nc, "latitude")
salinity <- ncvar_get(nc, "sss")  # "sss" is the variable name you specified

# Create a raster from the matrix and set its extent and projection
r <- raster(t(salinity))
r <- flip(r,direction="y")
extent(r) <- c(min(lon), max(lon), min(lat), max(lat))
projection(r) <- CRS("+proj=longlat +datum=WGS84")

# # Plot the raster
# plot(r)
# # Convert to data frame
# df <- data.frame(lon = as.vector(lon), lat = as.vector(lat), salinity = as.vector(salinity))
# glimpse(df)
# plot(df$lon,df$lat)
# # Close the netCDF file
# nc_close(nc)
# print(p)
# plot(p)

output <- data.frame(gridcell_id = integer(), year = integer(),  Mean_Salinity = numeric())
years <- c(2012:2021)
for (i in 1:length(salinity_files)) {
    nc <- nc_open(salinity_files[i])
    salinity <- ncvar_get(nc, "sss")  # "sss" is the variable name you specified
    # Create a raster from the matrix and set its extent and projection
    r <- raster(t(salinity))
    r <- flip(r,direction="y")
    extent(r) <- c(min(lon), max(lon), min(lat), max(lat))
    projection(r) <- CRS("+proj=longlat +datum=WGS84")
    if(i==1){
        salinity_monthly_brick <- brick(r)
    } else{
        salinity_monthly_brick <- addLayer(salinity_monthly_brick,r)
    }
    print(i)
}

sp_buffer <-st_buffer(grid,100000) 
sp_buffer2 <-st_buffer(grid,1000000) 
for(i in 1:9){
    if(i<10){
            salinity <- subset(salinity_monthly_brick,((i-1)*12+1):((i*12)))
    }else{
        salinity <- subset(salinity_monthly_brick,((i-1)*12+1):((i*12)-1))
    }
    
    #plot(salinity)
    annual_mean_salinity <- mean(salinity)
    #plot(annual_mean_salinity)  
    if(i==1){
        raster_crs <- crs(annual_mean_salinity)
        grid <- st_transform(grid, raster_crs)
    }
    mean_salinity <- exact_extract(annual_mean_salinity, grid,'mean')
    mean_salinity <- unlist(mean_salinity)
    na_salinity <- which(is.na(mean_salinity))
    sp_buffer_sub <-sp_buffer[na_salinity,]
    mean_salinity[na_salinity] <- exact_extract(annual_mean_salinity,sp_buffer_sub ,'mean')

    na_salinity <- which(is.na(mean_salinity))
    sp_buffer_sub <-sp_buffer2[na_salinity,]
    mean_salinity[na_salinity] <- exact_extract(annual_mean_salinity,sp_buffer_sub ,'mean')

    data <- data.frame(gridcell_id = grid$ID, year = as.integer(years[i]), Mean_Salinity = unlist(mean_salinity))
  
    
  output <- rbind(output, data)
  print(years[i])
}

glimpse(data)
glimpse(output)
ggplot(output,aes(x=year,y=Mean_Salinity,group=gridcell_id))+
geom_line()
# Mean temperature data

write.csv(output,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\salinity.csv")
