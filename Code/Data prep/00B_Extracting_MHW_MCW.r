# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)

# Set the path to your data files
gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
nc_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/MHW/mangrove.mhw.stats.1996-2020.nc"
# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)
print(nc_data)

# Read the gridcell data from .gpkg file
gridcell_data <- st_read(gpkg_file)

# Get the variable names from the NetCDF file
variable_names <- names(nc_data$var)

# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)
print(nc_data)

# Read the gridcell data from .gpkg file
gridcell_data <- st_read(gpkg_file)

#for(vari in variable_names){

brick(nc_file, varname=vari,level=2)
brick(nc_file, "count", 
        start = c(1, 1, year,1), count = c(-1,-1, 1,-1))

bins <- ncvar_get(nc_data, variable_names[2], 
        start = c(1), count = c(-1))
length(bins)
range_bin <- paste0("anom_b",bins[1:18],"_",bins[2:19],"C")
range_bin
range_bin <- str_replace_all(range_bin, "-", "n")

na_extracted_bin <- data.frame(id=gridcell_data$id,rbind(1:18))
grid_bin <- data.frame(id=gridcell_data$id,year=NA,var=NA)
glimpse(grid_bin)
glimpse(gridcell_data)
for(vari in variable_names){
  br <- rotate(brick(nc_file, varname=vari))
  for (j in 1:25){
    grid_bin[,2] <- 1850 + floor(nc_data$dim$time$vals[j]/365)
    extracted_bin <- extract(subset(br,j), gridcell_data)
    #plot(subset(br,j))
    #plot(gridcell_data,add=T)
    extracted_bin <- unlist(extracted_bin)
    #glimpse(extracted_bin)
    grid_bin[,3] <- extracted_bin
    
    if(j==1){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }
    print(paste0("year: ",1850 + floor(nc_data$dim$time$vals[j]/365)," ----var: ",vari))
    #glimpse(all_data)
  }
  if(vari=="sst"){
    all_var_data <- all_data
  }else{
    all_var_data <- cbind(all_var_data,all_data[,3])
    names(all_var_data)[dim(all_var_data)[2]] <- vari  
  }
}
  glimpse(all_data)
  glimpse(all_var_data)
  names(all_var_data)[3] <- "sst"

write.csv(all_var_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw_full.csv")




nc_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/MHW/mangrove.mcw.stats.1996-2020.nc"
# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)
print(nc_data)

# Read the gridcell data from .gpkg file
gridcell_data <- st_read(gpkg_file)

# Get the variable names from the NetCDF file
variable_names <- names(nc_data$var)

# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)
print(nc_data)

# Read the gridcell data from .gpkg file
gridcell_data <- st_read(gpkg_file)

#for(vari in variable_names){

brick(nc_file, varname=vari,level=2)
brick(nc_file, "count", 
        start = c(1, 1, year,1), count = c(-1,-1, 1,-1))

bins <- ncvar_get(nc_data, variable_names[2], 
        start = c(1), count = c(-1))
length(bins)
range_bin <- paste0("anom_b",bins[1:18],"_",bins[2:19],"C")
range_bin
range_bin <- str_replace_all(range_bin, "-", "n")

na_extracted_bin <- data.frame(id=gridcell_data$id,rbind(1:18))
grid_bin <- data.frame(id=gridcell_data$id,year=NA,var=NA)
glimpse(grid_bin)
glimpse(gridcell_data)
variable_names <- variable_names[-c(1)]
for(vari in variable_names){
  br <- rotate(brick(nc_file, varname=vari))
  for (j in 1:25){
    grid_bin[,2] <- 1850 + floor(nc_data$dim$time$vals[j]/365)
    extracted_bin <- extract(subset(br,j), gridcell_data)
    #plot(subset(br,j))
    #plot(gridcell_data,add=T)
    extracted_bin <- unlist(extracted_bin)
    #glimpse(extracted_bin)
    grid_bin[,3] <- extracted_bin
    
    if(j==1){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }
    print(paste0("year: ",1850 + floor(nc_data$dim$time$vals[j]/365)," ----var: ",vari))
    #glimpse(all_data)
  }
  if(vari=="mcw_int"){
    all_var_data <- all_data
    names(all_var_data)[dim(all_var_data)[2]] <- vari  
  }else{
    all_var_data <- cbind(all_var_data,all_data[,3])
    names(all_var_data)[dim(all_var_data)[2]] <- vari  
  }
}
  glimpse(all_data)
  glimpse(all_var_data)
  
  write.csv(all_var_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw_full.csv")
  














# Extract latitude and longitude from the geometry column
gridcell_coords <- st_coordinates(gridcell_data)
gridcell_lat <- gridcell_coords[, "Y"]
gridcell_lon <- gridcell_coords[, "X"]

# Create an empty dataframe to store the sampled data
sampled_data <- data.frame(
  gridcell_id = integer(),
  year = integer(),
  stringsAsFactors = FALSE
)

# Loop through each gridcell
for (i in 1:nrow(gridcell_data)) {
    print(i)
  # Get the gridcell ID
  gridcell_id <- gridcell_data$id[i]
  
  # Get the latitude and longitude of the current gridcell
  gridcell_lat_i <- gridcell_lat[i]
  gridcell_lon_i <- gridcell_lon[i]

  if(gridcell_lon_i<0){gridcell_lon_i<-180-gridcell_lon_i}
  
  # Find the indices of the nearest gridcell in the NetCDF file
  gridcell_lat_index <- which.min(abs(nc_data$dim$lat$vals - gridcell_lat_i))
  gridcell_lon_index <- which.min(abs(nc_data$dim$lon$vals - gridcell_lon_i))
  
  
  # Loop through each year
  for (year in 1:nc_data$dim$time$len) {
    # Initialize a list to store the sampled values for each variable
    sampled_values <- list()
    
    # Loop through each variable
    for (variable in variable_names) {
      # Get the variable data for the current year and gridcell
      variable_data <- ncvar_get(nc_data, variable, 
        start = c(gridcell_lon_index, gridcell_lat_index, year), count = c(1, 1, 1))

      # Add the sampled value to the list
      sampled_values[[variable]] <- variable_data
    }
    
    # Create a dataframe with the sampled values
    sampled_data_year <- data.frame(
      gridcell_id = gridcell_id,
      year = 1850 + nc_data$dim$time$vals[year]/365,
      stringsAsFactors = FALSE
    )
    
    # Add the sampled values to the dataframe
    for (variable in variable_names) {
      sampled_data_year[[variable]] <- sampled_values[[variable]]
    }
    
    # Append the dataframe to the sampled_data dataframe
    sampled_data <- rbind(sampled_data, sampled_data_year)
  }
}

# Clean up - close the NetCDF file
nc_close(nc_data)

# View the sampled data
head(sampled_data)
glimpse(sampled_data)

table(sampled_data$year)
sampled_data <- sampled_data[which(sampled_data$year < 3000),]
sampled_data$year <- floor(sampled_data$year)

#write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw.csv")

library(lfe)
glimpse(sampled_data)
summary(felm(mhw_int~ sst|gridcell_id+year|0|0,data=sampled_data))

summary(felm(sst~ year+I(year^2)|gridcell_id|0|0,data=sampled_data))

ggplot(sampled_data)+
geom_point(aes(x=year,y=sst))


nc_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/MHW/mangrove.mcw.stats.1996-2020.nc"
# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)

# Read the gridcell data from .gpkg file
gridcell_data <- st_read(gpkg_file)

# Get the variable names from the NetCDF file
variable_names <- names(nc_data$var)


# Extract latitude and longitude from the geometry column
gridcell_coords <- st_coordinates(gridcell_data)
gridcell_lat <- gridcell_coords[, "Y"]
gridcell_lon <- gridcell_coords[, "X"]

# Create an empty dataframe to store the sampled data
sampled_data <- data.frame(
  gridcell_id = integer(),
  year = integer(),
  stringsAsFactors = FALSE
)

# Loop through each gridcell
for (i in 1:nrow(gridcell_data)) {
    print(i)
  # Get the gridcell ID
  gridcell_id <- gridcell_data$id[i]
  
  # Get the latitude and longitude of the current gridcell
  gridcell_lat_i <- gridcell_lat[i]
  gridcell_lon_i <- gridcell_lon[i]

  if(gridcell_lon_i<0){gridcell_lon_i<-180-gridcell_lon_i}
  
  # Find the indices of the nearest gridcell in the NetCDF file
  gridcell_lat_index <- which.min(abs(nc_data$dim$lat$vals - gridcell_lat_i))
  gridcell_lon_index <- which.min(abs(nc_data$dim$lon$vals - gridcell_lon_i))
  
  
  # Loop through each year
  for (year in 1:nc_data$dim$time$len) {
    # Initialize a list to store the sampled values for each variable
    sampled_values <- list()
    
    # Loop through each variable
    for (variable in variable_names) {
      # Get the variable data for the current year and gridcell
      variable_data <- ncvar_get(nc_data, variable, 
        start = c(gridcell_lon_index, gridcell_lat_index, year), count = c(1, 1, 1))

      # Add the sampled value to the list
      sampled_values[[variable]] <- variable_data
    }
    
    # Create a dataframe with the sampled values
    sampled_data_year <- data.frame(
      gridcell_id = gridcell_id,
      year = 1850 + nc_data$dim$time$vals[year]/365,
      stringsAsFactors = FALSE
    )
    
    # Add the sampled values to the dataframe
    for (variable in variable_names) {
      sampled_data_year[[variable]] <- sampled_values[[variable]]
    }
    
    # Append the dataframe to the sampled_data dataframe
    sampled_data <- rbind(sampled_data, sampled_data_year)
  }
}

# Clean up - close the NetCDF file
nc_close(nc_data)

# View the sampled data
head(sampled_data)
glimpse(sampled_data)

table(sampled_data$year)
sampled_data <- sampled_data[which(sampled_data$year < 3000),]
sampled_data$year <- floor(sampled_data$year)
#write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw.csv")
