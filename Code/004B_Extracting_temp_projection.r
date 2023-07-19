# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)
library(raster)

# Set the path to your data files
gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
gridcell_data <- st_read(gpkg_file)

nc_file_p <- "C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\Atm\\pr_mon_mod_ssp585_192_ave.nc"
nc_file_t <- "C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\Atm\\tas_mon_mod_ssp585_192_ave.nc"

nc_file <- nc_file_p
nc_data <- nc_open(nc_file)
print(nc_data)
variable_names <- names(nc_data$var)

start <- which(1850 + floor(nc_data$dim$time$vals/365.25)==2020)

1850+nc_data$dim$time$vals[start]/365.25

1849+length(nc_data$dim$time$vals)/12

    br <- raster::rotate(brick(nc_file, varname=variable_names[1]))
    br
    print(crs(br))
    br_1 <- subset(br,1)
    gridcell_data <- st_read(gpkg_file)
    plot(br_1)
    plot(gridcell_data,add=T)

    # Check the projections
    cat("Raster projection: ", raster::proj4string(br_1), "\n")
    cat("Vector projection: ", st_crs(gridcell_data)$proj4string, "\n")

    # If the projections don't match, reproject the vector layer to match the raster
    if (raster::proj4string(br_1) != st_crs(gridcell_data)$proj4string) {
    gridcell_data <- st_transform(gridcell_data, crs = raster::proj4string(br_1))
    }

    gd <- data.frame(gridcell_id=gridcell_data$id)
for(i in 170:(length(nc_data$dim$time$vals)/12)){
    br_1 <- subset(br,c((1+(i-1)*12):(i*12)))
    br_mean <- mean(br_1)
    #plot(br_mean)
    #preci <- raster::extract(br_mean,gridcell_data)
    pr_flux <- exact_extract(br_mean,gridcell_data,'mean') #Preci flux: kg m-2 s-1
    #hist(pr_flux)
    #area_br <- exact_extract(area(br_mean),gridcell_data,'mean')
    #pr_flux_1deg <- area_br *pr_flux #Preci flux: kg s-1
    #hist(pr_flux_1deg)
    #area_br/st_area(gridcell_data) 
    #pr_1month_1deg <- pr_flux_1deg * 60*60*24*30 #Preci mass: kg
    #pr_1month_mm <- pr_1month_1deg / st_area(gridcell_data) #Preci mass over the area: mm

    #mean_values <- sapply(preci, function(x) mean(x, na.rm = TRUE))
    gd$preci <- pr_flux*60*60*24 #values were in mm/s (precipitation flux), now converting to Mean monthly mm
    gd$year <- as.double(substr(names(br_1)[1],start=2,stop=5))
    if(i==170){
        all_data <- gd
    }else{
        all_data <- rbind(all_data,gd)
    }
    print(paste0(i,"/",(length(nc_data$dim$time$vals)/12)))
}
glimpse(all_data)
write.csv(all_data,"preci_85.csv")


nc_file <- nc_file_t
nc_data <- nc_open(nc_file)
print(nc_data)
variable_names <- names(nc_data$var)


    br <- raster::rotate(brick(nc_file, varname="tas"))
    print(crs(br))
    br_1 <- subset(br,1)
    gridcell_data <- st_read(gpkg_file)
    plot(br_1)
    plot(gridcell_data,add=T)

    # Check the projections
    cat("Raster projection: ", raster::proj4string(br_1), "\n")
    cat("Vector projection: ", st_crs(gridcell_data)$proj4string, "\n")

    # If the projections don't match, reproject the vector layer to match the raster
    if (raster::proj4string(br_1) != st_crs(gridcell_data)$proj4string) {
    gridcell_data <- st_transform(gridcell_data, crs = raster::proj4string(br_1))
    }

    gd <- data.frame(gridcell_id=gridcell_data$id)
for(i in 170:(length(nc_data$dim$time$vals)/12)){
    br_1 <- subset(br,c((1+(i-1)*12):(i*12)))
    br_mean <- mean(br_1)
    #plot(br_mean)
    #preci <- raster::extract(br_mean,gridcell_data)
    temp_k <- exact_extract(br_mean,gridcell_data,'mean') #Preci flux: kg m-2 s-1
    #hist(pr_flux)
    #area_br <- exact_extract(area(br_mean),gridcell_data,'mean')
    #pr_flux_1deg <- area_br *pr_flux #Preci flux: kg s-1
    #hist(pr_flux_1deg)
    #area_br/st_area(gridcell_data) 
    #pr_1month_1deg <- pr_flux_1deg * 60*60*24*30 #Preci mass: kg
    #pr_1month_mm <- pr_1month_1deg / st_area(gridcell_data) #Preci mass over the area: mm

    #mean_values <- sapply(preci, function(x) mean(x, na.rm = TRUE))
    gd$temp <- temp_k -273.15#values were in mm/s (precipitation flux), now converting to Mean monthly mm
    gd$year <- as.double(substr(names(br_1)[1],start=2,stop=5))
    if(i==170){
        all_data <- gd
    }else{
        all_data <- rbind(all_data,gd)
    }
    print(paste0(i,"/",(length(nc_data$dim$time$vals)/12)))
}
glimpse(all_data)
write.csv(all_data,"temp_85.csv")






preci_data <- as.data.frame(all_data[,c(1:3)])
glimpse(preci_data)
write.csv()
gridcell_data <- st_read(gpkg_file)

  
    br <- raster::rotate(brick(nc_file, varname=variable_names[1]))
  
  for (j in 104:181){ #25 years
    grid_bin <- data.frame(id=gridcell_data$id,year= floor(nc_data$dim$time$vals[j]),sst=NA)
    extracted_bin <- raster::extract(raster::subset(br,j), gridcell_data)
    extracted_bin <- unlist(extracted_bin)
    grid_bin[,(3)] <- extracted_bin
    print(paste0("sst - year: ",nc_data$dim$time$vals[j]))
    if(j==104){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }}

    future_sst <- all_data
    write.csv(future_sst,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_projection85.csv")

    br <- raster::rotate(brick(nc_file, varname="sst_hot"))
  for (j in 104:181){ #25 years
    grid_bin <- data.frame(id=gridcell_data$id,year= floor(nc_data$dim$time$vals[j]),sst_hot=NA)
    extracted_bin <- raster::extract(raster::subset(br,j), gridcell_data)
    #plot(subset(br,j))
    extracted_bin <- unlist(extracted_bin)
    grid_bin[,(3)] <- extracted_bin
    print(paste0("sst hot - year: ",nc_data$dim$time$vals[j]))
    if(j==104){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }}

    
    future_sst_hot <- all_data
    
    write.csv(future_sst_hot,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_hot_projection85.csv")

for (j in 104:181){ #25 years
    br <- raster::rotate(brick(nc_file, varname="count",level=j))
    grid_bin <- data.frame(id=gridcell_data$id,year= floor(nc_data$dim$time$vals[j]),rbind(1:46)*0)
    
    for (i in 1:45){ #46 bins
    #plot(subset(br,i))
    extracted_bin <- raster::extract(raster::subset(br,i), gridcell_data)
    extracted_bin <- unlist(extracted_bin)
    print(paste0("year: ",(nc_data$dim$time$vals[j]),"  bin:",i,"/46"))
    grid_bin[,(2+i)] <- extracted_bin
    }
    
    print(j)
    if(j==104){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }
  }

  
  
    bins <- ncvar_get(nc_data,"bin_edges", 
            start = c(1), count = c(-1))
    range_bin <- paste0("b",bins[1:45],"_",bins[2:46],"C")
    range_bin
    names(all_data)<- c("gridcell_id","year",range_bin)

    for(n in 1:dim(all_data)[1]){
            if(sum(is.na(all_data[n,]))<45){
                all_data[n,which(is.na(all_data[n,]))] <- 0
            }
            print(n)
        }

    write.csv(all_data,"sst_bins_projection85_2.csv")
