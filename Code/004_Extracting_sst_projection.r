# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)
library(raster)

# Set the path to your data files
gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
nc_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\input\\mangrove.cesm1.lens.metrics.hist.RCP85.1920-2100.nc"
nc_data <- nc_open(nc_file)
print(nc_data)
variable_names <- names(nc_data$var)

gridcell_data <- st_read(gpkg_file)

  
    br <- raster::rotate(brick(nc_file, varname="sst_ann"))
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
    
    for (i in 1:46){ #46 bins
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

    write.csv(all_data,"sst_bins_projection85.csv")
