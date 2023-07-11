# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)
library(raster)

# Set the path to your data files
gpkg_file <- "grid_nighlights_spatial.gpkg"
nc_file <- "mangrove.sst.bin.count.1996-2020.nc"
nc_data <- nc_open(nc_file)

gridcell_data <- st_read(gpkg_file)

  for (j in 1:25){ #25 years
    br <- raster::rotate(brick(nc_file, varname="count",level=j))
    grid_bin <- data.frame(id=gridcell_data$id,year=1850 + floor(nc_data$dim$time$vals[j]/365),rbind(1:30)*0)
    
    for (i in 1:30){ #30 bins
    extracted_bin <- raster::extract(raster::subset(br,i), gridcell_data)
    extracted_bin <- unlist(extracted_bin)
    print(paste0("year: ",1850 + floor(nc_data$dim$time$vals[j]/365),"  bin:",i))
    grid_bin[,(2+i)] <- extracted_bin
    }
    
    print(j)
    if(j==1){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }
  }
  
    bins <- ncvar_get(nc_data, variable_names[2], 
            start = c(1), count = c(-1))
    range_bin <- paste0("b",bins[1:30],"_",bins[2:31],"C")
    range_bin
    names(all_data)<- c("gridcell_id","year",range_bin)

    for(n in 1:dim(all_data)[1]){
            if(sum(is.na(all_data[n,]))<30){
                all_data[n,which(is.na(all_data[n,]))] <- 0
            }
            print(n)
        }

    write.csv(all_data,"sst_bins_full.csv")
