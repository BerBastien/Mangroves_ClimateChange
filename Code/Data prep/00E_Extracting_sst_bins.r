# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)
library(raster)

# Set the path to your data files
gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.bin.count.1996-2020.nc"
# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)
print(nc_data)

# Read the gridcell data from .gpkg file
gridcell_data <- st_read(gpkg_file)
# Your original raster (e.g. a "RasterLayer" or "RasterBrick" object)

br <- brick(nc_file)
orig_raster <- br

# Rotate the raster
rotated_raster <- raster::rotate(orig_raster)

# Check the extent of the rotated raster
print(extent(rotated_raster))

# Plot the rotated raster
plot(rotated_raster)
br <- rotated_raster
# Plot the new raster
plot(br)
br
plot(subset(br,15))
brick(nc_file, varname="count",level=2)
brick(nc_file, "count", 
        start = c(1, 1, year,1), count = c(-1,-1, 1,-1))


na_extracted_bin <- data.frame(id=gridcell_data$id,rbind(1:30))
grid_bin <- data.frame(id=gridcell_data$id,year=NA,rbind(1:30))
glimpse(grid_bin)
glimpse(gridcell_data)
brick(nc_file)
  print(br)
  for (j in 1:25){
    br <- raster::rotate(brick(nc_file, varname="count",level=j))
    grid_bin <- data.frame(id=gridcell_data$id,year=1850 + floor(nc_data$dim$time$vals[j]/365),rbind(1:30)*0)
    
    for (i in 1:30){
    extracted_bin <- raster::extract(raster::subset(br,i), gridcell_data)
    #plot(subset(br,i))
    #plot(gridcell_data,add=T)
    #glimpse(extracted_bin)
    extracted_bin <- unlist(extracted_bin)
    print(paste0("year: ",1850 + floor(nc_data$dim$time$vals[j]/365),"  bin:",i))
    #glimpse(extracted_bin)
    grid_bin[,(2+i)] <- extracted_bin
    }
    
    #glimpse(grid_bin)
    #print(rowSums(grid_bin[,c(3:32)],na.rm=T))
    #not_na <- which(rowSums(is.na(grid_bin))<30)
    #grid_bin[not_na,which(colSums(is.na(grid_bin))] <- 0
    print(j)
    if(j==1){
      all_data <- grid_bin 
    }else{
      all_data <- bind_rows(all_data,grid_bin)
    }
  }
  glimpse(all_data)
#write.csv(all_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_full_raw.csv")
all_data <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_full_raw.csv")
all_data <- all_data[,-c(1)]
  
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

write.csv(all_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_full.csv")

############# ANOM
  gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
  nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.anom.bin.count.1996-2020.nc"
  # Read the variable data from netCDF file
  nc_data <- nc_open(nc_file)
 

  # Read the gridcell data from .gpkg file
  gridcell_data <- st_read(gpkg_file)

  varnames(nc_data)
  
  variable_names <- names(nc_data$var)
  bins <- ncvar_get(nc_data, variable_names[2], 
          start = c(1), count = c(-1))
 
  range_bin <- paste0("anom_b",bins[1:18],"_",bins[2:19],"C")
  
  range_bin <- str_replace_all(range_bin, "-", "n")

  na_extracted_bin <- data.frame(id=gridcell_data$id,rbind(1:18)*0)
    for (j in 1:25){
      grid_bin <- data.frame(id=gridcell_data$id,year=NA,rbind(1:18)*0)
      br <- raster::rotate(brick(nc_file, varname="count",level=j))
      grid_bin[,2] <- 1850 + floor(nc_data$dim$time$vals[j]/365)
      for (i in 1:18){
      extracted_bin <- raster::extract(subset(br,i), gridcell_data)
      extracted_bin <- unlist(extracted_bin)
      print(paste0("year: ",1850 + floor(nc_data$dim$time$vals[j]/365),"  bin:",i))
      #glimpse(extracted_bin)
      grid_bin[,(2+i)] <- extracted_bin
      }
      print(rowSums(grid_bin[,c(3:20)],na.rm=T))
      #not_na <- which(rowSums(is.na(grid_bin))<30)
      #grid_bin[not_na,which(is.na(grid_bin[n,]))] <- 0
      print(j)
      if(j==1){
        all_data <- grid_bin 
      }else{
        all_data <- bind_rows(all_data,grid_bin)
      }
    }
    glimpse(all_data)
  names(all_data)<- c("gridcell_id","year",range_bin)


  write.csv(all_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_full_raw.csv")
  for(n in 1:dim(all_data)[1]){
        if(sum(is.na(all_data[n,]))<30){
            all_data[n,which(is.na(all_data[n,]))] <- 0
        }
        print(n)
    }
  #write.csv(all_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_full.csv")



############# ANOM

############ ANOM 0.5

  nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.anom.med.res.bin.count.1996-2020.nc"
  # Read the variable data from netCDF file 
  nc_data <- nc_open(nc_file)
  variable_names <- names(nc_data$var)
  gridcell_data <- st_read(gpkg_file)
  bins <- ncvar_get(nc_data, variable_names[2], 
          start = c(1), count = c(-1))
  length(bins)
  bins
  range_bin <- paste0("anom_b",bins[1:36],"_",bins[2:37],"C")
  range_bin
  range_bin <- str_replace_all(range_bin, "-", "n")

  grid_bin <- data.frame(id=gridcell_data$id,year=NA,rbind(1:36))
  glimpse(grid_bin)
  glimpse(gridcell_data)
    for (j in 1:25){
      grid_bin <- data.frame(id=gridcell_data$id,year=NA,rbind(1:36))
      br <- raster::rotate(brick(nc_file, varname="count",level=j))
      grid_bin[,2] <- 1850 + floor(nc_data$dim$time$vals[j]/365)
      for (i in 1:36){
      extracted_bin <- raster::extract(subset(br,i), gridcell_data)
      extracted_bin <- unlist(extracted_bin)
      #glimpse(extracted_bin)
      grid_bin[,(2+i)] <- extracted_bin
      }
      #glimpse(grid_bin)
      #not_na <- which(rowSums(is.na(grid_bin))<36)
      #grid_bin[not_na,which(is.na(grid_bin[n,]))] <- 0
      print(j)
      if(j==1){
        all_data <- grid_bin 
      }else{
        all_data <- bind_rows(all_data,grid_bin)
      }
    }
    glimpse(all_data)
    for(n in 1:dim(all_data)[1]){
        if(sum(is.na(all_data[n,]))<36){
            all_data[n,which(is.na(all_data[n,]))] <- 0
        }
        print(n)
    }
  names(all_data)<- c("gridcell_id","year",range_bin)
  write.csv(all_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_mr05_full.csv")

############ ANOM 0.5

############ BINS 0.5

  nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.med.res.bin.count.1996-2020.nc"
  # Read the variable data from netCDF file 
  nc_data <- nc_open(nc_file)
  variable_names <- names(nc_data$var)
  gridcell_data <- st_read(gpkg_file)
  bins <- ncvar_get(nc_data, variable_names[2], 
          start = c(1), count = c(-1))
  length(bins)
  bins
  range_bin <- paste0("anom_b",bins[1:60],"_",bins[2:61],"C")
  range_bin
  #range_bin <- str_replace_all(range_bin, "-", "n")

  grid_bin <- data.frame(id=gridcell_data$id,year=NA,rbind(1:60))
  glimpse(grid_bin)
  glimpse(gridcell_data)
    for (j in 1:25){
      br <- rotate(brick(nc_file, varname="count",level=j))
      grid_bin[,2] <- 1850 + floor(nc_data$dim$time$vals[j]/365)
      for (i in 1:60){
      extracted_bin <- extract(subset(br,i), gridcell_data)
      extracted_bin <- unlist(extracted_bin)
      #glimpse(extracted_bin)
      grid_bin[,(2+i)] <- extracted_bin
      }
      #glimpse(grid_bin)
      not_na <- which(rowSums(is.na(grid_bin))<60)
      grid_bin[not_na,which(is.na(grid_bin[n,]))] <- 0
      print(j)
      if(j==1){
        all_data <- grid_bin 
      }else{
        all_data <- bind_rows(all_data,grid_bin)
      }
    }
    glimpse(all_data)
  names(all_data)<- c("gridcell_id","year",range_bin)
  write.csv(all_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_mr05_full.csv")

############ BINS 0.5























### PRevious CODE

#ids_28 <- grid_bin$id[rowSums(is.na(grid_bin)) == 28]
#grid_bin[which(grid_bin$id %in% ids_28),]

extract(subset(br,15),gridcell_data)

plot(gridcell_data,add=T)
plot(gridcell_data)
# Get the variable names from the NetCDF file
variable_names <- names(nc_data$var)


# Extract latitude and longitude from the geometry column
st_coordinates(gridcell_data) <- st_coordinates(gridcell_data)
gridcell_coords <- st_coordinates(gridcell_data)
gridcell_lat <- gridcell_coords[, "Y"]
gridcell_lon <- gridcell_coords[, "X"]

# Create an empty dataframe to store the sampled data
sampled_data <- data.frame(
  gridcell_id = integer(),
  year = integer(),
  stringsAsFactors = FALSE
)

br <- brick(nc_file)
plot(subset(br,15))
plot(gridcell_data$geom[which(gridcell_data$id %in% empty_gridcell)],col="red")
plot(gridcell_data$geom[which(gridcell_data$id %in% empty_gridcell)],col="red",add=T)
plot(gridcell_data$geom[-which(gridcell_data$id %in% empty_gridcell)],col="blue",add=T)
plot(subset(br,15),add=T)
# Loop through each gridcell
for (i in 1:nrow(gridcell_data)) {
    print(i)
  # Get the gridcell ID
  gridcell_id <- gridcell_data$id[i]
  
  # Get the latitude and longitude of the current gridcell
  gridcell_lat_i <- gridcell_lat[i]
  gridcell_lon_i <- gridcell_lon[i]

  if(gridcell_lon_i<0){gridcell_lon_i<-360+gridcell_lon_i}
  
  # Find the indices of the nearest gridcell in the NetCDF file
  gridcell_lat_index <- which.min(abs(nc_data$dim$lat$vals - gridcell_lat_i))
  gridcell_lon_index <- which.min(abs(nc_data$dim$lon$vals - gridcell_lon_i))
  
  
  # Loop through each year
  for (year in 1:nc_data$dim$time$len) {
    # Initialize a list to store the sampled values for each variable
    sampled_values <- list()
    
    # Loop through each variable
    for (variable in variable_names[1]) {
      # Get the variable data for the current year and gridcell
      #print(nc_data)
      variable_data <- ncvar_get(nc_data, variable, 
        start = c(gridcell_lon_index, gridcell_lat_index, year,1), count = c(1, 1, 1,-1))

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
    for (variable in variable_names[1]) {
      new_var_vec <- data.frame(t(sampled_values[[variable]]))
      if (sum(is.na(new_var_vec))!=length(new_var_vec)){
        new_var_vec[which(is.na(new_var_vec))] <- 0
      }
      sampled_data_year <- cbind(sampled_data_year,new_var_vec)
    }
    
    # Append the dataframe to the sampled_data dataframe
    sampled_data <- rbind(sampled_data, sampled_data_year)
  }
}

glimpse(sampled_data)
bins <- ncvar_get(nc_data, variable_names[2], 
        start = c(1), count = c(-1))
range_bin <- paste0("b",bins[1:30],"_",bins[2:31],"C")
names(sampled_data)<- c("gridcell_id","year",range_bin)

# Clean up - close the NetCDF file
nc_close(nc_data)

# View the sampled data
head(sampled_data)
glimpse(sampled_data)

table(sampled_data$year)
sampled_data <- sampled_data[which(sampled_data$year < 3000),]
sampled_data$year <- floor(sampled_data$year)


#write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_or2.csv")
sampled_data <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins.csv")
sampled_data_full <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_full.csv")

library(lfe)
library(scico)
glimpse(sampled_data)
table(sampled_data$year)

hist_30 <- ggplot(sampled_data[which(sampled_data$b30_31C!=0),])+
geom_histogram(aes(x=b30_31C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST 30-31°C")+
#scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
scale_fill_scico(palette="batlow") 
hist_30

max(sampled_data_full$year[which(sampled_data_full$b30_31C!=0)])
max(sampled_data$year[which(sampled_data$b30_31C!=0)])

min(sampled_data_full$year[which(sampled_data_full$b30_31C!=0)])
min(sampled_data$year[which(sampled_data$b30_31C!=0)])


hist_30 <- ggplot(sampled_data_full[which(sampled_data_full$b30_31C!=0),])+
geom_histogram(aes(x=b30_31C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST 30-31°C")+
#scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
scale_fill_scico(palette="batlow") 
hist_30

sum(sampled_data$b30_31C!=0,na.rm=TRUE)
sum(sampled_data_full$b30_31C!=0,na.rm=TRUE)
sum(sampled_data[which(sampled_data$year>2000),]$b30_31C!=0,na.rm=TRUE)

glimpse(sampled_data_full)
glimpse(sampled_data)
hist(rowSums(sampled_data_full[which(sampled_data_full$year==2002),c(4:33)],na.rm=TRUE))
hist(rowSums(sampled_data[which(sampled_data$year==2002),c(4:33)],na.rm=TRUE))
glimpse(sampled_data[which(sampled_data$year==2002),c(4:33)])

year_3031 <- aggregate(b30_31C ~ year, FUN="mean",data=sampled_data)

smooth_30 <- ggplot(year_3031)+
geom_point(aes(y=b30_31C,x=year,color=year))+theme_bw()+geom_smooth(aes(y=b30_31C,x=year)) + ylab("Average days of the year \nwith SST 30-31°C")+
scale_color_scico(palette="batlow") 
#geom_histogram(aes(y=b30_31C,x=year,fill=year),stat='identity')+theme_bw()



ggarrange(hist_30, smooth_30, common.legend=TRUE,ncol=2,nrow=1, 
                    widths=c(3,2),
                    legend="right")


year_3031 <- aggregate(b30_31C ~ year, FUN="mean",data=sampled_data_full)
smooth_30_full <- ggplot(year_3031)+
  geom_point(aes(y=b30_31C,x=year,color=year))+
  theme_bw()+geom_smooth(aes(y=b30_31C,x=year)) + ylab("Average days of the year \nwith SST 30-31°C")+
  scale_color_scico(palette="batlow") 
smooth_30_full

ggarrange(smooth_30_full, smooth_30, common.legend=TRUE,ncol=2,nrow=1, 
                    widths=c(3,2),
                    legend="right")
#ggsave("Figures/bins/count_hist.png",dpi=600)

hist_20 <- ggplot(sampled_data[which(sampled_data$b20_21C!=0),])+
geom_histogram(aes(x=b20_21C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST 20-21°C")+
#scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
scale_fill_scico(palette="batlow") 

year_2021 <- aggregate(b20_21C ~ year, FUN="mean",data=sampled_data)

smooth_20 <- ggplot(year_2021)+
geom_point(aes(y=b20_21C,x=year,color=year))+theme_bw()+geom_smooth(aes(y=b20_21C,x=year))+ ylab("Average days of the year \nwith SST 20-21°C")+
scale_color_scico(palette="batlow") 

ggarrange(hist_20,smooth_20, common.legend=TRUE,ncol=2,nrow=1, 
                    widths=c(3,2),
                    legend="right")

ggsave("Figures/bins/count_hist_20.png",dpi=600)




summary(felm(b30_31C~ year|gridcell_id|0|0,data=sampled_data))




############# Anomalies
nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.anom.bin.count.1996-2020.nc"
# Read the variable data from netCDF file
nc_data <- nc_open(nc_file)
print(nc_data)
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
    for (variable in variable_names[1]) {
      # Get the variable data for the current year and gridcell
      #print(nc_data)
      variable_data <- ncvar_get(nc_data, variable, 
        start = c(gridcell_lon_index, gridcell_lat_index, year,1), count = c(1, 1, 1,-1))

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
    for (variable in variable_names[1]) {
      new_var_vec <- data.frame(t(sampled_values[[variable]]))
      if (sum(is.na(new_var_vec))!=length(new_var_vec)){
        new_var_vec[which(is.na(new_var_vec))] <- 0
      }
      sampled_data_year <- cbind(sampled_data_year,new_var_vec)
    }
    
    # Append the dataframe to the sampled_data dataframe
    sampled_data <- rbind(sampled_data, sampled_data_year)
  }
}

glimpse(sampled_data)
bins <- ncvar_get(nc_data, variable_names[2], 
        start = c(1), count = c(-1))
range_bin <- paste0("anom_b",bins[1:18],"_",bins[2:19],"C")
names(sampled_data)<- c("gridcell_id","year",range_bin)

names(sampled_data)[3:12] <- c("anom_bn10_n9C", "anom_bn9_n8C" , "anom_bn8_n7C" , "anom_bn7_n6C",
  "anom_bn6_n5C",  "anom_bn5_n4C" , "anom_bn4_n3C" , "anom_bn3_n2C" , "anom_bn2_n1C" , "anom_bn1_0C")

# Clean up - close the NetCDF file
nc_close(nc_data)

# View the sampled data
head(sampled_data)
glimpse(sampled_data)

table(sampled_data$year)
sampled_data <- sampled_data[which(sampled_data$year < 3000),]
sampled_data$year <- floor(sampled_data$year)

#write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins.csv")

sampled_data <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins.csv")

hist_20 <- ggplot(sampled_data[which(sampled_data$anom_b1_2C!=0),])+
geom_histogram(aes(x=anom_b1_2C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST anomaly 1-2°C")+
#scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
scale_fill_scico(palette="batlow") 
hist_20

year_2021 <- aggregate(anom_b1_2C ~ year, FUN="mean",data=sampled_data)

smooth_20 <- ggplot(year_2021)+
geom_point(aes(y=anom_b1_2C,x=year,color=year))+theme_bw()+geom_smooth(aes(y=anom_b1_2C,x=year))+ ylab("Average days of the year \nwith SST anomaly 1-2°C")+
scale_color_scico(palette="batlow") 

ggarrange(hist_20,smooth_20, common.legend=TRUE,ncol=2,nrow=1, 
                    widths=c(3,2),
                    legend="right")

ggsave("Figures/bins/AnomBin_count_12.png",dpi=600)



library(lfe)
glimpse(sampled_data)
table(sampled_data$year)

ggplot(sampled_data)+
geom_histogram(aes(x=anom_b1_2C,group=year,fill=year))+theme_bw()

summary(felm(b18_19C~ year|gridcell_id|0|0,data=sampled_data))
