# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)

# Set the path to your data files
gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.high.res.bin.count.1996-2020.nc"
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

#write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins.csv")
sampled_data <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins.csv")

library(lfe)
glimpse(sampled_data)
table(sampled_data$year)

hist_30 <- ggplot(sampled_data[which(sampled_data$b30_31C!=0),])+
geom_histogram(aes(x=b30_31C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST 30-31°C")+
#scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
scale_fill_scico(palette="batlow") 
hist_30

year_3031 <- aggregate(b30_31C ~ year, FUN="mean",data=sampled_data)

smooth_30 <- ggplot(year_3031)+
geom_point(aes(y=b30_31C,x=year,color=year))+theme_bw()+geom_smooth(aes(y=b30_31C,x=year)) + ylab("Average days of the year \nwith SST 30-31°C")+
scale_color_scico(palette="batlow") 
#geom_histogram(aes(y=b30_31C,x=year,fill=year),stat='identity')+theme_bw()

ggarrange(hist_30, smooth_30, common.legend=TRUE,ncol=2,nrow=1, 
                    widths=c(3,2),
                    legend="right")

ggsave("Figures/bins/count_hist.png",dpi=600)

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



############# Anomalies 0.5 start
  nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.anom.med.res.bin.count.1996-2020.nc"
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
  bins <- (round(bins, 2))
  range_bin <- paste0("anom_b",bins[1:36],"_",bins[2:37],"C")
  names(sampled_data)<- c("gridcell_id","year",range_bin)

  names(sampled_data) <- str_replace_all(names(sampled_data), "-", "n")

  glimpse(sampled_data)
  # Clean up - close the NetCDF file
  nc_close(nc_data)

  # View the sampled data
  head(sampled_data)
  glimpse(sampled_data)

  table(sampled_data$year)
  sampled_data <- sampled_data[which(sampled_data$year < 3000),]
  sampled_data$year <- floor(sampled_data$year)

  #write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_mr05.csv")

  #sampled_data <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins.csv")
  library(scico)
  hist_20 <- ggplot(sampled_data[which(sampled_data$anom_b1_1.5C!=0),])+
  geom_histogram(aes(x=anom_b1_1.5C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST anomaly 1-2°C")+
  #scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
  scale_fill_scico(palette="batlow") 
  hist_20
### Anomalies 0.1 end


############# Anomalies 0.1 start
  nc_file <- "C:/Users/basti/Box/Data/Oceans/MHWs/mangrove.sst.anom.high.res.bin.count.1996-2020.nc"
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
  bins <- (round(bins, 2))
  range_bin <- paste0("anom_b",bins[1:180],"_",bins[2:181],"C")
  names(sampled_data)<- c("gridcell_id","year",range_bin)

  seqn <- seq(from=0,to=10,by=0.1)
  seqn <-rev(seqn)
  newnames <- paste0("anom_bn",seqn[1:100],"_n",seqn[2:101],"C")

  names(sampled_data)[3:102] <- paste0("anom_bn",seqn[1:100],"_n",seqn[2:101],"C")

  glimpse(sampled_data)
  # Clean up - close the NetCDF file
  nc_close(nc_data)

  # View the sampled data
  head(sampled_data)
  glimpse(sampled_data)

  table(sampled_data$year)
  sampled_data <- sampled_data[which(sampled_data$year < 3000),]
  sampled_data$year <- floor(sampled_data$year)

  #write.csv(sampled_data,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_hr01.csv")

  #sampled_data <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins.csv")
  library(scico)
  hist_20 <- ggplot(sampled_data[which(sampled_data$anom_b1.1_1.2C!=0),])+
  geom_histogram(aes(x=anom_b1.1_1.2C,group=year,fill=year),binwidth=1)+theme_bw()+xlab("Days with SST anomaly 1-2°C")+
  #scale_fill_scico(palette="batlow",direction=-1,trans="reverse") 
  scale_fill_scico(palette="batlow") 
  hist_20
### Anomalies 0.1 end
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
