# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)
## Function
        sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(1:end1),c(1:end1)]
            beta.hat <- coef(model)[c(1:end1)]
            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(x, x^2)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x))
        }
    ## Function

# Set the path to your data files
gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
nc_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/MHW/mangrove.mhw.stats.1996-2020.nc"
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
      variable_data <- ncvar_get(nc_data, variable, start = c(gridcell_lon_index, gridcell_lat_index, year), count = c(1, 1, 1))
      
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

library(lfe)
glimpse(sampled_data)
summary(felm(mhw_int~ sst|gridcell_id+year|0|0,data=sampled_data))

summary(felm(sst~ year+I(year^2)|gridcell_id|0|0,data=sampled_data))

ggplot(sampled_data)+
geom_point(aes(x=year,y=sst))










##PLOT start


glimpse(gridcell_data)

gridcell_sf <- st_as_sf(gridcell_data)
glimpse(gridcell_sf)

# Plot the polygons on a map
plot_ntl <- ggplot() +
  geom_sf(data = gridcell_sf, aes(fill = log(X2020)),color="transparent") +
  scale_fill_viridis_c() +
  labs(fill = "Intensity") +
  ggtitle("Nigthtime ligths")+
  theme_bw()


# Plot the data
plot_ntl <-ggplot() +
  geom_sf(data = gridcell_sf, aes(fill = log(X2020)), color = "transparent") +
  scale_fill_gradient(low = "#000000", high = "#FFFF00", na.value = "transparent") +
  labs(fill = "Intensity") +
  ggtitle("Night-time ligths")+
  theme_void()


# Read the NetCDF file
library(raster)
nc_data <- raster(nc_file, varname = "sst")

# Extract the data for the specific year (e.g., year 1)
year_index <- 1
sst_data <- nc_data[[year_index]]

# Convert the raster to a dataframe for plotting
df <- as.data.frame(sst_data, xy = TRUE)
colnames(df) <- c("lon", "lat", "sst")

glimpse(df)
# Modify the dataframe to adjust the longitude range
df$lon <- ifelse(df$lon >= 180, df$lon - 360, df$lon)
# Plot the data
plot_sst <- ggplot(df, aes(x = lon, y = lat, fill = sst)) +
  geom_raster() +
  scale_fill_viridis_c(na.value = "transparent") +  # Set the na.value to "transparent"
  labs(fill = "(°C)") +
  ggtitle("Sea surface temperature")+
  coord_equal() + theme_void()

#library(ggpubr)
ggarrange(plot_ntl,plot_sst,ncol=1,nrow=2)
ggsave("C:/Users/basti/Box/Data/Oceans/Mangroves/Figures/NTL_SST.png")




nc_data <- raster(nc_file, varname = "mhw_freq")

# Extract the data for the specific year (e.g., year 1)
year_index <- 1
sst_data <- nc_data[[year_index]]

# Convert the raster to a dataframe for plotting
df <- as.data.frame(sst_data, xy = TRUE)
colnames(df) <- c("lon", "lat", "mhw_freq")

glimpse(df)
# Modify the dataframe to adjust the longitude range
df$lon <- ifelse(df$lon >= 180, df$lon - 360, df$lon)
# Plot the data
plot_sst <- ggplot(df, aes(x = lon, y = lat, fill = mhw_freq*100)) +
  geom_raster() +
  scale_fill_viridis_c(na.value = "transparent",limits = c(0.001, 25)) +  # Set the na.value to "transparent"
  labs(fill = "(% of year)") +
  ggtitle("Marine Heatwaves Frequency")+
  coord_equal() + theme_void()

#library(ggpubr)
ggarrange(plot_ntl,plot_sst,ncol=1,nrow=2)
ggsave("C:/Users/basti/Box/Data/Oceans/Mangroves/Figures/NTL_MHW.png")
## Plots end



# Global Watch

library(sf)
library(dplyr)

# Path to the main folder containing sub-folders
main_folder <- "C:/Users/basti/Box/Data/Oceans/Mangroves/mangrove_watch/"

# Function to read and sample shapefile
read_and_sample_shapefile <- function(folder_path) {
  # Get the file path of the shapefile within the folder
  shapefile_path <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)
  
  # Read the shapefile
  shapefile <- st_read(shapefile_path)
  
  # Sample the shapefile using gridcell_data
  sampled_shapefile <- shapefile %>%
    st_join(gridcell_data, by = "id")
  
  return(sampled_shapefile)
}


# List all sub-folders within the main folder
sub_folders <- list.files(main_folder, pattern = "^gmw_v3_\\d{4}_vec$", full.names = TRUE)

years_str <- str_extract(list.files("C:/Users/basti/Box/Data/Oceans/Mangroves/mangrove_watch/rasters/"), "(?<=gmw_v3_)\\d{4}")

for (j in 1:length(years_str)){
    print(years_str[j])
    sub_folders <- list.files(paste0("C:/Users/basti/Box/Data/Oceans/Mangroves/mangrove_watch/rasters/gmw_v3_",years_str[j],"_gtiff/gmw_v3_",years_str[j]), full.names = TRUE)

    #sub_folders <- list.files("C:/Users/basti/Box/Data/Oceans/Mangroves/mangrove_watch/rasters/gmw_v3_2007_gtiff/gmw_v3_2007", full.names = TRUE)
    for(i in 1:length(sub_folders)){
        file_path <- sub_folders[i]
        print(i)
        raster_tile <- raster(sub_folders[i])
        raster_values <- raster::getValues(raster_tile)
        perc_cover <- 100*sum(raster_values,na.rm=TRUE)/ncell(raster_tile)
        # Extract the year
        year <- as.numeric(str_extract(file_path, "\\d{4}"))

        # Extract the degrees north/south
        lat_str <- str_extract(file_path, "(?<=S|N)\\d{2}")
        lat <- as.numeric(lat_str)
        if (str_sub(file_path, start = -19, end = -19) == "S") {
            lat <- -lat
        }   

        # Extract the degrees east/west
        lon_str <- str_extract(file_path, "(?<=E|W)\\d{3}")
        lon <- as.numeric(lon_str)
        if (str_sub(file_path, start = -16, end = -16) == "W") {
            lon <- -lon
        }

        
        df <- data.frame(year = year, lat = lat, lon = lon, perc_cover = perc_cover)
        if(i==1 & j==1){
            df_mangrove_cover <- df
        }else{
            df_mangrove_cover <- bind_rows(df_mangrove_cover,df)
        }



    }
}
write.csv(df_mangrove_cover,"mangrove_cover.csv")
glimpse(df_mangrove_cover)
ggplot(df_mangrove_cover,aes(x=lat,y=perc_cover,color=factor(year)))+
    geom_point(alpha=0.5)+
    theme_bw() + xlab("Latitude")+ylab("Mangrove percent cover")
    ggsave("Figures/mangroves/mangrove_cover.png",dpi=600)
    #getwd()
    #setwd("C:/Users/basti/Documents/GitHub/BlueDICE/")

# for (i in 1:nrow(gridcell_data)) {
#   # Get the gridcell ID
#   gridcell_id <- gridcell_data$id[i]
  
#   # Get the latitude and longitude of the current gridcell
#   gridcell_coords <- st_coordinates(gridcell_data)
     gridcell_lat <- gridcell_coords[, "Y"]
     gridcell_lon <- gridcell_coords[, "X"]

for(j in 1:length(years_str)){
    year <- years_str[j] 
    #print(year)
    df_mangrove_cover_y <- df_mangrove_cover[which(df_mangrove_cover$year==as.numeric(year)),]
    #head(df_mangrove_cover_y)


    for (i in 1:nrow(gridcell_data)) {
        # Get the gridcell ID
        print(paste0(year,"---",i))
        gridcell_id <- gridcell_data$id[i]
        
        # Get the latitude and longitude of the current gridcell
        gridcell_lat_i <- gridcell_lat[i]
        gridcell_lon_i <- gridcell_lon[i]
        
        # Find the indices of the nearest gridcell in the NetCDF file
        gridcell_lat_index <- which(abs(df_mangrove_cover_y$lat - gridcell_lat_i)<1)
        gridcell_lon_index <- which(abs(df_mangrove_cover_y$lon - gridcell_lon_i)<1)

        index_m <- gridcell_lat_index[which(gridcell_lat_index %in% gridcell_lon_index)]
        df_mangrove_cover_y$lon[index_m]
        df_mangrove_cover_y$lat[index_m]
        gridcell_lon[i]
        gridcell_lat[i]
        
        cov <- df_mangrove_cover_y$perc_cover[index_m]
        if(length(cov)>1){
            #print("more than one match")
            min_id <- which.min(abs(df_mangrove_cover_y$lat[index_m]-gridcell_lat[i])+
            abs(df_mangrove_cover_y$lon[index_m]-gridcell_lon[i]))
            cov <- cov[min_id]
            #which.min(abs(df_mangrove_cover_y$lon - gridcell_lon_i))
        }

        if(length(cov)==0){
            print("no data in mangroves")
            cov <- NA
        }

        cover1 <- data.frame(id=gridcell_id,mangrove_cover_perc = cov,year=year)
        if(i==1 & j==1){
                mangrove_cover_id <- cover1
        } else{mangrove_cover_id <- bind_rows(mangrove_cover_id,cover1)}
    }
}


glimpse(sampled_data)
names(mangrove_cover_id)[1]<-"gridcell_id"
mangrove_cover_id$year <- as.numeric(mangrove_cover_id$year)
glimpse(mangrove_cover_id)
glimpse(mng_sst)
mng_sst <- sampled_data

mng_sst <- merge(mng_sst,mangrove_cover_id,by=c("gridcell_id","year"),all.x=TRUE)
glimpse(mng_sst)

#summary(felm(mangrove_cover_perc~sst+I(sst^2)+mhw_int+mhw_freq|year+gridcell_id|0|0,data=mng_sst))



glimpse(gridcell_sf)

gridcell_sf <- gridcell_sf %>%
  mutate(area = st_area(geom))
library(units)

# Remove units from the area column
gridcell_sf$area <- set_units(gridcell_sf$area, NULL)
ntl_df <- data.frame(gridcell_sf)[,c(1:10,12)]
glimpse(ntl_df)

ntl_df_long <- ntl_df %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "ntl")

# Print the long format dataframe
glimpse(ntl_df_long)
table(ntl_df_long$year)
ntl_df_long$year <- as.numeric(gsub("X", "", ntl_df_long$year))
names(ntl_df_long)[1] <- "gridcell_id"

df_mangroves <- merge(mng_sst,ntl_df_long,by=c("year","gridcell_id"),all.x=TRUE)
glimpse(df_mangroves)
df_mangroves$mangrove_cover_area <- df_mangroves$mangrove_cover_perc * df_mangroves$area
df_mangroves$mhw <- df_mangroves$mhw_freq
df_mangroves$mhw[df_mangroves$mhw>0]<- 1

glimpse(df_mangroves)

summary(felm(log(mangrove_cover_area) ~ sst+I(sst^2)+mhw:mhw_int_anom+mhw:mhw_freq+
    #I(mhw_int^2)+
    #mhw_freq+
    #I(log(ntl)^2)+log(ntl)
    I((ntl)^2)#+(ntl)
    |year+gridcell_id|0|0,
    data=df_mangroves[which(df_mangroves$ntl>0),]))
df_mangroves$log_ntl <- log(df_mangroves$ntl)

summary(felm(log(mangrove_cover_area) ~ sst+I(sst^2)+mhw:mhw_int+
    I(mhw_int^2)+mhw:mhw_freq+
    #mhw_freq+
    log_ntl+I(log_ntl^2)
    #I((ntl)^2)#+(ntl)
    |year|0|0,
    data=df_mangroves[which(df_mangroves$ntl>0),]))

table(df_mangroves$mhw_freq)

# Group the data by gridcell_id
df_mangroves_grouped <- df_mangroves %>%
  group_by(gridcell_id) %>%
  arrange(year)  # Ensure the data is sorted by year

# Calculate the annual change in mangrove cover percentage
df_mangroves_change <- df_mangroves_grouped %>%
  mutate(annual_area_change =mangrove_cover_area / lag(mangrove_cover_area)-1)

# Print the dataset with annual change
glimpse(df_mangroves_change)


hist(df_mangroves_change$annual_area_change)

hist(df_mangroves_change$mangrove_cover_perc)


model_1 <- (felm(log(mangrove_cover_area)~sst+I(sst^2)+
    mhw_int_anom+mhw:mhw_freq+
    log_ntl+I(log_ntl^2)
    |year|0|0,data=
    df_mangroves_change[which(df_mangroves_change$ntl>0),]
    ))

summary(model_1)
df_estimates <- sqest(df_mangroves[which(df_mangroves$ntl>0),],model_1,"sst","all")
    
    ggplot(df_estimates,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Sea Surface Temperature",y="Effect")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impact on mangrove area cover")








getwd()
country <- read.csv("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\Nightlights\\grid_nightlights_countries.csv")
glimpse(country)
country <- country[,c(1,11:15)]
glimpse(df_mangroves)
table(df_mangroves$gridcell_id)
names(country)[1] <- "gridcell_id"
table(country$gridcell_id)

df_mangroves <- merge(df_mangroves,country,by="gridcell_id",all.x=TRUE)
glimpse(df_mangroves)

model_1 <- (felm(log(mangrove_cover_area) ~ sst+I(sst^2)+mhw:mhw_int+
    I(mhw_int^2)+mhw:mhw_freq+
    #mhw_freq+
    log_ntl+I(log_ntl^2)
    #I((ntl)^2)#+(ntl)
    |year+sovereignt+income_grp|0|0,
    data=df_mangroves[which(df_mangroves$ntl>0),]))
summary(model_1)

    #here
    model_1 <- (felm(log(mangrove_cover_area) ~ 
    log_ntl+I(log_ntl^2)+
    sst+I(sst^2)+
    mhw:mhw_int+ I(mhw_int^2)+
    mhw:mhw_freq
    |year+gridcell_id|0|0,
    data=df_mangroves[which(df_mangroves$ntl>0),]))
    summary(model_1)

    df_estimates <- sqest(df_mangroves[which(df_mangroves$ntl>0),],model_1,"log_ntl","all")
    
    ggplot(df_estimates,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Sea Surface Temperature",y="Effect")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impact on mangrove area cover")

    
    
    model_1 <- (felm(log(mangrove_cover_area) ~ 
    sst+I(sst^2)+
    log_ntl+I(log_ntl^2)
    #mhw:mhw_int+ I(mhw_int^2)+
    #mhw:mhw_freq
    |year+gridcell_id|0|0,
    data=df_mangroves[which(df_mangroves$ntl>0),]))
    summary(model_1)

    df_estimates <- sqest(df_mangroves[which(df_mangroves$ntl>0),],model_1,"sst","all")
    
    ggplot(df_estimates,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Sea Surface Temperature",y="Effect")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impact on mangrove area cover")
























for(j in 1:length(years_str)){
    year <- years_str[j] 
    #print(year)
    df_mangrove_cover_y <- df_mangrove_cover[which(df_mangrove_cover$year==as.numeric(year)),]
    #head(df_mangrove_cover_y)


    for (i in 1:nrow(gridcell_data)) {
        # Get the gridcell ID
        print(paste0(year,"---",i))
        gridcell_id <- gridcell_data$id[i]
        
        # Get the latitude and longitude of the current gridcell
        gridcell_lat_i <- gridcell_lat[i]
        gridcell_lon_i <- gridcell_lon[i]
        
        # Find the indices of the nearest gridcell in the NetCDF file
        gridcell_lat_index <- which(abs(df_mangrove_cover_y$lat - gridcell_lat_i)<1)
        gridcell_lon_index <- which(abs(df_mangrove_cover_y$lon - gridcell_lon_i)<1)

        index_m <- gridcell_lat_index[which(gridcell_lat_index %in% gridcell_lon_index)]
        df_mangrove_cover_y$lon[index_m]
        df_mangrove_cover_y$lat[index_m]
        gridcell_lon[i]
        gridcell_lat[i]
        
        cov <- df_mangrove_cover_y$perc_cover[index_m]
        if(length(cov)>1){
            #print("more than one match")
            min_id <- which.min(abs(df_mangrove_cover_y$lat[index_m]-gridcell_lat[i])+
            abs(df_mangrove_cover_y$lon[index_m]-gridcell_lon[i]))
            cov <- cov[min_id]
            #which.min(abs(df_mangrove_cover_y$lon - gridcell_lon_i))
        }

        if(length(cov)==0){
            print("no data in mangroves")
            cov <- NA
        }

        cover1 <- data.frame(id=gridcell_id,mangrove_cover_perc = cov,year=year)
        if(i==1 & j==1){
                mangrove_cover_id <- cover1
        } else{mangrove_cover_id <- bind_rows(mangrove_cover_id,cover1)}
    }
}
























  
  # Loop through each year
  for (year in 1:nc_data$dim$year$len) {
    # Initialize a list to store the sampled values for each variable
    sampled_values <- list()
    
    # Loop through each variable
    for (variable in variable_names) {
      # Get the variable data for the current year and gridcell
      variable_data <- ncvar_get(nc_data, variable, start = c(gridcell_lon_index, gridcell_lat_index, year), count = c(1, 1, 1))
      
      # Add the sampled value to the list
      sampled_values[[variable]] <- variable_data
    }
    
    # Create a dataframe with the sampled values
    sampled_data_year <- data.frame(
      gridcell_id = gridcell_id,
      year = nc_data$dim$year$vals[year],
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


plot(raster_tile)
# Extract all values from the raster
raster_values <- raster::extract(raster_tile)

glimpse(raster_tile)
raster_tile
extract(raster(sub_folders[1]))
gmw2007 <- st_read(sub_folders[1])
plot(gmw2007)
glimpse(gmw2007)
class(gmw2007)
class(gridcell_data)
glimpse(gridcell_data)

# Create spatial index for gmw2007
gmw2007_index <- st_sindex(gmw2007)

# Initialize an empty vector to store the counts
counts <- vector("integer", length = nrow(gridcell_data))

# Loop over each gridcell and count the number of intersecting geometries
for (i in seq_along(gridcell_data)) {
  gridcell <- gridcell_data[i, ]
  bbox <- st_bbox(gridcell$geom)
  intersecting_indices <- st_intersects(gmw2007, bbox, sparse = FALSE, indices = TRUE)
  counts[i] <- length(intersecting_indices)
}



intersections <- st_intersection(gridcell_data, gmw2007)

# Count the number of intersecting geometries by gridcell
count_data <- intersections %>%
  group_by(id) %>%
  summarize(Count = n())


# Read and sample shapefiles in each sub-folder
sampled_shapefiles <- lapply(sub_folders, read_and_sample_shapefile)
sampled_shapefiles

joined_data <- st_join(gridcell_data, gmw2007, join = st_intersects)
# Global Watch












# Create an empty dataframe to store the sampled data
sampled_data <- data.frame(
  gridcell_id = integer(),
  year = integer(),
  stringsAsFactors = FALSE
)

# Loop through each gridcell
for (i in 1:nrow(gridcell_data)) {
  # Get the gridcell ID
  gridcell_id <- gridcell_data$id[i]
  
  # Get the latitude and longitude of the current gridcell
  gridcell_coords <- st_coordinates(gridcell_data)
    gridcell_lat <- gridcell_coords[, "Y"]
    gridcell_lon <- gridcell_coords[, "X"]

    for (i in 1:nrow(gridcell_data)) {
  # Get the gridcell ID
  gridcell_id <- gridcell_data$id[i]
  
  # Get the latitude and longitude of the current gridcell
  gridcell_lat_i <- gridcell_lat[i]
  gridcell_lon_i <- gridcell_lon[i]
  
  # Find the indices of the nearest gridcell in the NetCDF file
  gridcell_lat_index <- which.min(abs(nc_data$dim$lat$vals - gridcell_lat_i))
  gridcell_lon_index <- which.min(abs(nc_data$dim$lon$vals - gridcell_lon_i))
  
  # Loop through each year
  for (year in 1:nc_data$dim$year$len) {
    # Initialize a list to store the sampled values for each variable
    sampled_values <- list()
    
    # Loop through each variable
    for (variable in variable_names) {
      # Get the variable data for the current year and gridcell
      variable_data <- ncvar_get(nc_data, variable, start = c(gridcell_lon_index, gridcell_lat_index, year), count = c(1, 1, 1))
      
      # Add the sampled value to the list
      sampled_values[[variable]] <- variable_data
    }
    
    # Create a dataframe with the sampled values
    sampled_data_year <- data.frame(
      gridcell_id = gridcell_id,
      year = nc_data$dim$year$vals[year],
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
  
  # Loop through each year
  for (year in 1:nc_data$dim$time$len) {
    # Initialize a list to store the sampled values for each variable
    sampled_values <- list()
    
    # Loop through each variable
    for (variable in variable_names) {
      # Get the latitude and longitude values from the NetCDF file
      lat <- ncvar_get(nc_data, "lat")
      lon <- ncvar_get(nc_data, "lon")
      
      # Find the indices of the nearest gridcell in the NetCDF file
      gridcell_lat_index <- which.min(abs(lat - gridcell_lat))
      gridcell_lon_index <- which.min(abs(lon - gridcell_lon))
      
      # Get the variable data for the current year and gridcell
      variable_data <- ncvar_get(nc_data, variable, start = c(gridcell_lon_index, gridcell_lat_index, year), count = c(1, 1, 1))
      
      # Add the sampled value to the list
      sampled_values[[variable]] <- variable_data
    }
    
    # Create a dataframe with the sampled values
    sampled_data_year <- data.frame(
      gridcell_id = gridcell_id,
      year = nc_data$dim$time$vals[year],
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
This code read

# Create a new NetCDF file
output_nc_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/file.nc"
nc_output <- nc_create(output_nc_file,gridcell_data)

# Define dimensions for the NetCDF file
gridcell_dim <- nrow(gridcell_data)
year_dim <- length(nc_data$dim$time$vals)

# Define the dimensions in the NetCDF file
nc_dim_def(nc_output, "gridcell", gridcell_dim)
nc_dim_def(nc_output, "year", year_dim)

# Define the variables in the NetCDF file
nc_var_def(nc_output, "gridcell_id", "int", c("gridcell"))
nc_var_def(nc_output, "year", "int", c("year"))
nc_var_def(nc_output, "variable", "double", c("gridcell", "year"))

# Set the variable names in the NetCDF file
ncatt_put(nc_output, 0, "varnames", names(nc_data$var))

# Create the NetCDF file
ncvar_put(nc_output, "gridcell_id", gridcell_data$id)
ncvar_put(nc_output, "year", nc_data$dim$year$vals)
ncvar_put(nc_output, "variable", extracted_data_linear$variable, start = c(1, 1), count = c(gridcell_dim, year_dim))

# Close the NetCDF file
nc_close(nc_output)

# Print the summary of the output NetCDF file
print(nc_output)



glimpse(nc_data)

# Extract variable names from netCDF file
variable_names <- names(nc_data$var)

# Create an empty dataframe to store the extracted data
extracted_data <- data.frame(
  #gridcell_id = integer(),
  year = integer(),
  stringsAsFactors = FALSE
)

# Loop through each variable and extract the data
for (variable in variable_names) {
  # Loop through each year
  for (year in 1:nc_data$dim$time$len) {
    # Extract data for the current variable and year
    variable_data <- ncvar_get(nc_data, variable, start = c(1, 1, year), count = c(-1, -1, 1))


        # Convert the variable_data matrix to a linear vector
        linear_vector <- as.vector(t(variable_data))
        glimpse(linear_vector)

        # Create latitude and longitude vectors
        latitude <- rep(gridcell_data$latitude, ncol(variable_data))
        longitude <- rep(gridcell_data$longitude, each = nrow(variable_data))

        # Create a new dataframe with the linear vector and coordinates
        extracted_data_linear <- data.frame(
        gridcell_id = rep(gridcell_data$gridcell_id, ncol(variable_data)),
        year = rep(nc_data$dim$time$vals[year], length(linear_vector)),
        variable = linear_vector,
        latitude = latitude,
        longitude = longitude,
        stringsAsFactors = FALSE
        )

        # View the resulting dataframe
        head(extracted_data_linear)


    dim(variable_data)
    glimpse(extracted_data_variable)
    
    # Prepare the data for merging with gridcell data
    extracted_data_variable <- data.frame(
      #gridcell_id = gridcell_data$id,
      year = nc_data$dim$time$vals[year],
      variable = variable_data,
      stringsAsFactors = FALSE
    )
    
    # Merge the extracted data with the existing extracted_data dataframe
    extracted_data <- merge(extracted_data, extracted_data_variable, by = c("year"), all = TRUE)
  }
}

# Clean up - close the netCDF file
nc_close(nc_data)

# View the resulting dataframe
head(extracted_data)
