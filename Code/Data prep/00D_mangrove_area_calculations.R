library(tidyverse)
library(sf)
library(raster)
#install.packages("landscapemetrics")
library(landscapemetrics)
library(exactextractr)
library(terra)


gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
grid <- st_read(gpkg_file)

Years <- c(2015,2016,2017,2018,2019,2020)

#Years <- c(1996,2007,2008,2009,2010)
year_i <- 1
first <- 1
for (year_i in 1:length(Years)){
    folder_name <- paste0("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_",Years[year_i],"_gtiff/")
    file_list<-list.files(folder_name, recursive =T, full.names = T, pattern = ".tif")
    for(j in 1:dim(grid)[1]){
            box_j <- st_bbox(grid$geom[j])
            if(box_j[1]<0){lon_letter <- "W"}else{
                lon_letter <- "E"
            }
            
            lon_str <- as.character(abs(box_j[1]))
            
            if(nchar(lon_str)==1){
                lon_str <- paste0("00",lon_str)
            }else if(nchar(lon_str)==2){
                lon_str <- paste0("0",lon_str)
            }
            
            matches_lon <-grepl(paste0(lon_letter,lon_str), file_list, ignore.case = TRUE)
            matches_lon <- which(matches_lon==TRUE)
            
            if(box_j[2]<0){
                lat_letter <- "S"
                }else{
                lat_letter <- "N"
                }
            
        
            lat_str <- as.character(abs(trunc(box_j[2])))
            if(nchar(lat_str)==1){
                lat_str <- paste0("0",lat_str)
            }
            
            matches_lat<-grepl(paste0(lat_letter,lat_str), file_list, ignore.case = TRUE)
            matches_lat <- which(matches_lat==TRUE)
            matches_center <- matches_lat[which(matches_lat %in% matches_lon)]
        
            ceiling <- trunc(box_j)[2]+1
            if(ceiling<0){
                lat_letter <- "S"
            }else{
                lat_letter <- "N"
            }
        
            lat_str <- as.character(abs( ceiling))
            if(nchar(lat_str)==1){
                lat_str <- paste0("0",lat_str)
            }
            
            matches_latc<-grepl(paste0(lat_letter,lat_str), file_list, ignore.case = TRUE)
            matches_latc <- which(matches_latc==TRUE)
            matches_ceiling <- matches_latc[which(matches_latc %in% matches_lon)]
        
            floor <- trunc(box_j)[2]-1
            if(floor<0){
                lat_letter <- "S"
            }else{
                lat_letter <- "N"
            }
            
            lat_str <- as.character(abs(floor))
            if(nchar(lat_str)==1){
                lat_str <- paste0("0",lat_str)
            }
            
            matches_latf<-grepl(paste0(lat_letter,lat_str), file_list, ignore.case = TRUE)
            matches_latf <- which(matches_latf==TRUE)
            matches_floor <- matches_latf[which(matches_latf %in% matches_lon)]
            
            all_matches <- c(matches_center,matches_floor,matches_ceiling)
            if(length(all_matches)==0){
                print(paste0(Years[year_i],"-",j,"- no matches"))
                next
            }
        
        for(m in 1:length(all_matches)){
            plot(grid[j,])
            plot(grid[j,1])
            r1 <- raster(file_list[all_matches[m]])
            plot(r1,add=T,col="red")
            plot(r1)

            r1
            class(grid)
            #ex_2 <- 
                
            ex_2 <- exactextractr::exact_extract(r1,st_as_sf(grid[j,1]))
            #extract(grid[j,1],r1)
            #extract(r1,grid[j,1],na.rm=TRUE)
            
            sum_ex <- sum(ex_2[[1]][,1],na.rm=TRUE)
            sum_all <- length(ex_2[[1]][,1])
            plot(area(r1))
            r1
            ex_2_area <- exact_extract(area(r1),grid[j,1])
            ex_2_area <- sum(ex_2_area[[1]][,1]*ex_2[[1]][,1],na.rm=TRUE)
            r1_terra <- rast(r1)
            plot(r1_terra)
            frac <- lsm_l_np(r1_terra)
            
            if(m==1){
                all_ex <- sum_ex 
                sum_all_all <-sum_all
                man_area <- ex_2_area
                np_all <- frac$value
            }else{
                all_ex <- all_ex + sum_ex
                sum_all_all <-sum_all+     sum_all_all
                man_area <- ex_2_area+man_area
                np_all <- c(np_all,frac$value)
            }
        }
        
        newdf <- data.frame(grid_id = grid$ID[j],year=Years[year_i],mangrove_area = man_area,sum_pixels_mangrove=all_ex,sum_all_pixels=sum_all_all,area=st_area(grid[j,]),np = sum(np_all,na.rm=TRUE))
        if(first==1){
            mangrove_cover <- newdf
            first <- 0
        }else{
            mangrove_cover <- bind_rows(mangrove_cover,newdf)
        }
        print(paste0(Years[year_i],"-",j))
    }
}

write.csv(mangrove_cover,"mangrove_cover_df_3.csv")

glimpse(mangrove_cover)

ggplot(mangrove_cover)+
    geom_point(aes(x=year,y=mangrove_area))

    library(terra)
        
# Calculate landscape level metrics
metrics <- lsm_landscape(tile_m, level = "landscape")

# Print the metrics
print(metrics)

# Convert it to a terra object
mangrove_terra <- rast(tile_m )

# Now you can use landscapemetrics
check_landscape(mangrove_terra)
metrics <-landscapemetrics::calculate_lsm(mangrove_terra, level = "landscape")
print(metrics)

frac <- lsm_l_np(landscape, directions = 8, verbose = TRUE)
frac$value


setwd("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2007_gtiff/")
tile_m <- raster("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2007_gtiff/gmw_v3_2007/GMW_N00E109_2007_v3.tif")
tile_m1 <- raster("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2007_gtiff/gmw_v3_2007/GMW_S01E109_2007_v3.tif")
tile_m2 <- raster("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2007_gtiff/gmw_v3_2007/GMW_S01E110_2007_v3.tif")
tile_m3 <- raster("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2007_gtiff/gmw_v3_2007/GMW_S01E108_2007_v3.tif")
grid_N<- grid[which(abs(st_bbox(grid)[2])<2),]
grid_N
glimpse(grid_N)
st_bbox(grid_N)
plot(grid_N)

# Merge the extents
ext <- extent(merge(tile_m, tile_m1,tile_m2,tile_m3))

# Plot an empty plot with the combined extent
plot(ext, xlim = c(ext@xmin, ext@xmax), ylim = c(ext@ymin, ext@ymax), xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')

# Add the rasters
plot(tile_m1, add=TRUE,col="black")
plot(tile_m, add=TRUE)
plot(tile_m2,add=TRUE,col="white")
plot(tile_m3,add=TRUE,col="green")
plot(grid,add=T)


file_list<-list.files("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2007_gtiff/", recursive =T, full.names = T, pattern = ".tif")
file_list


# Create a list of raster objects
raster_list <- lapply(file_list[1:5], raster)

# Mosaic all rasters in the list
mosaic_raster <- do.call(mosaic, c(raster_list, fun = mean))  # change 'fun' as needed
plot(mosaic_raster)


for(i in 1:length(file_list)){
    tile1 <- raster(file_list[i])
    if(i>1){
        tiles <- merge(tiles,tile1)
    }else{tiles <- tile1}
    print(i)
}
plot(tiles)



res(tile_m)
prod(res(tile_m))
num_cells <- sum(tile_m[] > 0, na.rm = TRUE)  # na.rm = TRUE to exclude NA values
num_cells


tile_values <- values(tile_m)
hist(tile_values)
max(tile_values,na.rm=TRUE)
min(tile_values,na.rm=TRUE)


tile_m2 <- raster("/home/aburtolab/R/mangroves/gmw_rasters/GMW-all_v3.0/gmw_v3_2020_gtiff/gmw_v3_2020/GMW_N00E109_2020_v3.tif")
plot(tile_m)
plot(tile_m2)
dif <- tile_m2
dif[tile_m > 0] <- NA
plot(dif,col='red')

num_cells <- sum(tile_m2[] > 0, na.rm = TRUE)  # na.rm = TRUE to exclude NA values
num_cells

area(dif)
sum(values(area(dif[dif > 0])))
sum(values(area(tile_m)))

grid <- st_read("/home/aburtolab/R/mangroves/shp/sampling_grid_mangroves_oned.gpkg")
plot(grid,add=T)
plot(tile_m,add=TRUE)
plot(tile_m2,add=T)
dif_tile <- tile_m - tile_m2
plot(dif_tile)
file_list<-list.files("01_GMW_001_GlobalMangroveWatch/", recursive =T, full.names = T, pattern = ".shp")

y<-st_read(file_list[1])

glimpse(y)
glimpse(grid)
class(y)
class(grid)

# Load required packages
library(sf)
library(raster)
library(exactextractr)

# Ensure 'y' and 'grid' have the same CRS
grid <- st_transform(grid, st_crs(y))

# Convert 'y' to a SpatialPolygonsDataFrame
y_sp <- as(y, "Spatial")

# Define the raster's extent and resolution based on 'y'
r <- raster(extent(y_sp), res=1)  # Adjust the resolution value as needed

# Rasterize 'y' polygons
r <- raster::rasterize(y_sp, r, field="PXLVAL")

# Count number of pixels within each polygon in 'grid'
counts <- exact_extract(r, grid, 'count')

# Add counts to 'grid'
grid$count <- counts

mangrove_intersect <- function(x) {
    y <- st_read(x)
    st_intersection(y,grid)
    gc()
}


mangrove_intersect(file_list[1])


dat  <- lapply(file_list, mangrove_intersect)

