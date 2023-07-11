#install.packages("Rcpp")
library("Rcpp")
library(tidyverse)
library(sf)
library(raster)
#install.packages("landscapemetrics")
library(landscapemetrics)
library(exactextractr)
library(terra)
library(ggmap)
library(maptools)
library(ggpubr)
data(wrld_simpl)
library(scico)
pal_scico <- scico(max_patch, palette = 'batlow')

#Function
    gplot_data <- function(x, maxpixels = 50000)  {
    x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
    coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
    ## Extract values
    dat <- utils::stack(as.data.frame(raster::getValues(x))) 
    names(dat) <- c('value', 'variable')

    dat <- dplyr::as.tbl(data.frame(coords, dat))

    if (!is.null(levels(x))) {
        dat <- dplyr::left_join(dat, levels(x)[[1]], 
                                by = c("value" = "ID"))
    }
    dat
    }
#Function


grid <- st_read("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\Grid\\sampling_grid_mangroves_oned.gpkg")
class(grid)
grid
glimpse(grid)
st_bbox(grid$geom[1])

Years <- c(2015,2016,2017,2018,2019,2020)

year_i <- 1

    folder_name <- paste0("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_",Years[year_i],"_gtiff/")
    file_list<-list.files(folder_name, recursive =T, full.names = T, pattern = ".tif")
    
    r07 <- raster::raster("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_2007_gtiff//gmw_v3_2007/GMW_N00E109_2007_v3.tif")
    r20 <- raster("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_2020_gtiff//gmw_v3_2020/GMW_N00E109_2020_v3.tif")
    

    ymin <- -0.78
    ymax <- -0.75
    xmin <- 109.47
    xmax <- 109.5
    e <- extent(c(xmin, xmax, ymin, ymax))  # Replace xmin, xmax, ymin, ymax with the desired coordinates
    extent(r20)
    plot(r20)
    # Crop the raster
    r20_zoomed <- crop(r20, e)

    # Now you can plot the zoomed raster
    plot(r20_zoomed)

    bbox <- c(left = xmin, bottom = ymin, right = xmax, top =ymax)
    map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 12)
        # Convert the raster to a dataframe
    df_r20 <- as.data.frame(r20_zoomed, xy=TRUE)
    glimpse(df_r20)
    names(df_r20)[3]<-"cover"
    table(df_r20$cover)
    sum(is.na(df_r20$cover))
    
    cover_2020 <- ggmap(map) +
        geom_raster(data = df_r20, aes(x = x, y = y, fill = cover), na.rm = TRUE, hjust = 0, vjust = 0) +
        scale_fill_gradient(low = "blue", high = "blue", na.value = "transparent",guide="none") +
        coord_quickmap()+
    ggtitle("2020")

    r07_zoomed <- crop(r07, e)
    df_r07 <- as.data.frame(r07_zoomed, xy=TRUE)
    names(df_r07)[3]<-"cover"
    table(df_r07$cover)
    sum(is.na(df_r07$cover))
    
    cover_2007 <- ggmap(map) +
    geom_raster(data = df_r07, aes(x = x, y = y, fill = cover), na.rm = TRUE, hjust = 0, vjust = 0) +
    scale_fill_gradient(low = "blue", high = "blue", na.value = "transparent",guide="none") +
    coord_quickmap()+
    ggtitle("2007")

    cover_maps <- ggarrange(cover_2007,cover_2020)

    df_r20$loss <- df_r07$cover 
    df_r20$loss[df_r20$cover==1] <- NA

    cover_2020loss <- ggmap(map) +
    geom_raster(data = df_r20, aes(x = x, y = y, fill = loss), na.rm = TRUE, hjust = 0, vjust = 0) +
    scale_fill_gradient(low = "red", high = "red", na.value = "transparent",guide="none") +
    coord_quickmap()+
    ggtitle("Loss")

    ggarrange(cover_maps,cover_2020loss,nrow=2,ncol=1)    
    glimpse(df_r20)

    ## Holes analysis start
        
        df_r20$holes <- 1 
        df_r20$holes[df_r20$cover==1] <- NA 

        holes2020 <- ggmap(map) +
        geom_raster(data = df_r20, aes(x = x, y = y, fill = holes), na.rm = TRUE, hjust = 0, vjust = 0) +
        scale_fill_gradient(low = "red", high = "red", na.value = "transparent",guide="none") +
        coord_quickmap()+
        ggtitle("Holes 2020")

        df_r07$holes <- 1 
        df_r07$holes[df_r07$cover==1] <- NA 
        holes2007 <- ggmap(map) +
        geom_raster(data = df_r07, aes(x = x, y = y, fill = holes), na.rm = TRUE, hjust = 0, vjust = 0) +
        scale_fill_gradient(low = "red", high = "red", na.value = "transparent",guide="none") +
        coord_quickmap()+
        ggtitle("Holes 2007")

        df_r20$extra_holes <- df_r20$holes
        df_r20$extra_holes[df_r07$holes==1] <- NA
        extra_holes2020 <- ggmap(map) +
        geom_raster(data = df_r20, aes(x = x, y = y, fill = extra_holes), na.rm = TRUE, hjust = 0, vjust = 0) +
        scale_fill_gradient(low = "red", high = "red", na.value = "transparent",guide="none") +
        coord_quickmap()+
        ggtitle("Extra Holes")

        ggarrange(holes2020,holes2007,extra_holes2020,nrow=2,ncol=2)   
        
        patches07z <- raster::clump(r07_zoomed)
            # Getting the unique values of clumps that touch the margins
            

        patches20z <- raster::clump(r20_zoomed)
        plot(patches20z)

        glimpse(df_r20)

        df_r20_xyz <- df_r20 %>%
        dplyr::select(x = x, y = y, z = extra_holes)
        r20zextraholes_raster <- rasterFromXYZ(df_r20_xyz)
        r20zextraholes_raster_n <- raster::clump(r20zextraholes_raster)
        
        df_r20_xyz <- df_r20 %>%
        dplyr::select(x = x, y = y, z = holes)
        r20zholes_raster <- rasterFromXYZ(df_r20_xyz)
        r20zholes_raster_n <- raster::clump(r20zholes_raster)

        df_r07_xyz <- df_r07 %>%
        dplyr::select(x = x, y = y, z = holes)
        r07zholes_raster <- rasterFromXYZ(df_r07_xyz)
        r07zholes_raster_n <- raster::clump(r07zholes_raster)
        
        
        
        df_r07h <- as.data.frame(r07zholes_raster_n, xy=TRUE)
        names(df_r07h)[3]<-"holes"
        df_r20h <- as.data.frame(r20zholes_raster_n, xy=TRUE)
        names(df_r20h)[3]<-"holes"
        df_r20eh <- as.data.frame(r20zextraholes_raster_n, xy=TRUE)
        names(df_r20eh)[3]<-"holes"
        glimpse(df_r07h)
        glimpse(df_r20h)

    
        max_patch <- max(
        max(df_r07h$holes,na.rm=TRUE),
        max(df_r20h$holes,na.rm=TRUE))
         
        pal_scico <- scico(max_patch, palette = 'batlow')
        #pal_scico <- scico(dim(df_r20eh)[1], palette = 'batlow')
        df_r20eh$color <- factor(df_r20eh$holes, levels = unique(df_r20eh$holes), 
                                labels = pal_scico[(1+max_patch-max(df_r20eh$holes,na.rm=TRUE)):max_patch])

        map_eholes_20 <- ggmap(map) +
            geom_raster(data = df_r20eh[which(!is.na(df_r20eh$holes)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
            scale_fill_manual(values = levels(df_r20eh$color),guide="none") +    
            coord_quickmap()+
            ggtitle("2020 (6 extra holes)")
        map_eholes_20

        df_r20h$color <- factor(df_r20h$holes, levels = unique(df_r20h$holes), 
                                labels = pal_scico[1:(max(df_r20h$holes,na.rm=TRUE))])
        map_holes_20 <- ggmap(map) +
            geom_raster(data = df_r20h[which(!is.na(df_r20h$holes)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
            scale_fill_manual(values = levels(df_r20h$color),guide="none") +    
            coord_quickmap()+
            ggtitle("2020 (51 holes)")
        map_holes_20

        df_r07h$color <- factor(df_r07h$holes, levels = unique(df_r07h$holes), 
                                labels = pal_scico[1:(max(df_r07h$holes,na.rm=TRUE))])
        map_holes_07 <- ggmap(map) +
            geom_raster(data = df_r07h[which(!is.na(df_r07h$holes)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
            scale_fill_manual(values = levels(df_r07h$color),guide="none") +    
            coord_quickmap()+
            ggtitle("2007 (45 holes)")
        map_holes_07

        just_for_legend <- ggmap(map) +
        geom_raster(data = df_r20h[which(!is.na(df_r20h$holes)),], aes(x = x, y = y, fill = holes), na.rm = TRUE, hjust = 0, vjust = 0) +
        #scale_fill_manual(values = (df_p07$color),guide="none") +    
        scale_fill_scico(palette="batlow",direction=-1,trans="reverse") +
        coord_quickmap()+
        ggtitle("2007")
        just_for_legend
        leg <- get_legend(just_for_legend)
        leg_plot <- as_ggplot(leg)
        emptyplot <- ggplot() + theme_void()
        ggarrange(map_holes_07,map_holes_20,ncol=2,nrow=1)

        ggarrange(map_holes_07,map_holes_20,leg_plot,ncol=3,nrow=1,widths=c(5,5,1))
        ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\holes_zom_2007_2020.png")


        plot(area(r07zholes_raster_n)*r07zholes_raster_n)
        plot(area(r20zholes_raster_n)*r20zholes_raster_n)

        areas_holes <- data.frame(values(area(r20zholes_raster_n))* (values(r20zholes_raster_n)*0+1))
        areas_holes$clumps <- values(r20zholes_raster_n)
        names(areas_holes)[1] <- "area"
        area_h20 <- aggregate(area~clumps,FUN="sum",data=areas_holes)
        area_h20$year <- 2020

        areas_holes <- data.frame(values(area(r07zholes_raster_n))* (values(r07zholes_raster_n)*0+1))
        areas_holes$clumps <- values(r07zholes_raster_n)
        names(areas_holes)[1] <- "area"
        area_h07 <- aggregate(area~clumps,FUN="sum",data=areas_holes)
        area_h07$year <- 2007

        areas_holes <-  rbind(area_h07,area_h20)

       
        holes_area_bar <- ggplot(areas_holes, aes(x = factor(year), y = area, fill = clumps)) +
        geom_bar(stat = "identity", width = 1, color = "transparent") +
        scale_fill_manual(values = rev(levels(patch_counts$color)),guide="none") +
        theme_minimal() +
        #scale_y_continuous(trans="log")
        labs(title = "Patch Counts",x="Year",y="Pixels covered by mangrove")

        
        median(area_h07$area)
        median(area_h20$area)
    ## Holes analysis end

    ## Holes analysis with rasters
        ## Zoom in: Conver raster to holes raster and eliminate the patches in the borders (start)
            rec_matrix <- matrix(c(1, 1, NA,
                            NA, NA, 1), 
                            ncol = 3, byrow = TRUE)

                            
            r07z_holes <- reclassify(r07_zoomed, rec_matrix, right = NA)
            r20z_holes <- reclassify(r20_zoomed, rec_matrix, right = NA)
            
            lsm_l_np(r07z_holes)
                            ###
                                edge_clump_values07 <- unique(c(r07z_holes[1, ],   # Top row
                                                r07z_holes[nrow(r07z_holes), ],   # Bottom row
                                                r07z_holes[, 1],   # Left column
                                                r07z_holes[, ncol(r07z_holes)], na.rm=TRUE    # Right column
                                                ))
                                r07z_holes[r07z_holes %in% edge_clump_values07] <- NA
                            ###

            patches07z <- raster::clump(r07z_holes)
            patches20z <- raster::clump(r20z_holes)
            lsm_l_np(r07z_holes)
            lsm_l_np(patches07z)

            #patches07z <- lsm_l_np(r07z_holes)

            values(patches07z[1, ])
            
            edge_clump_values07 <- unique(c(patches07z[1, ],   # Top row
                              patches07z[nrow(patches07z), ],   # Bottom row
                              patches07z[, 1],   # Left column
                              patches07z[, ncol(patches07z)], na.rm=TRUE    # Right column
                             ))
            patches07z[patches07z %in% edge_clump_values07] <- NA

            edge_clump_values20 <- unique(c(patches20z[1, ],   # Top row
                              patches20z[nrow(patches20z), ],   # Bottom row
                              patches20z[, 1],   # Left column
                              patches20z[, ncol(patches20z)], na.rm=TRUE    # Right column
                             ))
            patches20z[patches20z %in% edge_clump_values20] <- NA
            holes20 <-  clump(patches20z)
            holes07 <-  clump(patches07z)
            lsm_l_np(patches07z)
            lsm_l_np(patches20z)
            
            eholes20 <- (holes20*(holes07*0))
            plot(eholes20)                
            plot(holes20)      
            plot(holes07)

            plot(area(holes07)*(holes07*0+1))
            area_07 <- data.frame(area=values(area(holes07)*(holes07*0+1)))
            area_20 <- data.frame(area=values(area(holes20)*(holes20*0+1)))
            area_07$clump <- values(holes07)
            area_20$clump <- values(holes20)
            area_dif07 <- aggregate(area~clump,data=area_07,FUN="sum")
            area_dif07$year <- 2007            
            
            area_dif07$color <- factor(area_dif07$clump, levels = unique(area_dif07$clump), 
                                labels = pal_scico[1:(dim(area_dif07$clump)[1])])

            area_dif20 <- aggregate(area~clump,data=area_20,FUN="sum")
            area_dif20$year <- 2020  
                
                max_patch <- max(area_dif20$clump,na.rm=TRUE)
                max(area_dif07$clump,na.rm=TRUE)
                
                pal_scico <- scico(max_patch, palette = 'romaO')
                #pal_scico <- scico(dim(df_r20eh)[1], palette = 'batlow')
                area_dif20$color <- factor(area_dif20$clump, levels = unique(area_dif20$clump), 
                                        labels = pal_scico[(1+max_patch-max(area_dif20$clump,na.rm=TRUE)):max_patch])
                                        #labels = pal_scico[(1:max(area_dif20$clump,na.rm=TRUE))])
                levels(factor(area_dif20$clump))
                                        
                area_dif07$color <- factor(area_dif07$clump, levels = unique(area_dif07$clump), 
                                        labels = pal_scico[(1+max_patch-max(area_dif07$clump,na.rm=TRUE)):max_patch])

            areas_holes<- rbind(area_dif20,area_dif07)
            aggregate(area~year,data=areas_holes,FUN="sum")
            0.004306102/0.004695229 #size of holes
            39.69364/39.58078 #area change

            holes_area_bar <- ggplot(areas_holes, aes(x = factor(year), y = (area), fill = factor(clump))) +
                geom_bar(stat = "identity", width = 1, color = "transparent") +
                scale_fill_manual(values =(as.character(areas_holes$color)),guide="none") +
                theme_minimal() +
                #scale_y_continuous(trans="log")
                labs(title = "",x="Year",y="Area (km^2)")
            holes_area_bar

            
            df_holes07 <- as.data.frame(holes07, xy=TRUE)
            df_holes20 <- as.data.frame(holes20, xy=TRUE)
            glimpse(df_holes07)
            
            df_holes07$color <- factor(df_holes07$clumps, levels = unique(df_holes07$clumps), 
                                        #labels = pal_scico[(1+max_patch-max(area_dif07$clump,na.rm=TRUE)):max_patch])
                                        labels = pal_scico[(1:max(area_dif07$clump,na.rm=TRUE))])
            df_holes20$color <- factor(df_holes20$clumps, levels = unique(df_holes20$clumps), 
                                        labels = pal_scico[(1+max_patch-max(area_dif20$clump,na.rm=TRUE)):max_patch])

            bbox <- c(left = min(df_holes20$x), bottom = min(df_holes20$y), right = max(df_holes20$x), top =max(df_holes20$y))
            map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 10)
    
            map_patch_07 <- ggmap(map) +
                geom_raster(data = df_holes07[which(!is.na(df_holes07$clumps)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_holes07$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2007 - 8430 holes")

            map_patch_20 <- ggmap(map) +
                geom_raster(data = df_holes20[which(!is.na(df_holes20$clumps)),], aes(x = x, y = y, fill = (color)), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_holes20$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2020 - 9218 holes (10% increase)")
            map_patch_20

            just_for_legend <- ggmap(map) +
                geom_raster(data = df_holes20[which(!is.na(df_holes20$clumps)),], aes(x = x, y = y, fill = clumps), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_scico(palette="romaO",direction=-1,trans="reverse") +
                #scale_fill_manual(values = levels(df_holes20$color)) +    
                coord_quickmap()+
                ggtitle("2020")+
                labs(fill="Number of \nnon-vegetated \nfragments")
                just_for_legend
                
                leg <- get_legend(just_for_legend)
                leg_plot <- as_ggplot(leg)

            plot_holes <- ggarrange(map_patch_07,map_patch_20,holes_area_bar,leg_plot,nrow=1,ncol=4,widths=c(5,5,3,2))

            annotate_figure(plot_holes, top = text_grob("Non-vegetated fragments within mangroves", 
               color = "black", face = "bold", size = 14))

            ggsave("Figures/holes_2007_2020.png",dpi=900)
        ## Zoom in: Conver raster to holes raster and eliminate the patches in the borders (start)
           
        ## Tile: Conver raster to holes raster and eliminate the patches in the borders (start)
            rec_matrix <- matrix(c(1, 1, NA,
                            NA, NA, 1), 
                            ncol = 3, byrow = TRUE)
            r07z_holes <- reclassify(r07, rec_matrix, right = NA)
            r20z_holes <- reclassify(r20, rec_matrix, right = NA)

            patches07z <- raster::clump(r07z_holes)
            patches20z <- raster::clump(r20z_holes)

            
            edge_clump_values07 <- unique(c(patches07z[1, ],   # Top row
                              patches07z[nrow(patches07z), ],   # Bottom row
                              patches07z[, 1],   # Left column
                              patches07z[, ncol(patches07z)], na.rm=TRUE    # Right column
                             ))
            patches07z[patches07z %in% edge_clump_values07] <- NA

            edge_clump_values20 <- unique(c(patches20z[1, ],   # Top row
                              patches20z[nrow(patches20z), ],   # Bottom row
                              patches20z[, 1],   # Left column
                              patches20z[, ncol(patches20z)], na.rm=TRUE    # Right column
                             ))
            patches20z[patches20z %in% edge_clump_values20] <- NA
            holes20 <-  clump(patches20z)
            holes07 <-  clump(patches07z)
            
            eholes20 <- (holes20*(holes07*0))
            # plot(eholes20)                
            # plot(holes20)      
            # plot(holes07)

            # plot(area(holes07)*(holes07*0+1))
            area_07 <- data.frame(area=values(area(holes07)*(holes07*0+1)))
            area_20 <- data.frame(area=values(area(holes20)*(holes20*0+1)))
            area_07$clump <- values(holes07)
            area_20$clump <- values(holes20)
            area_dif07 <- aggregate(area~clump,data=area_07,FUN="sum")
            area_dif07$year <- 2007       
            glimpse(area_dif07)     
            
            max_patch <- max(area_dif20$clump,na.rm=TRUE)
            pal_scico <- scico(max_patch, palette = 'romaO')
            area_dif07$color <- factor(area_dif07$clump, levels = unique(area_dif07$clump), 
                                labels = pal_scico[1:(max(area_dif07$clump,na.rm=TRUE))])

            area_dif20 <- aggregate(area~clump,data=area_20,FUN="sum")
            area_dif20$year <- 2020  
                
                #max_patch <- max(area_dif20$clump,na.rm=TRUE)
                #pal_scico <- scico(max_patch, palette = 'romaO')
                ##pal_scico <- scico(dim(df_r20eh)[1], palette = 'batlow')
                #area_dif20$color <- factor(area_dif20$clump, levels = unique(area_dif20$clump), 
                 #                       labels = pal_scico[(1+max_patch-max(area_dif20$clump,na.rm=TRUE)):max_patch])
                  #                      #labels = pal_scico[(1:max(area_dif20$clump,na.rm=TRUE))])
                levels(factor(area_dif20$clump))
                                        
                area_dif07$color <- factor(area_dif07$clump, levels = unique(area_dif07$clump), 
                                        labels = pal_scico[(1+max_patch-max(area_dif07$clump,na.rm=TRUE)):max_patch])

            areas_holes<- rbind(area_dif20,area_dif07)
            aggregate(area~year,data=areas_holes,FUN="mean")

            class(areas_holes$color)
            
            holes_area_bar <- ggplot(areas_holes, aes(x = factor(year), y = (area), fill = factor(clump))) +
                geom_bar(stat = "identity", width = 1, color = "transparent") +
                scale_fill_manual(values =(as.character(areas_holes$color)),guide="none") +
                theme_minimal() +
                #scale_y_continuous(trans="log")
                labs(title = "",x="Year",y="Area (km^2)")
            holes_area_bar

            
            df_holes07 <- as.data.frame(holes07, xy=TRUE)
            df_holes20 <- as.data.frame(holes20, xy=TRUE)
            glimpse(df_holes07)
            
            df_holes07$color <- factor(df_holes07$clumps, levels = unique(df_holes07$clumps), 
                                        #labels = pal_scico[(1+max_patch-max(area_dif07$clump,na.rm=TRUE)):max_patch])
                                        labels = pal_scico[(1:max(area_dif07$clump,na.rm=TRUE))])
            df_holes20$color <- factor(df_holes20$clumps, levels = unique(df_holes20$clumps), 
                                        labels = pal_scico[(1+max_patch-max(area_dif20$clump,na.rm=TRUE)):max_patch])

            map_patch_07 <- ggmap(map) +
                geom_raster(data = df_holes07[which(!is.na(df_holes07$clumps)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_holes07$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2007")

            map_patch_20 <- ggmap(map) +
                geom_raster(data = df_holes20[which(!is.na(df_holes20$clumps)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_holes20$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2020")
            
            df_holes20$color <- as.double(df_holes20$color)
            just_for_legend <- ggmap(map) +
                geom_raster(data = df_holes20[which(!is.na(df_holes20$clumps)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_scico(palette="romaO",direction=-1,trans="reverse") +
                #scale_fill_manual(values = levels(df_holes20$color)) +    
                coord_quickmap()+
                ggtitle("2020")+
                labs(fill="Number of \nnon-vegetated \nfragments")
                just_for_legend
                
                leg <- get_legend(just_for_legend)
                leg_plot <- as_ggplot(leg)

            plot_holes <- ggarrange(map_patch_07,map_patch_20,holes_area_bar,leg_plot,nrow=1,ncol=4,widths=c(5,5,3,2))

            annotate_figure(plot_holes, top = text_grob("Non-vegetated fragments within mangroves", 
               color = "black", face = "bold", size = 14))

            ggsave("Figures/holes_2007_2020.png",dpi=300)
        ## Conver raster to holes raster and eliminate the patches in the borders (start)
        
        patches07z <- raster::clump(r07_zoomed)
        

        patches20z <- raster::clump(r20_zoomed)
    ## Holes analysis with rasters
    

## Clean code
    patches07 <- raster::clump(r07) 
    df_p07 <- as.data.frame(patches07, xy=TRUE)
    names(df_p07)[3]<-"patches"
    glimpse(df_p07)
    sum(!is.na(df_p07$patches))

    

    patches20 <- raster::clump(r20) 
    df_p20 <- as.data.frame(patches20, xy=TRUE)
    names(df_p20)[3]<-"patches"
    
    sum(!is.na(df_p20$patches))/sum(!is.na(df_p07$patches))
    
    bbox <- c(left = min(df_p07$x), bottom = min(df_p07$y), right = max(df_p07$x), top =max(df_p07$y))
    map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 10)


    patch_counts07 <- as.data.frame(table(df_p07$patches))
    colnames(patch_counts07) <- c("patch", "count")

    patch_counts20 <- as.data.frame(table(df_p20$patches))
    colnames(patch_counts20) <- c("patch", "count")

    glimpse(patch_counts20)
    glimpse(patch_counts07)
    max_patch <- max(dim(patch_counts07)[1],dim(patch_counts20)[1])
    (dim(patch_counts20)[1]/dim(patch_counts07)[1])
    
    pal_scico <- scico(max_patch, palette = 'batlow')

    # Assign the palette colors to the 'patch' column
    patch_counts07$color <- factor(patch_counts07$patch, levels = unique(patch_counts07$patch), 
                                labels = pal_scico[1:(dim(patch_counts07)[1])])
    
    ggplot(patch_counts07, aes(x = "", y = count, fill = color)) +
        geom_bar(stat = "identity", width = 1, color = "transparent") +
        scale_fill_manual(values = levels(patch_counts07$color),guide="none") +
        theme_void() +
        labs(title = "Patch Counts")

    patch_counts20$color <- factor(patch_counts20$patch, levels = unique(patch_counts20$patch), 
                                labels = pal_scico[1:(dim(patch_counts20)[1])])
    
    ggplot(patch_counts20, aes(x = "", y = count, fill = color)) +
        geom_bar(stat = "identity", width = 1, color = "transparent") +
        scale_fill_manual(values = levels(patch_counts20$color),guide="none") +
        theme_void() +
        labs(title = "Patch Counts")

    glimpse(patch_counts07)
    glimpse(patch_counts20)
    patch_counts07$year <- 2007
    patch_counts20$year <- 2020
    patch_counts<-rbind(patch_counts07,patch_counts20)

    patch_area_bar <- ggplot(patch_counts, aes(x = factor(year), y = count, fill = color)) +
        geom_bar(stat = "identity", width = 1, color = "transparent") +
        scale_fill_manual(values = rev(levels(patch_counts$color)),guide="none") +
        theme_minimal() +
        #scale_y_continuous(trans="log")
        labs(title = "Patch Counts",x="Year",y="Pixels covered by mangrove")
        
        df_p07$color <- factor(df_p07$patch, levels = unique(df_p07$patch), 
                             labels = pal_scico[1:(dim(patch_counts07)[1])])

        map_patch_07 <- ggmap(map) +
            geom_raster(data = df_p07[which(!is.na(df_p07$patches)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
            scale_fill_manual(values = levels(df_p07$color),guide="none") +    
            coord_quickmap()+
            ggtitle("2007")

        df_p20$color <- factor(df_p20$patch, levels = unique(df_p20$patch), 
                             labels = pal_scico[1:(dim(patch_counts20)[1])])
                             
        map_patch_20 <- ggmap(map) +
            geom_raster(data = df_p20[which(!is.na(df_p20$patches)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
            scale_fill_manual(values = levels(df_p20$color),guide="none") +    
            coord_quickmap()+
            ggtitle("2020")
        map_patch_20

        just_for_legend <- ggmap(map) +
        geom_raster(data = df_p20[which(!is.na(df_p20$patches)),], aes(x = x, y = y, fill = patches), na.rm = TRUE, hjust = 0, vjust = 0) +
        #scale_fill_manual(values = (df_p07$color),guide="none") +    
        scale_fill_scico(palette="batlow",direction=1,trans="reverse") +
        coord_quickmap()+
        ggtitle("2007")
        just_for_legend
        leg <- get_legend(just_for_legend)
        leg_plot <- as_ggplot(leg)
        emptyplot <- ggplot() + theme_void()
        ggarrange(map_patch_07,map_patch_20,width=c(1,1))
        ggarrange(map_patch_07,map_patch_20,patch_area_bar,ncol=3,nrow=1,width=c(2,2,1))
        ggarrange(map_patch_07,map_patch_20,patch_area_bar,leg_plot,ncol=4,nrow=1)
        library(gridExtra)    
        grid.arrange(grobs=list(map_patch_07,map_patch_20,patch_area_bar,leg_plot), 
            nrow=1,ncol=4,widths=c(5,5,2,1))
        ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\patches_2007_2020.png",g)

        g <- arrangeGrob(map_patch_07,map_patch_20,patch_area_bar,leg_plot, ncol=4,nrow=1,widths=c(5,5,2,1))
        ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\patches_2007_2020.png",g)

## Clean code
    
   