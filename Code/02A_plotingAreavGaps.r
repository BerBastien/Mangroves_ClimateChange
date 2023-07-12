
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")

## Libraries
library(viridis)
library("ggridges")
library("lfe")
library("ggridges")
library("stargazer")
library("ggplot2")
library('scico')
library("ggpubr")
glimpse(mcn)

## Plots Mangroves (start)

    glimpse(filtered_mcn)

    
    ## Plot Area timeseries (start)
    
        
        mcn_grouped <- mcn %>%
        arrange(gridcell_id,year)  %>% group_by(gridcell_id)%>%
        filter(year %in% c(2007,2020)) %>%
        mutate(long_change =mangrove_area / dplyr::lag(mangrove_area)-1)
        mcn_grouped_area <- mcn_grouped
        
        

    ## Plot Area timeseries (end)

    ## Plot Area timeseries (start)
    
        glimpse(mcn)
        mcn_grouped <- mcn %>%
        arrange(gridcell_id,year)  %>% group_by(gridcell_id)%>%
        filter(year %in% c(2007,2020)) %>%
        mutate(long_changeholes =holes / dplyr::lag(holes)-1)
        glimpse(mcn_grouped)
        mcn_grouped_holes <- mcn_grouped
        mcn_grouped_area$long_changeholes <- mcn_grouped$long_changeholes
        

    ## Plot Area timeseries (end)

    ## Plot dimension changes in map (start)
        #@tutorial: https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
        #install.packages("biscale")
        #install.packages(c("cowplot", "sf"))
        #install.packages("biscale", dependencies = TRUE)
        library(cowplot)   
        library(biscale)
        library(sf)
        library("rnaturalearth")
        library("rnaturalearthdata")
        library(mapproj)
        
        ## Calculating Area and holes percentiles (start)
            gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
            gridcell_data <- st_read(gpkg_file)
            gridcell_sf <- st_as_sf(gridcell_data)
            #glimpse(gridcell_sf)
            names(gridcell_sf)[1] <- "gridcell_id"
            
            
            # data <- bi_class(mcn[which(mcn$year>2005 & 
            #                 is.finite(mcn$annual_holes_change) & 
            #                 is.finite(mcn$annual_area_change)),], 
            #     x = annual_area_change, 
            #     y = annual_holes_change, 
            #     style = "quantile", dim = 3)

            

            mcn_grouped_area$long_change_neg <- -mcn_grouped_area$long_change #So I plot Area Loss (not Area change)

            data <- bi_class(mcn_grouped_area[which(mcn_grouped_area$year>2010 & 
                            is.finite(mcn_grouped_area$long_change) & 
                            is.finite(mcn_grouped_area$long_changeholes)),], 
                x = long_change_neg, 
                y = long_changeholes, 
                style = "quantile", dim = 3,
                keep_factors=TRUE)
            
            data <- merge(data,gridcell_sf,by="gridcell_id")
            glimpse(data)
        ## Calculating Area and holes percentiles (start)

        ## Plot time-series for percentiles (start)
            unique(data$bi_x)
            gridcell_area_loss_high <- data$gridcell_id[which(data$bi_x==unique(data$bi_x)[1])]
            gridcell_area_loss_med <- data$gridcell_id[which(data$bi_x==unique(data$bi_x)[3])]
            gridcell_area_loss_low <- data$gridcell_id[which(data$bi_x==unique(data$bi_x)[2])]

            mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
            
            mcn$qarea <- "33 - 66%"
            mcn$qarea[which(mcn$gridcell_id %in% gridcell_area_loss_low)] <- "0 - 33%" 
            mcn$qarea[which(mcn$gridcell_id %in% gridcell_area_loss_high)] <-"66 - 100%"

            area_mcn_sum <- aggregate(mangrove_area~year+qarea,FUN="sum",data=mcn[which(mcn$year>2006),])
            area_mcn_sum_temp <- aggregate(sst~year+qarea,FUN="mean",data=mcn[which(mcn$year %in% unique(area_mcn_sum$year)),])
            area_mcn_sum$sst <- area_mcn_sum_temp$sst
            

            a07 <- area_mcn_sum[which(area_mcn_sum$year==2007),c(2,3)]
            names(a07)[2] <- "a07"
            area_mcn_sum <- merge(area_mcn_sum,a07,by="qarea",all=T)
            area_mcn_sum$area_perc <- 100*area_mcn_sum$mangrove_area/area_mcn_sum$a07

            area_mcn_sum$qarea <- factor(area_mcn_sum$qarea, levels = c("66 - 100%", "33 - 66%", "0 - 33%"))
            levels(factor(area_mcn_sum$qarea))
            
            plot_sum_area <- ggplot(area_mcn_sum)+
            geom_point(aes(x=year,y=100-area_perc,col=factor(qarea)))+
            geom_smooth(aes(x=year,y=100-area_perc,col=factor(qarea)),se=F)+
            scale_color_manual(values = c("0 - 33%"= "#028833",
                                    "33 - 66%"= "#cfe68b",
                                    "66 - 100%"= "#f27301")) + theme_bw()+
            labs(color = "Percentile") + 
            #ylab("Area loss since 2007 (%)")+
            ylab("Loss since 2007 (%)")+
            theme(plot.title = element_text(hjust = 0.5)) +
            ggtitle("Area Loss")

            plot_sum_area


            unique(data$bi_y)
            gridcell_frag_med <- data$gridcell_id[which(data$bi_y==unique(data$bi_y)[2])]
            gridcell_frag_high <- data$gridcell_id[which(data$bi_y==unique(data$bi_y)[1])]
            gridcell_frag_low <- data$gridcell_id[which(data$bi_y==unique(data$bi_y)[3])]

            
            
            glimpse(mcn)        #here 
            mcn$q_holes <- "33 - 66%"
            mcn$q_holes[which(mcn$gridcell_id %in% gridcell_frag_low)] <- "0 - 33%"
            mcn$q_holes[which(mcn$gridcell_id %in% gridcell_frag_high)] <- "66 - 100%"

            holes_mcn_sum <- aggregate(holes~year+q_holes,FUN="sum",data=mcn[which(mcn$year>2006),])
            holes_mcn_sum_temp <- aggregate(sst~year+q_holes,FUN="mean",data=mcn[which(mcn$year %in% unique(holes_mcn_sum$year)),])
            holes_mcn_sum$sst <- holes_mcn_sum_temp$sst
            
            ggplot(holes_mcn_sum)+
            geom_point(aes(x=sst,y=holes),alpha=0.2)
            

            a07 <- holes_mcn_sum[which(holes_mcn_sum$year==2007),c(2,3)]
            names(a07)[2] <- "a07"
            holes_mcn_sum <- merge(holes_mcn_sum,a07,by="q_holes",all=T)
            holes_mcn_sum$holes_perc <- 100*holes_mcn_sum$holes/holes_mcn_sum$a07

            holes_mcn_sum$q_holes <- factor(holes_mcn_sum$q_holes, levels = c("66 - 100%", "33 - 66%", "0 - 33%"))
            levels((holes_mcn_sum$q_holes))
            
            plot_sum_holes <- ggplot(holes_mcn_sum)+
            geom_point(aes(x=year,y=holes_perc-100,col=factor(q_holes)))+
            #geom_line(aes(x=year,y=holes_perc,col=factor(q_holes)))+
            geom_smooth(aes(x=year,y=holes_perc-100,col=factor(q_holes)),se=F)+
            # scale_color_manual(values = c("low" = "#028833",
            #                         "med" = "#cfe68b",
            #                         "high" = "#f27301")) + theme_bw()+
            scale_color_manual(values = c("0 - 33%" = "#028833",
                                    "33 - 66%" = "#9ac8d5",
                                    "66 - 100%" = "#5a4da5")) + theme_bw()+
            labs(color = "Percentile") + 
            ylab("Change since 2007 (%)")+
            theme(plot.title = element_text(hjust = 0.5)) +
            ggtitle("Gaps")
            plot_sum_holes
        ## Plot time-series for percentiles (end)
            
        ## Plot Mangrove Map    (start)
            # Transform the data to Robinson projection
            data$geom <- st_transform(data$geom, "+proj=robin")

            
            world <- ne_countries(scale = "medium", returnclass = "sf")
            class(world)

            world$geometry <- st_transform(world$geometry, "+proj=robin")

            custom_pal3 <- c(
            "1-1" = "#028833", # low x, low y
            "2-1" = "#cfe68b",
            "3-1" = "#f27301", # high x, low y
            "1-2" = "#9ac8d5",
            "2-2" = "#e7e5e6", # medium x, medium y
            "3-2" = "#fe9aa6",
            "1-3" = "#5a4da5", # low x, high y
            "2-3" = "#cd9bc9",
            "3-3" = "#f10480" # high x, high y
            )
            
            
            
            mangrove_map_basemap <- ggplot()+
            #geom_sf(data = sea, fill = "lightblue") +
            geom_sf(data =world, color = "black", fill = "black")+
            geom_sf(data = data, mapping = aes(fill = bi_class, geometry=geom), 
            color = NA, size = 1, show.legend = FALSE,, inherit.aes = FALSE) + 
            #+ coord_map("robinson")+
            coord_sf(xlim = c(-180*10^5, 180*10^5), ylim = c(-40*10^5, 31*10^5))+
            bi_scale_fill(
                #pal = "DkBlue2",
                pal = custom_pal3,
                            #rotate_pal=TRUE,
                            #flip_axes=TRUE, 
                            dim = 3) +
            theme_minimal()#+
            #theme(plot.background = element_rect(fill = "lightblue"))
            #labs(
             #   title = "Magrove Change"#,
                #subtitle = "Gray Pink (GrPink) Palette"
            #) +
            #bi_theme() #+ 
            #theme_void()

            mangrove_map_basemap

        

            legend <- bi_legend(pal = custom_pal3,
                    dim = 3,
                    xlab = "Area Loss ",
                    ylab = "Gaps ",
                    #rotate_pal=TRUE,
                    #flip_axes=TRUE,
                    size = 2) +
                    theme_void() +
                    theme(
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 2), 
                    axis.title.x = element_text(size=9),
                    #axis.text.x = element_blank(),
                    axis.title.y = element_text(angle = 90,size=9),
                    axis.text.y = element_blank())
                legend

                finalPlot <- ggdraw() +
                    draw_plot(mangrove_map_basemap, 0, 0, 1, 1) +
                    draw_plot(legend, x=0.14, y=.13, width = 0.2, height = 0.4)
                finalPlot

            #ggsave("Figures/Draft/Map_Change.png",dpi=2000)


            finalPlot <- ggdraw() +
                    draw_plot(mangrove_map_basemap, 0, 0, 1, 1) +
                    draw_plot(legend, x=0.14, y=.25, width = 0.2, height = 0.3)
            blank_plot <- ggplot() + theme_void()
            
            ggarrange(finalPlot,ggarrange(blank_plot,plot_sum_holes,plot_sum_area,ncol=3,widths=c(1,3,3)),nrow=2,heights=c(3,2))
            #ggsave("Figures/Draft/Fig_1_Map_Area_v_Gaps.png",dpi=600)


            mangrove_map_basemap <- ggplot()+
            #geom_sf(data = sea, fill = "lightblue") +
            geom_sf(data =world, color = "black", fill = "black")+
            geom_sf(data = data, mapping = aes(fill = bi_class, geometry=geom), 
            color = NA, show.legend = FALSE, inherit.aes = FALSE) + 
            geom_sf(data = data, aes(geometry=geom), fill = NA, color = "white", size = 0.01, show.legend = FALSE)+
            #+ coord_map("robinson")+
            coord_sf(xlim = c(-180*10^5, 180*10^5), ylim = c(-40*10^5, 31*10^5))+
            bi_scale_fill(
                #pal = "DkBlue2",
                pal = custom_pal3,
                            #rotate_pal=TRUE,
                            #flip_axes=TRUE, 
                            dim = 3) +
            theme_minimal()#+
            mangrove_map_basemap


            finalPlot <- ggdraw() +
                    draw_plot(mangrove_map_basemap, 0, 0, 1, 1) +
                    draw_plot(legend, x=0.14, y=.25, width = 0.2, height = 0.3)
            blank_plot <- ggplot() + theme_void()
            
            ggarrange(finalPlot,ggarrange(blank_plot,plot_sum_holes,plot_sum_area,ncol=3,widths=c(1.5,3,3)),nrow=2,heights=c(3,2.5))
            #ggsave("Figures/Draft/Fig_1_Map_whiteborder.png",dpi=600)

        
        ## Plot Mangrove Map    (end)

        ## Figure with distribuion of classes
            groups <- bi_class_breaks(mcn_grouped_area[which(mcn_grouped_area$year>2010 & 
                        is.finite(mcn_grouped_area$long_change_neg) & 
                        is.finite(mcn_grouped_area$long_changeholes)),], 
            x = long_change_neg, 
            y = long_changeholes, 
            #style = "quantile", dim = 3,split=T)
            style = "quantile", dim = 3,split=T)


            bi_class_breaks(mcn_grouped_area[which(mcn_grouped_area$year>2010 & 
                        is.finite(mcn_grouped_area$long_change_neg) & 
                        is.finite(mcn_grouped_area$long_changeholes)),], 
            x = long_change_neg, 
            y = long_changeholes, 
            #style = "quantile", dim = 3)
            style 
            = "quantile", dim = 3)

            groups 

            legend_with_dots <- ggplot(data,aes( x = long_change_neg, 
            y = long_changeholes))+
            annotate("rect", xmin=-groups$bi_x[2], xmax=groups$bi_x[3], ymin=-groups$bi_y[2], ymax=-groups$bi_y[3], fill=custom_pal3[1])+
            annotate("rect", xmin=groups$bi_x[3], xmax=groups$bi_x[4], ymin=-groups$bi_y[2], ymax=-groups$bi_y[3], fill=custom_pal3[2])+
            annotate("rect", xmin=groups$bi_x[4], xmax=groups$bi_x[5], ymin=-groups$bi_y[2], ymax=-groups$bi_y[3], fill=custom_pal3[3])+
            annotate("rect", xmin=-groups$bi_x[2], xmax=groups$bi_x[3], ymin=-groups$bi_y[3], ymax=groups$bi_y[4], fill=custom_pal3[4])+
            annotate("rect", xmin=groups$bi_x[3], xmax=groups$bi_x[4], ymin=-groups$bi_y[3], ymax=groups$bi_y[4], fill=custom_pal3[5])+
            annotate("rect", xmin=groups$bi_x[4], xmax=groups$bi_x[5], ymin=-groups$bi_y[3], ymax=groups$bi_y[4], fill=custom_pal3[6])+
            annotate("rect", xmin=-groups$bi_x[2], xmax=groups$bi_x[3], ymin=groups$bi_y[4], ymax=groups$bi_y[5], fill=custom_pal3[7])+
            annotate("rect", xmin=groups$bi_x[3], xmax=groups$bi_x[4], ymin=groups$bi_y[4], ymax=groups$bi_y[5], fill=custom_pal3[8])+
            annotate("rect", xmin=groups$bi_x[4], xmax=groups$bi_x[5], ymin=groups$bi_y[4], ymax=groups$bi_y[5], fill=custom_pal3[9])+
            #geom_hline(aes(yintercept=0),linetype="dashed")+
            #geom_vline(aes(xintercept=0),linetype="dashed")+
            geom_point(alpha=0.2)+
            theme_minimal()+
            coord_cartesian(
            xlim = c(-1,1),
            ylim = c(-1,1))+
            theme_void() +
            ylab(expression("Fragmentation" %->% ""))+
            xlab(expression("Area Loss" %->% ""))+
            theme(
            #legend.title = element_blank(),
            legend.text = element_text(size = 2), 
            axis.title.x = element_text(size=9),
            axis.text.x = element_blank(),
            axis.title.y = element_text(angle = 90,size=9),
            axis.text.y = element_blank())

            legend_with_dots
            


            #ggsave("Figures/Draft/Supp/Legend_with_Dots.png",dpi=1000)
        ## Figure with distribution of classes (end)
            
    ## Plot dimension changes in map (end)

    ## Plot individual study unit high fragmentation low area loss (start)
        low_arealoss_high_fragmentation <- unique(mcn$gridcell_id[which(mcn$qarea =="33 - 66%" & mcn$q_holes =="66 - 100%" )])
        low_arealoss_high_fragmentation <- unique(mcn$gridcell_id[which(mcn$qarea =="66 - 100%" & mcn$q_holes =="66 - 100%" )])
        glimpse(mcn_grouped_area)
        m2 <- mcn_grouped_area[which(mcn_grouped_area$gridcell_id %in% low_arealoss_high_fragmentation),]
        glimpse(m2)
        m2$Longitude[which(m2$long_changeholes==max(m2$long_changeholes,na.rm=TRUE) )]        
        m2$Latitude[which(m2$long_changeholes==max(m2$long_changeholes,na.rm=TRUE) )]

        m3 <- cbind(m2$Longitude,m2$Latitude,m2$mangrove_area,m2$long_changeholes)
        m3 <- m3[order(m3[,3]),]
        m3
        
        library(raster)
        r07 <- raster::raster("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_2007_gtiff//gmw_v3_2007/GMW_S20E034_2007_v3.tif")
        r20 <- raster::raster("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_2020_gtiff//gmw_v3_2020/GMW_S20E034_2020_v3.tif")
        #here
        plot(r20)

        patches07 <- raster::clump(r07) 
        df_p07 <- as.data.frame(patches07, xy=TRUE)
        names(df_p07)[3]<-"patches"
        #glimpse(df_p07)
        sum(!is.na(df_p07$patches))

        

        patches20 <- raster::clump(r20) 
        df_p20 <- as.data.frame(patches20, xy=TRUE)
        names(df_p20)[3]<-"patches"
        glimpse(df_p20[which(!is.na(df_p20$patches)),])
        glimpse(df_p07[which(!is.na(df_p07$patches)),])
        
        sum(!is.na(df_p20$patches))/sum(!is.na(df_p07$patches))
        max(df_p20$patches,na.rm=T)/max(df_p07$patches,na.rm=T)-1


        bbox <- c(left = min(df_p07$x), bottom = min(df_p07$y), right = max(df_p07$x), top =max(df_p07$y))
        map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 10)
        ggmap(map)
        #bbox_zoom <- c(left = -44.9, bottom = -3.4, right = -44.4, top =-3)
        #map <- get_stamenmap(bbox_zoom, maptype = "terrain-background", zoom = 12)


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
            scale_fill_manual(values = (levels(patch_counts$color)),guide="none") +
            theme_minimal() +
            #scale_y_continuous(trans="log")
            labs(title = "Patch Counts",x="Year",y="Pixels covered by mangrove")
        patch_area_bar            
            
            df_p07$color <- factor(df_p07$patch, levels = unique(df_p07$patch), 
                                labels = pal_scico[1:(dim(patch_counts07)[1])])

            map_patch_07 <- ggmap(map) +
                geom_raster(data = df_p07[which(!is.na(df_p07$patches)),], 
                aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_p07$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2007")+theme_map()#+
                #coord_sf(xlim = c(-44.9, -44.4), ylim = c(-3, -3.4))
            map_patch_07
            
            df_p20$color <- factor(df_p20$patch, levels = unique(df_p20$patch), 
                                labels = pal_scico[1:(dim(patch_counts20)[1])])
                                
            map_patch_20 <- ggmap(map) +
                geom_raster(data = df_p20[which(!is.na(df_p20$patches)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_p20$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2020")+
                #coord_sf(xlim = c(-44.9, -44.4), ylim = c(-3, -3.4))+
                theme_map()
            map_patch_20

            ggarrange(map_patch_07,map_patch_20,ncol=1)

            just_for_legend <- ggmap(map) +
            geom_raster(data = df_p20[which(!is.na(df_p20$patches)),], aes(x = x, y = y, fill = patches), na.rm = TRUE, hjust = 0, vjust = 0) +
            #scale_fill_manual(values = (df_p07$color),guide="none") +    
            scale_fill_scico(palette="batlow",direction=-1,trans="reverse") +
            #scale_fill_scico(palette="batlow",direction=1) +
            coord_quickmap()+
            ggtitle("2007")
            just_for_legend
            leg <- get_legend(just_for_legend)
            leg_plot <- as_ggplot(leg)
            emptyplot <- ggplot() + theme_void()
            #ggarrange(map_patch_07,map_patch_20,width=c(1,1))
            #ggarrange(map_patch_07,map_patch_20,patch_area_bar,ncol=3,nrow=1,width=c(2,2,1))
            #ggarrange(map_patch_07,map_patch_20,patch_area_bar,leg_plot,ncol=4,nrow=1)
            library(gridExtra)    
            grid.arrange(grobs=list(map_patch_07,map_patch_20,patch_area_bar,leg_plot), 
                nrow=1,ncol=4,widths=c(5,5,2,1))
            #ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft\\patches_2007_2020.png",g)

            g <- arrangeGrob(map_patch_07,map_patch_20,patch_area_bar,leg_plot, ncol=4,nrow=1,widths=c(5,5,2,1))
            ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\patches_2007_2020.png",g)

        ## Holes analysis with rasters
            ## Zoom in: Conver raster to holes raster and eliminate the patches in the borders (start)
                rec_matrix <- matrix(c(1, 1, NA,
                                NA, NA, 1), 
                                ncol = 3, byrow = TRUE)

                                
                r07z_holes <- reclassify(r07_zoomed, rec_matrix, right = NA)
                plot(r07z_holes)
                r20z_holes <- reclassify(r20_zoomed, rec_matrix, right = NA)
                library("landscapemetrics")
                
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
                plot(patches07z)
                plot(r07z_holes)
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
                    
                    pal_scico <- scico(max_patch, palette = 'lapaz')
                    #pal_scico <- scico(dim(df_r20eh)[1], palette = 'batlow')
                    area_dif20$color <- factor(area_dif20$clump, levels = unique(area_dif20$clump), 
                                            labels = pal_scico[(1+max_patch-max(area_dif20$clump,na.rm=TRUE)):max_patch])
                                            #labels = pal_scico[(1:max(area_dif20$clump,na.rm=TRUE))])
                    max(as.double(levels(factor(area_dif20$clump))))/max(as.double(levels(factor(area_dif07$clump))))
                                            
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
                    labs(title = "",x="Year",y=bquote("Area (" ~ km^2 ~ ")"))
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
        
                df_r07 <- as.data.frame(r07_zoomed, xy=TRUE)
                glimpse(df_r07)
                map_patch_07 <- ggmap(map) +
                    geom_raster(data = df_r07, aes(x = x, y = y),fill="black")+
                    geom_raster(data = df_r07[which(!is.na(df_r07$GMW_S20E034_2007_v3)),], 
                    aes(x = x, y = y), fill="gray17",na.rm = TRUE, hjust = 0, vjust = 0)+
                    geom_raster(data = df_holes07[which(!is.na(df_holes07$clumps)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                    scale_fill_manual(values = levels(df_holes07$color),guide="none") +    
                    coord_quickmap()+
                    ggtitle("2007 - 6147 Gaps")+theme_void()+
                coord_sf(xlim = c(34.6, 35), ylim = c(-20.8, -20.3))

                df_r20 <- as.data.frame(r20_zoomed, xy=TRUE)
                glimpse(df_r20)
                
                map_patch_20 <- ggmap(map) +
                    geom_raster(data = df_r20, aes(x = x, y = y),fill="black")+
                    geom_raster(data = df_r20[which(!is.na(df_r20$GMW_S20E034_2020_v3)),],aes(x = x, y = y), fill="gray17",na.rm = TRUE, hjust = 0, vjust = 0) +
                    geom_raster(data = df_holes20[which(!is.na(df_holes20$clumps)),], aes(x = x, y = y, fill = (color)), na.rm = TRUE, hjust = 0, vjust = 0) +
                    scale_fill_manual(values = levels(df_holes20$color),guide="none") +    
                    coord_quickmap()+
                    ggtitle("2020 - 7096 gaps (15% increase)")+ theme_void()+
                coord_sf(xlim = c(34.6, 35), ylim = c(-20.8, -20.3))
                map_patch_20

                just_for_legend <- ggmap(map) +
                    geom_raster(data = df_holes20[which(!is.na(df_holes20$clumps)),], aes(x = x, y = y, fill = clumps), na.rm = TRUE, hjust = 0, vjust = 0) +
                    scale_fill_scico(palette="lapaz",direction=-1,trans="reverse") +
                    #scale_fill_manual(values = levels(df_holes20$color)) +    
                    coord_quickmap()+
                    ggtitle("2020")+
                    labs(fill="Number of \nnon-vegetated \nfragments")
                    just_for_legend
                    
                    leg <- get_legend(just_for_legend)
                    leg_plot <- as_ggplot(leg)

                plot_holes <- ggarrange(map_patch_07,map_patch_20,holes_area_bar,leg_plot,nrow=1,ncol=4,widths=c(5,5,3,2))
                plot_holes
                #annotate_figure(plot_holes, top = text_grob("", 
                #color = "black", face = "bold", size = 14))

                ggsave("Figures/Draft/holes_2007_2020.png",dpi=900)
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

        ymin <- -3
        ymax <- -2
        xmin <- 133
        xmax <- 134
        e <- raster::extent(c(xmin, xmax, ymin, ymax))  # Replace xmin, xmax, ymin, ymax with the desired coordinates
        e <- raster::extent(r20)
        plot(r20)
        # Crop the raster
        r20_zoomed <- crop(r20, e)

        # Now you can plot the zoomed raster
        plot(r20_zoomed)

        #bbox <- c(left = xmin, bottom = ymin, right = xmax, top =ymax)
        bbox <- c(left = min(df_p07$x), bottom = min(df_p07$y), right = max(df_p07$x), top =max(df_p07$y))
        map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 9)
            # Convert the raster to a dataframe
        df_r20 <- as.data.frame(r20_zoomed, xy=TRUE)
        glimpse(df_r20)
        names(df_r20)[3]<-"cover"
        #table(df_r20$cover)
        sum(is.na(df_r20$cover))
        
        cover_2020 <- ggmap(map) +
            geom_raster(data = df_r20, aes(x = x, y = y, fill = cover), na.rm = TRUE, hjust = 0, vjust = 0) +
            scale_fill_gradient(low = "blue", high = "blue", na.value = "transparent",guide="none") +
            coord_quickmap()+
        ggtitle("2020")+
        coord_sf(xlim = c(34.5, 35), ylim = c(-21, -20))
        cover_2020

        r07_zoomed <- crop(r07, e)
        df_r07 <- as.data.frame(r07_zoomed, xy=TRUE)
        names(df_r07)[3]<-"cover"
        #table(df_r07$cover)
        sum(is.na(df_r07$cover))
        
        cover_2007 <- ggmap(map) +
        geom_raster(data = df_r07, aes(x = x, y = y, fill = cover), na.rm = TRUE, hjust = 0, vjust = 0) +
        scale_fill_gradient(low = "blue", high = "blue", na.value = "transparent",guide="none") +
        coord_quickmap()+
        ggtitle("2007")+
        coord_sf(xlim = c(34.5, 35), ylim = c(-21, -20))

        cover_maps <- ggarrange(cover_2007,cover_2020)

        df_r20$loss <- df_r07$cover 
        df_r20$loss[df_r20$cover==1] <- NA

        cover_2020loss <- ggmap(map) +
        geom_raster(data = df_r20, aes(x = x, y = y, fill = loss), na.rm = TRUE, hjust = 0, vjust = 0) +
        scale_fill_gradient(low = "red", high = "red", na.value = "transparent",guide="none") +
        coord_quickmap()+
        ggtitle("Loss") +
        coord_sf(xlim = c(34.5, 35), ylim = c(-21, -20))


        ggarrange(cover_maps,cover_2020loss,nrow=2,ncol=1)   
        ggarrange(cover_2007,cover_2020,cover_2020loss,nrow=1,ncol=3) 
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
    ## Plot individual study unit high fragmentation low area loss (end)

    mcn$holes_density <- mcn$holes / mcn$mangrove_area
    
    mcn_grouped <- mcn %>%
    arrange(gridcell_id,year)  %>% group_by(gridcell_id)%>%
    #filter(year %in% c(1996,2020)) %>%
    #mutate(long_change =holes_density / dplyr::lag(holes_density)-1)
    mutate(long_change =holes / dplyr::lag(holes)-1)
    #mutate(annual_holes_dens_change =holes_density / dplyr::lag(holes_density)-1) 

    #mean_holes_dens_change <- aggregate(annual_holes_dens_change~gridcell_id,FUN="mean",nr.rm=T,data=mcn_grouped)
    #qholes <- quantile(mean_holes_dens_change$annual_holes_dens_change, probs=c(0.25,0.75))
    #id_low_holes <- mean_holes_dens_change$gridcell_id[which(mean_holes_dens_change$annual_holes_dens_change < q_holes[1])]
    #id_high_holes <- mean_holes_dens_change$gridcell_id[which(mean_holes_dens_change$annual_holes_dens_change > q_holes[2])]
    


    # q_holes <- quantile(mcn_grouped$long_change[which(is.finite(mcn$holes_density))],probs = c(0.25,0.75), na.rm = TRUE)
    # id_low_holes <- mcn_grouped$gridcell_id[which(mcn_grouped$long_change < q_holes[1] & is.finite(mcn$holes_density))]
    # id_high_holes <- mcn_grouped$gridcell_id[which(mcn_grouped$long_change > q_holes[2]& is.finite(mcn$holes_density))]

    q_holes <- quantile(mcn_grouped$long_change,probs = c(0.25,0.75), na.rm = TRUE)
    id_low_holes <- mcn_grouped$gridcell_id[which(mcn_grouped$long_change < q_holes[1])]
    id_high_holes <- mcn_grouped$gridcell_id[which(mcn_grouped$long_change > q_holes[2])]

    mcn$qarea <- "med"
    mcn$qarea[which(mcn$gridcell_id %in% id_low_area)] <- "low"
    mcn$qarea[which(mcn$gridcell_id %in% id_high_area)] <- "high"

    

    mcn$qholes <- "med"
    mcn$qholes[which(mcn$gridcell_id %in% id_low_holes)] <- "low"
    mcn$qholes[which(mcn$gridcell_id %in% id_high_holes)] <- "high"
    gaps_mcn_sum <- aggregate(holes~year+qholes,FUN="mean",data=mcn,na.rm=T)

    glimpse(gaps_mcn_sum)

    gaps_mcn_sum96 <- gaps_mcn_sum[which(gaps_mcn_sum$year==1996),]
    names(gaps_mcn_sum96)[3] <- "density96"
    gaps_mcn_sum <- merge(gaps_mcn_sum,gaps_mcn_sum96,by=c("qholes"),all=T)
    gaps_mcn_sum$holes_perc <- 100*gaps_mcn_sum$holes / gaps_mcn_sum$density96
    gaps_mcn_sum$year <- gaps_mcn_sum$year.x

    plot_sum_gaps <- ggplot(gaps_mcn_sum[which(gaps_mcn_sum$qholes != "med"),])+
    geom_point(aes(x=year,y=holes_perc),alpha=0.8)+
    geom_line(aes(x=year,y=holes_perc,color=qholes),alpha=0.8)+
    #geom_smooth(aes(x=year,y=holes_perc,color=qholes),method="lm")+
    #coord_cartesian(xlim=c(2007,2020),ylim=)+
    theme_bw()
    plot_sum_gaps

    plot_sum_gaps <- ggplot(gaps_mcn_sum[which(gaps_mcn_sum$qholes != "med"),])+
    geom_point(aes(x=year,y=holes_density),alpha=0.8)+
    geom_smooth(aes(x=year,y=holes_density,color=qholes))+
    #coord_cartesian(xlim=c(2007,2020),ylim=)+
    theme_bw()
    plot_sum_gaps

    ggarrange(plot_sum_gaps,plot_sum_area,ncol=1)

    holes_mcn_sum <- aggregate(holes~year,FUN="sum",data=mcn)
    holes_mcn_sum$mangrove_area <- area_mcn_sum$mangrove_area
    glimpse(holes_mcn_sum)
    ggplot(holes_mcn_sum)+
    geom_point(aes(x=year,y=holes/mangrove_area),alpha=0.2)+
    geom_smooth(aes(x=year,y=holes/mangrove_area))+
    theme_bw()
    
    ggplot(mcn)+
    geom_point(aes(x=year,y=log(mangrove_area)),alpha=0.2)+
    geom_smooth(aes(x=year,y=log(mangrove_area)))+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    #coord_cartesian(xlim=c(1995,2020),ylim=c(2,2.5))+
    theme_bw()+
    ylab("Log Mangrove area")
    
    
    ggplot(mcn)+
    geom_line(aes(x=year,y=log(mangrove_area),group=gridcell_id),alpha=0.2)+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    #coord_cartesian(xlim=c(1995,2020))+
    theme_bw()+
    ylab("Log Mangrove area")
    ggsave("Figures/mangrove_area.png")

    man_ag <- aggregate(mangrove_area~R5+year,data=mcn,FUN="sum")
    glimpse(man_ag)
    
    

    m2015 <- man_ag[which(man_ag$year %in% c(2015)),]
    m2020 <- man_ag[which(man_ag$year %in% c(2020)),]
    m2015$m2020 <- m2020$mangrove_area
    m2015$dif <- 100*(m2015$m2020 - m2015$mangrove_area)/m2015$mangrove_area

    ggplot(man_ag[which(man_ag$year %in% c(2015,2020)),])+
    geom_histogram(aes(x=factor(year),y=mangrove_area,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Mangrove_Area")+ggtitle("Average NTL growth by region")

    
    ggplot(m2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Area change (%)")+ggtitle("Mangrove area change (2015 to 2020)")
    ggsave("Figures/Mangrove_area_change.png")



    holes_ag <- aggregate(holes~R5+year+countrycode,data=mcn,FUN="sum")
    glimpse(holes_ag)
    holes_ag <- aggregate(holes~R5+year,data=holes_ag,FUN="mean")
    holes2015 <- holes_ag[which(holes_ag$year %in% c(2015)),]
    holes2020 <- holes_ag[which(holes_ag$year %in% c(2020)),]
    holes2015$holes2020 <- holes2020$holes
    holes2015$dif <- 100*(holes2015$holes2020 - holes2015$holes)/holes2015$holes

    ggplot(holes2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Number of patches change (%)")+ggtitle("Mangrove patches change (2015 to 2020)")
    #ggsave("Figures/Mangrove_holes_change.png")

    glimpse(mcn)
    holes_ag <- aggregate(patch_size~R5+year+countrycode,data=mcn,FUN="mean")
    glimpse(holes_ag)
    holes_ag <- aggregate(patch_size~R5+year,data=holes_ag,FUN="mean")
    holes2015 <- holes_ag[which(holes_ag$year %in% c(2015)),]
    holes2020 <- holes_ag[which(holes_ag$year %in% c(2020)),]
    holes2015$patch_size2020 <- holes2020$patch_size
    holes2015$dif <- 100*(holes2015$patch_size2020 - holes2015$patch_size)/holes2015$patch_size

    ggplot(holes2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Number of patches change (%)")+ggtitle("Mangrove patches change (2015 to 2020)")
    ggsave("Figures/Mangrove_patchsize_change.png")

    ggplot(mcn)+
    geom_line(aes(x=year,y=(holes),group=gridcell_id,color=R5),alpha=0.2)+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()+
    ylab("Log Mangrove area")
    ggsave("Figures/Mangrove_holes.png")

    glimpse(mcn)
    ggplot(mcn)+
    geom_line(aes(x=year,y=log(patch_size),group=gridcell_id,color=R5),alpha=0.2)+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()+
    ylab("Log Patch size")
    ggsave("Figures/Mangrove_patchsize.png")

    holes_ag <- aggregate(patch_size~R5+year,data=mcn,FUN="mean")
    glimpse(holes_ag)
    holes2015 <- holes_ag[which(holes_ag$year %in% c(2015)),]
    holes2020 <- holes_ag[which(holes_ag$year %in% c(2020)),]
    holes2015$patch_size2020 <- holes2020$patch_size
    holes2015$dif <- 100*(holes2015$patch_size2020 - holes2015$patch_size)/holes2015$patch_size

    ggplot(holes2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Number of patches change (%)")+ggtitle("Mangrove patches change (2015 to 2020)")
    #ggsave("Figures/Mangrove_holes_change.png")


    #table(mcn$annual_area_change)
    model3 <- felm(log(mangrove_area)~year|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model3)

    change_holes <- ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=annual_holes_change,group=gridcell_id,color=continent),alpha=0.2)+
    coord_cartesian(xlim=c(2016,2020))+
    ylab("Fragmentation change")+
    theme_bw()
    
    model4 <- felm(annual_holes_change~year|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model4)

    ggarrange(change_area,change_holes,common.legend=T,legend="bottom")
    ggsave("Figures/Mangrove_change.png")

    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=(holes),group=gridcell_id,color=continent),alpha=0.2)+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()

    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=log(mangrove_area/holes),group=gridcell_id,color=continent),alpha=0.2)+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()
    
    model5 <- felm(log(holes)~sst+I(sst^2)+year+log(mangrove_area)|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model5)

    filtered_mcn$patch_area <- filtered_mcn$mangrove_area/filtered_mcn$holes


    ggplot(mcn)+
    geom_line(aes(x=(sst),y=annual_holes_change,group=gridcell_id,color=continent),alpha=0.2)+
    theme_bw()

## Plots Mangroves (start)

## Plot local GDP (start)
    ggplot(mcn)+
        geom_line(aes(x=log(Mean_GDP_50km),y=log(Population_Count_50km),group=gridcell_id,color=R5),alpha=0.2)+
        theme_bw()

    ggplot(mcn)+
        geom_point(aes(x=log(Mean_GDP_50km),y=log(Population_Count_50km),group=gridcell_id,color=R5),alpha=0.1)+
        theme_bw()

    ggplot(mcn)+
        geom_point(aes(x=log(Mean_GDP_50km),y=log(ntl),group=gridcell_id,color=R5),alpha=0.1)+
        theme_bw()

    ggplot(mcn)+
        geom_point(aes(x=log(Population_Count_50km),y=log(ntl),group=gridcell_id,color=R5),alpha=0.1)+
        theme_bw()

    ggplot(mcn)+
        geom_point(aes(x=log(Mean_GDP_50km/Population_Count_50km),y=log(ntl),group=gridcell_id,color=R5),alpha=0.1)+
        theme_bw()


## Plot local GDP (end)

## Plot Polygons on map (start)

    gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
    gridcell_data <- st_read(gpkg_file)
    gridcell_sf <- st_as_sf(gridcell_data)
    glimpse(gridcell_sf)

    plot_ntl <- ggplot() +
    geom_sf(data = gridcell_sf, aes(fill = log(X2020)),color="transparent") +
    scale_fill_viridis_c() +
    labs(fill = "Intensity") +
    ggtitle("Nigthtime ligths")+
    theme_bw()

    ggsave("Figures/NTL_map.png")



        glimpse(filtered_mcn)
        write.csv(filtered_mcn,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\filtered_mcn.csv")


    #Sanity check of fraction 
    min_frac <- mcn$gridcell_id[which(mcn$pafrac==min(mcn$pafrac,na.rm=TRUE))]
    min_frac_year <- mcn$year[which(mcn$pafrac==min(mcn$pafrac,na.rm=TRUE))]
    mcn$gridcell_id[which(mcn$pafrac==max(mcn$pafrac,na.rm=TRUE))]
    gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
    gridcell_data <- st_read(gpkg_file)
    gridcell_sf <- st_as_sf(gridcell_data)
    gridcell_sf$geom[which(gridcell_sf$id==min_frac)]


    max_frac <- mcn$gridcell_id[which(mcn$pafrac==max(mcn$pafrac,na.rm=TRUE))]
    max_frac_year <- mcn$year[which(mcn$pafrac==max(mcn$pafrac,na.rm=TRUE))]
    gridcell_sf$geom[which(gridcell_sf$id==max_frac)]

    hist(mcn$pafrac)
## Plot Polygons on map (end)


