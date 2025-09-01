## Slopes for Figure 1

library(ggalluvial)
    library(cowplot)   
    library(biscale)
    library(sf)
    library("rnaturalearth")
    library("rnaturalearthdata")
    library("dplyr")
    library("zoo")
    library("scico")
    library("ggplot2")
    library("ggpubr")
    library(mapproj)
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
    gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
    gridcell_data <- st_read(gpkg_file)
    gridcell_sf <- st_as_sf(gridcell_data)


    ### Slopes  (start)
    
        mcn_holes_slope <- mcn %>% filter(mangrove_area >0)%>% filter(!is.na(holes))%>% filter(year>2015)%>% 
                    group_by(gridcell_id) %>% 
                    do({
                        model <- lm(holes/mangrove_area ~ year, data = .)
                        data.frame(slope = coef(model)["year"])
                    })%>%
                    mutate(holes_pos = ifelse(slope >= 0, 1, 0))
                    
        mcn_pafrac_slope <- mcn %>% filter(!is.na(pafrac))%>% filter(year>2015)%>% 
                    group_by(gridcell_id) %>% 
                    do({
                        model <- lm(pafrac ~ year, data = .)
                        data.frame(slope = coef(model)["year"])
                    })%>%
                    mutate(pafrac_pos = ifelse(slope >= 0, 1, 0))

        mcn_holes_slope <- mcn %>% filter(mangrove_area >0)%>% filter(!is.na(holes))%>% filter(year>2015)%>% 
                    group_by(gridcell_id) %>% 
                    do({
                        model <- lm(holes/mangrove_area ~ year, data = .)
                        data.frame(slope = coef(model)["year"])
                    })%>%
                    mutate(holes_pos = ifelse(slope >= 0, 1, 0))

        mcn_np_slope <- mcn %>% filter(mangrove_area >0)  %>% filter(!is.na(np))%>% filter(year>2015)%>% 
                    group_by(gridcell_id) %>% 
                    do({
                        model <- lm(np/mangrove_area ~ year, data = .)
                        data.frame(slope = coef(model)["year"])
                    }) %>%
                    mutate(np_pos = ifelse(slope >= 0, 1, 0))

        mcn_area_slope <- mcn %>%  filter(mangrove_area >0) %>%  filter(year>2015)%>% 
                    group_by(gridcell_id) %>% 
                    do({
                        model <- lm(mangrove_area ~ year, data = .)
                        data.frame(slope_area = coef(model)["year"])
                    }) %>%
                    mutate(area_pos = ifelse(slope_area >= 0, 1, 0))
            
            summary(lm(mangrove_area ~ year, data = mcn %>% filter(gridcell_id==1)))

            all_slopes <- merge(mcn_np_slope,mcn_holes_slope,by="gridcell_id",all=F)
            all_slopes <- merge(all_slopes,mcn_pafrac_slope,by="gridcell_id",all=F)
            all_slopes <- merge(all_slopes,mcn_area_slope,by="gridcell_id",all=F)
            r5_gridcell <- mcn[mcn$year==2020,which(names(mcn) %in% c("gridcell_id","R5"))]
            all_slopes <- merge(all_slopes,r5_gridcell,by="gridcell_id",all=F)
            all_slopes <- all_slopes[,-which(names(all_slopes) %in% c("slope.x","slope.y"))]
            data_slopes <- merge(all_slopes,gridcell_sf,by.x="gridcell_id",by.y="id")
            data_slopes$geom <- st_transform(data_slopes$geom, "+proj=robin")
            
            save(data_slopes,file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\data_slopes_gridcell.Rds")
                    

            # # Create a frequency column to plot (since you have binary flags, we just sum them)
            # all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>% 
            # group_by(area_pos, np_pos, holes_pos, pafrac_pos, R5) %>%
            # summarise(Freq = n(), .groups = "drop")
            # glimpse(all_slopes_long)
            
            # all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>% 
            # group_by(area_pos, holes_pos) %>%
            # summarise(Freq = n(), .groups = "drop")

            # all_slopes_long$area_trend <- "Area Increasing"
            # all_slopes_long$area_trend[which(all_slopes_long$area_pos==0)] <- "Area Decreasing"
            # all_slopes_long$area_trend <- factor(all_slopes_long$area_trend, levels=c("Area Decreasing","Area Increasing"))

            # all_slopes_long$holes_trend <- "Gaps Increasing"
            # all_slopes_long$holes_trend[which(all_slopes_long$holes_pos==0)] <- "Gaps Decreasing"
            # all_slopes_long$holes_trend <- factor(all_slopes_long$holes_trend, levels=c("Gaps Increasing","Gaps Decreasing"))

            # all_slopes_long$Percent <- round(100*all_slopes_long$Freq/sum(all_slopes_long$Freq),1)
            # all_slopes_long$Percent <- factor(all_slopes_long$Percent,levels=c("32.6","24.4","19.3","23.8"))

            # #save all_slopes_long
            # write.csv(all_slopes_long,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\all_slopes_long.csv")
            
            # load(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\data_slopes.Rds")

    ### Slopes  (end)


    ### Yearly and Cumulative Change (start)glimpse(mcn)
        mcn <- mcn %>%
                        group_by(gridcell_id) %>%
                        arrange(year) %>%
                        mutate(
                            area_change = mangrove_area - dplyr::lag(na.locf(mangrove_area, na.rm = FALSE))) %>%
                        ungroup() %>%  
                        arrange(gridcell_id, year)
        mcn <- mcn %>%
                        mutate(
                                area_pos = ifelse(area_change >= 0, 1, 0),
                                )

        sum_area <- mcn %>% group_by(year) %>%
                                        summarise(area_change_sum = sum(area_change, na.rm = TRUE)) %>% filter(year==1996 | area_change_sum!=0) %>% as.data.frame() 
        
        # library("ggbreak")
        # bars_cumsum_area_long <- ggplot(sum_area)+
        #                 #geom_bar(aes(x=(year),y=area_change_sum,
        #                 #    fill=factor(R5)),stat="identity")+
        #                 xlab("Year")+ylab("Area Change \n(km2)")+
        #                 #labs(fill=guide_legend("Region"))+
        #                 #scale_fill_manual(values = color_vector)+
        #                 geom_hline(aes(yintercept=0),linetype="dashed")+
        #                 geom_line(aes(x=year,y=cumsum(area_change_sum)),size=1.5,color="indianred")+
                        
        #                 #geom_smooth(data=sum_area,aes(x=year,y=cumsum(area_change_sum),color=factor(color)),size=1.5)+
        #                 theme_bw() + 
        #                 scale_x_break(c(1998.8,2006))+ 
        #                 scale_x_break(c(2010.5,2015))+ 
        #                 guides(fill=guide_legend(reverse = TRUE))+ 
        #             sq_estimate_sst_area    #scale_color_manual(values = c(" " = "indianred")) +
        #                 guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
                        
        #                 bars_cumsum_area_long

        sum_area_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(area_pos)) %>%
                                        group_by(year,R5,area_pos) %>%
                                        summarise(area_change_sum = sum(area_change, na.rm = TRUE),
                                                mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                                ) %>%  as.data.frame() 
        sum_area$color <- " "
            save(sum_area,file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\sum_area.Rds")
            save(sum_area_R5_pos,file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\sum_area_R5_pos.Rds")




    ### Yearly and Cumulative Change (end)