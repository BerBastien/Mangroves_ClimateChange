
    gridcell_data <- st_read(gpkg_file)
    gridcell_sf <- st_as_sf(gridcell_data)

    #hist(log(mcn$mean_mangrove_area[which(mcn$year==2017)]))
    #hist(log(mcn$mangrove_area[which(mcn$year==2017)]))
    #quantile(log(mcn$mangrove_area[which(mcn$year==2017& mcn$mangrove_area>0)]),na.rm=TRUE,c(0.2))
    #quantile((mcn$mean_mangrove_area[which(mcn$year==2017 & is.finite(mcn$mean_mangrove_area))]),na.rm=TRUE)
    mcn_f <- mcn %>% filter(mean_mangrove_area > 0 & year>2014)





    ggplot(mcn_f,aes(x=year,y=sum_pixels_mangrove))+geom_point()
    mcn <- mcn_f
    glimpse(mcn)

    

    # Create a subset of the data frame containing only the desired columns
    subset_mcn <- mcn[,c("b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", "b36_37C")]
    # Compute the sum, accounting for NA values
    mcn$hot_bin <- rowSums(subset_mcn, na.rm = TRUE)
    subset_mcn <- mcn[,c("b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", "b36_37C")]
    # Compute the sum, accounting for NA values
    mcn$hot_bin <- rowSums(subset_mcn, na.rm = TRUE)
    hist(mcn$hot_bin)
    variable_names <-  c("mangrove_area", "sst_hottest","sst","preci","temp","gap_density","logPop","logSal","logGDPpc_country","logGDPpc","hot_bin")
    long_df <- long_dif(mcn, variable_names)
    glimpse(long_df)

    df_f <- long_df %>% filter(is.finite(logPop) &
                                is.finite(logGDPpc)&
                                is.finite(logGDPpc_country) &
                                ntl>0 &
                                is.finite(logSal)&
                                change_mangrove_area < quantile(long_df$change_mangrove_area,0.99,na.rm=TRUE)&
                                change_mangrove_area > quantile(long_df$change_mangrove_area,0.01,na.rm=TRUE))

    long_model <- felm(change_mangrove_area ~   #sst_hottest + I(sst_hottest^2)+
                                                preci + I(preci^2)+
                                                logPop + I(logPop^2)+
                                                logGDPpc + I(logGDPpc^2) +
                                                logGDPpc_country + I(logGDPpc_country^2) +
                                                I(logSal) +
                                                #log(ntl) + I(log(ntl)^2)+
                                                sst + I(sst^2) +
                                                #mhw_int + mhw_freq #
                                                hot_bin + log(mangrove_area) + gap_density# + log(ntl)
                                                #change_sst +   
                                                #change_preci + I(change_preci^2)+
                                                #change_temp + 
                                                #change_logSal+
                                                #temp:change_sst+
                                                #change_logPop+
                                                #change_logGDPpc+
                                                #change_logGDPpc_country
                                            # ,data=df_f)   
                                            |0|0|0,data=df_f)
    summary(long_model)

    model <- long_model
            data <- df_f
            filename <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Area_Model\\Experiments\\LongDiff\\logmangrovearea.png"
            variable <- df_f$change_mangrove_area
            #variable <- subset_mcn1$annual_area_change
            #model_diagnostics(model, data, variable)
            adjust_and_plot(model, data, variable, filename,save=FALSE)
            adjust_and_plot_long(model, data, variable, filename,save=F)



    glimpse(change)            
    ggplot(change,aes(x=percent_change_sst_hottest,y=percent_change_area))+geom_point()


    world <- ne_countries(scale = "medium", returnclass = "sf")
        world$geometry <- st_transform(world$geometry, "+proj=robin")
            
       data_slopes <- merge(df_f,gridcell_sf,by.x="gridcell_id",by.y="id")
    data_slopes$geom <- st_transform(data_slopes$geom, "+proj=robin")
    
    library(scales)
        mangrove_map_basemap <- ggplot() +
            geom_sf(data = world, color = "black", fill = "black") +
            geom_sf(data = data_slopes, mapping = aes(color = change_mangrove_area, fill = change_mangrove_area, geometry = geom), size = 15) + 
            coord_sf(xlim = c(-180*10^5, 180*10^5), ylim = c(-40*10^5, 31*10^5)) +
            theme_minimal() +
            scale_color_scico(palette = "vikO", midpoint = 0, direction = -1, limits = c(-10, 10), oob = squish) +
            scale_fill_scico(palette = "vikO", midpoint = 0, direction = -1, limits = c(-10, 10), oob = squish)

        mangrove_map_basemap









    variable_names <-  c("mangrove_area", "sst_hottest","sst","preci","temp","gap_density","logPop","logSal","logGDPpc_country","logGDPpc","hot_bin","ntl")
    long_df <- long_dif_delta(mcn, variable_names)
    glimpse(long_df)

    df_f <- long_df %>% filter(is.finite(logPop) &
                                is.finite(logGDPpc)&
                                is.finite(logGDPpc_country) &
                                ntl>0 &
                                is.finite(logSal)&
                                change_mangrove_area < quantile(long_df$change_mangrove_area,0.99,na.rm=TRUE)&
                                change_mangrove_area > quantile(long_df$change_mangrove_area,0.01,na.rm=TRUE))

    long_model <- felm(change_mangrove_area ~   #sst_hottest + I(sst_hottest^2)+
                                                #change_preci + I(preci^2)+
                                                #logPop + I(logPop^2)+
                                                #logGDPpc + I(logGDPpc^2) +
                                                #logGDPpc_country + I(logGDPpc_country^2) +
                                                #I(logSal) +
                                                #log(ntl) + I(log(ntl)^2)+
                                                #sst + I(sst^2) +
                                                #mhw_int + mhw_freq #
                                                #hot_bin + log(mangrove_area) + gap_density# + log(ntl)
                                                #change_sst:hot_month +   
                                                change_preci + #I(change_preci^2)+
                                                change_temp:hot_month + 
                                                change_logSal+
                                                #temp:change_sst+
                                                change_logPop:income+
                                                #change_logGDPpc:income+
                                                change_logGDPpc_country:income
                                            # ,data=df_f)   
                                            |0|0|0,data=df_f)
    
    summary(long_model)
    glimpse(df_f)







































    mcn_holes_slope <- mcn %>% filter(mangrove_area >0)%>% filter(!is.na(gap_density))%>%  filter(year>2014)%>% 
                group_by(gridcell_id) %>% 
                do({
                    model <- lm(gap_density ~ year, data = .)
                    data.frame(slope_gap = coef(model)["year"])
                })%>%
                mutate(holes_pos = ifelse(slope_gap >= 0, 1, 0))
                
    mcn_pafrac_slope <- mcn %>% filter(!is.na(pafrac))%>%  filter(year>2014)%>% 
                group_by(gridcell_id) %>% 
                do({
                    model <- lm(pafrac ~ year, data = .)
                    data.frame(slope_pafrac = coef(model)["year"])
                })%>%
                mutate(pafrac_pos = ifelse(slope_pafrac >= 0, 1, 0))

    # mcn_holes_slope <- mcn %>% filter(mangrove_area >0)%>% filter(!is.na(holes))%>%  filter(year>2014)%>% 
    #             group_by(gridcell_id) %>% 
    #             do({
    #                 model <- lm(holes/mangrove_area ~ year, data = .)
    #                 data.frame(slope_holes = coef(model)["year"])
    #             })%>%
    #             mutate(holes_pos = ifelse(slope_holes >= 0, 1, 0))

    mcn_np_slope <- mcn %>% filter(mangrove_area >0)  %>% filter(!is.na(np))%>%  filter(year>2014)%>% 
                group_by(gridcell_id) %>% 
                do({
                    model <- lm(np/mangrove_area ~ year, data = .)
                    data.frame(slope_np = coef(model)["year"])
                }) %>%
                mutate(np_pos = ifelse(slope_np >= 0, 1, 0))

    mcn_area_slope <- mcn %>%  filter(mangrove_area >0) %>%  filter(year>2014)%>% 
                group_by(gridcell_id) %>% 
                do({
                    model <- lm(log(mangrove_area) ~ year, data = .)
                    data.frame(slope_area = coef(model)["year"])
                }) %>%
                mutate(area_pos = ifelse(slope_area >= 0, 1, 0))

        all_slopes <- merge(mcn_np_slope,mcn_holes_slope,by="gridcell_id",all=F)
        all_slopes <- merge(all_slopes,mcn_pafrac_slope,by="gridcell_id",all=F)
        all_slopes <- merge(all_slopes,mcn_area_slope,by="gridcell_id",all=F)
        r5_gridcell <- mcn[mcn$year==2020,which(names(mcn) %in% c("gridcell_id","R5"))]
        all_slopes <- merge(all_slopes,r5_gridcell,by="gridcell_id",all=F)
        #all_slopes <- all_slopes[,-which(names(all_slopes) %in% c("slope.x","slope.y"))]
        glimpse(all_slopes)

        ggplot(all_slopes,aes(x=slope_np,slope_area,color=R5))+geom_point()+scale_color_scico_d()
                

        # Create a frequency column to plot (since you have binary flags, we just sum them)
        all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>%
        group_by(area_pos, np_pos, holes_pos, pafrac_pos, R5) %>%
        summarise(Freq = n(), .groups = "drop")
        
        all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>% 
        group_by(area_pos, holes_pos) %>%
        summarise(Freq = n(), .groups = "drop")

        all_slopes_long$area_trend <- "Area Increasing"
        all_slopes_long$area_trend[which(all_slopes_long$area_pos==0)] <- "Area Decreasing"
        all_slopes_long$area_trend <- factor(all_slopes_long$area_trend, levels=c("Area Decreasing","Area Increasing"))

        all_slopes_long$holes_trend <- "Gaps Increasing"
        all_slopes_long$holes_trend[which(all_slopes_long$holes_pos==0)] <- "Gaps Decreasing"
        all_slopes_long$holes_trend <- factor(all_slopes_long$holes_trend, levels=c("Gaps Increasing","Gaps Decreasing"))

        all_slopes_long$Percent <- round(100*all_slopes_long$Freq/sum(all_slopes_long$Freq),1)
        all_slopes_long$Percent <- factor(all_slopes_long$Percent,levels=c("32.6","24.4","19.3","23.8"))

        #save all_slopes_long
        #write.csv(all_slopes_long,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\all_slopes_long.csv")

        
        

                #blank_gg <- ggplot()+theme_void()
                #ggarrange(ggarrange(bars_2_trends_area,bars_2_trends,ncol=1,common.legend=TRUE,legend="bottom"),
                #ggarrange(blank_gg,alluvium_interaction+theme(legend.position="bottom"),blank_gg,heights=c(1,3,1),ncol=1),ncol=2,widths=c(4,2.7))
                #ggsave("Figures/Draft2/Trends_Sankey_notrends.png",dpi=600)

            
                #ggarrange(ggarrange(bars_2_trends_area_perc,bars_2_trends_perc,ncol=1,common.legend=TRUE,legend="bottom"),
                 #   ggarrange(blank_gg,alluvium_interaction+theme(legend.position="bottom"),blank_gg,heights=c(1,3,1),ncol=1),ncol=2,widths=c(4,2.7))
                #ggsave("Figures/Draft2/Trends_Sankey_Percent.png",dpi=600)

            ## Alluvium

    
    all_slopes <- merge(mcn_area_slope,mcn_holes_slope,by="gridcell_id",all=F)
    all_slopes$bi_class <- factor(paste0("area_trend",all_slopes$area_pos,"_holes_trend",all_slopes$holes_pos))
    
    glimpse(all_slopes)
    glimpse(all_slopes)
    glimpse(gridcell_sf)
    data_slopes <- merge(all_slopes,gridcell_sf,by.x="gridcell_id",by.y="id")
    data_slopes$geom <- st_transform(data_slopes$geom, "+proj=robin")
    
    save(data_slopes,file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\data_slopes.Rds")




#####




                mcn$holes_density <- mcn$holes / mcn$mangrove_area            
                mcn$np_density <- mcn$np / mcn$mangrove_area
                mcn <- mcn %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(holes_change = holes - dplyr::lag(na.locf(holes, na.rm = FALSE)),
                        area_change = mangrove_area - dplyr::lag(na.locf(mangrove_area, na.rm = FALSE)),
                        holes_d_change = holes_density - dplyr::lag(na.locf(holes_density, na.rm = FALSE)),
                        patch_size_change = patch_size - dplyr::lag(na.locf(patch_size, na.rm = FALSE)) ,
                        np_change = np - dplyr::lag(na.locf(np, na.rm = FALSE)) ,
                        pafrac_change = pafrac - dplyr::lag(na.locf(pafrac, na.rm = FALSE))  ,
                        np_d_change = np_density - dplyr::lag(na.locf(np_density, na.rm = FALSE))) %>%
                    ungroup() %>%  
                    arrange(gridcell_id, year)
                mcn <- mcn %>%
                    mutate(holes_pos = ifelse(holes_change >= 0, 1, 0),
                            area_pos = ifelse(area_change >= 0, 1, 0),
                            holes_d_pos = ifelse(holes_d_change >= 0, 1, 0),
                            patch_size_pos = ifelse(patch_size_change >= 0, 1, 0),
                            np_pos = ifelse(np_change >= 0, 1, 0),
                            pafrac_pos = ifelse(pafrac_change >= 0, 1, 0),
                            np_d_pos = ifelse(np_d_change >= 0, 1, 0))

                my <- aggregate(mangrove_area~year,FUN="mean",data=mcn)
                
                weighted_means_holes_np_pafrac <- mcn %>%
                                    group_by(year) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(holes_weighted_mean = sum(holes * weight, na.rm = TRUE),
                                              np_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              patch_size_weighted_mean = sum(patch_size * weight, na.rm = TRUE),
                                              pafrac_weighted_mean = sum(pafrac * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)) %>% 
                                              as.data.frame() %>%
                                    mutate(holes_perkm2 = holes_weighted_mean/mangrove_area_mean,
                                           np_perkm2 =  np_weighted_mean /mangrove_area_mean) %>%
                                           filter(year>1995) %>%
                                           mutate(change_holes_perkm2_wrt1996 = holes_perkm2 - first(holes_perkm2),
                                                change_np_perkm2_wrt1996 = np_perkm2 - first(np_perkm2),
                                                change_pafrac_wrt1996 = pafrac_weighted_mean - first(pafrac_weighted_mean)  ) %>%
                                                filter(!is.na(np_perkm2))
                                
                #write.csv(weighted_means_holes_np_pafrac,"Data/output/weighted_means_h_new.csv")

                weighted_means_h_R5 <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>%
                                    group_by(year,R5) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(holes_weighted_mean = sum(holes * weight, na.rm = TRUE),
                                              np_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              pafrac_weighted_mean = sum(pafrac * weight, na.rm = TRUE),
                                                annualchange_holes_weighted_mean = sum(holes_change * weight, na.rm = TRUE),
                                                annualchange_np_weighted_mean = sum(np_change * weight, na.rm = TRUE),
                                                pafrac_annualchange_weighted_mean = sum(pafrac_change * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)) %>% 
                                              #as.data.frame() %>%
                                    #arrange(year) %>% group_by(R5) %>% filter(!is.na(mangrove_area_mean)) %>%
                                    #mutate(annualchange_holes_weighted_mean = holes_weighted_mean - dplyr::lag(na.locf(holes_weighted_mean, na.rm = FALSE) ),
                                    #        annualchange_np_weighted_mean = np_weighted_mean - dplyr::lag(na.locf(np_weighted_mean, na.rm = FALSE) )) %>% 
                                    as.data.frame() %>%
                                    mutate(holes_perkm2 = holes_weighted_mean/mangrove_area_mean,
                                           np_perkm2 =  np_weighted_mean /mangrove_area_mean) %>%
                                           filter(year>1995) %>% group_by(R5) %>%
                                           mutate(change_holes_perkm2_wrt1996 = holes_perkm2 - first(holes_perkm2),
                                                change_np_perkm2_wrt1996 = np_perkm2 - first(np_perkm2),
                                                change_pafrac_wrt1996 = pafrac_weighted_mean - first(pafrac_weighted_mean) ) %>%
                                                as.data.frame() %>%
                                            filter(!is.na(np_perkm2))
                
                # ggplot(weighted_means_h_R5,aes(x=year,y=(annualchange_holes_weighted_mean)))+geom_line(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=annualchange_np_weighted_mean,y=(annualchange_holes_weighted_mean)))+geom_point(aes(color=R5,size=mangrove_area_mean))
                # ggplot(weighted_means_h_R5,aes(x=year,y=(change_holes_perkm2_wrt1996)))+geom_line(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=year,y=(holes_perkm2)))+geom_line(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=year,y=(pafrac_weighted_mean)))+geom_point(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=year,y=(np_perkm2)))+geom_line(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=year,y=(change_np_perkm2_wrt1996)))+geom_line(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=year,y=(change_pafrac_wrt1996)))+geom_line(aes(color=R5))
                # ggplot(weighted_means_h_R5,aes(x=change_holes_perkm2_wrt1996,y=(change_np_perkm2_wrt1996)))+geom_line(aes(color=R5))
                
                sum_area_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(area_pos)) %>%
                                    group_by(year,R5,area_pos) %>%
                                    summarise(area_change_sum = sum(area_change, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>%  as.data.frame() 
                sum_area <- mcn %>% group_by(year) %>%
                                    summarise(area_change_sum = sum(area_change, na.rm = TRUE)) %>% filter(year==1996 | area_change_sum!=0) %>% as.data.frame() 
                years_i <- c(2016,2017,2018,2019,2020)
                sum_area_change <- aggregate(area_change_sum~year,FUN="sum",data=sum_area_R5_pos)
                sum_area_change_pos <- aggregate(area_change_sum~year+area_pos,FUN="sum",data=sum_area_R5_pos)
                msum <- mcn %>% group_by(year) %>% summarise(total_area=sum(mangrove_area,na.rm=TRUE))
                #msum <- mcn %>% group_by(year) %>% summarise(mean_holes_perha=mean(holes_density,na.rm=TRUE))
                sum_area_change <- merge(sum_area_change,msum,by="year",all=F)
                sum_area_R5_pos <- merge(sum_area_R5_pos,msum,by="year",all=F)


                weighted_means_h_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>%
                                    group_by(year,R5,holes_d_pos) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(holes_weighted_mean = sum(holes * weight, na.rm = TRUE),
                                                holes_annualchange_weighted_mean = sum(holes_change * weight, na.rm = TRUE),
                                                holes_d_annualchange_weighted_mean = sum(holes_d_change * weight, na.rm = TRUE),
                                                salinity_annualchange_weighted_mean = sum(salinity_change * weight, na.rm = TRUE),
                                              #np_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>% 
                                              as.data.frame() %>%
                                    mutate(holes_perkm2 = holes_weighted_mean/mangrove_area_mean,
                                           holes_annualchange_perkm2 = holes_annualchange_weighted_mean/mangrove_area_mean
                                           ) %>%
                                           filter(year>1995) %>% group_by(R5) %>%
                                           mutate(change_holes_perkm2_wrt1996 = holes_perkm2 - first(holes_perkm2)) %>%
                                                as.data.frame() %>%
                                            filter(!is.na(holes_perkm2))
                
                sum_h_change <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>%
                                    group_by(year) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(annualchange_holes_weighted_mean = sum(holes_change * weight, na.rm = TRUE),
                                                mangrove_area = mean(mangrove_area, na.rm = TRUE),
                                               holes_weighted_mean = sum(holes * weight, na.rm = TRUE) ) %>%  
                                    mutate(holes_annualchange_perkm2 = annualchange_holes_weighted_mean /mangrove_area,
                                     holes_perkm2 = holes_weighted_mean /mangrove_area) %>%
                                    as.data.frame() %>%filter(year==1996 | annualchange_holes_weighted_mean!=0)
                sum_h_change$color <-" "

                years_i <- c(2016,2017,2018,2019,2020)
                sum_h_change <- aggregate(holes_annualchange_perkm2~year,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change_pos <- aggregate(holes_annualchange_perkm2~year+holes_d_pos,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change$color <-" "
                mcn$holes_density <- mcn$holes / mcn$mangrove_area
                msum <- mcn %>% group_by(year) %>% filter(is.finite(holes_density)) %>% summarise(mean_holes_perha=median(holes_density,na.rm=TRUE))
                sum_h_change <- merge(sum_h_change,msum,by="year",all=F)
                sum_h_change$perc <- sum_h_change$holes_annualchange_perkm2/sum_h_change$mean_holes_perha
                weighted_means_h_R5_pos <- merge(weighted_means_h_R5_pos,msum,by="year",all=F)


                
        #write.csv(weighted_means_h_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\weighted_means_h_R5_pos.csv")  
        #write.csv(sum_h_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_h_change.csv")  




                
                


