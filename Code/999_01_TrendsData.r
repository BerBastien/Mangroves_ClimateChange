libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj")

lapply(libraries, library, character.only = TRUE)

    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_aug2023.csv")
    mcn$holes_size <- mcn$h_area / (mcn$holes+1)
    #mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
    gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
    gridcell_data <- st_read(gpkg_file)
    gridcell_sf <- st_as_sf(gridcell_data)

        # A generalized function to compute slope and its positive/negative indication
        compute_slope <- function(df, formula, column_name) {
        df %>% 
            group_by(gridcell_id) %>% 
            do({
            model <- lm(formula, data = .)
            data.frame(slope = coef(model)["year"])
            }) %>%
            mutate(!!column_name := ifelse(slope >= 0, 1, 0))
        }

        # Filter conditions
        common_filters <- mcn %>% filter(mangrove_area > 0, year > 2015)

        # Computing slopes for different conditions
        mcn_holes_slope <- common_filters %>% 
        filter(!is.na(holes)) %>% 
        compute_slope(holes/mangrove_area ~ year, "holes_pos")

        mcn_pafrac_slope <- mcn %>% 
        filter(!is.na(pafrac), year > 2015) %>% 
        compute_slope(pafrac ~ year, "pafrac_pos")

        mcn_holes_size_slope <- mcn %>% 
        filter(is.finite(holes_size), year > 2015) %>% 
        compute_slope(holes_size ~ year, "holesize_pos")

        mcn_np_slope <- common_filters %>% 
        filter(!is.na(np)) %>% 
        compute_slope(np/mangrove_area ~ year, "np_pos")

        mcn_area_slope <- common_filters %>% 
        compute_slope(mangrove_area ~ year, "area_pos")

        # List of data frames to be merged
        dfs_to_merge <- list(mcn_np_slope, mcn_holes_slope, mcn_pafrac_slope, mcn_area_slope)

        # Using Reduce to merge data frames on gridcell_id
        all_slopes <- Reduce(function(df1, df2) merge(df1, df2, by = "gridcell_id", all = F), dfs_to_merge)

        # Adding r5_gridcell
        r5_gridcell <- mcn[mcn$year == 2020, c("gridcell_id", "R5")]
        all_slopes <- merge(all_slopes, r5_gridcell, by = "gridcell_id", all = F)

        # Removing unwanted columns
        all_slopes <- all_slopes[ , !names(all_slopes) %in% c("slope.x", "slope.y")]

        
        
                

        # Create a frequency column to plot
        all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>% 
        group_by(area_pos, holes_pos) %>%
        summarise(Freq = n(), .groups = "drop")

        all_slopes_long <- all_slopes_long %>%
        mutate(
            area_trend = ifelse(area_pos == 1, "Area Increasing", "Area Decreasing"),
            holes_trend = ifelse(holes_pos == 1, "Gaps Increasing", "Gaps Decreasing")) %>% 
            mutate(
                #area_trend = factor(area_trend, levels = c("Area Decreasing" ,"Area Increasing")),
                #holes_trend = factor(holes_trend, levels = c("Gaps Increasing","Gaps Decreasing")),
                area_trend = factor(area_trend, levels = c("Area Decreasing" ,"Area Increasing")),
                holes_trend = factor(holes_trend, levels = c("Gaps Increasing","Gaps Decreasing")),
                Percent = round(100 * Freq / sum(Freq), 1)
        )
        all_slopes_long
        #all_slopes_long$Percent <- factor(all_slopes_long$Percent,levels=c("32.6","24.4","19.3","23.8"))
        all_slopes_long$Percent <- factor(all_slopes_long$Percent,levels=c("27.2","29.6","22.9","20.3"))
        levels(all_slopes_long$holes_trend)
        levels(all_slopes_long$area_trend)


        #save all_slopes_long
        save(all_slopes_long,file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\all_slopes_long.Rdat")
            ## Alluvium

    
    all_slopes <- merge(mcn_area_slope,mcn_holes_slope,by="gridcell_id",all=F)
    all_slopes$bi_class <- factor(paste0("area_trend",all_slopes$area_pos,"_holes_trend",all_slopes$holes_pos))
    data_slopes <- merge(all_slopes,gridcell_sf,by.x="gridcell_id",by.y="id")
    data_slopes$geom <- st_transform(data_slopes$geom, "+proj=robin")
    
    save(data_slopes,file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\data_slopes.Rds")




#####




                glimpse(mcn)
                mcn$holes_density <- mcn$holes / mcn$mangrove_area            
                mcn$np_density <- mcn$np / mcn$mangrove_area       
                mcn$np_density <- mcn$np / mcn$mangrove_area
                mcn <- mcn %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>% filter(is.finite(holes_size)) %>%
                    mutate(holes_change = holes - dplyr::lag(na.locf(holes, na.rm = FALSE)),
                        area_change = mangrove_area - dplyr::lag(na.locf(mangrove_area, na.rm = FALSE)),
                        holes_d_change = holes_density - dplyr::lag(na.locf(holes_density, na.rm = FALSE)),
                        patch_size_change = patch_size - dplyr::lag(na.locf(patch_size, na.rm = FALSE)) ,
                        h_size_change = holes_size - dplyr::lag(na.locf(holes_size, na.rm = FALSE)) ,
                        np_change = np - dplyr::lag(na.locf(np, na.rm = FALSE)) ,
                        pafrac_change = pafrac - dplyr::lag(na.locf(pafrac, na.rm = FALSE))  ,
                        np_d_change = np_density - dplyr::lag(na.locf(np_density, na.rm = FALSE))) %>%
                    ungroup() %>%  
                    arrange(gridcell_id, year)
                mcn <- mcn %>%
                    mutate(holes_pos = ifelse(holes_change >= 0, 1, 0),
                            area_pos = ifelse(area_change >= 0, 1, 0),
                            holes_d_pos = ifelse(holes_d_change >= 0, 1, 0),
                            h_size_pos = ifelse(h_size_change >= 0, 1, 0),
                            patch_size_pos = ifelse(patch_size_change >= 0, 1, 0),
                            np_pos = ifelse(np_change >= 0, 1, 0),
                            pafrac_pos = ifelse(pafrac_change >= 0, 1, 0),
                            np_d_pos = ifelse(np_d_change >= 0, 1, 0))

                my <- aggregate(mangrove_area~year,FUN="mean",data=mcn)
                
                weighted_means_holes_np_pafrac <- mcn %>%
                                    group_by(year) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    filter(is.finite(holes_size)) %>%
                                    summarise(holes_weighted_mean = sum(holes * weight, na.rm = TRUE),
                                              np_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              holes_size_weighted_mean = sum(holes_size * weight, na.rm = TRUE),
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
                weighted_means_holes_np_pafrac
                ggplot(weighted_means_holes_np_pafrac,aes(x=holes_perkm2,y=holes_size_weighted_mean,color=year))+
                geom_point()+
                geom_smooth(method="lm")

                                
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
                sum_h_change$color <-" "
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
                
                
                    write.csv(sum_area_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_area_R5_pos.csv")  
                    write.csv(sum_area_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_area_change.csv")  
                            
                sum_h_change <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>%
                                    group_by(year) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(annualchange_holes_weighted_mean = sum(holes_change * weight, na.rm = TRUE),
                                                mangrove_area = mean(mangrove_area, na.rm = TRUE),
                                               holes_weighted_mean = sum(holes * weight, na.rm = TRUE) ) %>%  
                                    mutate(holes_annualchange_perkm2 = annualchange_holes_weighted_mean /mangrove_area,
                                     holes_perkm2 = holes_weighted_mean /mangrove_area) %>%
                                    as.data.frame() %>%filter(year==1996 | annualchange_holes_weighted_mean!=0)

                years_i <- c(2016,2017,2018,2019,2020)
                sum_h_change <- aggregate(holes_annualchange_perkm2~year,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change_pos <- aggregate(holes_annualchange_perkm2~year+holes_d_pos,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change$color <-" "
                mcn$holes_density <- mcn$holes / mcn$mangrove_area
                msum <- mcn %>% group_by(year) %>% filter(is.finite(holes_density)) %>% summarise(mean_holes_perha=median(holes_density,na.rm=TRUE))
                sum_h_change <- merge(sum_h_change,msum,by="year",all=F)
                sum_h_change$perc <- sum_h_change$holes_annualchange_perkm2/sum_h_change$mean_holes_perha
                weighted_means_h_R5_pos <- merge(weighted_means_h_R5_pos,msum,by="year",all=F)


                
        write.csv(weighted_means_h_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\weighted_means_h_R5_pos.csv")  
        write.csv(sum_h_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_h_change.csv")  
                
                
##### PAFRAC and Patches Change
    weighted_means_h_size_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF", is.finite(holes_size), !is.na(h_size_pos),mangrove_area>0) %>%
                                    group_by(year,R5,h_size_pos) %>%
                                    mutate(weight = log(mangrove_area) / sum(log(mangrove_area), na.rm = TRUE)) %>%
                                    summarise(h_size_weighted_mean = sum(holes_size * weight, na.rm = TRUE),
                                                h_size_annualchange_weighted_mean = sum(h_size_change * weight, na.rm = TRUE),
                                              #pafrac_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>% 
                                              as.data.frame() %>%
                                    mutate(h_size_perkm2 = h_size_weighted_mean/mangrove_area_mean,
                                           h_size_annualchange_perkm2 = h_size_annualchange_weighted_mean/mangrove_area_mean
                                           ) %>%
                                           filter(year>1995) %>% group_by(R5) %>%
                                           mutate(h_size_perkm2_wrt1996 = h_size_perkm2 - first(h_size_perkm2)) %>%
                                                as.data.frame() %>%
                                            filter(!is.na(h_size_perkm2))
                                        
                
    weighted_means_patch_size_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(patch_size_pos)) %>% filter(mangrove_area>0) %>%
                                    group_by(year,R5,patch_size_pos) %>%
                                    mutate(weight = log(mangrove_area) / sum(log(mangrove_area), na.rm = TRUE)) %>%
                                    summarise(patch_size_weighted_mean = sum(np * weight, na.rm = TRUE),
                                                patch_size_annualchange_weighted_mean = sum(patch_size_change * weight, na.rm = TRUE),
                                              #pafrac_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>% 
                                              as.data.frame() %>%
                                    mutate(patch_size_perkm2 = patch_size_weighted_mean/mangrove_area_mean,
                                           patch_size_annualchange_perkm2 = patch_size_annualchange_weighted_mean/mangrove_area_mean
                                           ) %>%
                                           filter(year>1995) %>% group_by(R5) %>%
                                           mutate(change_patch_size_perkm2_wrt1996 = patch_size_perkm2 - first(patch_size_perkm2)) %>%
                                                as.data.frame() %>%
                                            filter(!is.na(patch_size_perkm2))
                                        
                

                            weighted_means_np_d_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(np_d_pos)) %>%filter(is.finite(np_d_change )) %>%
                                    group_by(year,R5,np_d_pos) %>%
                                    mutate(weight = log(mangrove_area+1) / sum(log(mangrove_area+1), na.rm = TRUE)) %>%
                                    summarise(npd_change_weighted_mean = sum(np_d_change * weight, na.rm = TRUE),
                                              npd_weighted_mean = sum(np_density * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>% 
                                              as.data.frame() %>%
                                    mutate(npd_change_perc = 100*npd_change_weighted_mean/npd_weighted_mean) %>%
                                    #       np_annualchange_perkm2 = np_annualchange_weighted_mean/mangrove_area_mean
                                     #      ) %>%
                                      #     filter(year>1995) %>% group_by(R5) %>%
                                       #    mutate(change_np_perkm2_wrt1996 = np_perkm2 - first(np_perkm2)) %>%
                                        #        as.data.frame() %>%
                                            filter(!is.na(npd_weighted_mean))

            
                weighted_means_np_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(np_d_pos)) %>%
                                    group_by(year,R5,np_d_pos) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(np_weighted_mean = sum(np_d_change * weight, na.rm = TRUE),
                                                np_annualchange_weighted_mean = sum(np_change * weight, na.rm = TRUE),
                                              #np_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>% 
                                              as.data.frame() %>%
                                    mutate(np_perkm2 = np_weighted_mean/mangrove_area_mean,
                                           np_annualchange_perkm2 = np_annualchange_weighted_mean/mangrove_area_mean
                                           ) %>%
                                           filter(year>1995) %>% group_by(R5) %>%
                                           mutate(change_np_perkm2_wrt1996 = np_perkm2 - first(np_perkm2)) %>%
                                                as.data.frame() %>%
                                            filter(!is.na(np_perkm2))
                sum_np_change <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>%
                                    group_by(year) %>%
                                    mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                                    summarise(annualchange_np_weighted_mean = sum(np_change * weight, na.rm = TRUE),
                                                mangrove_area = mean(mangrove_area, na.rm = TRUE)) %>%  
                                    mutate(np_annualchange_perkm2 = annualchange_np_weighted_mean /mangrove_area ) %>%
                                    as.data.frame() %>%filter(year==1996 | annualchange_np_weighted_mean!=0)
                

                weighted_means_pafrac_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(pafrac_pos)) %>% filter(mangrove_area>0) %>%
                                    group_by(year,R5,pafrac_pos) %>%
                                    mutate(weight = log(mangrove_area) / sum(log(mangrove_area), na.rm = TRUE)) %>%
                                    summarise(pafrac_weighted_mean = sum(np * weight, na.rm = TRUE),
                                                pafrac_annualchange_weighted_mean = sum(pafrac_change * weight, na.rm = TRUE),
                                              #pafrac_weighted_mean = sum(np * weight, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>% 
                                              as.data.frame() %>%
                                    mutate(pafrac_perkm2 = pafrac_weighted_mean/mangrove_area_mean,
                                           pafrac_annualchange_perkm2 = pafrac_annualchange_weighted_mean/mangrove_area_mean
                                           ) %>%
                                           filter(year>1995) %>% group_by(R5) %>%
                                           mutate(change_pafrac_perkm2_wrt1996 = pafrac_perkm2 - first(pafrac_perkm2)) %>%
                                                as.data.frame() %>%
                                            filter(!is.na(pafrac_perkm2))
                
                sum_pafrac_change <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(mangrove_area>0)  %>% 
                                    group_by(year) %>%
                                    mutate(weight = log(mangrove_area) / sum(log(mangrove_area), na.rm = TRUE)) %>%
                                    summarise(annualchange_pafrac_weighted_mean = sum(pafrac_change * weight, na.rm = TRUE),
                                                mangrove_area = mean(mangrove_area, na.rm = TRUE)) %>%  
                                    mutate(pafrac_annualchange_perkm2 = annualchange_pafrac_weighted_mean /mangrove_area ) %>%
                                    as.data.frame() %>%filter(year==1996 | annualchange_pafrac_weighted_mean!=0)
                
                sum_pafrac_change <- aggregate(pafrac_annualchange_weighted_mean~year,FUN="mean",data=weighted_means_pafrac_R5_pos)



            ## 2-trends and Year Changes in Gaps size (start) - Figure 1 - Panel B
                
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                glimpse(weighted_means_h_size_R5_pos)
                sum_h_size_change <- aggregate(h_size_annualchange_weighted_mean~year,FUN="sum",data=weighted_means_h_size_R5_pos)
                sum_h_size_change_pos <- aggregate(h_size_annualchange_weighted_mean~year+h_size_pos,FUN="sum",data=weighted_means_h_size_R5_pos)
                sum_h_size_change$color <-" "
                msum <- mcn %>% group_by(year) %>% filter(is.finite(holes_size)) %>% filter((mangrove_area>0)) %>% 
                                    mutate(weight = log(mangrove_area) / sum(log(mangrove_area), na.rm = TRUE)) %>%
                                    summarise(h_size_weighted_mean_y = sum(holes_size * weight, na.rm = TRUE)) #%>% summarise(patch_size_weighted_mean=mean(patch_size,na.rm=TRUE))
                sum_h_size_change <- merge(sum_h_size_change,msum,by="year",all=F)
                weighted_means_h_size_R5_pos <- merge(weighted_means_h_size_R5_pos,msum,by="year",all=F)
            ## 2-trends and Year Changes in Gaps Size (start) - Figure 1 - Panel B


            ## 2-trends and Year Changes in PATCHSIZE (start) - Figure 1 - Panel B
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                sum_patch_size_change <- aggregate(patch_size_annualchange_weighted_mean~year,FUN="sum",data=weighted_means_patch_size_R5_pos)
                sum_patch_size_change_pos <- aggregate(patch_size_annualchange_weighted_mean~year+patch_size_pos,FUN="sum",data=weighted_means_patch_size_R5_pos)
                sum_patch_size_change$color <-" "
                msum <- mcn %>% group_by(year) %>% filter(is.finite(patch_size)) %>% filter((mangrove_area>0)) %>% 
                                    mutate(weight = log(mangrove_area) / sum(log(mangrove_area), na.rm = TRUE)) %>%
                                    summarise(patch_size_weighted_mean_y = sum(patch_size * weight, na.rm = TRUE)) #%>% summarise(patch_size_weighted_mean=mean(patch_size,na.rm=TRUE))
                sum_patch_size_change <- merge(sum_patch_size_change,msum,by="year",all=F)
                weighted_means_patch_size_R5_pos <- merge(weighted_means_patch_size_R5_pos,msum,by="year",all=F)


            
              
            ## 2-trends Year Changes in PATCHSIZE (end) - Figure 1 - Panel B
            
            
            ## 2-trends and Year Changes in PAFRAC (start) - Figure 1 - Panel B
                
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                sum_pafrac_change <- aggregate(pafrac_annualchange_weighted_mean~year,FUN="sum",data=weighted_means_pafrac_R5_pos)
                sum_pafrac_change_pos <- aggregate(pafrac_annualchange_weighted_mean~year+pafrac_pos,FUN="sum",data=weighted_means_pafrac_R5_pos)
                sum_pafrac_change$color <-" "
                msum <- mcn %>% group_by(year) %>% filter(is.finite(pafrac)) %>% summarise(mean_pafrac_perha=median(pafrac,na.rm=TRUE))
                sum_pafrac_change <- merge(sum_pafrac_change,msum,by="year",all=F)
                weighted_means_pafrac_R5_pos <- merge(weighted_means_pafrac_R5_pos,msum,by="year",all=F)
              
            ## 2-trends Year Changes in PAFRAC (end) - Figure 1 - Panel B
            
            ## 2-trends and Year Changes in NP (start) - Figure 1 - Panel B
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                mcn$np_density <- mcn$np/mcn$mangrove_area
               sum_np_change <- mcn %>% group_by(gridcell_id) %>% 
                                    mutate(np_density_change = np_density - lag(np_density)) %>% as.data.frame() %>%
                                    group_by(year) %>% filter(is.finite(np_density)) %>% filter((mangrove_area>0)) %>% 
                                    
                                    mutate(weight = log(mangrove_area+1) / sum(log(mangrove_area+1), na.rm = TRUE)) %>%
                                    summarise(np_d_weighted_mean_y = sum(np_density * weight, na.rm = TRUE),
                                                np_annualchange_perkm2 = sum(np_density_change*weight, na.rm = TRUE)) %>% 
                                                as.data.frame()#%>% summarise(patch_size_weighted_mean=mean(patch_size,na.rm=TRUE))
                sum_np_change$color <-" "
                
            ## 2-trends Year Changes in NP (end) - Figure 1 - Panel B
               
        write.csv(weighted_means_np_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\weighted_means_np_R5_pos.csv")  
        write.csv(sum_np_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_np_change.csv")  
            write.csv(weighted_means_pafrac_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\weighted_means_pafrac_R5_pos.csv")  
            write.csv(sum_pafrac_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_pafrac_change.csv")  
            write.csv(weighted_means_patch_size_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\weighted_means_patch_size_R5_pos.csv")  
            write.csv(sum_patch_size_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_patch_size_change.csv")  
write.csv(weighted_means_h_size_R5_pos,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\weighted_means_h_size_R5_pos.csv")  
            write.csv(sum_h_size_change,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\Fig_Vals\\sum_h_size_change.csv")  
