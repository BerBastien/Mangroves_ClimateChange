##Descriptive Plots

library(cowplot)   
        library(biscale)
        library(sf)
        library("rnaturalearth")
        library("rnaturalearthdata")
        library(mapproj)
                mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")


    ## Plot dimension changes in map (start)
        #@tutorial: https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
        #install.packages("biscale")
        #install.packages(c("cowplot", "sf"))
        #install.packages("biscale", dependencies = TRUE)
        
        ## Cumulative and Yearly Changes (start) - Figure 1 - Panels
            ## Data Processing Year Changes and Long-term Change (start)
                mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                mcn$holes_density <- mcn$holes / mcn$mangrove_area
                #mcn$holes_area <- (mcn$sum_all_pixels - mcn$sum_pixels_mangrove)  * mcn$mangrove_area /  mcn$sum_pixels_mangrove
                #glimpse(mcn)

                min(mcn$area/mcn$sum_all_pixels,na.rm=TRUE)
                max((mcn$area/mcn$sum_all_pixels),na.rm=TRUE)
            
                
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
                
                
                    
                #my <- aggregate(mangrove_area~year,FUN="sum",data=mcn)
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
                
                ggplot(weighted_means_holes_np_pafrac,aes(x=year,y=(change_holes_perkm2_wrt1996)))+geom_line()
                ggplot(weighted_means_holes_np_pafrac,aes(x=year,y=(change_np_perkm2_wrt1996)))+geom_line()
                ggplot(weighted_means_holes_np_pafrac,aes(x=year,y=(change_pafrac_wrt1996)))+geom_line()
                
                write.csv(weighted_means_holes_np_pafrac,"Data/output/weighted_means_h_new.csv")

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
                glimpse(weighted_means_h_R5 )
                
                ggplot(weighted_means_h_R5,aes(x=year,y=(annualchange_holes_weighted_mean)))+geom_line(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=annualchange_np_weighted_mean,y=(annualchange_holes_weighted_mean)))+geom_point(aes(color=R5,size=mangrove_area_mean))
                ggplot(weighted_means_h_R5,aes(x=year,y=(change_holes_perkm2_wrt1996)))+geom_line(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=year,y=(holes_perkm2)))+geom_line(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=year,y=(pafrac_weighted_mean)))+geom_point(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=year,y=(np_perkm2)))+geom_line(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=year,y=(change_np_perkm2_wrt1996)))+geom_line(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=year,y=(change_pafrac_wrt1996)))+geom_line(aes(color=R5))
                ggplot(weighted_means_h_R5,aes(x=change_holes_perkm2_wrt1996,y=(change_np_perkm2_wrt1996)))+geom_line(aes(color=R5))
                                              
                sum_area_R5_pos <- mcn %>% filter(!is.na(R5)) %>% filter(R5!="REF") %>% filter(!is.na(area_pos)) %>%
                                    group_by(year,R5,area_pos) %>%
                                    summarise(area_change_sum = sum(area_change, na.rm = TRUE),
                                              mangrove_area_mean = mean(mangrove_area,na.rm=TRUE)
                                              ) %>%  as.data.frame() 
                sum_area <- mcn %>% group_by(year) %>%
                                    summarise(area_change_sum = sum(area_change, na.rm = TRUE)) %>% filter(year==1996 | area_change_sum!=0) %>% as.data.frame() 


                sum_area
                ggplot()+geom_bar(data=sum_area_R5_pos,stat="identity",aes(x=year,y=area_change_sum,fill=R5))+
                geom_line(data=sum_area,aes(x=year,y=cumsum(area_change_sum)))
                                            
                
                                  
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
                
                ggplot()+geom_bar(data=weighted_means_h_R5_pos,stat="identity",aes(x=year,y=holes_annualchange_perkm2,fill=R5))+
                geom_line(data=sum_h_change,aes(x=year,y=cumsum(holes_annualchange_perkm2)))

                ggplot()+geom_bar(data=weighted_means_h_R5_pos,stat="identity",aes(x=year,y=holes_d_annualchange,fill=R5))+
                geom_line(data=sum_h_change,aes(x=year,y=cumsum(holes_annualchange_perkm2)))




                glimpse(mcn)
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

                glimpse(weighted_means_np_d_R5_pos)
            
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
                
                ggplot()+geom_bar(data=weighted_means_np_R5_pos,stat="identity",aes(x=year,y=np_annualchange_perkm2,fill=R5))+
                geom_line(data=sum_np_change,aes(x=year,y=cumsum(np_annualchange_perkm2)))

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


                
                ggplot()+geom_bar(data=weighted_means_pafrac_R5_pos,stat="identity",aes(x=year,y=pafrac_annualchange_weighted_mean,fill=R5))+
                geom_line(data=sum_pafrac_change,aes(x=year,y=(pafrac_annualchange_weighted_mean)))

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
                                        
                
            ## Data Processing Year Changes and Long-term Change (end)

            ## Cum and Year Changes in Area (start) - Figure 1 - Panel A
                    sum_area$color <- " "
                    
                    bars_cumsum_area_long <- ggplot(sum_area_R5_pos)+
                    geom_bar(aes(x=(year),y=area_change_sum,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Area Change \n(km2)")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=sum_area,aes(x=year,y=cumsum(area_change_sum),color=factor(color)),size=1.5)+
                    
                    #geom_smooth(data=sum_area,aes(x=year,y=cumsum(area_change_sum),color=factor(color)),size=1.5)+
                    theme_bw() + 
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))+ 
                    guides(fill=guide_legend(reverse = TRUE))+ 
                    scale_color_manual(values = c(" " = "indianred")) +
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
                    bars_cumsum_area_long
            ## Cum and Year Changes in Area (start) - Figure 1 - Panel A
        
       
            ## Cum and Year Changes in GAPS (start) - Figure 1 - Panel B
                
                
                sum_h_change <- aggregate(holes_annualchange_perkm2~year,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change$color <-" "
                
                bars_cumsum_holes_short <- ggplot(weighted_means_h_R5_pos)+
                    geom_bar(aes(x=(year),y=holes_annualchange_perkm2,
                        fill=factor(R5)),stat="identity")+  
                    xlab("Year")+ylab("Change in \nGaps per km2")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=sum_h_change,aes(x=year,y=cumsum(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_h_change,aes(x=year,y=cumsum(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    scale_color_manual(values = c(" " = "indianred")) +
                    theme_bw() +theme(legend.position="bottom")+
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))
                    bars_cumsum_holes_short
                leg <- get_legend(bars_cumsum_holes_short)
        
                ((bars_cumsum_area_short+ theme(legend.position = "none")) /(( bars_cumsum_holes_short)+ theme(legend.position = "none"))/(leg)) +
                    plot_layout(heights=c(4,4,1))
                ggsave("Figures/Draft2/Bar_CumLine_Historical_Long.png",dpi=600)
            ## Cum and Year Changes in GAPS (end) - Figure 1 - Panel B


            ## Cum and Year Changes in PATCHES (start) - Figure 1 - Panel B
                
                
                sum_np_change <- aggregate(np_annualchange_perkm2~year,FUN="sum",data=weighted_means_np_R5_pos)
                sum_np_change$color <-" "
                
                bars_cumsum_np_short <- ggplot(weighted_means_np_R5_pos)+
                    geom_bar(aes(x=(year),y=np_annualchange_perkm2,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in \nGaps per km2")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=sum_np_change,aes(x=year,y=cumsum(np_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_np_change,aes(x=year,y=(np_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    scale_color_manual(values = c(" " = "indianred")) +
                    theme_bw() +theme(legend.position="bottom")+
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015))+ 
                    scale_y_break(c(15,230))
                    bars_cumsum_np_short
                leg <- get_legend(bars_cumsum_np_short)
        
                ((bars_cumsum_area_short+ theme(legend.position = "none")) /(( bars_cumsum_np_short)+ theme(legend.position = "none"))/(leg)) +
                    plot_layout(heights=c(4,4,1))
                ggsave("Figures/Draft2/Bar_CumLine_Historical_Long.png",dpi=600)

            ## Cum and Year Changes in PATCHES (end) - Figure 1 - Panel B


            ## Cum and Year Changes in PATCHES (start) - Figure 1 - Panel B
                
                
                sum_np_change <- aggregate(np_annualchange_perkm2~year,FUN="sum",data=weighted_means_np_R5_pos)
                sum_np_change$color <-" "
                
                bars_cumsum_np_short <- ggplot(weighted_means_np_R5_pos)+
                    geom_bar(aes(x=(year),y=np_annualchange_perkm2,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in \nGaps per km2")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=sum_np_change,aes(x=year,y=cumsum(np_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_np_change,aes(x=year,y=(np_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    scale_color_manual(values = c(" " = "indianred")) +
                    theme_bw() +theme(legend.position="bottom")+
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015))+ 
                    scale_y_break(c(15,230))
                    bars_cumsum_np_short
                leg <- get_legend(bars_cumsum_np_short)
        
                ((bars_cumsum_area_short+ theme(legend.position = "none")) /(( bars_cumsum_np_short)+ theme(legend.position = "none"))/(leg)) +
                    plot_layout(heights=c(4,4,1))
                ggsave("Figures/Draft2/Bar_CumLine_Historical_Long.png",dpi=600)

            ## Cum and Year Changes in PATCHES (end) - Figure 1 - Panel B


            ## Cum and Year Changes in Area (start) - Figure 1 - Panel A
                    sum_area$color <- " "
                    years_i <- c(2008,2009,2010,2016,2017,2018,2019,2020)
                    bars_cumsum_area_short <- ggplot(sum_area_R5_pos[which(sum_area_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=area_change_sum,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Area Change \n(km2)")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_area[which(sum_area$year %in% years_i),],aes(x=year,y=(area_change_sum),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    geom_smooth(data=sum_area[which(sum_area$year %in% years_i),],aes(x=year,y=(area_change_sum),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    theme_bw() + 
                    #scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015.5))+ 
                    guides(fill=guide_legend(reverse = TRUE))+ 
                    scale_color_manual(values = c(" " = "indianred")) 
                    
                    bars_cumsum_area_short
            ## Cum and Year Changes in Area (start) - Figure 1 - Panel A
        
       
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


            
            
                
                bars_2_trends_perc_patch_size <- ggplot(weighted_means_patch_size_R5_pos[which(weighted_means_patch_size_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=100*patch_size_annualchange_weighted_mean/patch_size_weighted_mean_y,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual Change of \nthe Avg. Patch Size  (%)")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_patch_size_change[which(sum_patch_size_change$year %in% years_i),],aes(x=year,y=(100*patch_size_annualchange_weighted_mean/patch_size_weighted_mean_y)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_h_change_pos[which(sum_h_change_pos$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(holes_d_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Negative","Positive")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                    bars_2_trends_perc_patch_size
                    
              
            ## 2-trends Year Changes in PATCHSIZE (end) - Figure 1 - Panel B
            
            
            ## 2-trends and Year Changes in PAFRAC (start) - Figure 1 - Panel B
                
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                sum_pafrac_change <- aggregate(pafrac_annualchange_weighted_mean~year,FUN="sum",data=weighted_means_pafrac_R5_pos)
                sum_pafrac_change_pos <- aggregate(pafrac_annualchange_weighted_mean~year+pafrac_pos,FUN="sum",data=weighted_means_pafrac_R5_pos)
                sum_pafrac_change$color <-" "
                msum <- mcn %>% group_by(year) %>% filter(is.finite(pafrac)) %>% summarise(mean_pafrac_perha=median(pafrac,na.rm=TRUE))
                sum_pafrac_change <- merge(sum_pafrac_change,msum,by="year",all=F)
                weighted_means_pafrac_R5_pos <- merge(weighted_means_pafrac_R5_pos,msum,by="year",all=F)


            
  
                
                bars_2_trends_perc_pafrac <- ggplot(weighted_means_pafrac_R5_pos[which(weighted_means_pafrac_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=100*pafrac_annualchange_weighted_mean/mean_pafrac_perha,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual PAFRAC Change (%)")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_pafrac_change[which(sum_pafrac_change$year %in% years_i),],aes(x=year,y=(100*pafrac_annualchange_weighted_mean/mean_pafrac_perha)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_h_change_pos[which(sum_h_change_pos$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(holes_d_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Negative","Positive")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                    bars_2_trends_perc_pafrac
                    
              
            ## 2-trends Year Changes in PAFRAC (end) - Figure 1 - Panel B
            
            ## 2-trends and Year Changes in NP (start) - Figure 1 - Panel B
                
                weighted_means_np_R5_pos
                
                years_i <- c(2016,2017,2018,2019,2020)
                mcn$np_density <- mcn$np/mcn$mangrove_area
               sum_np_change <- mcn %>% group_by(gridcell_id) %>% 
                                    mutate(np_density_change = np_density - lag(np_density)) %>% as.data.frame() %>%
                                    group_by(year) %>% filter(is.finite(np_density)) %>% filter((mangrove_area>0)) %>% 
                                    
                                    mutate(weight = log(mangrove_area+1) / sum(log(mangrove_area+1), na.rm = TRUE)) %>%
                                    summarise(np_d_weighted_mean_y = sum(np_density * weight, na.rm = TRUE),
                                                np_annualchange_perkm2 = sum(np_density_change*weight, na.rm = TRUE)) %>% 
                                                as.data.frame()#%>% summarise(patch_size_weighted_mean=mean(patch_size,na.rm=TRUE))
                sum_np_change 

                #sum_np_change <- aggregate(np_annualchange_perkm2~year,FUN="sum",data=weighted_means_np_R5_pos)
                #sum_np_change_pos <- aggregate(np_annualchange_perkm2~year+np_d_pos,FUN="sum",data=weighted_means_np_R5_pos)
                sum_np_change$color <-" "
                #msum <- mcn %>% group_by(year) %>% filter(is.finite(np)) %>% filter((mangrove_area>0)) %>% 
                #                    mutate(weight = log(mangrove_area+1) / sum(log(mangrove_area+1), na.rm = TRUE)) %>%
                #                    summarise(np_weighted_mean_y = sum(np * weight, na.rm = TRUE)) #%>% summarise(patch_size_weighted_mean=mean(patch_size,na.rm=TRUE))

                #msum <- mcn %>% group_by(year) %>% filter(is.finite(np)) %>% summarise(mean_np_perha=median(np,na.rm=TRUE))
                #sum_np_change <- merge(sum_np_change,msum,by="year",all=F)
                
                #weighted_means_np_R5_pos <- merge(weighted_means_np_R5_pos,msum,by="year",all=F)
                
                #msum <- mcn %>% group_by(year) %>% summarise(total_area=mean(mangrove_area,na.rm=TRUE))
                #sum_np_change <- merge(sum_np_change,msum,by="year",all=F)
                #sum_np_change$mean_np_perha <- sum_np_change$np_weighted_mean_y/sum_np_change$total_area


            
  
                
                bars_2_trends_perc_np_d <- ggplot(weighted_means_np_d_R5_pos[which(weighted_means_np_d_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=npd_change_perc,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual Patches per km2 Change (%)")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_np_change[which(sum_np_change$year %in% years_i),],aes(x=year,y=(100*np_annualchange_perkm2/np_d_weighted_mean_y)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_h_change_pos[which(sum_h_change_pos$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(holes_d_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Negative","Positive")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                    bars_2_trends_perc_np_d
                    
              
            ## 2-trends Year Changes in NP (end) - Figure 1 - Panel B
            
            ## 2-trends and Year Changes in GAPS (start) - Figure 1 - Panel B
                
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                sum_h_change <- aggregate(holes_annualchange_perkm2~year,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change_pos <- aggregate(holes_annualchange_perkm2~year+holes_d_pos,FUN="sum",data=weighted_means_h_R5_pos)
                sum_h_change$color <-" "
                mcn$holes_density <- mcn$holes / mcn$mangrove_area
                msum <- mcn %>% group_by(year) %>% filter(is.finite(holes_density)) %>% summarise(mean_holes_perha=median(holes_density,na.rm=TRUE))
                sum_h_change <- merge(sum_h_change,msum,by="year",all=F)
                sum_h_change$perc <- sum_h_change$holes_annualchange_perkm2/sum_h_change$mean_holes_perha
                weighted_means_h_R5_pos <- merge(weighted_means_h_R5_pos,msum,by="year",all=F)
                
                bars_2_trends_perc <- ggplot(weighted_means_h_R5_pos[which(weighted_means_h_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=100*holes_annualchange_perkm2/mean_holes_perha,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual Gaps Change (%)")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(100*holes_annualchange_perkm2/mean_holes_perha)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_h_change_pos[which(sum_h_change_pos$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(holes_d_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Negative","Positive")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                    bars_2_trends_perc
                    
                bars_2_trends <- ggplot(weighted_means_h_R5_pos[which(weighted_means_h_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=holes_annualchange_perkm2,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual Gaps Change (gaps/km"^2, ")")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_h_change_pos[which(sum_h_change_pos$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(holes_d_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Negative","Positive")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                    bars_2_trends
                
                leg <- get_legend(bars_cumsum_holes_short)
        
                ((bars_cumsum_area_short+ theme(legend.position = "none")) /(( bars_cumsum_holes_short)+ theme(legend.position = "none"))/(leg)) +
                    plot_layout(heights=c(5,5,1))
            ## 2-trends Year Changes in GAPS (end) - Figure 1 - Panel B

            ## 2-trends and Year Changes in AREA (start) - Figure 1 - Panel B
                
                
                
                years_i <- c(2016,2017,2018,2019,2020)
                sum_area_change <- aggregate(area_change_sum~year,FUN="sum",data=sum_area_R5_pos)
                sum_area_change_pos <- aggregate(area_change_sum~year+area_pos,FUN="sum",data=sum_area_R5_pos)
                sum_h_change$color <-" "
                msum <- mcn %>% group_by(year) %>% summarise(total_area=sum(mangrove_area,na.rm=TRUE))
                #msum <- mcn %>% group_by(year) %>% summarise(mean_holes_perha=mean(holes_density,na.rm=TRUE))
                sum_area_change <- merge(sum_area_change,msum,by="year",all=F)
                sum_area_R5_pos <- merge(sum_area_R5_pos,msum,by="year",all=F)
                
                bars_2_trends_area <- ggplot(sum_area_R5_pos[which(sum_area_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=area_change_sum,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual Area Change (km"^2, ")")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_area_change[which(sum_area_change$year %in% years_i),],aes(x=year,y=(area_change_sum)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_area_change_pos[which(sum_area_change_pos$year %in% years_i),],aes(x=year,y=(area_change_sum),color=factor(area_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Decreasing","Increasing")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                   bars_2_trends_area

                   bars_2_trends_area_perc <- ggplot(sum_area_R5_pos[which(sum_area_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=100*area_change_sum/total_area,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ ylab(expression(paste("Annual Area Change (%)")))+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_point(data=sum_area_change[which(sum_area_change$year %in% years_i),],aes(x=year,y=(100*area_change_sum/total_area)),size=1.5) + 
                    #geom_smooth(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    #geom_smooth(data=sum_area_change_pos[which(sum_area_change_pos$year %in% years_i),],aes(x=year,y=(area_change_sum),color=factor(area_pos)),size=1.5,method="lm") + 
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Trend"))+
                    scale_color_manual(values = c("0" = "indianred","1"="seagreen"),labels=c("Decreasing","Increasing")) +
                    theme_bw() +theme(legend.position="bottom")
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015.5))
                   bars_2_trends_area_perc
                
                leg <- get_legend(bars_2_trends_area)
        
                ((bars_cumsum_area_short+ theme(legend.position = "none")) /(( bars_cumsum_holes_short)+ theme(legend.position = "none"))/(leg)) +
                    plot_layout(heights=c(5,5,1))
            ## 2-trends and Year Changes in AREA (end) - Figure 1 - Panel B

            plot_bars <- ggarrange(bars_2_trends_area_perc+ylab("")+ggtitle("Area"),bars_2_trends_perc+ylab("")+ggtitle("Gaps"),
                            bars_2_trends_perc_pafrac+ylab("")+ggtitle("PAFRAC"),bars_2_trends_perc_patch_size+ylab("")+ggtitle("No. of Patches per Area"),common.legend=TRUE,legend="bottom")
            

            annotate_figure(plot_bars, top = text_grob("Annual Change in Mangrove Characteristics By Region (%)"))
            ggsave("Figures/Draft2/Bars_Sums.png",dpi=600)
            ## Cum and Year Changes in PAFRAC (start) - Figure 1 - Panel B
                
                
                    years_i <- c(2008,2009,2010,2016,2017,2018,2019,2020)
                
                bars_cumsum_pafra_short <- ggplot(weighted_means_pafrac_R5_pos[which(weighted_means_pafrac_R5_pos$year %in% years_i),])+
                    geom_bar(aes(x=(year),y=pafrac_annualchange_weighted_mean,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in \nGaps per km2")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=sum_h_change[which(sum_h_change$year %in% years_i),],aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5)+guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    #geom_line(data=sum_h_change,aes(x=year,y=(holes_annualchange_perkm2),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    geom_smooth(data=sum_pafrac_change[which(sum_pafrac_change$year %in% years_i),],aes(x=year,y=(pafrac_annualchange_weighted_mean),color=factor(color)),size=1.5) + guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Change"))+
                    scale_color_manual(values = c(" " = "indianred")) +
                    theme_bw() +theme(legend.position="bottom")+
                    #scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015.5))
                bars_cumsum_pafra_short
                leg <- get_legend(bars_cumsum_holes_short)
        
                ((bars_cumsum_area_short+ theme(legend.position = "none")) /(( bars_cumsum_holes_short)+ theme(legend.position = "none"))/(leg)) +
                    plot_layout(heights=c(5,5,1))
            ## Cum and Year Changes in GAPS (end) - Figure 1 - Panel B


            ## Alluvium
                #install.packages("ggalluvial")
                library(ggalluvial)
                mcn_pafrac_slope <- mcn %>% filter(!is.na(pafrac))%>%  filter(year>2015)%>% 
                group_by(gridcell_id) %>% 
                do({
                    model <- lm(pafrac ~ year, data = .)
                    data.frame(slope = coef(model)["year"])
                })%>%
                mutate(pafrac_pos = ifelse(slope >= 0, 1, 0))

                mcn_holes_slope <- mcn %>% filter(mangrove_area >0)%>% filter(!is.na(holes))%>%  filter(year>2015)%>% 
                group_by(gridcell_id) %>% 
                do({
                    model <- lm(holes/mangrove_area ~ year, data = .)
                    data.frame(slope = coef(model)["year"])
                })%>%
                mutate(holes_pos = ifelse(slope >= 0, 1, 0))

                mcn_np_slope <- mcn %>%  %>% filter(mangrove_area >0) filter(!is.na(np))%>%  filter(year>2015)%>% 
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
                    data.frame(slope = coef(model)["year"])
                }) %>%
                mutate(area_pos = ifelse(slope >= 0, 1, 0))

                glimpse(mcn_np_slope)
                all_slopes <- merge(mcn_np_slope,mcn_holes_slope,by="gridcell_id",all=F)
                all_slopes <- merge(all_slopes,mcn_pafrac_slope,by="gridcell_id",all=F)
                all_slopes <- merge(all_slopes,mcn_area_slope,by="gridcell_id",all=F)
                r5_gridcell <- mcn[mcn$year==2020,which(names(mcn) %in% c("gridcell_id","R5"))]
                all_slopes <- merge(all_slopes,r5_gridcell,by="gridcell_id",all=F)
                glimpse(all_slopes)
                all_slopes <- all_slopes[,-which(names(all_slopes) %in% c("slope.x","slope.y"))]
                

                # Create a frequency column to plot (since you have binary flags, we just sum them)
                all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>% 
                group_by(area_pos, np_pos, holes_pos, pafrac_pos, R5) %>%
                summarise(Freq = n(), .groups = "drop")
                
                all_slopes_long <- all_slopes %>% filter(!is.na(R5))%>% filter(pafrac_pos %in% c(0,1))%>% 
                group_by(area_pos, holes_pos) %>%
                summarise(Freq = n(), .groups = "drop")
                
                head(all_slopes_long)

                head(all_slopes_long)

                library(scico)

                all_slopes_long$area_trend <- "Area Increasing"
                all_slopes_long$area_trend[which(all_slopes_long$area_pos==0)] <- "Area Decreasing"
                all_slopes_long$area_trend <- factor(all_slopes_long$area_trend, levels=c("Area Decreasing","Area Increasing"))

                all_slopes_long$holes_trend <- "Gaps Increasing"
                all_slopes_long$holes_trend[which(all_slopes_long$holes_pos==0)] <- "Gaps Decreasing"
                all_slopes_long$holes_trend <- factor(all_slopes_long$holes_trend, levels=c("Gaps Increasing","Gaps Decreasing"))

                all_slopes_long$Percent <- round(100*all_slopes_long$Freq/sum(all_slopes_long$Freq),1)
                all_slopes_long$Percent <- factor(all_slopes_long$Percent,levels=c("32.6","24.4","19.2","23.7"))

                
                alluvium_interaction <- ggplot(data = all_slopes_long ,
                    aes(axis1=holes_trend , #axis2 = np_pos, axis3 =pafrac_pos , 
                            axis2 =area_trend,
                        y = Freq)) +
                #scale_x_discrete(limits = c("Area","Gaps")) +
                xlab("") +
                geom_alluvium(aes(fill = factor(Percent)),alpha=0.7) +
                geom_stratum() +
                geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
                theme_void() +
                scale_fill_scico_d(direction=-1,palette="hawaii", begin = 0., end = .8)+coord_flip()+
                theme(legend.position = "bottom")+
                guides(fill=guide_legend(title="Percent of Cases",title.position = "top",title.hjust =0.5))
                alluvium_interaction

                blank_gg <- ggplot()+theme_void()
                ggarrange(ggarrange(bars_2_trends_area,bars_2_trends,ncol=1,common.legend=TRUE,legend="bottom"),
                ggarrange(blank_gg,alluvium_interaction+theme(legend.position="bottom"),blank_gg,heights=c(1,3,1),ncol=1),ncol=2,widths=c(4,2.7))
                #ggsave("Figures/Draft2/Trends_Sankey_notrends.png",dpi=600)

            
                ggarrange(ggarrange(bars_2_trends_area_perc,bars_2_trends_perc,ncol=1,common.legend=TRUE,legend="bottom"),
                    ggarrange(blank_gg,alluvium_interaction+theme(legend.position="bottom"),blank_gg,heights=c(1,3,1),ncol=1),ncol=2,widths=c(4,2.7))
                #ggsave("Figures/Draft2/Trends_Sankey_Percent.png",dpi=600)

            ## Alluvium

            
            ## Map Alluvium Classes
                
                all_slopes <- merge(mcn_area_slope,mcn_holes_slope,by="gridcell_id",all=F)
                all_slopes$bi_class <- factor(paste0("area_trend",all_slopes$area_pos,"_holes_trend",all_slopes$holes_pos))
                glimpse(all_slopes)
                data <- merge(all_slopes,gridcell_sf,by="gridcell_id")
                data$geom <- st_transform(data$geom, "+proj=robin")
                levels(factor(data$bi_class))

            
            world <- ne_countries(scale = "medium", returnclass = "sf")
            class(world)

            world$geometry <- st_transform(world$geometry, "+proj=robin")

            custom_pal3 <- c( 
            "area_trend0_holes_trend0" = "#965239",
            "area_trend1_holes_trend0" = "#61dda9",
            "area_trend0_holes_trend1" = "#8c0173", 
            "area_trend1_holes_trend1" = "#99a021"
            )

            custom_pal3_or <- c( 
            "area_trend0_holes_trend0" = "#965239",
            "area_trend1_holes_trend0" = "#61dda9",
            "area_trend0_holes_trend1" = "#8c0173", 
            "area_trend1_holes_trend1" = "#99a021"
            )
            
            barbie_pal <- c( 
            "area_trend1_holes_trend1" = "#dfd601",
            "area_trend1_holes_trend0" = "#21a8c5", 
            "area_trend0_holes_trend0" = "#5e72d4",
            "area_trend0_holes_trend1" = "#c90590"
            )

            opp_pal <- c( 
            "area_trend1_holes_trend1" = "#9aada5",
            "area_trend1_holes_trend0" = "#51828b", 
            "area_trend0_holes_trend0" = "#d4bda9",
            "area_trend0_holes_trend1" = "#a45c50"
            )
            
            
            mangrove_map_basemap <- ggplot()+
            geom_sf(data =world, color = "black", fill = "black")+
            geom_sf(data = data, mapping = aes(fill = bi_class, geometry=geom), 
            color = "white", size = 1, show.legend = FALSE, inherit.aes = FALSE) + 
            coord_sf(xlim = c(-180*10^5, 180*10^5), ylim = c(-40*10^5, 31*10^5))+
            scale_fill_manual(values =  #opp_pal
                                        #barbie_pal
                                        custom_pal3
                                        )+
            theme_minimal()
            mangrove_map_basemap

            
            library(biscale)

            
            names(custom_pal3) <- c(
                                    "1-1" ,
                                    "2-1" ,
                                    "1-2" ,
                                    "2-2"
                                    )
                                    
            legend <- bi_legend(pal = custom_pal3,
                    dim = 2,
                    xlab = "Area ",
                    ylab = "Gaps Desnity ",
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
                    draw_plot(legend, x=0.05, y=.25, width = 0.3, height = 0.3)
                finalPlot

                
                #ggarrange(ggarrange(bars_2_trends_area_perc,bars_2_trends_perc,ncol=1,common.legend=TRUE,legend="bottom"),
                 #   ggarrange(blank_gg,alluvium_interaction+theme(legend.position="bottom"),blank_gg,heights=c(1,3,1),ncol=1),ncol=2,widths=c(4,2.7))

                
                ggarrange(finalPlot,ggarrange(ggarrange(bars_2_trends_area_perc,bars_2_trends_perc,ncol=2,common.legend=TRUE,legend="bottom"),
                            alluvium_interaction+theme(legend.position="bottom"),ncol=2,widths=c(5,2)),nrow=2,heights=c(3.5,2))
                ggsave("Figures/Draft2/Maps_Figure1_ceaproj.png",dpi=600)

                ggarrange(finalPlot,ggarrange(ggarrange(bars_2_trends_area_perc,bars_2_trends_perc,ncol=2,common.legend=TRUE,legend="right"),
                            alluvium_interaction+theme(legend.position="bottom"),ncol=2,widths=c(5,2)),nrow=2,heights=c(3.5,2))
                ggsave("Figures/Draft2/Maps_Figure1_ceaproj.png",dpi=600)


                if (is.null(st_crs(data))) {
                    st_crs(data) <- 4326  # EPSG code for WGS84
                    }

                    
                    proj_i <- "boggs" #rpoly #omerc #laea

                    # Now transform your data to the desired projection
                    data$geom_transformed <- st_transform(data$geom, "+proj=cea")

                    # Then use data_transformed in your ggplot call
                    mangrove_map_basemap <- ggplot() +
                    geom_sf(data = world, color = "black", fill = "black") +
                    geom_sf(data = data, mapping = aes(fill = bi_class, geometry = geom_transformed), 
                            color = "white", size = 1, show.legend = FALSE, inherit.aes = FALSE) + 
                    coord_sf(crs = "+proj=cea",
                                xlim = c(-180*10^5, 180*10^5), ylim = c(-40*10^5, 31*10^5)
                                )+  
                    scale_fill_manual(values = custom_pal3_or) +
                    theme_minimal()

                    mangrove_map_basemap



            ## Map Alluvium Clasees


        
        ##
        ## Calculating Area and NP percentiles (start)
            gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
            gridcell_data <- st_read(gpkg_file)
            gridcell_sf <- st_as_sf(gridcell_data)
            names(gridcell_sf)[1] <- "gridcell_id"
            
            
            # data <- bi_class(mcn[which(mcn$year>2005 & 
            #                 is.finite(mcn$annual_np_change) & 
            #                 is.finite(mcn$annual_area_change)),], 
            #     x = annual_area_change, 
            #     y = annual_np_change, 
            #     style = "quantile", dim = 3)

            

            mcn_grouped_area$long_change_neg <- -mcn_grouped_area$long_change #So I plot Area Loss (not Area change)

            data <- bi_class(mcn_grouped_area[which(mcn_grouped_area$year>2010 & 
                            is.finite(mcn_grouped_area$long_change) & 
                            is.finite(mcn_grouped_area$long_changenp)),], 
                x = long_change_neg, 
                y = long_changenp, 
                style = "quantile", dim = 3,
                keep_factors=TRUE)
            
            data <- merge(data,gridcell_sf,by="gridcell_id")
            glimpse(data)
        ## Calculating Area and NP percentiles (start)

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
            gridcell_frag_med <- data$gridcell_id[which(data$bi_y==unique(data$bi_y)[1])]
            gridcell_frag_high <- data$gridcell_id[which(data$bi_y==unique(data$bi_y)[2])]
            gridcell_frag_low <- data$gridcell_id[which(data$bi_y==unique(data$bi_y)[3])]

            
            
            glimpse(mcn)        #here 
            mcn$q_np <- "33 - 66%"
            mcn$q_np[which(mcn$gridcell_id %in% gridcell_frag_low)] <- "0 - 33%"
            mcn$q_np[which(mcn$gridcell_id %in% gridcell_frag_high)] <- "66 - 100%"

            np_mcn_sum <- aggregate(np~year+q_np,FUN="sum",data=mcn[which(mcn$year>2006),])
            np_mcn_sum_temp <- aggregate(sst~year+q_np,FUN="mean",data=mcn[which(mcn$year %in% unique(np_mcn_sum$year)),])
            np_mcn_sum$sst <- np_mcn_sum_temp$sst
            
            ggplot(np_mcn_sum)+
            geom_point(aes(x=sst,y=np),alpha=0.2)
            

            a07 <- np_mcn_sum[which(np_mcn_sum$year==2007),c(2,3)]
            names(a07)[2] <- "a07"
            np_mcn_sum <- merge(np_mcn_sum,a07,by="q_np",all=T)
            np_mcn_sum$np_perc <- 100*np_mcn_sum$np/np_mcn_sum$a07

            np_mcn_sum$q_np <- factor(np_mcn_sum$q_np, levels = c("66 - 100%", "33 - 66%", "0 - 33%"))
            levels((np_mcn_sum$q_np))
            
            plot_sum_np <- ggplot(np_mcn_sum)+
            geom_point(aes(x=year,y=np_perc-100,col=factor(q_np)))+
            #geom_line(aes(x=year,y=np_perc,col=factor(q_np)))+
            geom_smooth(aes(x=year,y=np_perc-100,col=factor(q_np)),se=F)+
            # scale_color_manual(values = c("low" = "#028833",
            #                         "med" = "#cfe68b",
            #                         "high" = "#f27301")) + theme_bw()+
            scale_color_manual(values = c("0 - 33%" = "#028833",
                                    "33 - 66%" = "#9ac8d5",
                                    "66 - 100%" = "#5a4da5")) + theme_bw()+
            labs(color = "Percentile") + 
            ylab("Change since 2007 (%)")+
            theme(plot.title = element_text(hjust = 0.5)) +
            ggtitle("Fragmentation")
            plot_sum_np
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
                    ylab = "Fragmentation ",
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
            
            ggarrange(finalPlot,ggarrange(blank_plot,plot_sum_np,plot_sum_area,ncol=3,widths=c(1,3,3)),nrow=2,heights=c(3,2))
            #ggsave("Figures/Draft/Fig_1_Map.png",dpi=600)


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
            
            ggarrange(finalPlot,ggarrange(blank_plot,plot_sum_np,plot_sum_area,ncol=3,widths=c(1.5,3,3)),nrow=2,heights=c(3,2.5))
            #ggsave("Figures/Draft/Fig_1_Map_whiteborder.png",dpi=600)

        
        ## Plot Mangrove Map    (end)

        ## Figure with distribuion of classes
            groups <- bi_class_breaks(mcn_grouped_area[which(mcn_grouped_area$year>2010 & 
                        is.finite(mcn_grouped_area$long_change_neg) & 
                        is.finite(mcn_grouped_area$long_changenp)),], 
            x = long_change_neg, 
            y = long_changenp, 
            #style = "quantile", dim = 3,split=T)
            style = "quantile", dim = 3,split=T)


            bi_class_breaks(mcn_grouped_area[which(mcn_grouped_area$year>2010 & 
                        is.finite(mcn_grouped_area$long_change_neg) & 
                        is.finite(mcn_grouped_area$long_changenp)),], 
            x = long_change_neg, 
            y = long_changenp, 
            #style = "quantile", dim = 3)
            style 
            = "quantile", dim = 3)

            groups 

            legend_with_dots <- ggplot(data,aes( x = long_change_neg, 
            y = long_changenp))+
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