
    library("ggbreak")
    library("tidyverse")
    library("scico")
    library("colorspace")
    library("ggpubr")
    library("dplyr")
    library(ggrepel)
    library("ggplot2")
    "%notin%" <- Negate("%in%")

    setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all_ssps.csv")
    scen_2100 <- scen %>% filter(year==2100)
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
    mcn_2020 <- mcn %>% filter(year==2020)
    
    image(volcano, col = hcl.colors(12, "YlOrRd"))

## Map
#### Gridcells
###### Slopes

    ## Map
    load(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\data_slopes_gridcell.Rds") #data_slopes,
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_robin <- st_transform(world, "+proj=robin")
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_robin <- st_transform(world, "+proj=robin")

    oceans <- read_sf("C:\\Users\\basti\\Box\\Data\\Oceans\\ne_50m_ocean.shp")
    oceans_robin <- st_transform(oceans, "+proj=robin")

    mangrove_map_basemap <- ggplot() +
        #geom_sf(data = oceans_robin, fill = "gray20", alpha=0.7) +
        #geom_sf(data = world, color = "black", fill = "black") +
        geom_sf(data = oceans_robin, fill = "black",alpha=0.85) +
        geom_sf(data = world, color = "gray40", fill = "gray40") +
        geom_sf(data = data_slopes, mapping = aes(fill = slope_area, geometry = geom, color = slope_area), #color="white", 
            size = 1000000, show.legend = TRUE, inherit.aes = FALSE) +
        coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))+
        scale_color_scico(palette = "vik", midpoint = 0, 
                        limits = c(-0.5, 0.5), oob = scales::squish, direction = -1,
                        guide = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10)) +
    scale_fill_scico(palette = "vik", midpoint = 0, 
                    limits = c(-0.5, 0.5), oob = scales::squish, direction = -1,
                    guide = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10)) +
    theme_minimal()+
    theme(legend.position = "bottom",
            legend.margin=margin(t = -10, b = -10)) +
    labs(fill = expression(paste("Recent Area Trend (km"^2, "/year)")), 
        color = expression(paste("Recent Area Trend (km"^2, "/year)")))

            mangrove_map_basemap

    glimpse(map_aloss_neg_country_onlyCC)
                data_slopes$gainsince1996 <- 0
                data_slopes$gainsince1996[which(data_slopes$gridcell_id %in% gain_since_1996)] <- 1
                ggplot(data = data_slopes) +
                        geom_sf(aes(fill =gainsince1996, geometry=geom)) 
    ## Histogram 

    # Define bin widths
    bin_width <- 5  # for instance, this will create bins like (0,5], (5,10], etc.
    mcn_sl <- merge(mcn,data_slopes,by="gridcell_id",all=TRUE)
    breaks <- seq(min(mcn$Latitude, na.rm = TRUE),
              max(mcn$Latitude, na.rm = TRUE) + bin_width,
              by = bin_width)
    labels <- sprintf("%.0f", head(breaks, -1))

    glimpse(mcn_sl)
    mcn_binned <- mcn_sl %>% 
    filter(year == 2020) %>%
    mutate(Latitude_bin = cut(Latitude, 
                            breaks = breaks,
                            include.lowest = TRUE, 
                            labels = labels)) %>%
    group_by(Latitude_bin, area_pos) %>%
    summarize(total_mangrove_area = sum(mangrove_area, na.rm = TRUE))


    #mcn_binned$Latitude_bin <- as.double(mcn_binned$Latitude_bin)
    
    latitude_histogram <- ggplot(mcn_binned %>% filter(Latitude_bin!="31")) +
    geom_bar(aes(x =(Latitude_bin), y = total_mangrove_area,fill=factor(area_pos)), stat = "identity") +
    labs(x = "Latitude (°)", y = "Area \nDistribution") +
    scale_fill_scico_d(palette="vik",direction=-1,begin=0.15,end=0.85)+theme_minimal()+
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),      # Hides x-axis text
        axis.ticks.x = element_blank(),     # Hides x-axis ticks
        panel.grid.major.x = element_blank(), # Hides major grid lines
        panel.grid.minor.x = element_blank(), # Hides minor grid lines
        panel.grid.major.y = element_blank(), # Hides y-axis major grid lines
        panel.grid.minor.y = element_blank()  # Hides y-axis minor grid lines
    ) + #xlab("")+ 
    theme(legend.position = "none")
    
    empty_plot <- ggplot()+theme_void()

    ggarrange(mangrove_map_basemap,ggarrange(latitude_histogram+coord_flip(),empty_plot,ncol=1,heights=c(3,1)),ncol=2,widths=c(6,1))

    
    
    load("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\sum_area.Rds")
    load("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\sum_area_R5_pos.Rds")

    bars_cumsum_area_long <- ggplot(sum_area_R5_pos)+
                    geom_bar(aes(x=(year),y=area_change_sum,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Area Change \n(km2)")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_scico_d(palette = "batlow",end=0.9)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=sum_area,aes(x=year,y=cumsum(area_change_sum),color=factor(color)),size=1.5)+
                    theme_bw() + 
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))+ 
                    guides(fill=guide_legend(reverse = TRUE))+ 
                    scale_color_manual(values = c(" " = "indianred")) +
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
    bars_cumsum_area_long



    
    ggarrange(ggarrange(mangrove_map_basemap,ggarrange(latitude_histogram+coord_flip(),empty_plot,ncol=1,heights=c(3,1)),ncol=2,widths=c(6,1)),
    bars_cumsum_area_long, ncol=1, heights=c(2,1))

## Map
#### Gridcells
###### Slopes


    create_plot_for_years <- function(data, start_year, end_year, show_y_axis=TRUE) {
    p <- ggplot(data) +
    geom_bar(aes(x=(year),y=area_change_sum,
                        fill=factor(R5)),stat="identity")+
    xlab("Year")+
    scale_fill_scico_d(palette = "batlow",end=0.9)+
    geom_hline(aes(yintercept=0),linetype="dashed")+
    geom_line(data=sum_area,aes(x=year,y=cumsum(area_change_sum),color=factor(color)),size=1.5)+
    theme_bw() + 
    scale_color_manual(values = c(" " = "indianred")) +
    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative Change")) +
    coord_cartesian(xlim=c(start_year, end_year)) +
    labs(fill="Region")
  
  if (!show_y_axis) {
    p <- p + theme(axis.title.y=element_blank(), 
                  axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank())
  } else {
    p <- p + ylab("Area Change \n(km2)")
  }
  
  return(p)
    }


    # Create separate plots
    plot1 <- create_plot_for_years(sum_area_R5_pos, min(sum_area$year), 1998.8, show_y_axis=TRUE)
    plot2 <- create_plot_for_years(sum_area_R5_pos, 2006, 2010.5, show_y_axis=FALSE)
    plot3 <- create_plot_for_years(sum_area_R5_pos, 2014, max(sum_area_R5_pos$year), show_y_axis=FALSE)

    # Combine the plots
    combined_plot <- ggarrange(plot1+xlab(""), plot2, plot3+xlab(""),empty_plot, ncol=4, common.legend=TRUE,legend="bottom",widths=c(1.5,2,2,1))

# # Combine the combined_plot with other plots
    # final_plot <- ggarrange(
    #     ggarrange(mangrove_map_basemap, ggarrange(latitude_histogram + coord_flip()+xlab(""), empty_plot, ncol = 1, heights = c(3, 1)), ncol = 2, widths = c(6.5, 1),align="v"),
    #     ggarrange(combined_plot, empty_plot, ncol = 2, widths = c(6, 1)),
    #     ncol = 1, 
    #     heights = c(2, 1.2)
    # )

    final_plot <- ggarrange(
        ggarrange(mangrove_map_basemap, ggarrange(latitude_histogram + coord_flip()
                    +xlab("Latitude")+
                    theme(
                    axis.text.y=element_blank(), 
                    axis.ticks.y=element_blank())
                    , empty_plot, ncol = 1, heights = c(3.5, 1)), ncol = 2, widths = c(6.8, 1),align="v"),
        combined_plot,
        ncol = 1, 
        heights = c(2, 1.5)
    )

    final_plot
    ggsave("Figures/Draft2/Fig1_draft2_1.png",dpi=600)
## Models
#### Area
###### Effects

                load(file="Models/Round2/sq_estimate_sst_area.RData") 
                load(file="Models/Round2/sq_estimate_preci_area.RData") 
                load(file="Models/Round2/sq_estimate_gdp_area.RData") 
                load(file="Models/Round2/sq_estimate_gap_area.RData") 
                

                
                pal_lapaz <- scico(15, palette = 'batlow')
                        pal_lapaz <- pal_lapaz[c(2,7,12)]
                        pal_roma <- scico(15, palette = 'roma')


                Models_ssthot_plot_gdp <- ggplot(sq_estimate_gdp_area)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase (%)")+xlab("Log GDP per capita***")+theme_minimal()+ xlim(c(0,15))
                       
                Models_ssthot_plot_gdp

                histogram_plot_gdp <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        geom_density(data=scen_2100,aes(log(GDP_SSP5/POP_SSP5)),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void() + xlim(c(0,15))
                
                Models_ssthot_plot_gdp <- ggarrange(Models_ssthot_plot_gdp, histogram_plot_gdp,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_gdp

                Models_preci_hot_plot_area <- ggplot(sq_estimate_preci_area)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_minimal()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1 mm Rain Increase")+xlab("Monthly Mean Precipitation (mm)")
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                    Models_preci_hot_plot_area

                histogram_plot_preci <- ggplot(mcn_2020, aes(x = Mean_Precipitation)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        geom_density(data=scen_2100,aes(preci_85),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_preci_hot_plot_area<-ggarrange(Models_preci_hot_plot_area, histogram_plot_preci,legend="none", 
                            ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_preci_hot_plot_area
                
                Models_ssthot_plot_area <- ggplot(sq_estimate_sst_area)+
                    geom_line(aes(x=temp,y=gestimated*100),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1*100,ymax=ci2*100),fill="#e9995c",alpha=0.2)+
                    theme_minimal()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+xlim(c(18,35))+
                    ylab("Effect of 1°C Increase (%)")+xlab("Mean SST in the Hottest Month (°C)***")#+
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_area

                histogram_plot_ssthot <- ggplot(mcn_2020, aes(x = sst_hottest)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        geom_density(data=scen_2100,aes(sst_hot_70),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw()+xlim(c(18,35)) +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_ssthot_plot_area<-ggarrange(Models_ssthot_plot_area, histogram_plot_ssthot,legend="none", 
                ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_area

                

                Models_holes_plot_area <- ggplot(sq_estimate_gap_area)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+#xlim(c(18,35))+
                    ylab("Effect of 1°C Increase (%)")+xlab("Number of Gaps (°C)***")#+
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_holes_plot_area

                histogram_plot_holes <- ggplot(mcn_2020, aes(x = lag_gap_density)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(data=scen_2100,aes(sst_hot_85),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw()+xlim(c(0,60)) +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_holes_plot_area<-ggarrange(Models_holes_plot_area, histogram_plot_holes,legend="none", 
                ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_holes_plot_area


                glimpse(scen_2100)
                


                df_dummy <- data.frame(value = c(rnorm(100), rnorm(100)), Variable = rep(c("Climatic", "Socioeconomic"), each = 100))
                df_dummy_line <- data.frame(value = c(rnorm(100)), Projection = "RCP7.0 & SSP5")
                
                dummy_plot <- ggplot() +
                geom_histogram(data=df_dummy, aes(value, fill = Variable),color = "black") +
                scale_fill_manual(values = c("Climatic" = "#e9995c", "Socioeconomic" = "#25625f")) +
                geom_line(data=df_dummy_line,aes(x=value,y=value,color=Projection),size=1.3)+
                scale_color_manual(values = c("RCP7.0 & SSP5" = "indianred")) +
                theme_bw()+
                theme(legend.position="bottom",legend.box="vertical", legend.margin=margin())  # Remove all non-data ink+

                combined_legend <- get_legend(dummy_plot)

                df_dummy$Variable <- "Observations"
                dummy_plot <- ggplot() +
                geom_histogram(data=df_dummy, aes(value, fill = Variable),color = "black") +
                scale_fill_manual(values = c("Observations" = "#e9995c")) +
                guides(fill=guide_legend(""))+
                geom_line(data=df_dummy_line,aes(x=value,y=value,color=Projection),size=1.3)+
                scale_color_manual(values = c("RCP7.0 & SSP5" = "indianred")) +
                theme_bw()+
                theme(legend.position="bottom",legend.box="vertical", legend.margin=margin())  # Remove all non-data ink+

                combined_legend_1 <- get_legend(dummy_plot)

                arr_area_loss_plot <- ggarrange(ggarrange(Models_ssthot_plot_area,
                                    #Models_preci_hot_plot_area,
                                    Models_ssthot_plot_gdp,
                                    #Models_ssthot_plot_pop,
                                    ncol=2),
                                    combined_legend,ncol=1,heights=c(4,1))
                                    
                arr_area_loss_plot_onlyTemp <- ggarrange(Models_ssthot_plot_area,
                                    combined_legend_1,ncol=1,heights=c(4,1))
                arr_area_loss_plot_onlyTemp
                #ggsave("Figures/15Sept2023/Temp_MarginalEffect.png",dpi=600)
                
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Area Loss Model"),#, face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        ) 

                # Printing the annotated figure
                print(annotated_figure)
               ggsave("Figures/Draft2/AreaModel_MarginalEffect_SSP570.png",dpi=600)

## Models
#### Area
###### Effects

## Projection
    #### Area

        ## ONLY CLIMATE EFFECT (start)
        
            ######## By Country
            ######## and by Region
                    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\scen_arealoss_perc_onlyCC.csv")
                    scen_arealoss_perc_onlyCC_2100 <-  read.csv(file="Results\\Area\\scen_arealoss_perc_onlyCC_2100.csv")
                    agg_aloss_neg_onlyCC <-  read.csv(file="Results\\Area\\agg_aloss_neg_onlyCC.csv")
                    agg_aloss_neg_total_onlyCC <- read.csv(file="Results\\Area\\agg_aloss_neg_total_onlyCC.csv")
                    agg_loss_by_country <- read.csv(file="Results\\Area\\agg_loss_by_country.csv")
                    load(file="Results\\Area\\map_aloss_neg_country_onlyCC.Rda")

                    timeseries_arealoss_onlyCC <-ggplot(scen_arealoss_perc_onlyCC,aes(x=(year),y=arealoss_perc_cumulative_onlyCC))+
                                        geom_line(aes(x=(year),y=arealoss_perc_cumulative_onlyCC,group=gridcell_id,color=R5),alpha=0.8)+#+ylim(c(-1,1))+
                                        theme_bw()+guides(color="none")+#ylim(c(-7.5,20))+
                                        #guides(color=guide_legend("Region"))+
                                            scale_color_scico_d(palette="batlow",end=0.8) + 
                                            xlab("Year")+ylab("Area Loss (%)")
                                            
                    boxplot_arealoss_onlyCC <- ggplot(scen_arealoss_perc_onlyCC_2100[which(!is.na(scen_arealoss_perc_onlyCC_2100$R5)),])+
                                            geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=arealoss_perc_cumulative_onlyCC), width = 0.2)+
                                            geom_boxplot(aes(fill=factor(R5),y=arealoss_perc_cumulative_onlyCC),outlier.shape = NA, width = 0.2)+
                                            geom_hline(aes(yintercept=0),linetype="dashed")+
                                            theme_void()+#ylim(c(-7.5,20))+
                                            scale_fill_scico_d(palette="batlow",end=0.8) +
                                            scale_color_scico_d(palette="batlow",end=0.8)+
                                            guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                                            #guides(fill="none",color="none")
                                
                ggarrange(timeseries_arealoss_onlyCC,boxplot_arealoss_onlyCC,widths=c(3,1),
                            align="hv")
                #ggsave("Figures/15Sept2023/Projection_byR5_RCP85.png",dpi=600)

                
                    Projection_Loss_Gain_RCP85 <- ggplot(agg_aloss_neg_onlyCC)+
                        geom_bar(aes(x=year,y=mangrove_area_future_loss,
                            #fill=factor(loss)),stat="identity")+
                            fill=factor(R5)),stat="identity")+
                        xlab("Year")+ylab("Mangrove Area Loss (km2)")+
                        labs(fill=guide_legend("Region"))+
                        #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                        scale_fill_scico_d(palette="batlow",end=0.8) +
                        scale_color_scico_d(palette="batlow",end=0.8)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=mangrove_area_future_loss),color="indianred",size=1.5)+
                        geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=mangrove_area_future_loss_lower_limit),color="indianred",size=1,linetype="dashed")+
                        geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=mangrove_area_future_loss_upper_limit),color="indianred",size=1,linetype="dashed")+
                        theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                                    
                Projection_Loss_Gain_RCP85
                #ggsave("Figures/15Sept2023/Projection_Loss_Gain_RCP85.png",dpi=600)

                    
                    map_mangrove_loss2100 <- ggplot(data = map_aloss_neg_country_onlyCC) +
                        geom_sf(aes(fill = mangrove_area_future_loss)) +
                        #scale_fill_viridis_c(na.value = "white") +
                        theme_minimal() +
                        labs(title = "Mangrove Area Loss in 2100",
                            fill = "Future Loss (km2)")+
                        scale_fill_scico(palette="lajolla",direction=-1,na.value = "white")
                map_mangrove_loss2100
               
                #ggsave("Figures/15Sept2023/Map_Loss_2100_RCP85.png",dpi=600)

                

                

                    glimpse(agg_loss_by_country)
                    agg_loss_by_country %>%
                    top_n(20, -total_loss_2023PV) %>%
                    ggplot(aes(x = reorder(countryname, -total_loss_2023PV), y = -total_loss_2023PV/1000000000)) +
                    geom_bar(stat = "identity") +
                    coord_flip() + theme_bw()+
                    labs(title = "Top 20 Countries Most Affected by Mangrove Loss",
                        x = "Country",
                        y = "Well-being Loss (Billion USD)")
                    #ggsave("Figures/15Sept2023/Barplot_Loss_2100_RCP85_USD.png",dpi=600)

                    agg_loss_by_country
            ######## By Country
            ######## and by Region

            ######## Total with Historical
                
                summary_dfs_total_sum <- read.csv(file="Results\\Area\\summary_dfs_total_sum.csv")
                agg_aloss_neg_total_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_OnlyCC_SSP170.csv") 
                ggplot()+
                    # geom_bar(aes(x=(year),y=-mangrove_area_future_loss,
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Area Change \n(km2)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_ribbon(data=agg_aloss_neg_total_onlyCC,
                        aes(x=year,ymin=area_change_wrt_1996_lower_limit,ymax=area_change_wrt_1996_upper_limit),color="transparent",fill="indianred",alpha=0.2)+
                    geom_line(data=agg_aloss_neg_total_onlyCC,
                        aes(x=year,y=area_change_wrt_1996),size=1.5,color="indianred")+
                    theme_bw() + 
                    geom_line(data=summary_dfs_total_sum,
                    aes(x=year,y=cumsum(area_change),color=factor(color)),size=1.5)+
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))+ 
                    guides(fill=guide_legend(reverse = TRUE))+ 
                    scale_color_manual(values = c(" " = "indianred")) +
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))

            ######## Total with Historical

        ## ONLY CLIMATE EFFECT (end)
        
        ## Only GDP Effect (start)
        
            agg_aloss_neg_total_onlygdp <- read.csv(file="Results\\Area\\agg_aloss_neg_total_onlygdp_ssp2_cappedArea_cappedEffect.csv")
            agg_aloss_neg_total_onlygdp <- read.csv(file="Results\\Area\\agg_aloss_neg_total_onlygdp_ssp370_cappedArea_cappedEffect.csv")
            agg_aloss_neg_total_onlygdp <- read.csv(file="Results\\Area\\agg_aloss_neg_total_onlygdp_ssp5_cappedArea_cappedEffect.csv")
            ggplot()+
                    # geom_bar(aes(x=(year),y=-mangrove_area_future_loss,
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Area Change \n(km2)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_ribbon(data=agg_aloss_neg_total_onlyCC,aes(x=year,ymin=area_change_wrt_1996_lower_limit,ymax=area_change_wrt_1996_upper_limit),color="transparent",fill="indianred",alpha=0.2)+
                    geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=area_change_wrt_1996),size=1.5,color="indianred")+
                    geom_ribbon(data=agg_aloss_neg_total_onlygdp,aes(x=year,ymin=area_change_wrt_1996_lower_limit,ymax=area_change_wrt_1996_upper_limit),color="transparent",fill="#25625f",alpha=0.2)+
                    geom_line(data=agg_aloss_neg_total_onlygdp,aes(x=year,y=area_change_wrt_1996),size=1.5,color="#25625f")+
                    theme_bw() + 
                    geom_line(data=summary_dfs_total_sum,
                    aes(x=year,y=cumsum(area_change),color=factor(color)),size=1.5)+
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))+ 
                    guides(fill=guide_legend(reverse = TRUE))+ 
                    scale_color_manual(values = c(" " = "indianred")) +
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
        ## Only GDP Effect (end)

        ## Both (start)
            
            #agg_aloss_neg_total_both <- read.csv(file="Results\\Area\\agg_aloss_neg_total_both_ssp2_cappedArea_cappedEffect.csv")
            #agg_aloss_neg_total_both <- read.csv(file="Results\\Area\\agg_aloss_neg_total_both_ssp370_cappedArea_cappedEffect.csv")
            #agg_aloss_neg_total_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP170.csv")
            agg_aloss_neg_total_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP570.csv")
            #agg_aloss_neg_total_onlyCC <- read.csv(file="Results\\Area\\agg_aloss_neg_total_onlyCC_ssp2_cappedArea_cappedEffect.csv")
            #agg_aloss_neg_total_onlyCC <- read.csv(file="Results\\Area\\agg_aloss_neg_total_onlyCC_ssp370_cappedArea_cappedEffect.csv")
            #agg_aloss_neg_total_onlyCC$color <-  read.csv(file="Results\\Area\\agg_aloss_neg_total_onlyCC_ssp2_cappedArea_cappedEffect.csv")
            agg_aloss_neg_total_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_OnlyCC_SSP570.csv")
            agg_aloss_neg_total_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_Onlygdp_SSP570.csv")
            agg_aloss_neg_total_both$color <- "Both Factors"
            agg_aloss_neg_total_onlygdp$color <- "Socioeconomic"
            agg_aloss_neg_total_onlyCC$color <- "Climate"
            glimpse(agg_aloss_neg_total_onlyCC)
            agg_aloss_neg_total_both %>% filter(year==2100)
            agg_aloss_neg_total_onlygdp %>% filter(year==2100)
            
            summary_dfs_total_sum$color <- "Historical"
            area_change_both_plot <- ggplot()+
                    # geom_bar(aes(x=(year),y=-mangrove_area_future_loss,
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Area Change \n(km2)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=-2500),linetype="dashed")+
                    geom_text(aes(x=2060,y=-2250,label="50% Recovery Target"))+
                    #geom_ribbon(data=agg_aloss_neg_total_both,aes(x=year,ymin=area_change_wrt_1996_lower_limit,ymax=area_change_wrt_1996_upper_limit),color="transparent",fill="cyan",alpha=0.1)+
                    geom_line(data=agg_aloss_neg_total_both,aes(x=year,y=area_change_wrt_1996,color=color,linetype=color),size=1)+
                    geom_ribbon(data=agg_aloss_neg_total_onlyCC,aes(x=year,ymin=area_change_wrt_1996_lower_limit,ymax=area_change_wrt_1996_upper_limit),color="transparent",fill="#e9995c",alpha=0.2)+
                    geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=area_change_wrt_1996,color=color,linetype=color),size=1)+ #,color="#e9995c"
                    geom_ribbon(data=agg_aloss_neg_total_onlygdp,aes(x=year,ymin=area_change_wrt_1996_lower_limit,ymax=area_change_wrt_1996_upper_limit),color="transparent",fill="#25625f",alpha=0.2)+
                    geom_line(data=agg_aloss_neg_total_onlygdp,aes(x=year,y=area_change_wrt_1996,color=color,linetype=color),size=1)+ #,color="#25625f"
                    theme_bw() + 
                    geom_line(data=summary_dfs_total_sum,
                    aes(x=year,y=cumsum(area_change),color=factor(color),linetype=color),size=1)+
                    geom_point(data=summary_dfs_total_sum,
                    aes(x=year,y=cumsum(area_change),color=factor(color),linetype=color),size=1)+
                    #scale_x_break(c(1998.8,2006))+ 
                    #scale_x_break(c(2010.5,2015))+ 
                    guides(fill=guide_legend(reverse = FALSE))+ 
                    scale_color_manual(values = c("Historical"="gray30","Both Factors" = "darkblue","Climate"="#e9995c","Socioeconomic"="#25625f"),
                       breaks = c("Historical", "Socioeconomic", "Both Factors", "Climate"),
                       labels = c("Historical", "Socioeconomic", "Both Factors", "Climate")) +
                    scale_linetype_manual(values=c("Historical"="solid","Both Factors" = "dashed", "Climate"="solid", "Socioeconomic"="solid"),
                       breaks = c("Historical", "Socioeconomic", "Both Factors", "Climate"),
                       labels = c("Historical", "Socioeconomic", "Both Factors", "Climate")) +
                    #scale_linetype_manual(values=c("Historical"="solid","Climate + Socioeconomic" = "dotted", "Climate"="solid", "Socioeconomic"="solid"))+
                    #scale_color_manual(values = c("Historical"="gray30","Climate + Socioeconomic" = "darkblue","Climate"="#e9995c","Socioeconomic"="#25625f")) +
                    #scale_color_manual(values = c(" " = "indianred")) +
                    guides()+
                    ylab(expression("Net Area Change from Historical (km"^{2}*")"))+
                    xlab("Year")+ theme(legend.position = "bottom")+
                    xlim(c(2007,2100))+
                    ylim(c(-9000,-2000))+
                    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,title="Forcings"),
                            color = guide_legend(title.position="top", title.hjust = 0.5,title="Forcings"),
                            linetype = guide_legend(title.position="top", title.hjust = 0.5,title="Forcings"))

                    area_change_both_plot

                    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Projection_Area_SSP570.png")

        ## Both (end)

        ## By continents (start)
            scen_arealoss_perc_onlygdp_countrycode <- read.csv(file="Results\\Area\\Proj_Area_Perc_Country_Onlygdp_SSP570.csv")
            scen_arealoss_perc_onlyCC_countrycode <- read.csv(file="Results\\Area\\Proj_Area_Perc_Country_OnlyCC_SSP570.csv")
            scen_arealoss_perc_both_countrycode <- read.csv(file="Results\\Area\\Proj_Area_Perc_Country_both_SSP570.csv")
            #scen_arealoss_perc_onlygdp_countrycode <- read.csv(file="Results\\Area\\scen_arealoss_perc_onlygdp_countrycode_ssp370_cappedArea_cappedEffect.csv")
            #scen_arealoss_perc_onlyCC_countrycode <- read.csv(file="Results\\Area\\scen_arealoss_perc_onlyCC_countrycode_ssp370_cappedArea_cappedEffect.csv")
            #scen_arealoss_perc_both_countrycode <- read.csv(file="Results\\Area\\scen_arealoss_perc_both_countrycode_ssp370_cappedArea_cappedEffect.csv")
            glimpse(scen_arealoss_perc_both_countrycode)
            glimpse(scen_arealoss_perc_both_countrycode)
            outlier_bounds <- scen_arealoss_perc_onlyCC_countrycode %>% filter(year==2100) %>%
            group_by(R5) %>% 
            summarise(
                lower = quantile(mangrove_area_future_loss, 0.25,na.rm=TRUE) - 1.5 * IQR(mangrove_area_future_loss,na.rm=TRUE),
                upper = quantile(mangrove_area_future_loss, 0.75,na.rm=TRUE) + 1.5 * IQR(mangrove_area_future_loss,na.rm=TRUE)
            )
            outliers_onlyCC <- scen_arealoss_perc_onlyCC_countrycode %>% 
            inner_join(outlier_bounds, by = "R5") %>% 
            filter(mangrove_area_future_loss < lower | mangrove_area_future_loss > upper) %>% filter(year==2100)%>% filter(countrycode %notin% c("GMB"))

            outlier_bounds <- scen_arealoss_perc_onlygdp_countrycode %>% filter(year==2100) %>%
            group_by(R5) %>% 
            summarise(
                lower = quantile(mangrove_area_future_loss, 0.25,na.rm=TRUE) - 1.5 * IQR(mangrove_area_future_loss,na.rm=TRUE),
                upper = quantile(mangrove_area_future_loss, 0.75,na.rm=TRUE) + 1.5 * IQR(mangrove_area_future_loss,na.rm=TRUE)
            )
            outliers_onlygdp <- scen_arealoss_perc_onlygdp_countrycode %>% 
            inner_join(outlier_bounds, by = "R5") %>% 
            filter(mangrove_area_future_loss < lower | mangrove_area_future_loss > upper) %>% filter(year==2100)

            outlier_bounds <- scen_arealoss_perc_both_countrycode %>% filter(year==2100) %>%
            group_by(R5) %>% 
            summarise(
                lower = quantile(mangrove_area_future_loss, 0.25,na.rm=TRUE) - 1.5 * IQR(mangrove_area_future_loss,na.rm=TRUE),
                upper = quantile(mangrove_area_future_loss, 0.75,na.rm=TRUE) + 1.5 * IQR(mangrove_area_future_loss,na.rm=TRUE)
            )
            outliers_both <- scen_arealoss_perc_both_countrycode %>% 
            inner_join(outlier_bounds, by = "R5") %>% 
            filter(mangrove_area_future_loss < lower | mangrove_area_future_loss > upper) %>% filter(year==2100) %>% filter(countrycode %notin% c("PHL","BGD","CMR","AGO","KEN","GMB","IRN","TZA"))
            

            library(ggrepel)
            gdp_r5_ssp2 <- ggplot(scen_arealoss_perc_onlygdp_countrycode %>% filter(year==2100)) + 
                            geom_hline(aes(yintercept=0),linetype="dashed")+ 
                            geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss,fill=R5))+
                            geom_text(data = outliers_onlygdp, aes(x = factor(R5), y = mangrove_area_future_loss+5, label = countrycode),size=2.5) +
                            scale_fill_scico_d(palette="batlow",end=0.8)+theme_minimal()+xlab("")+ylab("")+ 
                            theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())   + 
                            #coord_cartesian(ylim = c(-110,150))
                            coord_cartesian(ylim = c(-250,150))
            
            CC_r5_ssp2 <-  ggplot(scen_arealoss_perc_onlyCC_countrycode %>% filter(year==2100)) + geom_text(data = outliers_onlyCC, aes(x = factor(R5), y = mangrove_area_future_loss+5, label = countrycode),size=2.5) +
                            geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss,fill=R5))+
                            scale_fill_scico_d(palette="batlow",end=0.8,name="Region")+theme_minimal()+xlab("")+ylab("Country-level Area Change in 2100")+ 
                            theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) + #coord_cartesian(ylim = c(-125,150))
                            #coord_cartesian(ylim = c(-110,150))
                            coord_cartesian(ylim = c(-250,150))
            
            both_r5_ssp2 <-  ggplot(scen_arealoss_perc_both_countrycode %>% filter(year==2100))  + geom_text(data = outliers_both, aes(x = factor(R5), y = mangrove_area_future_loss+5, label = countrycode),size=2.5) +
                             geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8)+
                             theme_minimal() +xlab("")+ylab("")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank()) + 
                             #coord_cartesian(ylim = c(-110,150)) 
                             #coord_cartesian(ylim = c(-110,150))
                             coord_cartesian(ylim = c(-250,150))
            
            Change_2100_Both_ByCountry <- ggarrange(CC_r5_ssp2+labs(title="Climate"),gdp_r5_ssp2+labs(title="Socioeconomic"),both_r5_ssp2+labs(title="Both Factors"),ncol=3,common.legend=TRUE,legend="bottom")
             Change_2100_Both_ByCountry
            # gdp_r5_ssp2 <- ggplot(scen_arealoss_perc_onlygdp_countrycode %>% filter(year==2100)) + geom_hline(aes(yintercept=0),linetype="dashed")+ geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss_wrt1996,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8)+theme_minimal()+xlab("")+ylab("")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())  #+ coord_cartesian(ylim = c(-10,150))
            # CC_r5_ssp2 <-  ggplot(scen_arealoss_perc_onlyCC_countrycode %>% filter(year==2100)) + geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss_wrt1996,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8,name="Region")+theme_minimal()+xlab("")+ylab("Country-level Area Change in 2100")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
            # both_r5_ssp2 <-  ggplot(scen_arealoss_perc_both_countrycode %>% filter(year==2100))  + geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss_wrt1996,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8)+theme_minimal() +xlab("")+ylab("")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) #+ coord_cartesian(ylim = c(-100,150))
            
            # ggarrange(CC_r5_ssp2,gdp_r5_ssp2,both_r5_ssp2,ncol=3,common.legend=TRUE,legend="bottom")

            # scen_arealoss_perc_onlygdp_countrycode %>% filter(year==2100 & R5=="OECD") %>% glimpse()
            # glimpse(mcn)
            # mangrove_2020_countrycode <- mcn %>% filter(year==2020) %>% group_by(countrycode) %>% summarise(mangrove_country_area_2020 = sum(mangrove_area, na.rm=TRUE))# %>% ungroup()
            # glimpse(mangrove_2020_countrycode)

            # scen_arealoss_perc_onlygdp_countrycode <- merge(scen_arealoss_perc_onlygdp_countrycode,mangrove_2020_countrycode,by="countrycode",all=TRUE)
            # scen_arealoss_perc_onlyCC_countrycode <- merge(scen_arealoss_perc_onlyCC_countrycode,mangrove_2020_countrycode,by="countrycode",all=TRUE)
            # scen_arealoss_perc_both_countrycode <- merge(scen_arealoss_perc_both_countrycode,mangrove_2020_countrycode,by="countrycode",all=TRUE)
            # ggplot(scen_arealoss_perc_both_countrycode %>% filter(year==2100))  + geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss_wrt1996/mangrove_country_area_2020,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8)+theme_minimal() +xlab("")+ylab("")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) #+ coord_cartesian(ylim = c(-100,150))
            # ggplot(scen_arealoss_perc_onlyCC_countrycode %>% filter(year==2100))  + geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss_wrt1996/mangrove_country_area_2020,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8)+theme_minimal() +xlab("")+ylab("")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) #+ coord_cartesian(ylim = c(-100,150))
            # ggplot(scen_arealoss_perc_onlygdp_countrycode %>% filter(year==2100))  + geom_hline(aes(yintercept=0),linetype="dashed") + geom_boxplot(aes(x=factor(R5),y=mangrove_area_future_loss_wrt1996/mangrove_country_area_2020,fill=R5))+scale_fill_scico_d(palette="batlow",end=0.8)+theme_minimal() +xlab("")+ylab("")+ theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) + coord_cartesian(ylim = c(-5,10))
            

        ## By continents (end)

        ggarrange(area_change_both_plot,Change_2100_Both_ByCountry)
        
        ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft3\\Projection_Area_SSP570_ByCountry.png",dpi=600)

        # Comparison 5 SSPS
            agg_aloss_neg_total_both_ssp1 <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP170.csv")
            agg_aloss_neg_total_both_ssp2 <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP270.csv")
            agg_aloss_neg_total_both_ssp3 <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP370.csv")
            agg_aloss_neg_total_both_ssp4 <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP470.csv")
            agg_aloss_neg_total_both_ssp5 <- read.csv(file="Results\\Area\\Proj_Area_Perc_Total_both_SSP570.csv")
                
            agg_aloss_neg_total_both_ssp1$color <- "SSP1"
            agg_aloss_neg_total_both_ssp2$color <- "SSP2"
            agg_aloss_neg_total_both_ssp3$color <- "SSP3"
            agg_aloss_neg_total_both_ssp4$color <- "SSP4"
            agg_aloss_neg_total_both_ssp5$color <- "SSP5"

            summary_dfs_total_sum$color <- "Historical"
            area_change_both_ssps<- ggplot()+
                   
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total_both_ssp1,aes(x=year,y=area_change_wrt_1996,color=color),size=1.5)+
                    geom_line(data=agg_aloss_neg_total_both_ssp2,aes(x=year,y=area_change_wrt_1996,color=color),size=1.5)+
                    geom_line(data=agg_aloss_neg_total_both_ssp3,aes(x=year,y=area_change_wrt_1996,color=color),size=1.5)+
                    geom_line(data=agg_aloss_neg_total_both_ssp4,aes(x=year,y=area_change_wrt_1996,color=color),size=1.5)+
                    geom_line(data=agg_aloss_neg_total_both_ssp5,aes(x=year,y=area_change_wrt_1996,color=color),size=1.5)+
                    theme_bw() + 
                    geom_line(data=summary_dfs_total_sum,
                    aes(x=year,y=cumsum(area_change)),color="gray30",size=1.5)+
                    guides(fill=guide_legend(reverse = FALSE))+ 
                    scale_color_discrete_qualitative(palette = "Set 2")+
                    guides()+
                    ylab(expression("Area Change (km"^{2}*")"))+
                    xlab("Year")+ #theme(legend.position = "bottom")+
                    # guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,title="Net Cumulative Change"),
                    #         color = guide_legend(title.position="top", title.hjust = 0.5,title="Net Cumulative Change"),
                    #         linetype = guide_legend(title.position="top", title.hjust = 0.5,title="Net Cumulative Change"))+
                    labs(title="Different Socioeconomic Pathways and RCP7.0",color ="Scenario")

                    area_change_both_ssps
        ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\area_change_both_ssps.png",dpi=600)

            
        # Comparison 5 SSPs

    #### Area
## Projection

## Benefit Losses (start)

    
    
    # Benefit Losses in Gridcells
        scen_allforcings_allES_dif <- read.csv(file="Data/projections/scen_allforcings_allES_dif_ssp570.csv")
        scen_allforcings_allES <- read.csv(file="Data/projections/scen_allforcings_allES_ssp570.csv")
        mex_gridcell_lines <- ggplot() + 
            geom_point(data=scen_allforcings_allES %>% filter(year %in% c(2100,2080,2050),countrycode=="MEX",type=="cultural"),aes(x=mangrove_area_future_loss*100,
                            #color=log(benefit_change/10^6),
                            color=forcing,shape=factor(year),y=log(GDP_SSP5/POP_SSP5))) + theme_bw()+
            geom_line(data=scen_allforcings_allES%>%
                            arrange(gridcell_id, gdppc5) %>% filter(year %in% c(2020+seq(1:16)*10),countrycode=="MEX",type=="cultural"),
                            aes(x=mangrove_area_future_loss*100,
                            color=forcing,y=log(GDP_SSP5/POP_SSP5),group=interaction(gridcell_id,forcing)),alpha=0.5)+
            scale_color_manual(values = c("both" = "darkblue","onlyCC"="#e9995c","onlygdp"="#25625f"),
                     labels = c("Both Factors", "Climate", "Socioeconomic"))+
            ylab("Log local GDP per capita") + 
            labs(color="",shape="Year",title="Mangroves in Mexico")+xlab("Mangrove Area Change (ha)") +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2))
            mex_gridcell_lines

        min_y_value <- scen_allforcings_allES %>%
            filter(year %in% c(2100, 2080, 2050, 2020 + seq(1:16) * 10), 
                    countrycode == "MEX", 
                    type == "cultural") %>%
            summarize(min_log_value = min(log(GDP_SSP5 / POP_SSP5), na.rm = TRUE))%>% unlist()

        max_y_value <- scen_allforcings_allES %>%
            filter(year %in% c(2100, 2080, 2050, 2020 + seq(1:16) * 10), 
                    countrycode == "MEX", 
                    type == "cultural") %>%
            summarize(max_log_value = max(log(GDP_SSP5 / POP_SSP5), na.rm = TRUE)) %>% unlist()



        library(wesanderson)
        names(wes_palettes)

        mex_gridcell_loss_perc <- ggplot(data = scen_allforcings_allES_dif %>% filter(year %in% c(2100), countrycode == "MEX")) + 
            geom_bar(stat = "identity", aes(y = 100*dif_benefit_change/GDP_SSP5 , fill = type, 
                                            x = log(GDP_SSP5 / POP_SSP5)), 
                    position = "dodge", width = 0.5,alpha=0.9) +
            #scale_fill_scico_d(palette="batlow",begin=0.1,end=0.9) +
            theme_bw() +
            labs(x = "", y = "Relative Damages\n (% local GDP)",title=" ") + coord_flip()  + #xlim(c(min_y_value,max_y_value))+
            scale_fill_manual(values = wes_palette("Darjeeling1", n = 3),
                                labels = c("Cultural", "Provisioning", "Regulating")) + 
                                labs(fill="Type of Benefit")
        mex_gridcell_loss_perc

        mex_gridcell_loss <- ggplot(data = scen_allforcings_allES_dif %>% filter(year %in% c(2100), countrycode == "MEX")) + 
            geom_bar(stat = "identity", aes(y = dif_benefit_change/10^6 , fill = type, 
                                            x = log(GDP_SSP5 / POP_SSP5)), 
                    position = "dodge", width = 0.5,alpha=0.9) +
            scale_fill_scico_d(palette="batlow",begin=0.1,end=0.9) +
            theme_bw() +
            labs(x = "Log local GDP per capita", y = "Absolute Damages\n (million USD)",title="Climate Damages in 2100") + coord_flip()  + #xlim(c(min_y_value,max_y_value))
            scale_fill_manual(values = wes_palette("Darjeeling1", n = 3),
                                labels = c("Cultural", "Provisioning", "Regulating")) + 
                                labs(fill="Type of Benefit")
        mex_gridcell_loss

        library(scales)
        mex_gridcell_loss_log <- ggplot(data = scen_allforcings_allES_dif %>% filter(year %in% c(2100), countrycode == "MEX")) + 
            geom_point(stat = "identity", aes(y = (-dif_benefit_change/10^6) , col = type, 
                                            x = log(GDP_SSP5 / POP_SSP5)), 
                    position = "dodge", width = 0.5,alpha=0.9) +
            scale_fill_scico_d(palette="batlow",begin=0.1,end=0.9) +
            theme_bw() + scale_y_continuous(trans="log10",labels=comma) +
            labs(x = "Log local GDP per capita", y = "Economic Losses in 2100\n (million USD)",title="Climate Damages in 2100") + 
            coord_flip()  + #xlim(c(min_y_value,max_y_value))
            scale_color_manual(values = wes_palette("Darjeeling1", n = 3),
                                labels = c("Cultural", "Provisioning", "Regulating")) + 
                                labs(color="Type of Benefit")+
            guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
        mex_gridcell_loss_log
        
        combined_plot <- ggarrange(ggarrange(mex_gridcell_lines+labs(title="Mangrove trajectories"),
                    mex_gridcell_loss_log,
                    widths=c(4,4),ncol=2,align="hv"),benefit_loss_2100_total,ncol=1,heights=c(3,2))
        
        
    
            
    #         ggarrange(mex_gridcell_lines,legend="bottom",
    #                     ggarrange(mex_gridcell_loss,mex_gridcell_loss_perc,common.legend=TRUE,ncol=2,legend="bottom"),
    #                     widths=c(3,4),ncol=2,align="hv")

    #         ggarrange(mex_gridcell_lines,legend="bottom",
    #                     mex_gridcell_loss_log,
    #                     widths=c(3,3),ncol=2,align="hv")
            
    #         library(ggplot2)
    # library(dplyr)

    # # Assuming scen_allforcings_allES_dif is your dataset
    # # Combining both plots in a single ggplot call
    # combined_plot <- ggplot(data = scen_allforcings_allES_dif %>% filter(year %in% c(2100), countrycode == "MEX")) +
    #   geom_bar(stat = "identity", aes(y = 100*dif_benefit_change/GDP_SSP5, fill = type, x = log(GDP_SSP5 / POP_SSP5)), position = "dodge", width = 0.5, alpha = 0.9) +
    #   geom_segment(aes(x = log(GDP_SSP5 / POP_SSP5), xend = log(GDP_SSP5 / POP_SSP5), y = 0, yend = (dif_benefit_change/10^6) * scale_ratio, group = type, color = type)) +
    #   scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
    #   theme_bw() +
    #   labs(x = "Log local GDP per capita", y = "Relative Damages (% local GDP)", color = "Type of Benefit", fill = "Type of Benefit") +
    #   scale_y_continuous(name = "Relative Damages (% local GDP)", 
    #                      sec.axis = sec_axis(~ . / scale_ratio, name = "Absolute Damages (million USD)"))

    # # Calculating the scale ratio for the secondary axis
    # max_rel_damage <- scen_allforcings_allES_dif %>% filter(year %in% c(2100), countrycode == "MEX")%>%
    #                     mutate(relative_damage = 100 * dif_benefit_change / GDP_SSP5) %>%
    #                     summarize(max_damage = min(relative_damage, na.rm = TRUE)) %>%
    #                     pull(max_damage)

    # max_abs_damage <- scen_allforcings_allES_dif %>% filter(year %in% c(2100), countrycode == "MEX")%>%
    #                     mutate(relative_damage = dif_benefit_change / 10^6) %>%
    #                     summarize(max_damage = min(relative_damage, na.rm = TRUE)) %>%
    #                     pull(max_damage)
    # scale_ratio <- max_rel_damage / max_abs_damage

    # # Updating the secondary axis scale
    # combined_plot <- combined_plot + scale_y_continuous(sec.axis = sec_axis(~ . / scale_ratio, name = "Absolute Damages (million USD)")) + coord_flip()

    # # Print the combined plot
    # print(combined_plot)


    # combined_plot_dummy <- ggplot() + geom_col(aes(y = 0, x = 1, fill = "Relative (% GDP)"), show.legend = TRUE) +
    #   # Dummy geom for "Absolute (USD)" in legend
    #   geom_segment(aes(x = 1, xend = 1, y = 0, yend = 0, color = "Absolute (USD)"), show.legend = TRUE) +
    #   # Customizing the legend
    #   scale_fill_manual(name = "Legend", 
    #                     values = c("Relative (% GDP)" = "gray"),
    #                     labels = c("Relative (% GDP)")) +
    #   scale_color_manual(name = "Legend",
    #                      values = c("Absolute (USD)" = "black"),
    #                      labels = c("Absolute (USD)"))

    # # Print the combined plot
    # combined_plot_dummy


    # ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Path_Mexico2100.png",dpi=600)
        
    #     # Benefit Losses in Gridcells
        
    long_df <- read.csv(file="Data/projections/benefit_loss_2types_ssp585.csv")
    long_df <- read.csv(file="Data/projections/benefit_loss_2types.csv") #SSP285
    long_df <- read.csv(file="Data/projections/benefit_loss_2types_ssp370.csv")
    long_df <- read.csv(file="Data/projections/benefit_loss_2types_ssp570.csv")
    glimpse(long_df)


    
    benefit_loss_2100_3types <- ggplot(long_df %>% #filter(countrycode %notin% c("IND","IDN")) %>% 
                            filter(year %in% c(2100)))+
        geom_text_repel(data=long_df %>% #filter(countrycode %notin% c("IND","IDN")) %>% 
                                filter(year %in% c(2100), type=="provision", diff_benefit_change<0) ,
                                aes(x=GDP_Country_ssp5*1.62,
                                y=-diff_benefit_change/10^6,label=countrycode),alpha=0.5)+
                                #y=1000000,label=countrycode))+
        geom_point(aes(#x=diff_mangrove_area_future_loss*100,
                                x=(GDP_Country_ssp5*1.62), #2005 to 2020 usd
                                        #y=log(-diff_benefit_change/(GDP_Country_ssp5*10^9)), color=R5, 
                        y=-diff_benefit_change/10^6, #color=R5, 
                        #alpha=((year-20#alpha=((year-2020)/(80)),
                        color=type))+
            
                     scale_color_manual(values = wes_palette("Darjeeling1", n = 3),
                                labels = c("Cultural", "Provisioning", "Regulating")) + 
                                labs(color="Type of Benefit",y="Benefit Loss in 2100 (Million 2020 USD)",x="Country-level GDP (Billion 2020 USD)")+
        scale_y_continuous(trans="log10", labels = label_number(scale = 1, accuracy = 0.01)) +   
        scale_x_continuous(trans="log10", labels = comma) + theme_bw()
    
    ggarrange(ggarrange(mex_gridcell_lines,legend="bottom",
                    ggarrange(mex_gridcell_loss,mex_gridcell_loss_perc,common.legend=TRUE,ncol=2,legend="bottom"),
                    widths=c(3,4),ncol=2,align="hv"),benefit_loss_2100_3types,ncol=1)

    geom_line(data=long_df%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year %in% c(2020+seq(1:3)*5)),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5,group=interaction(countrycode,type)),alpha=0.3)+ 
    scale_color_scico_d(palette="batlow",end=0.7)+theme_bw()+
    scale_alpha_continuous(range = c(0.2, 0.8))+
    geom_text(data=long_df %>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year ==2030),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Region",shape="ES Type")+xlab("Area Change (ha)") + ggtitle("Climate Impacts on Mangroves")+
    xlim(c(-10,50))


    ggplot(long_df)+
    geom_point(data=long_df%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year %in% c(2020+seq(1:3)*5)),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, 
                     #alpha=((year-2020)/(80)),
                     shape=type))+ 
    geom_line(data=long_df%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year %in% c(2020+seq(1:3)*5)),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5,group=interaction(countrycode,type)),alpha=0.3)+ 
    scale_color_scico_d(palette="batlow",end=0.7)+theme_bw()+
    scale_alpha_continuous(range = c(0.2, 0.8))+
    geom_text(data=long_df %>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year ==2030),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Region",shape="ES Type")+xlab("Area Change (ha)") + ggtitle("Climate Impacts on Mangroves")+
    xlim(c(-10,50))


    BenefitChange_2100_2types <- ggplot(long_df)+
    geom_point(data=long_df%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year %in% c(2020+seq(1:16)*5)),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, 
                     #alpha=((year-2020)/(80)),
                     shape=type))+ 
    geom_line(data=long_df%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year %in% c(2020+seq(1:16)*5)),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5,group=interaction(countrycode,type)),alpha=0.3)+ 
    scale_color_scico_d(palette="batlow",end=0.7)+theme_bw()+
    scale_alpha_continuous(range = c(0.2, 0.8))+
    geom_text_repel(data=long_df %>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year ==2100),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Region",shape="ES Type")+xlab("Area Change (ha)") + ggtitle("Climate Impacts on Mangroves")

    BenefitChange_2100_2types     

    
    diff_country_total <- read.csv(file="Data/projections/diff_country_total.csv")
    #diff_country_total <- read.csv(file="Data/projections/diff_country_total_ssp585.csv")
    diff_country_total <- read.csv(file="Data/projections/diff_country_total_ssp370.csv")
    diff_country_total <- read.csv(file="Data/projections/diff_country_total_ssp570.csv")
        
        glimpse(diff_country_total)

    
    benefit_loss_2100_total <- ggplot(diff_country_total %>% filter(year %in% c(2100)))+
        geom_text_repel(data=diff_country_total%>% 
                                filter(year %in% c(2100), benefit_change<0) ,
                                aes(x=GDP_Country_ssp5*1.62,
                                y=-benefit_change/10^6,label=countrycode),alpha=0.5,size=3)+
        geom_point(aes(x=(GDP_Country_ssp5*1.62), y=-benefit_change/10^6, color = R5)) + 
        scale_color_scico_d(palette = "batlow",end=0.9,begin=0.1)+
        labs(color="Region",y="Benefit Losses in 2100 \n(Million 2020 USD)",x="Country-level GDP (Billion 2020 USD)",title="Country-level Damages in 2100")+
        scale_y_continuous(trans="log10", labels = label_number(scale = 1, accuracy = 0.01)) +   
        scale_x_continuous(trans="log10", labels = comma)  + theme_bw()# + theme(legend.position = "bottom")

    benefit_loss_2100_total

    diff_country_total %>% filter(year %in% c(2100))%>% group_by(R5) %>% summarise(median(benefit_change)/10^6)

    ggarrange(ggarrange(mex_gridcell_lines,legend="bottom",
                    ggarrange(mex_gridcell_loss+xlab(""),mex_gridcell_loss_perc,common.legend=TRUE,ncol=2,legend="bottom"),
                    widths=c(2,4),ncol=2,align="hv"),benefit_loss_2100_total,ncol=1,heights=c(3,2))

    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Path_Mexico2100_And_GLOBAL.png",dpi=600)

    annotated_plot <- ggarrange(annotate_figure(ggarrange(mex_gridcell_lines+labs(title="A. Mangrove Trajectories"),
                                            mex_gridcell_loss_log+labs(x="",title="B. Economic Losses"),legend="bottom",
                                            widths=c(4,4),ncol=2,align="hv"),
                                  top = text_grob("Damages in Mexico", face = "bold", size = 14),
                                  bottom = text_grob("Damages Globally", face = "bold", size = 14)),
                                  benefit_loss_2100_total+labs(title="C. Economic Losses"),ncol=1,heights=c(3,2))

        annotated_plot
        
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Path_Mexico2100_Global2100.png",dpi=600)


    ggarrange(mex_gridcell_lines+labs(title="A. Mangrove Trajectories\n in Mexico"),
                        mex_gridcell_loss_log+labs(title="B. Economic Losses\n in Mexico"),
                        benefit_loss_2100_total+labs(title="C. Economic Losses Globally")+
            guides(colour = guide_legend(title.position="top", title.hjust = 0.5))+coord_flip(),legend="bottom",ncol=3,widths=c(2,2,3))
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Path_Mexico2100_Global2100_v2.png",dpi=600)


    ggarrange(ggarrange(mex_gridcell_lines,
                    mex_gridcell_loss_log+xlab(""),legend="bottom",
                    widths=c(3,4),ncol=2,align="hv"),benefit_loss_2100_total,ncol=1,heights=c(3,2))

    ggarrange(mex_gridcell_lines,
        ggarrange(
                    ggarrange(mex_gridcell_loss,mex_gridcell_loss_perc,common.legend=TRUE,ncol=2,legend="bottom"),
                    benefit_loss_2100_total,ncol=1),ncol=2)
    

    
    log_breaks <- pretty(log10(diff_country_total$POP_SSP5/10^6))
    actual_breaks <- 10^(log_breaks)
    library('scales')

    losses_2100_pop <- ggplot(data = diff_country_total %>% filter(year == 2100& benefit_change<0),
                          #aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log10(pop_ssp2/10^6))) +
                          aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log10(POP_SSP5/10^6))) +
                          #aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log10(pop_ssp5/10^6))) +
        geom_point() +
        geom_text_repel(aes(label=countrycode)) +
        scale_y_continuous(trans="log10", labels = comma) +   
        scale_x_continuous(trans="log10", labels = comma) +
        
        #scale_color_scico(palette="lajolla",begin=0.2,end=0.9, breaks = log_breaks, labels = comma(actual_breaks)) +
        
        scale_color_scico(palette="devon",begin=0.1,end=0.8, breaks = log_breaks, labels = comma(actual_breaks),direction=-1,
                    guide = guide_colourbar(title.position = "top", title.hjust = 0.1, barwidth = 11)) +
        xlab("Area Loss (ha)") +
        ylab("Benefit Losses (million USD)") + 
        ggtitle("Mangrove Losses in 2100") + 
        theme_bw() + labs(color="Population near Mangroves (millions)")

    gains_2100_pop <- ggplot(data = diff_country_total %>% filter(year == 2100 & benefit_change>0),
                          aes(x = diff_mangrove_area_future_loss*100, y = benefit_change/10^6, color=log10(POP_SSP5/10^6))) +
                          #aes(x = diff_mangrove_area_future_loss*100, y = benefit_change/10^6, color=log10(pop_ssp2/10^6))) +
                          #aes(x = diff_mangrove_area_future_loss*100, y = benefit_change/10^6, color=log10(pop_ssp2/10^6))) +
        geom_point() +
        geom_text_repel(aes(label=countrycode)) +
        scale_y_continuous(trans="log10", labels = comma) +   
        scale_x_continuous(trans="log10", labels = comma) +
        scale_color_scico(palette="devon",begin=0.1,end=0.8, breaks = log_breaks, labels = comma(actual_breaks),direction=-1,
                    guide = guide_colourbar(title.position = "top", title.hjust = 0.1, barwidth = 11)) +
        xlab("Area Gain (ha)") +
        ylab("Benefit Gains (million USD)") + 
        ggtitle("Mangrove Gains in 2100") + 
        theme_bw() + labs(color="Population near Mangroves (millions)")
    

    # losses_2100_pop <- ggplot(data = diff_country_total %>% filter(year == 2100),
    #     #aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log(pop_ssp2))) +
    #     aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log(pop_ssp5))) +
    #     geom_point(alpha=0.5)+
    #     geom_text_repel(aes(label=countrycode))+
    #     scale_y_continuous(trans="log10", labels = comma) +   
    #     scale_x_continuous(trans="log10", labels = comma) +
    #     scale_color_scico(palette="lajolla",begin=0.2,end=0.9)+
    #     xlab("Area Loss (ha)")+
    #     ylab("Benefit Losses (million USD)") + ggtitle("Mangrove Losses in 2100") + theme_bw()
    # losses_2100_pop
    
    ggarrange(BenefitChange_2100_2types,ggarrange(losses_2100_pop,gains_2100_pop,common.legend=T,ncol=2,legend="bottom"),ncol=1,heights=c(3.5,4))
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Benefit_Projection_pop_SSP370.png",dpi=600)
    #ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Benefit_Projection_pop.png",dpi=600)

    # losses_2100 <- ggplot(data = diff_country_total %>% filter(year == 2100),
    #     aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=R5)) +
    #     geom_point(alpha=0.5)+
    #     geom_text_repel(aes(label=countrycode))+
    #     scale_y_continuous(trans="log10", labels = comma) +   
    #     scale_x_continuous(trans="log10", labels = comma) +
    #     scale_color_scico_d(palette="batlow",end=0.8)+
    #     xlab("Area Loss (ha)")+
    #     ylab("Benefit Losses (million USD)") + ggtitle("Mangrove Losses in 2100") + theme_bw()

    # gains_2100 <-ggplot(data = diff_country_total %>% filter(year == 2100),
    #     aes(x = diff_mangrove_area_future_loss*100, y = benefit_change/10^6, color=R5)) +
    #     geom_point(alpha=0.5)+
    #     geom_text_repel(aes(label=countrycode))+
    #     scale_y_continuous(trans="log10", labels = comma) +   
    #     scale_x_continuous(trans="log10", labels = comma) +
    #     scale_color_scico_d(palette="batlow",end=0.8)+
    #     xlab("Area Gain (ha)")+
    #     ylab("Benefit Gains (million USD)") + ggtitle("Mangrove Gains in 2100") + theme_bw()

        
    # ggarrange(BenefitChange_2100_2types,ggarrange(losses_2100,gains_2100,legend=FALSE,ncol=2),ncol=1)
    # ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Benefit_Projection.png",dpi=600)

# 3. Plot the transformed data using those breaks


load(file="Data/projections/world_ne_with_coeffs_sspAll_RCP70.Rds")
#load(file="Data/projections/world_ne_with_coeffs_perc_ssp370.Rds")

log_breaks <- pretty(log10(-world_ne_with_coeffs$coefficient_temp_USD_allSSPs_temp/10^6))[-c(1)]
    actual_breaks <- 10^(log_breaks)

gg <- ggplot(data = world_ne_with_coeffs) +
  geom_sf(aes(fill = log10(-coefficient_temp_USD_allSSPs_temp/10^6))) +
  scale_fill_scico(palette = "lajolla", breaks = log_breaks, 
    labels = comma(actual_breaks),na.value="transparent",
    begin=0.1,end=0.9) +
  labs(fill = "Losses \n(Million USD/yr/C)")+
        geom_sf(data= world_ne_with_coeffs%>% filter(coefficient_temp_USD_allSSPs_temp>0), fill="darkcyan") +
  theme_minimal()+
        coord_sf(crs = "+proj=robin", ylim = c(-50*10^5, 39*10^5))
    gg
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Benefit_USD_MAP_SSP370_Brander.png",dpi=600)

world_ne_with_coeffs%>% filter(coefficient_temp_USD_allSSPs_temp>0) 
gg_perc <- ggplot(data = world_ne_with_coeffs) +
  geom_sf(aes(fill = coefficient_temp_percGDP_allSSPs_temp)) +
  scale_fill_scico(palette = "vik", 
                   #begin = 0.1,
                   #end = 0.9,
                   direction=-1,
                   midpoint=0,
                   limits = c(-0.05, 0.002), # Set your limits here
                   oob = scales::squish)+ # Use squish function for out of bounds
  labs(fill = "Benefit Change \n(%GDP/C)") +
  theme_minimal() +
  coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))

    gg_perc
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Damage_perc_MAP_SSPsAll_Brander.png",dpi=600)


    glimpse(world_ne_with_coeffs)
    coefs <- data.frame(rbind(world_ne_with_coeffs %>% select(iso_a3,coefficient_temp_percGDP_SSP1) %>% mutate(scenario="SSP1") %>% rename("coef"="coefficient_temp_percGDP_SSP1"),
                                world_ne_with_coeffs %>% select(iso_a3,coefficient_temp_percGDP_SSP2) %>% mutate(scenario="SSP2") %>% rename("coef"="coefficient_temp_percGDP_SSP2"),
                                world_ne_with_coeffs %>% select(iso_a3,coefficient_temp_percGDP_SSP3) %>% mutate(scenario="SSP3") %>% rename("coef"="coefficient_temp_percGDP_SSP3"),
                                world_ne_with_coeffs %>% select(iso_a3,coefficient_temp_percGDP_SSP4) %>% mutate(scenario="SSP4") %>% rename("coef"="coefficient_temp_percGDP_SSP4"),
                                world_ne_with_coeffs %>% select(iso_a3,coefficient_temp_percGDP_SSP5) %>% mutate(scenario="SSP5") %>% rename("coef"="coefficient_temp_percGDP_SSP5")))


    ggplot(data = coefs) +
        geom_violin(aes(x=scenario,y = coef)) 



    # ggarrange(gg,ggarrange(losses_2100_pop,gains_2100_pop,common.legend=T,ncol=2,legend="bottom"),ncol=1,heights=c(2,4))
    # ggarrange(ggarrange(losses_2100_pop,gains_2100_pop,common.legend=T,ncol=2,legend="bottom"),gg,ncol=1,heights=c(4,2))
    # ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Benefit_Projection_pop_MAP.png",dpi=600)
## Benefit Losses (end)

## Benefits Mexico
    benefit_ssp <- read.csv(file = "Results/Benefits/benefit_ssp.csv")
    ggplot(benefit_ssp %>% filter(countrycode=="MEX") %>%filter(year %in% c(2020+seq(1:30)*5))) + 
    geom_point(aes(x=temp,y=benefit_change_perGDP,col=log(GDP/POP),size=POP/10^6,shape=scenario.x),alpha=0.9)+
    scale_color_scico(begin=0.3) + theme_bw() + 
    labs(title="Climate Impacts on Benefits Provided by Mangroves in Mexico",x = "Temperature Change (C)",size="Population \n(million)", y = "Benefit Change (%GDP)",shape="Scenario",color="Log GDPpc")
    ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\Benefits_Mexico.png",dpi=600)
## Benefits Mexico

######## and by Region

## Supplementary
load(file="Data/output/effects_threshold.Rds")
plot_effects <- ggplot()+
        geom_point(data=results_f %>% filter(Variable=="hot" & hot_threshold<34),aes(x=hot_threshold,y=100*Coefficient))+
        geom_errorbar(data=results_f %>% filter(Variable=="hot"& hot_threshold<34),aes(x=hot_threshold,ymin=(100*Coefficient-100*SE*1.64),ymax=(100*Coefficient+100*SE*1.64)),col=pal_lapaz[1])+
        geom_text(data=results_f %>% filter(Variable=="hot"& hot_threshold==31) %>% slice(1),aes(x=hot_threshold,y=(100*Coefficient-100*SE*1.64*3.5),label=co),angle=90)+
        geom_point(data=results_f %>% filter(Variable=="cold" & cold_threshold>12) ,aes(x=cold_threshold,y=100*Coefficient))+
        geom_errorbar(data=results_f %>% filter(Variable=="cold"& cold_threshold>12),aes(x=cold_threshold,ymin=(100*Coefficient-100*SE*1.64),ymax=(100*Coefficient+100*SE*1.64)),col=pal_lapaz[4])+
        geom_text(data=results_f %>% filter(Variable=="cold"& cold_threshold==18) %>% slice(1),aes(x=cold_threshold,y=(100*Coefficient-100*SE*1.64*3),label=co),angle=90)+
        coord_cartesian(xlim=c(13,33)) + theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+ylab("Effect of One Additional Day\n Below or Above Threshold (percent points)")+xlab("Temperature Threshold")
        
        plot_effects
        ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\BinEffects.png",dpi=600)
