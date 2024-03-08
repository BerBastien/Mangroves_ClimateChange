libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe")

lapply(libraries, library, character.only = TRUE)

setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
mcn$GDPpc_country <- (mcn$GDP_country/mcn$Population_country)
subset_mcn <- mcn[,c("b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", "b36_37C")]
mcn$hot_bin <- rowSums(subset_mcn, na.rm = TRUE)
subset_mcn <- mcn[,c("b8_9C", "b9_10C", "b10_11C", "b11_12C", "b12_13C", "b13_14C","b14_15C","b15_16C","b16_17C","b17_18C")]
mcn$cold_bin <- rowSums(subset_mcn, na.rm = TRUE)
mcn$logGDPpc_country <- log(mcn$GDPpc_country)
mcn_2020 <- mcn %>% filter(year == 2020)

# df_f <- mcn_country %>% filter(mangrove_area>0 & 
#                                 #year > 2014 & 
#                                 is.finite(logGDPpc) & 
#                                 abs(Latitude)>20)
# ggplot(df_f,aes(x=log(mangrove_area),y=hot_bin))+geom_line(aes(group=countrycode))

# summary(felm(log(mangrove_area) ~   #temp*preci+
#                                     #sst_hottest+I(sst_hottest^2)+
#                                     #preci+I(preci^2)+
#                                     #temp + I(temp^2)+
#                                     mcw_int + I(mcw_int^2) +
#                                     logGDPpc + logPop
#                                     #mcw_freq + I(mcw_freq^2) 
#                                     #year+I(year^2)
#                                     |countrycode+year|0|countrycode,data=df_f))


df_f <- mcn %>% filter(
                               
                                mean_mangrove_area>1 & 
                                lag_holes>0 & 
                                #np>0 &
                                #lag_gap_density>0 & 
                                #gap_density>0 &
                                #year > 2014 & 
                                is.finite(logGDPpc_country) &
                                #is.finite(pafrac) &
                                #is.finite(lag_gap_density) &
                                #gap_density <50 &
                                holes_size>0 &
                                is.finite(logGDPpc)# & 
                                #is.finite(mcw_int)
                                #abs(Latitude)>20
                                )
df_f$abs_lat <- abs(df_f$Latitude)
#glimpse(df_f)
#ggplot(df_f,aes(x=gap_density,y=log(mangrove_area)))+geom_point()
#ggplot(df_f,aes(x=hot_bin,y=log(mangrove_area)))+geom_point()
#ggplot(df_f,aes(x=cold_bin,y=log(mangrove_area)))+geom_point()
#ggplot(df_f,aes(x=cold_bin+hot_bin,y=log(mangrove_area)))+geom_point()
#ggplot(df_f,aes(x=cold_bin,y=hot_bin))+geom_point()
    # ggplot(df_f,aes(x=log(np),y=log(mangrove_area)))+
    # geom_line(aes(group=gridcell_id))+
    # geom_point(data=df_f %>% filter(year==2007 | year==2020),aes(x=log(np),y=log(mangrove_area),col=factor(year)))+
    # theme_bw()

    # ggplot(df_f,aes(x=log(np/mangrove_area),y=log(mangrove_area)))+
    # geom_line(aes(group=gridcell_id))+
    # geom_point(data=df_f %>% filter(year==2007),aes(x=log(np/mangrove_area),y=log(mangrove_area)))



    # ggplot(df_f,aes(x=log(holes/exp(mean_mangrove_area)),y=log(mangrove_area)))+
    # geom_line(aes(group=gridcell_id))+
    # geom_point(data=df_f %>% filter(year==2007),aes(x=log(holes/exp(mean_mangrove_area)),y=log(mangrove_area)))

     ggplot(df_f,aes(x=log(holes_size),y=log(mangrove_area)))+geom_point()
    # df_f$patch_density <-  df_f$np / df_f$mangrove_area

    # ggplot(df_f %>% filter(year==2020  & 
    #                         mangrove_area > 1
    #                         ))+
    # geom_histogram(aes(patch_density,group=year,fill=year))+#xlim(c(0,100))+
    # theme_bw()


    # ggplot(df_f %>% filter(year==2020  & 
    #                         mangrove_area > 1
    #                         ))+
    # geom_histogram(aes(np,group=year,fill=year))+#xlim(c(0,100))+
    # theme_bw()

    # df_f %>% filter(year==2020  & 
    #                         mangrove_area > 1
    #                         ) %>% select(np) %>% filter(is.finite(np)) %>% mean()

 
model_area1 <- felm(log(mangrove_area) ~   #temp*preci+
                                    sst_hottest+I(sst_hottest^2)+
                                    preci+I(preci^2)+#:I(temp^2)+
                                    #temp + I(temp^2)+
                                    #abs_lat:mcw_int + #I(mcw_int^2) +
                                    #I(Latitude^2):
                                    #mcw_int+ #logPop + I(logPop^2)+
                                    logGDPpc + I(logGDPpc^2) + #logPop + 
                                    #logGDPpc*rich + #logPop + 
                                    logGDPpc_country+I(logGDPpc_country^2)+
                                    #countrycode:year + countrycode:I(year^2)
                                    #mcw_freq + I(mcw_freq^2) 
                                    #year+I(year^2)
                                    #I(logGDPpc^2) + 
                                    #pafrac + 
                                    #I(log(np/mean_mangrove_area))+
                                    #I(holes/mean_mangrove_area) +
                                    #holes_size+ I(holes_size^2)+
                                    lag_gap_density_avg + I(lag_gap_density_avg^2)#+
                                    #gap_density+
                                    #lag_gap_density + I((lag_gap_density)^2)#+
                                    #lag_gap_density #+ I(lag_gap_density^2)#+
                                    #lag2_gap_density + #I(lag2_gap_density^2)+
                                    #lag3_gap_density  #I(lag3_gap_density^2)+
                                    #+ I(gap_density^2)
                                    #income:year + income:I(year^2)
                                    |gridcell_id  +  year|0|gridcell_id,data=df_f)
            summary(model_area1)



            data <- df_f
            model <- model_area1
            filename <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Area_Model\\Experiments\\Diagnostic_Model1_logArea_gap2_logmangroveareaPOS.png"
            variable <- log(df_f$mangrove_area)
            #variable <- subset_mcn1$annual_area_change
            #model_diagnostics(model, data, variable)
            adjust_and_plot(model, data, variable, filename,save=FALSE)
            residuals <- resid(model)

            
            t_sst <- sqest(data, model, "sst_hottest", "model_area1")
            t_laggap <- sqest(data, model, "lag_gap_density_avg", "model_area1")
            t_preci <- sqest(data, model, "preci", "model_area1")
            t_logGDPpc <- sqest(data, model, "logGDPpc", "model_area1")
            t_holessize <- sqest(data, model, "holes_size", "model_area1")
            t_logGDPpc_country <- sqest(data, model, "logGDPpc_country", "model_area1")

            Models_plotline_logGDPpc_country <- ggplot(t_logGDPpc_country)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log Country GDP per capita**")+ theme_bw()
                
            Models_plotline_logGDPpc <- ggplot(t_logGDPpc)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log Local GDP per capita**") + theme_bw()

            blank_plot <- ggplot()+geom_blank()+theme_void()
            

            # Extracting the coefficients from the model
            coefs <- coef(model_area1)

            # Computing the combined marginal effect
            df_f$marginal_effect_GDP <- coefs["logGDPpc"]  + 
                2 * coefs["I(logGDPpc^2)"] * df_f$logGDPpc + 
                coefs["logGDPpc_country"]  + 
                2 * coefs["I(logGDPpc_country^2)"] * df_f$logGDPpc_country 

            Models_xy_logGDPpc <- ggplot(df_f, aes(x = logGDPpc, y = logGDPpc_country)) +
                geom_point(aes(color = marginal_effect_GDP)) +
                #geom_line(aes(group=gridcell_id,color = marginal_effect)) +
                geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
                scale_color_scico(palette = "vikO", midpoint = 0, direction = -1, 
                                guide = guide_colorbar(direction = "horizontal",title.position = "top", title.hjust = 0.5)) +
                theme_bw()+
                xlab("")+
                ylab("Log Country GDP per capita**")+
                labs(color="Marginal Effect \nof 1% increase")+
                geom_text_repel(data= df_f %>% filter(year==2020) %>% group_by(iso2c) %>% arrange(desc(logGDPpc)) %>% slice(1),aes(label=iso2c),size=2) +
                theme(legend.title.align = 0,
                    legend.position = c(0.69, 0.05),  # Adjust these values to position the legend where you prefer
                    legend.justification = c(0, 0), 
                    legend.background = element_rect(fill = "transparent", colour = NA))  # Removes background and border

            Models_xy_logGDPpc
            leg <-  get_legend(Models_xy_logGDPpc)   

            ggarrange(ggarrange(Models_xy_logGDPpc,Models_plotline_logGDPpc_country+coord_flip()+ scale_y_reverse(),ncol=2,widths=c(3,1)),
                ggarrange(Models_plotline_logGDPpc,blank_plot,ncol=2,widths=c(3,1)),nrow=2,heights=c(3,1))

            ggsave("Figures/Sept/Model_Area_GDPEffect.png",dpi=600)


            mcn_2020 <- df_f %>% filter(year==2020)
            scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all.csv")
            gridcells_2020 <- unique(df_f$gridcell_id)
            scen2100 <- scen %>% filter(year==2100 & gridcell_id %in% gridcells_2020) 
            scen2020 <- scen %>% filter(year==2025)
            sum((scen2020$Pop_Country_ssp2),na.rm=TRUE)
            sum((scen2020$GDP_Country_ssp5),na.rm=TRUE)
            sum(mcn_2020$GDP_country)
            sum(mcn_2020$Population_country)
            scen_2100 <- merge(scen2100,mcn_2020,by="gridcell_id",suffixes=c("2100","2020"),all=F)
            glimpse(scen_2100)
            glimpse(mcn_2020)
            
            
            histogram_plot_gdp <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        geom_density(data=scen_2100,aes(log(gdp_ssp5/pop_ssp5)),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
            scen_2100$logGDPpc_c <-log((scen_2100$GDP_Country_ssp5 * 10^9) / (scen_2100$Pop_Country_ssp5 * 10^6))
            
            histogram_plot_gdp_country <- ggplot(mcn_2020, aes(x = logGDPpc_country)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        geom_density(data=scen_2100,aes(logGDPpc_c),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()

            max(scen_2100$logGDPpc_c,na.rm=T)
            max(scen_2100$logGDPpc,na.rm=T)
            glimpse(mcn_2020)
            length(scen_2100$logGDPpc_c[which(!is.na(scen_2100$logGDPpc_c))])
            length(mcn_2020$logGDPpc_country[which(!is.na(mcn_2020$logGDPpc_country))])

            

            combined_plot <- ggarrange(ggarrange(Models_xy_logGDPpc+ylim(c(6,12.7))+ xlim(c(2,15))+ylab("Country-level Log GDPpc** \n"),
                                Models_plotline_logGDPpc_country + coord_flip()+ scale_y_reverse()+ xlim(c(6,12.7))+xlab(""),
                                histogram_plot_gdp_country + coord_flip() + scale_y_reverse() + xlim(c(6,12.7)),
                                    ncol=3,widths=c(5,2,1)),
                    ggarrange( ggarrange(Models_plotline_logGDPpc+ xlim(c(2,15))+xlab("Local Log GDPpc**"),histogram_plot_gdp+ xlim(c(2,15)),ncol=1,heights=c(3,1)),
                                blank_plot,
                                blank_plot,
                                ncol=3,widths=c(5,2,1)),nrow=2,heights=c(2,1))  

            annotate_figure(combined_plot, top = text_grob("Effect of 1% GDP per capita Increase on Mangrove Area (Percent Points)"))

            #ggsave("Figures/Sept/Model_Area_GDPEffect_full.png",dpi=600)

### Hotter and Dryer


df_f$marginal_effect_hotterdryer <- coefs["sst_hottest"]  + 
                        2 * coefs["I(sst_hottest^2)"] * df_f$sst_hottest - 
                        coefs["preci"]  - 
                        2 * coefs["I(preci^2)"] * df_f$preci
                        
                        
#glimpse(df_f)
#library("ggrepel")
Models_xy_sstpreci <- ggplot(df_f, aes(x = preci, y = sst_hottest)) +
    geom_point(aes(color = marginal_effect_hotterdryer)) +
    #geom_line(aes(group=gridcell_id,color = marginal_effect)) +
    #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    scale_color_scico(palette = "vikO", midpoint = 0, direction = -1, 
                                guide = guide_colorbar(direction = "horizontal",title.position = "top", title.hjust = 0.5)) +
    theme_bw()+
    labs(color="Marginal Effect of \nWarmer and Dryer Climate")+
    xlab("Monthly Precipitation (mm)")+
    ylab("SSt of the Hottest Month (C)")+
    geom_text_repel(data= df_f %>% filter(year==2020) %>% group_by(iso2c) %>% arrange(desc(sst_hottest)) %>% slice(1),aes(label=iso2c),size=2) +
    theme(legend.title.align = 0,
                    legend.position = c(0.5, 0.05),  # Adjust these values to position the legend where you prefer
                    legend.justification = c(0, 0), 
                    legend.background = element_rect(fill = "transparent", colour = NA))  # Removes background and border


            glimpse(scen_2100)
            histogram_plot_sst <- ggplot(mcn_2020, aes(x = sst_hottest)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        geom_density(data=scen_2100,aes(sst_hot_85),color="indianred",alpha = .2, size=1.3) +
                        theme_void()
            
            
            histogram_plot_preci <- ggplot(mcn_2020, aes(x = preci)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        geom_density(data=scen_2100,aes(preci_85),color="indianred",alpha = .2, size=1.3) +
                        theme_void()

            max(scen_2100$logGDPpc_c,na.rm=T)
            max(scen_2100$logGDPpc,na.rm=T)
            glimpse(mcn_2020)
            length(scen_2100$logGDPpc_c[which(!is.na(scen_2100$logGDPpc_c))])
            length(mcn_2020$logGDPpc_country[which(!is.na(mcn_2020$logGDPpc_country))])

            Models_plotline_sst <- ggplot(t_sst)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log Country GDP per capita**")+ theme_bw()
                
            Models_plotline_preci <- ggplot(t_preci)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log Local GDP per capita**") + theme_bw()

            combined_plot <- ggarrange(ggarrange(Models_xy_sstpreci+ylim(c(17,37))+ xlim(c(0,22))+ylab("SST of hte Hottest Month (C) \n \n"),
                                Models_plotline_sst + coord_flip()+ scale_y_reverse()+ xlim(c(17,37))+xlab(""),
                                histogram_plot_sst + coord_flip() + scale_y_reverse() + xlim(c(17,37)),
                                    ncol=3,widths=c(5,2,1)),
                    ggarrange( ggarrange(Models_plotline_preci+ xlim(c(2,15))+xlab("Monthly Precipitation"),
                                ggarrange(blank_plot,histogram_plot_preci+ xlim(c(2,15)),ncol=2,widths=c(1,5)),ncol=1,heights=c(3,1)),
                                blank_plot,
                                blank_plot,
                                ncol=3,widths=c(5,2,1)),nrow=2,heights=c(2,1))  
            combined_plot

            annotate_figure(combined_plot, top = text_grob("Effect of 1C Warmer and 1mm Dryer on Mangrove Area (Percent Points)"))

    ggsave("Figures/Sept/Model_Area_WarmerDryerEffect_full.png",dpi=600)




    glimpse(mcn_2020)
    glimpse(scen_2100)


    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all.csv")
                mcn <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                mcn2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("gridcell_id","Mean_Salinity"))]
                glimpse(scen)
                load("Models/Round1/pref_holes_model.RData")
                summary(model_holes_ssthot)
                coef_sal <- summary(model_holes_ssthot)$coef[3]

               
                
                ## All forcings  (start)
                    scen$gaps_per_area <-  (scen$perc_inc_sal*coef_sal)

                    scen$gaps_per_area <- ifelse(is.na(scen$gaps_per_area), 0, scen$gaps_per_area)
                        
                    
                    gaps_per_area <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(gaps_per_area_cumulative = cumsum(gaps_per_area)) %>%
                            ungroup() 
                    
                    num_levels <- length(unique(gaps_per_area$R5))

                    # Create a vector of colors from the scico palette
                    color_vector <- scico(n = num_levels, palette = "batlow")
                    gaps_per_area_2100 <- gaps_per_area %>% filter(year==2100)
                   
                    #ggsave("Figures/Draft/GAPS_Projeciton_RCP85.png",dpi=600)

                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("holes","gridcell_id","mangrove_area"))]
                    sum(mcn_2020$holes,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("holes","gridcell_id","mangrove_area"))]
                    sum(mcn_1996$holes,na.rm=T)

                    gaps_per_area_scen<- merge(gaps_per_area,mcn_2020,by=c("gridcell_id"),all=T)
                    gaps_per_area_scen$gaps_per_area2020 <- gaps_per_area_scen$holes / gaps_per_area_scen$mangrove_area
                    glimpse(gaps_per_area_scen)
                    max(gaps_per_area_scen$gaps_per_area2020[is.finite(gaps_per_area_scen$gaps_per_area2020)],na.rm=TRUE)
                    gaps_per_area_scen <- gaps_per_area_scen[which(is.finite(gaps_per_area_scen$gaps_per_area2020) & !is.na(gaps_per_area_scen$gaps_per_area2020)),]
                    gaps_per_area_scen$gaps_per_area_future <- gaps_per_area_scen$gaps_per_area2020 *(0.01*gaps_per_area_scen$gaps_per_area_cumulative)
                    gaps_per_area_scen$gaps_per_area_future2 <- gaps_per_area_scen$gaps_per_area2020 + (gaps_per_area_scen$gaps_per_area_cumulative)
                    #scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    gaps_per_area_scen$loss <- 1
                    gaps_per_area_scen$loss[which(gaps_per_area_scen$gaps_per_area_future<0)] <- 0
                    
                    agg_aloss_neg_gaps <- aggregate(gaps_per_area_future~year+loss+R5,data=gaps_per_area_scen,FUN="median",na.rm=TRUE)
                    num_levels <- length(unique(agg_aloss_neg_gaps$R5))
                    agg_aloss_neg_gaps$color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    glimpse(gaps_per_area_scen)


    Models_plotline_laggap <- ggplot(t_laggap)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1 Gap per km Increase")+xlab("Gap Density (Gaps per Km2)***")+ theme_bw()
    Models_plotline_laggap

     glimpse(gaps_per_area_scen)
     gaps_per_area_scen2100 <- gaps_per_area_scen %>% filter(year==2100 & gridcell_id %in% unique(df_f$gridcell_id))
     gaps_per_area_scen2100$gaps_per_area_future2[which(gaps_per_area_scen2100$gaps_per_area_future2>100)] <- 100
     histogram_plot_laggap <- ggplot(df_f, aes(x = lag_gap_density_avg)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f",binwidth=3) +
                        geom_density(data=gaps_per_area_scen2100,aes(gaps_per_area_future2),color="indianred",alpha = .2, size=1.3) +
                        xlim(0,110)+ylim(0,0.11)+theme_void()
    histogram_plot_laggap 

    ggarrange(Models_plotline_laggap+
                        xlim(0,110),
                        ggarrange(blank_plot,histogram_plot_laggap,ncol=2,widths=c(1,10)),
                        ncol=1,heights=c(2,1))
    ggsave("Figures/Sept/Model_Area_Gaps.png",dpi=600)



                    
Models_plotline_holessize<- ggplot(t_holessize)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log Country GDP per capita**")+ theme_bw()
    Models_plotline_holessize

    histogram_plot_holessize <- ggplot(mcn_2020, aes(x = holes_size)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(data=scen_2100,aes(preci_85),color="indianred",alpha = .2, size=1.3) +
                        theme_void()











# Create QQ plot
qqnorm(residuals)
qqline(residuals)


##Plots SST Hot-
        qgdp <- quantile(mcn$logGDPpc_country[which(mcn$mangrove_area>0 & mcn$year==2018)],0.66,na.rm=T)
        crich <- unique(mcn$countrycode[which(mcn$logGDPpc_country>qgdp)])
        mcn$rich <- 0
        mcn$rich[which(mcn$countrycode %in% crich)] <- 1
        mcn$logPop <- log(mcn$Population_Count)
    

        model_area_ssthot <- felm(I(-log(mangrove_area))~
            sst_hottest + I(sst_hottest^2)+
            Mean_Precipitation +I(Mean_Precipitation^2) + 
                logGDPpc+I(logGDPpc^2)+
                year:countrycode + 
                R5:year + 
                income:year
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],
        #weights=(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc))])
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc))]+1)
        )
        
        summary(model_area_ssthot)

