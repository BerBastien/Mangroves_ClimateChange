## Make Projections
library("ggplot2")
library("ggpubr")
library("scico")
library("jtools")
library("scales")

## Read Data (start)
    setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")

    mcn$logSal <- log(mcn$Mean_Salinity)
                scen$logGDPpc <- log(scen$gdp_ssp5/scen$pop_ssp5)
                scen$logPop <- log(scen$pop_ssp5)
                #scen$preci2 <- scen$Mean_Precipitation^2
                #scen$temp2 <- scen$temp^2
                #scen$preci_temp <- scen$temp*scen$Mean_Precipitation
                #scen$pop_preci <- scen$logPop*scen$Mean_Precipitation
                #scen$gdp_preci <- scen$logGDPpc*scen$Mean_Precipitation
                scen$pop_sst <- scen$logPop*scen$sst_85
                scen$gdp_sst <- scen$logGDPpc*scen$sst_85
                scen$gdp_sst_hot <- scen$logGDPpc*scen$sst_hot_85
                scen$pop_sst_hot <- scen$logPop*scen$sst_hot_85
                scen$sst <- scen$sst_85
                scen$sst_hottest <- scen$sst_hot_85
                scen$sst_hot <- scen$sst_hot_85
                mcn_2020 <- mcn %>% filter(year==2020)
                scen_2100 <- scen %>% filter(year==2100)

    
## Read Data (end)

## Plot before and after (start)
    ## Distribution Plots 
        ## Gaps
            #load data
                
                load("Models/Round1/sq_estimate_sst_holes.RData")
                load("Models/Round1/sq_estimate_sst_holes.RData") 
                load("Models/Round1/sq_estimate_sst_holes.RData") 
                load("Models/Round1/sq_estimate_sst_holes.RData") 
                load("Models/Round1/sq_estimate_sst_holes.RData") 
                load("Models/Round1/sss_available.RData")


                ## Project Salinity (Cond4)
                    glimpse(scen)
                    scen$logGDPpc <- log(scen$gdp_ssp5/scen$pop_ssp5)
                    scen$logPop <- log(scen$pop_ssp5)
                    #scen$preci2 <- scen$Mean_Precipitation^2
                    #scen$temp2 <- scen$temp^2
                    #scen$preci_temp <- scen$temp*scen$Mean_Precipitation
                    #scen$pop_preci <- scen$logPop*scen$Mean_Precipitation
                    #scen$gdp_preci <- scen$logGDPpc*scen$Mean_Precipitation
                    scen$pop_sst <- scen$logPop*scen$sst_85
                    scen$gdp_sst <- scen$logGDPpc*scen$sst_85
                    scen$gdp_sst_hot <- scen$logGDPpc*scen$sst_hot_85
                    scen$pop_sst_hot <- scen$logPop*scen$sst_hot_85
                    scen$sst <- scen$sst_85
                    scen$sst_hottest <- scen$sst_hot_85
                    scen$temp <- scen$temp_85
                    scen$Mean_Precipitation <- scen$preci_85
                    scen$preci2 <- scen$Mean_Precipitation^2
                    scen$temp2 <- scen$temp^2
                    scen$preci_temp <- scen$temp*scen$Mean_Precipitation
                    scen$pop_preci <- scen$logPop*scen$Mean_Precipitation
                    scen$gdp_preci <- scen$logGDPpc*scen$Mean_Precipitation
                    
                    variables_of_interest_available4 <- c("pop_sst_hot","gdp_sst_hot","gdp_sst","pop_sst","gridcell_id","pop_preci",
                                                            "preci_temp","gdp_preci","Mean_Salinity","sst","sst_hottest","temp","logGDPpc",
                                                            "logPop","Latitude","Longitude","Mean_Precipitation","year") 
                    #variables_of_interest_available4_noint <- c("gridcell_id","Mean_Salinity","sst","sst_hottest","temp","logGDPpc","logPop","Latitude","Longitude","Mean_Precipitation") 
                    #variables_of_interest_available <- c("pop_sst_hot","gdp_sst_hot","gdp_sst","pop_sst","gridcell_id","Mean_Salinity","sst","sst_hottest","logGDPpc","logPop","Latitude","Longitude","year","gridcell_id") 

                    pred_data <- scen[which(names(scen)%in%c(variables_of_interest_available4))]
                    pred_data <- pred_data[which(is.finite(pred_data$logGDPpc)),]

                    load("sss_available4.RData") 
                    glimpse(pred_data)
                    pred_data$Mean_Salinity <- predict(sss.rf, newdata = pred_data)
                    pred_data <- pred_data[,which(names(pred_data) %in% c("Mean_Salinity","gridcell_id","year"))]
                    scen <- merge(scen,pred_data,by=c("gridcell_id","year"))
                    glimpse(scen)
                    scen_2100 <- scen %>% filter(year==2100)

                    ggplot(scen)+geom_point(aes(y=Mean_Salinity,x=logGDPpc))
                    library("lfe")
                    summary(felm(Mean_Salinity~year|gridcell_id|0|0,data=scen))

                    # val_plot <- ggplot(pred_data, aes(x = year, y = predicted)) +
                    # geom_point() +
                    # geom_abline(slope = 1, intercept = 0, color = "red") +
                    # labs(x = "year", y = "Predicted Salinity (sst)", title = "Prediciton From Random Forest Model")+theme_bw()

                    # val_plot             
                ## Project Salinity (end)


                ## Project Salinity (FE)
                    
                    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
                    glimpse(scen)
                                    glimpse(mcn)

                    sub_mcn <- mcn
                    sub_mcn$preci2 <- sub_mcn$Mean_Precipitation^2
                    sub_mcn$temp2 <- sub_mcn$temp^2
                    sub_mcn$preci_temp <- sub_mcn$temp*sub_mcn$Mean_Precipitation
                    sub_mcn$pop_preci <- sub_mcn$logPop*sub_mcn$Mean_Precipitation
                    sub_mcn$gdp_preci <- sub_mcn$logGDPpc*sub_mcn$Mean_Precipitation
                    sub_mcn$pop_sst <- sub_mcn$logPop*sub_mcn$sst
                    sub_mcn$gdp_sst <- sub_mcn$logGDPpc*sub_mcn$sst
                    sub_mcn$gdp_sst_hot <- sub_mcn$logGDPpc*sub_mcn$sst_hottest
                    sub_mcn$pop_sst_hot <- sub_mcn$logPop*sub_mcn$sst_hottest
                    
                    variables_of_interest_available4_noint <- c("gridcell_id","Mean_Salinity","sst","sst_hottest","temp","logGDPpc","logPop","Latitude","Longitude","Mean_Precipitation","year") 
                    #variables_of_interest_available4_noint <- c("gridcell_id","sst","sst_hottest","temp","logGDPpc","logPop","Latitude","Longitude","Mean_Precipitation") 
                    v <- variables_of_interest_available4_noint
                    sub_mcn <- sub_mcn[,which(names(sub_mcn) %in% variables_of_interest_available4_noint)]
                    sub_mcn <- sub_mcn[complete.cases(sub_mcn), ] 
                    sub_mcn <- as.data.frame(sub_mcn)
                    #sub_mcn <- sub_mcn[is.finite(sub_mcn$Mean_Salinity),]
                    sub_mcn <- sub_mcn[is.finite(sub_mcn$logGDPpc),]
                    sub_mcn <- sub_mcn[is.finite(sub_mcn$logPop),]
                    glimpse(sub_mcn)

                    grid_means <- aggregate(. ~ gridcell_id, data=sub_mcn, FUN=mean)
                    sub_mcn <- merge(sub_mcn,grid_means,by="gridcell_id",all=T,suffixes=c("","_mean"))
                    
                    glimpse(grid_means)
                    names(grid_means)[c(2:dim(grid_means)[2])] <- paste0(names(grid_means)[c(2:dim(grid_means)[2])],"_mean")
                    scen2 <- merge(scen,grid_means,by="gridcell_id",all=T)
                    glimpse(scen2)                    
                    
                    scen2$sst <- scen2$sst_85 - scen2$sst_mean
                    scen2$sst_hottest <- scen2$sst_hottest - scen2$sst_hottest_mean
                    scen2$temp <- scen2$temp_85 - scen2$temp_mean
                    scen2$logGDPpc <- log(scen2$gdp_ssp5/scen2$pop_ssp5) - scen2$logGDPpc_mean
                    scen2$logPop <- log(scen2$pop_ssp5) - scen2$logPop_mean
                    scen2$Mean_Precipitation <- scen2$preci_85 - scen2$Mean_Precipitation_mean
                    scen2$Longitude <- scen2$Longitude_mean
                    scen2$Latitude <- scen2$Latitude_mean

                    variables_of_interest_available4_noint <- c("gridcell_id","Mean_Salinity","sst","sst_hottest","temp","logGDPpc","logPop","Latitude","Longitude","Mean_Precipitation") 
                    v <- variables_of_interest_available4_noint
                    

                    pred_data <- scen2[which(names(scen2)%in%c(variables_of_interest_available4))]
                    pred_data <- pred_data[which(is.finite(pred_data$logGDPpc)),]

                    load("sss_available4_fe_70p_nooutliers.RData") 
                    glimpse(pred_data)
                    pred_data$Mean_Salinity_change <- predict(sss.rf, newdata = pred_data)
                    pred_data <- pred_data[,which(names(pred_data) %in% c("Mean_Salinity_change","gridcell_id","year"))]
                    scen2 <- merge(scen2,pred_data,by=c("gridcell_id","year"))
                    glimpse(scen2)
                    scen_2100 <- scen2 %>% filter(year==2100)

                    ggplot(scen2)+geom_point(aes(y=(Mean_Salinity_mean+Mean_Salinity_change),x=logGDPpc))

               
                    ggplot(scen2, aes(x = Mean_Precipitation, y = logPop, fill = Mean_Salinity_change)) +
                        geom_tile() +
                        scale_fill_scico(palette="vik", 
                                        #limits = c(-0.3, 0.2), 
                                        #limits = c(-0.3, 0.2), 
                                        limits = c(-0.5, 0.5), 
                                        midpoint=0,direction=1, oob=squish)  + theme_minimal()+
                                        xlab("Mean Precipitation (mm/day)")+
                                        ylab("Log Population")+
                                        labs(fill=guides("Salinity \nChange (pss)\n"))


                    library("lfe")
                    scen2$sal <- scen2$Mean_Salinity_mean+scen2$Mean_Salinity_change
                    summary(felm(sal~year|gridcell_id|0|0,data=scen2))

                    mcn_c <- mcn[,which(names(mcn) %in% c("gridcell_id","countrycode","R5"))]
                    glimpse(mcn_c)
                    mcn_c <- mcn_c[complete.cases(mcn_c),]
                    c1 <- aggregate(countrycode~gridcell_id,FUN="first",data=mcn_c)
                    r1 <- aggregate(R5~gridcell_id,FUN="first",data=mcn_c)
                    glimpse(c1)
                    c2 <- data.frame(gridcell_id=c1$gridcell_id,countrycode=c1$countrycode,R5=r1$R5)
                    scen2 <- merge(c2,scen2,by="gridcell_id",all=T)  
                    summary(felm(sal~year:R5|gridcell_id|0|0,data=scen2))       
                    glimpse(scen)           
                    
                    ggplot(scen2,aes(x=year,y=Mean_Salinity_change))+#
                    geom_point(aes(x=year,y=Mean_Salinity_change,group=gridcell_id,color=R5),alpha=0.3)+
                    #geom_smooth(aes(col=R5))+theme_bw()
                    geom_smooth()+theme_bw()

                    scen2_summary <- scen2 %>%
                        group_by(year) %>%
                        summarize(
                            q01 = quantile(Mean_Salinity_change, 0.0, na.rm = TRUE),
                            q25 = quantile(Mean_Salinity_change, 0.25, na.rm = TRUE),
                            median = quantile(Mean_Salinity_change, 0.50, na.rm = TRUE),
                            q75 = quantile(Mean_Salinity_change, 0.75, na.rm = TRUE),
                            q90 = quantile(Mean_Salinity_change, 1, na.rm = TRUE)
                        )

                    scen_sal_plot <- ggplot(scen2_summary, aes(x = year)) +
                            geom_ribbon(aes(ymin = q01, ymax = q90), alpha = 0.4) +
                            geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4) +
                            geom_line(aes(y = median), color = "black") +
                            theme_bw()+ylim(c(-0.3,0.4))
                    scen_sal_plot

                    glimpse(sub_mcn)    
                    ggplot(sub_mcn,aes(x=year,y=(Mean_Salinity-Mean_Salinity_mean)))+#
                    geom_point()+
                    #geom_point(aes(x=year,y=Mean_Salinity_change,group=gridcell_id,color=R5),alpha=0.3)+
                    #geom_smooth(aes(col=R5))+theme_bw()
                    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 5)) +theme_bw()

                    ggplot(sub_mcn,aes(x=factor(year),y=(Mean_Salinity-Mean_Salinity_mean)))+#
                    geom_boxplot(outlier.shape = NA)+
                    scale_y_continuous(limits = quantile(sub_mcn$y, c(0.1, 0.9)))

                    sub_mcn_summary <- sub_mcn %>%
                        group_by(year) %>%
                        summarize(
                            q01 = quantile(Mean_Salinity - Mean_Salinity_mean, 0.1, na.rm = TRUE),
                            q25 = quantile(Mean_Salinity - Mean_Salinity_mean, 0.25, na.rm = TRUE),
                            median = quantile(Mean_Salinity - Mean_Salinity_mean, 0.50, na.rm = TRUE),
                            q75 = quantile(Mean_Salinity - Mean_Salinity_mean, 0.75, na.rm = TRUE),
                            q90 = quantile(Mean_Salinity - Mean_Salinity_mean, 0.9, na.rm = TRUE)
                        )

                    mcn_sal_plot <- ggplot(sub_mcn_summary, aes(x = year)) +
                            geom_ribbon(aes(ymin = q01, ymax = q90), alpha = 0.4) +
                            geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.4) +
                            geom_line(aes(y = median), color = "black") +
                            theme_bw()+ylim(c(-0.3,0.4))

                    ggarrange(mcn_sal_plot,scen_sal_plot,ncol=2,widths=c(2,5))


                    ggplot(scen2,aes(x=year,y=sal))+#
                    #geom_point(aes(x=year,y=Mean_Salinity_change,group=gridcell_id,color=R5),alpha=0.3)+
                    geom_smooth(aes(col=R5))+theme_bw()
                    #geom_smooth()+theme_bw()

                    glimpse(scen2)

                ## Project Salinity (FE)

                ## Project Salinity (Annual Changes)
                    
                    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
                    glimpse(scen)
                    glimpse(mcn)

                    scen$sst <- scen$sst_85 
                    scen$sst_hottest <- scen$sst_hot_85
                    scen$temp <- scen$temp_85
                    scen$logGDPpc <- log(scen$gdp_ssp5/scen$pop_ssp5)
                    scen$logPop <- log(scen$pop_ssp5)
                    scen$Mean_Precipitation <- scen$preci_85 
                    scen$Longitude <- scen$Longitude
                    scen$Latitude <- scen$Latitude

                    var_annual_change <- c("sst","sst_hottest","temp","logGDPpc","logPop","Mean_Precipitation","Longitude","Latitude") 

                    scen <- scen %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(across(all_of(var_annual_change), ~ . - lag(.), .names = 'change_{.col}'))
                    glimpse(scen)

                    ggplot(scen,aes(x=year,y=logPop))+geom_point()
                    
                    var_annual_change <- c("year","change_sst","change_sst_hottest","change_temp","change_logGDPpc","change_logPop","change_Mean_Precipitation","gridcell_id","Longitude","Latitude") 


                    pred_data <- ungroup(scen[which(names(scen)%in%c(var_annual_change))])
                    pred_data <- pred_data[which(is.finite(pred_data$change_logGDPpc)),]
                    glimpse(pred_data)


                    load("change_sss_70p_nooutliers01.RData") 
                    pred_data$Mean_Salinity_change <- predict(change_sss.rf, newdata = pred_data)
                    pred_data <- pred_data[,which(names(pred_data) %in% c("Mean_Salinity_change","gridcell_id","year"))]
                    scen2 <- merge(scen,pred_data,by=c("gridcell_id","year"))
                    glimpse(scen2)
                    scen_2100 <- scen2 %>% filter(year==2100)
                    glimpse(mcn)
                    mcn_2020 <- mcn %>% filter(year==2020)
                    mcn_2020$Mean_Salinity2020 <- mcn_2020$Mean_Salinity
                    ggplot(scen2)+
                    geom_boxplot(aes(x=factor(year),y=Mean_Salinity_change))

                    scen2$Mean_Salinity_change <- ifelse(is.na(scen2$Mean_Salinity_change), 0, scen2$Mean_Salinity_change)

                    scen3 <- scen2 %>%
                        arrange(gridcell_id, year) %>%
                        group_by(gridcell_id) %>%
                        mutate(Mean_Salinity_Change_cumulative = cumsum(Mean_Salinity_change)) %>%
                        ungroup()


                    scen3_2100<- ungroup(scen3 %>% filter(year==2100))
                    glimpse(scen3_2100)
                    gneg <- scen3_2100$gridcell_id[which(scen3_2100$Mean_Salinity_Change_cumulative<0)]
                    
                    scen3$pos <- 1
                    scen3$pos[which(scen3$gridcell_id %in% gneg)] <- 0
                    years_i <- c(2030,2040,2050,2060,2070,2080,2090,2100)
                    glimpse(scen3)
                    
                    mcn_2020 <- mcn %>% filter(year==2020)
                    mcn_2020$Mean_Salinity2020 <- mcn_2020$Mean_Salinity
                    mcn_2020 <- mcn_2020[,which(names(mcn_2020) %in% c("Mean_Salinity2020","gridcell_id","countrycode","R5"))]
                    scen3 <- merge(scen3,mcn_2020,by="gridcell_id",all=T)
                    
                    
                    scen3$Salinity_perc <- 100*scen3$Mean_Salinity_Change_cumulative/scen3$Mean_Salinity2020
                    glimpse(scen3)
                    scen_sal_85 <- scen3[,which(names(scen3) %in% c("gridcell_id","year","Salinity_perc","Mean_Salinity_Change_cumulative","Mean_Salinity_change"))]
                    glimpse(scen_sal_85)
                    write.csv(scen_sal_85,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_sal_85.csv")
                    
                    plot_proj_salinity <- ggplot(scen3[which(scen3$year %in% years_i),])+
                    #ggplot(scen3)+
                    geom_boxplot(aes(x=factor(year),y=100*Mean_Salinity_Change_cumulative/Mean_Salinity2020,color=factor(pos)))+
                    stat_summary(aes(x=factor(year),y=100*Mean_Salinity_Change_cumulative/Mean_Salinity2020,shape="circle"),fun=mean) +
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    theme_bw()+
                    scale_shape_manual(values=c("circle"),labels=c("Mean"))+ 
                    scale_color_manual(values=c("#20719e","#be6635"),labels=c("Decreasing","Increasing"))+  
                    labs(color = "Trend",shape="")+  
                    guides(color = guide_legend(reverse = TRUE))+xlab("Year")+ylab("Salinity Change from 2020 (%)")
                    plot_proj_salinity

                    load(file="Models/Round1/pref_holes_model.RData") 

                    coefs_pref_holes <- plot_coefs(model_holes_ssthot, ci_level = 0.95,
                        coefs = c("Precipitation"="Mean_Precipitation","SST"="sst","SST2"="I(sst^2)","Log Salinity" = "logSal","Log GDPpc"="logGDPpc","Log Population"="logPop"),
                        colors = "black")+
                        theme_minimal()+xlab("Effect of 1 unit increase")+ylab("")
                    coefs_pref_holes+theme_bw()
                    
                    ggarrange(coefs_pref_holes+ggtitle("Gaps Model"),plot_proj_salinity+ggtitle("Salinity Projection SSP5-85")+theme_minimal(),widths=c(1,2)) #here
                    
                    ggsave("Figures/Draft/Model_Holes_Projection.png",dpi=600)
                     
                    
                ## Project Salinity (Annual Changes)   
                   
                    # val_plot             
            ## Project Salinity (end)
                

            #load data

                Models_ssthot_plot_sal_holes <- ggplot(sq_estimate_sal_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#06215b") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#06215b",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log Sea Surface Salinity (pss)**")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_sal_holes

                
                
                glimpse(scen_2100)
                histogram_plot_sal_holes <- ggplot(mcn_2020, aes(x = logSal)) +
                        #geom_histogram(aes(y = ..density..), colour = "black", fill = "#06215b") +
                        geom_density(aes(y = ..density..), colour = "black", fill = "#06215b") +
                        geom_density(data=scen_2100,aes(log(Mean_Salinity_mean+Mean_Salinity_change)),color="indianred",alpha = .2, size=1.3) +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_sal_holes <- ggarrange(Models_ssthot_plot_sal_holes, histogram_plot_sal_holes,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_sal_holes
                
                Models_ssthot_plot_pop_holes <- ggplot(sq_estimate_pop_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log Population")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_pop_holes
                
            
                histogram_plot <- ggplot(mcn_2020, aes(x = logPop)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_pop_holes <- ggarrange(Models_ssthot_plot_pop_holes, histogram_plot,legend="none",
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_ssthot_plot_pop_holes
                
                Models_ssthot_plot_gdp_holes <- ggplot(sq_estimate_gdp_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log GDP per capita")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_gdp_holes

                histogram_plot_gdp_holes <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_gdp_holes <- ggarrange(Models_ssthot_plot_gdp_holes, histogram_plot_gdp,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_gdp_holes
                
                Models_preci_hot_plot_holes <- ggplot(sq_estimate_preci_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1 mm Increase")+xlab("Monthly Mean Precipitation (mm)")
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                    Models_preci_hot_plot_holes

                histogram_plot_preci_holes <- ggplot(mcn_2020, aes(x = Mean_Precipitation)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_preci_hot_plot_holes<-ggarrange(Models_preci_hot_plot_holes, histogram_plot_preci_holes,legend="none", 
                            ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_preci_hot_plot_holes


                Models_ssthot_plot_holes <- ggplot(sq_estimate_sst_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1°C Increase")+xlab("Annual Air Temperature (°C)")#+
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_holes

                histogram_plot_ssthot_holes <- ggplot(mcn_2020, aes(x = temp)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_ssthot_plot_holes<-ggarrange(Models_ssthot_plot_holes, histogram_plot_ssthot_holes,legend="none", 
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                
                arr_area_loss_plot <- ggarrange(ggarrange(ggarrange(Models_ssthot_plot_holes,
                                    Models_preci_hot_plot_holes,
                                    Models_ssthot_plot_gdp_holes,
                                    Models_ssthot_plot_pop_holes,nrow=2,ncol=2),Models_ssthot_plot_sal_holes,widths=c(3,2),ncol=2),
                                    combined_legend,ncol=1,heights=c(11,1))
                
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Gaps Model", face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n **: p<0.05; ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        )
        ## Gaps

        ## Area Loss

            ## Forcing Projections (start)

                load(file="Models/Round1/sq_estimate_sst_area.RData") 
                load(file="Models/Round1/sq_estimate_preci_area.RData") 
                load(file="Models/Round1/sq_estimate_gdp_area.RData") 
                load(file="Models/Round1/sq_estimate_pop_area.RData") 
                

                # Models_ssthot_plot_pop <- ggplot(sq_estimate_pop_area)+
                #     geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                #     geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                #         theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                #         geom_hline(aes(yintercept=0),linetype="dashed")+
                #         ylab("Effect of 1% Increase")+xlab("Log Population**")
                #         #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                #         #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                #         #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #         #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                # Models_ssthot_plot_pop
                
                # library(ggExtra)
                # # Select the population data for 2020
                # #mcn_2020$logPop
                # histogram_plot <- ggplot(mcn_2020, aes(x = logPop)) +
                #         geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                #         geom_density(data=scen_2100,aes(logPop),color="indianred",alpha = .2, size=1.3) +
                #         #geom_density(alpha = .2, fill = "#FF6666") +
                #         theme_bw() +
                #         xlab("Log Pop") +
                #         ylab("Density") + theme_void()
                # #library(gridExtra)
                # Models_ssthot_plot_pop <- ggarrange(Models_ssthot_plot_pop, histogram_plot,legend="none",
                # ncol = 1,heights=c(3,1),align="hv", hjust=0)

                # Models_ssthot_plot_pop

                Models_ssthot_plot_gdp <- ggplot(sq_estimate_gdp_area)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log GDP per capita***")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_gdp

                histogram_plot_gdp <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        geom_density(data=scen_2100,aes(logGDPpc),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_gdp <- ggarrange(Models_ssthot_plot_gdp, histogram_plot_gdp,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_gdp

                Models_preci_hot_plot_area <- ggplot(sq_estimate_preci_area)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
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
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+xlim(c(18,35))+
                    ylab("Effect of 1°C Increase")+xlab("Mean SST in the Hottest Month (°C)***")#+
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_area

                glimpse(scen_2100)
                histogram_plot_ssthot <- ggplot(mcn_2020, aes(x = sst_hottest)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        geom_density(data=scen_2100,aes(sst_hot),color="indianred",alpha = .2, size=1.3) +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw()+xlim(c(18,35)) +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_ssthot_plot_area<-ggarrange(Models_ssthot_plot_area, histogram_plot_ssthot,legend="none", 
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_ssthot_plot_area


                df_dummy <- data.frame(value = c(rnorm(100), rnorm(100)), Variable = rep(c("Climatic", "Socioeconomic"), each = 100))
                df_dummy_line <- data.frame(value = c(rnorm(100)), Projection = "SSP5-8.5")
                
                dummy_plot <- ggplot() +
                geom_histogram(data=df_dummy, aes(value, fill = Variable),color = "black") +
                scale_fill_manual(values = c("Climatic" = "#e9995c", "Socioeconomic" = "#25625f")) +
                geom_line(data=df_dummy_line,aes(x=value,y=value,color=Projection),size=1.3)+
                scale_color_manual(values = c("SSP5-8.5" = "indianred")) +
                theme_bw()+
                theme(legend.position="bottom",legend.box="vertical", legend.margin=margin())  # Remove all non-data ink+


                dummy_plot

                # Extract the legend
                combined_legend <- get_legend(dummy_plot)

                arr_area_loss_plot <- ggarrange(ggarrange(Models_ssthot_plot_area,
                                    Models_preci_hot_plot_area,
                                    Models_ssthot_plot_gdp,
                                    #Models_ssthot_plot_pop,
                                    ncol=3),
                                    combined_legend,ncol=1,heights=c(4,1))
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Area Loss Model"),#, face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        ) 

                # Printing the annotated figure
                print(annotated_figure)
                ggsave("Figures/Draft/Model_AreaLoss_wProj_v1.png",dpi=600)
            
            ## Forcing Projections (end)

            ## Area Loss Projections (start)
                
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all.csv")
                load("Models/Round1/pref_area_model.RData")
                summary(model_area_ssthot)
                coef_ssth <- summary(model_area_ssthot)$coef[1]
                coef_ssth2 <- summary(model_area_ssthot)$coef[2]
                coef_p <- summary(model_area_ssthot)$coef[3]
                coef_p2 <- summary(model_area_ssthot)$coef[4]
                coef_gdp <- summary(model_area_ssthot)$coef[5]
                coef_gdp2 <- summary(model_area_ssthot)$coef[6]

                glimpse(scen)

                scen$marg_ssth_area <-  coef_ssth + 2*coef_ssth2*scen$sst_hot_85
                hist(scen$marg_ssth_area)
                hist(scen$sst_85[which(scen$year==2100)])
                ggplot(scen)+geom_line(aes(x=year,y=sst_85,group=gridcell_id))
                glimpse(scen)
                
                scen <- scen[which(is.finite(scen$pop_ssp5) & scen$pop_ssp5>10),]
                scen <- scen[which(is.finite(scen$gdppc5) & !is.na(scen$gdppc5)),]
                scen$marg_gdppc_area <-  coef_gdp + 2*coef_gdp2* log(scen$gdppc2)

                hist(scen$marg_gdppc_area)
                ggplot(scen,aes(x=log(gdppc2),y=marg_gdppc_area))+geom_point()

                
                            
                
                ## Only Climate Change (start)
                    scen$arealoss_perc_onlyCC <-  (100*scen$delta_sst_hot*scen$marg_ssth_area)

                    scen$arealoss_perc_onlyCC <- ifelse(is.na(scen$arealoss_perc_onlyCC), 0, scen$arealoss_perc_onlyCC)
                        
                    glimpse(scen)
                    scen_arealoss_perc_onlyCC <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlyCC = cumsum(arealoss_perc_onlyCC)) %>%
                            ungroup() 
                    
                    
                    glimpse(scen_arealoss_perc_onlyCC)
                    
                    library(scico)
                    
                    # Get the number of levels in your factor variable
                    num_levels <- length(unique(scen_arealoss_perc_onlyCC_2100$R5))

                    # Create a vector of colors from the scico palette
                    color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    timeseries_arealoss_onlyCC <-ggplot(scen_arealoss_perc_onlyCC,aes(x=(year),y=arealoss_perc_cumulative_onlyCC))+
                    geom_line(aes(x=(year),y=arealoss_perc_cumulative_onlyCC,group=gridcell_id,color=R5),alpha=0.2)+#+ylim(c(-1,1))+
                    theme_bw()+guides(color="none")+ylim(c(-7.5,20))+
                        scale_color_manual(values = color_vector) + xlab("Year")+ylab("Area Loss (%)")
                    timeseries_arealoss_onlyCC


                    scen_arealoss_perc_onlyCC_2100 <- scen_arealoss_perc_onlyCC %>% filter(year==2100)
                    
                    min(scen_arealoss_perc_onlyCC_2100$arealoss_perc_cumulative_onlyCC)
                    max(scen_arealoss_perc_onlyCC_2100$arealoss_perc_cumulative_onlyCC)
                        
                    
                    
                    boxplot_arealoss_onlyCC <- ggplot(scen_arealoss_perc_onlyCC_2100[which(!is.na(scen_arealoss_perc_onlyCC_2100$R5)),])+
                        geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=arealoss_perc_cumulative_onlyCC), width = 0.2)+
                        geom_boxplot(aes(fill=factor(R5),y=arealoss_perc_cumulative_onlyCC),outlier.shape = NA, width = 0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        theme_void()+ylim(c(-7.5,20))+
                        scale_fill_manual(values = color_vector) +
                        scale_color_manual(values = color_vector)+
                        guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                        #guides(fill="none",color="none")
                    boxplot_arealoss_onlyCC 

                    ggarrange(timeseries_arealoss_onlyCC,boxplot_arealoss_onlyCC,widths=c(3,1),
                                align="hv")

                    #ggsave("Figures/Draft/AreaLoss_Projeciton_RCP85.png",dpi=600)

                    glimpse(scen_arealoss_perc_onlyCC_2100)
                    glimpse(scen_arealoss_perc_onlyCC)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)

                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    glimpse(scen_arealoss_perc_onlyCC)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$loss <- 1
                    scen_arealoss_perc_onlyCC$loss[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss<0)] <- 0
                    
                    scen_arealoss_perc_onlyCC$pos <- 1
                    scen_arealoss_perc_onlyCC$pos[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss>0)] <- 0
                    
                    agg_aloss_neg_onlyCC <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    num_levels <- length(unique(agg_aloss_neg$R5))
                    agg_aloss_neg_onlyCC$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total_onlyCC <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    
                    ggplot(agg_aloss_neg_onlyCC)+
                    geom_bar(aes(x=year,y=mangrove_area_future_loss,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Mangrove Area Loss (km2)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=mangrove_area_future_loss),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    #ggsave("Figures/Draft/Projection_Loss_Gain_RCP85.png",dpi=600)

                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    ggplot(agg_aloss_neg_onlyCC)+
                    geom_bar(aes(x=year,y=100*mangrove_area_future_loss/sum2020 ,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Mangrove Area Loss (%)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future_loss/sum2020),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    ggsave("Figures/Draft/Projection_Loss_Gain_RCP85_percent.png",dpi=600)


                    ## New Figure Pas + Future (start)
                        
                        agg_aloss_pos_onlyCC <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        glimpse(agg_aloss_pos_onlyCC)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_onlyCC)
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996 <- level_2020_loss - agg_aloss_neg_total_onlyCC$mangrove_area_future_loss


                        #bars_sumsum_area_short_past_projection <- 
                        ggplot(agg_aloss_pos_onlyCC)+
                                geom_bar(aes(x=(year),y=-mangrove_area_future_loss,
                                    fill=factor(R5)),stat="identity")+
                                xlab("Year")+ylab("Area Change \n(km2)")+
                                labs(fill=guide_legend("Region"))+
                                #scale_fill_manual(values = color_vector)+
                                geom_hline(aes(yintercept=0),linetype="dashed")+
                                geom_line(data=agg_aloss_neg_total_onlyCC,
                                    aes(x=year,y=area_change_wrt_1996),size=1.5)+
                                theme_bw() + 
                                geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                                aes(x=year,y=cumsum(area_change),color=factor(color)),size=1.5)+
                                scale_x_break(c(1998.8,2006))+ 
                                scale_x_break(c(2010.5,2015))+ 
                                guides(fill=guide_legend(reverse = TRUE))+ 
                                scale_color_manual(values = c(" " = "indianred")) +
                                guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
                    bars_sumsum_area_short
                    
                    ## New Figure (end)
                ## Only Climate Change (end)

                ## Climate Change + GDP (start)
                    scen$arealoss_perc <- (scen$delta_sst_hot*scen$marg_ssth_area)+(scen$perc_inc_gdppc2*scen$marg_gdppc_area)

                    scen$arealoss_perc <- ifelse(is.na(scen$arealoss_perc), 0, scen$arealoss_perc)
                        
                    glimpse(scen)
                    scen_arealoss_perc <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative = cumsum(arealoss_perc)) %>%
                            ungroup() 
                    
                    # Get the number of levels in your factor variable
                    num_levels <- length(unique(scen_arealoss_perc_2100$R5))

                    # Create a vector of colors from the scico palette
                    color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    timeseries_arealoss <-ggplot(scen_arealoss_perc,aes(x=(year),y=arealoss_perc_cumulative))+
                    geom_line(aes(x=(year),y=arealoss_perc_cumulative,group=gridcell_id,color=R5),alpha=0.2)+#+ylim(c(-1,1))+
                    theme_bw()+guides(color="none")+#ylim(c(-7.5,20))+
                        scale_color_manual(values = color_vector) + xlab("Year")+ylab("Area Loss (%)")



                    scen_arealoss_perc_2100 <- scen_arealoss_perc %>% filter(year==2100)
                    
                    min(scen_arealoss_perc_2100$arealoss_perc_cumulative)
                    max(scen_arealoss_perc_2100$arealoss_perc_cumulative)
                        
                    
                    
                    boxplot_arealoss <- ggplot(scen_arealoss_perc_2100[which(!is.na(scen_arealoss_perc_2100$R5)),])+
                        geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=arealoss_perc_cumulative), width = 0.2)+
                        geom_boxplot(aes(fill=factor(R5),y=arealoss_perc_cumulative),outlier.shape = NA, width = 0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        theme_void()+#ylim(c(-7.5,20))+
                        scale_fill_manual(values = color_vector) +
                        scale_color_manual(values = color_vector)+
                        guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                        #guides(fill="none",color="none")
                    boxplot_arealoss 

                    ggarrange(timeseries_arealoss,boxplot_arealoss,widths=c(3,1),
                                align="hv")

                    #ggsave("Figures/Draft/AreaLoss_Projeciton_RCP85.png",dpi=600)

                    glimpse(scen_arealoss_perc_2100)
                    glimpse(scen_arealoss_perc)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)

                    scen_arealoss_perc<- merge(scen_arealoss_perc,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    scen_arealoss_perc$mangrove_area_future_loss <- scen_arealoss_perc$mangrove_area *(0.01*scen_arealoss_perc$arealoss_perc_cumulative)
                    scen_arealoss_perc$mangrove_area_future <- scen_arealoss_perc$mangrove_area *(1-0.01*scen_arealoss_perc$arealoss_perc_cumulative)
                    scen_arealoss_perc$loss <- 1
                    scen_arealoss_perc$loss[which(scen_arealoss_perc$mangrove_area_future_loss<0)] <- 0
                    
                    scen_arealoss_perc$pos <- 1
                    scen_arealoss_perc$pos[which(scen_arealoss_perc$mangrove_area_future_loss>0)] <- 0

                    #here
                    mcn <-read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)
                    scen_arealoss_perc <- merge(scen_arealoss_perc,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    glimpse(scen_arealoss_perc)

                    #install.packages("matrixStats")
                    library(matrixStats)
                    scen_arealoss_perc$mangrove_area_future_capped <- rowMaxs(cbind(scen_arealoss_perc$mangrove_area_future,scen_arealoss_perc$mangrove_area1996))
                    hist(scen_arealoss_perc$mangrove_area_future_capped)
                    time_area_CCsoc <- aggregate(mangrove_area_future_capped~year,data=scen_arealoss_perc,FUN="sum")
                    
                    #hereeee
                    scen_arealoss_perc$not_loss <- scen_arealoss_perc$mangrove_area_future_capped - scen_arealoss_perc$mangrove_area_future  
                    hist(scen_arealoss_perc$not_loss)
                    scen_arealoss_perc$mangrove_area_future_loss_capped <- scen_arealoss_perc$mangrove_area_future_loss - scen_arealoss_perc$not_loss
                    hist(scen_arealoss_perc$mangrove_area_future_loss_capped)    

                    agg_aloss_neg <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc,FUN="sum")
                    agg_aloss_neg_capped <- aggregate(mangrove_area_future_loss_capped~year+loss+R5,data=scen_arealoss_perc,FUN="sum")
                    num_levels <- length(unique(agg_aloss_neg$R5))
                    agg_aloss_neg$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_capped$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc,FUN="sum")
                    agg_aloss_neg_total_capped <- aggregate(mangrove_area_future_loss_capped~year,data=scen_arealoss_perc,FUN="sum")
                    
                    ggplot(agg_aloss_neg)+
                    geom_bar(aes(x=year,y=mangrove_area_future_loss,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Mangrove Area Loss (km2)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=mangrove_area_future_loss),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    ggsave("Figures/Draft/Projection_Loss_Gain_SSP2.png",dpi=600)

                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    ggplot(agg_aloss_neg)+
                    geom_bar(aes(x=year,y=100*mangrove_area_future_loss/sum2020 ,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Mangrove Area Loss (%)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future_loss/sum2020),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    ggsave("Figures/Draft/Projection_Loss_SSP2RCP85.png",dpi=600)

                    glimpse(scen_arealoss_perc)
                    write.csv(scen_arealoss_perc,"Data/output/scen_arealoss_perc.csv")
                    ggplot(scen_arealoss_perc,aes(x=log(pop_ssp2),y=mangrove_area_future_loss))+geom_point()


                    ##New Figure

                    agg_aloss_pos_CCsoc <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc,FUN="sum")
                        glimpse(agg_aloss_pos_CCsoc)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_capped)
                    
                    agg_aloss_neg_total_capped$area_change_wrt_1996 <- level_2020_loss - agg_aloss_neg_total_capped$mangrove_area_future_loss_capped 
                    agg_aloss_neg_total$area_change_wrt_1996 <- level_2020_loss - agg_aloss_neg_total$mangrove_area_future_loss
                    
                    agg_aloss_neg_total_f <- agg_aloss_neg_total[which(agg_aloss_neg_total$year>2024),]
                    agg_aloss_neg_total_f$area_change_wrt_1996 <- agg_aloss_neg_total_f$area_change_wrt_1996 - (agg_aloss_neg_total_f$area_change_wrt_1996[agg_aloss_neg_total_f$year==2025] - agg_aloss_neg_total_onlyCC$area_change_wrt_1996[agg_aloss_neg_total_onlyCC$year==2025])
                    agg_aloss_neg_total_f$color <- "Climate + Socioeconomic"
                    agg_aloss_neg_total_onlyCC$color <- "Climate"
                    summary_dfs_total_sum$color <- "Historical"

                        #bars_sumsum_area_short_past_projection <- 
                        ggplot(agg_aloss_neg_total_f)+ geom_line(aes(x=(year),y=area_change_wrt_1996,color=color),size=1,stat="identity")+
                        #ggplot(agg_aloss_neg_total_capped)+ geom_line(aes(x=(year),y=mangrove_area_future_loss_capped),stat="identity")+
                                xlab("Year")+ylab("Area Change (km2)")+
                                labs(fill=guide_legend("Region"))+
                            geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=area_change_wrt_1996,color=color),size=1,linetype="dashed")+
                            geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                                aes(x=year,y=cumsum(area_change),color=color),size=1)+
                                scale_x_break(c(1998.8,2006))+ 
                                scale_x_break(c(2010.5,2015))+ 
                                guides(fill=guide_legend(reverse = TRUE))+ 
                                geom_hline(aes(yintercept=0),linetype="dashed")+
                                scale_color_manual(values = c("Historical"="gray30","Climate + Socioeconomic" = "darkblue","Climate"="indianred")) +
                                guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Forcings"))+
                                theme_bw()+ #theme(legend.position="bottom")+
                                scale_x_continuous(breaks = c(1996,2008,2020,2050,2100))
                            
                        #ggsave("Figures/Draft/Projections.png",dpi=600)


                        # install.packages("ggpattern")
                        # library("ggpattern")
                        # df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
                        # ggplot(df, aes(trt, outcome)) +
                        #     geom_col_pattern(
                        #         aes(fill = trt, pattern_density = trt),
                        #         colour          = 'black', 
                        #         pattern         = 'circle'
                        #     ) +
                        #     theme_bw() +
                        #     labs(title = "Aesthetic Mapping of 'trt' to Density") + 
                        #     theme(legend.key.size = unit(1.5, 'cm')) + 
                        #     scale_pattern_density_manual(values = c(a = 0.1, b=0.3, c=0.5))

                               
                                
                                
                        ggplot(agg_aloss_pos)+
                                geom_bar(aes(x=(year),y=-mangrove_area_future_loss,
                                    fill=factor(R5)),stat="identity")+
                                xlab("Year")+ylab("Area Change \n(km2)")+
                                labs(fill=guide_legend("Region"))+
                                #scale_fill_manual(values = color_vector)+
                                geom_hline(aes(yintercept=0),linetype="dashed")+
                                geom_line(data=agg_aloss_neg_total_capped,
                                    aes(x=year,y=area_change_wrt_1996),size=1.5)+
                                theme_bw() + 
                                geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                                aes(x=year,y=cumsum(area_change),color=factor(color)),size=1.5)+
                                scale_x_break(c(1998.8,2006))+ 
                                scale_x_break(c(2010.5,2015))+ 
                                guides(fill=guide_legend(reverse = TRUE))+ 
                                scale_color_manual(values = c(" " = "indianred")) +
                                guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
                    bars_sumsum_area_short
                ## Climate Change + GDP (end)


                ## Bins (start)
                    
                    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all.csv")
                    scen$area_change <- scen$days32_85 * (-0.002)
                    ggplot(scen,aes(x=year,y=area_change))+geom_point()
                    scen$area_change <- ifelse(is.na(scen$area_change), 0, scen$area_change)
                    scen <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(area_change_cum = cumsum(area_change)) %>%
                            ungroup() 
                    
                    
                    ggplot(scen,aes(x=year,y=area_change_cum))+geom_point()

                    num_levels <- length(unique(scen$R5))
                    color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    timeseries_area <-ggplot(scen,aes(x=(year),y=area_change_cum))+
                    geom_line(aes(x=(year),y=area_change_cum,group=gridcell_id,color=R5),alpha=0.2)+#+ylim(c(-1,1))+
                    theme_bw()+guides(color="none")+#ylim(c(-7.5,20))+
                        scale_color_manual(values = color_vector) + xlab("Year")+ylab("Area Change (%)")
                    timeseries_area



                    scen2100 <- scen %>% filter(year==2100)
                    
                    min(scen2100$area_change_cum)
                    max(scen2100$area_change_cum)
                        
                    
                    
                    boxplot_area <- ggplot(scen2100[which(!is.na(scen2100$R5)),])+
                        geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=area_change_cum), width = 0.2)+
                        geom_boxplot(aes(fill=factor(R5),y=area_change_cum),outlier.shape = NA, width = 0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        theme_void()+#ylim(c(-7.5,20))+
                        scale_fill_manual(values = color_vector) +
                        scale_color_manual(values = color_vector)+
                        guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                        #guides(fill="none",color="none")
                    boxplot_area 

                    ggarrange(timeseries_area,boxplot_area,widths=c(3,1),
                                align="hv")

                    ggsave("Figures/Draft/AreaLoss_Projeciton_RCP85_THRESHOLD.png",dpi=600)

                    
                    scen<- merge(scen,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    #scen$mangrove_area_future <- scen$mangrove_area *(0.01*scen$area_change_cum)
                    scen$mangrove_area_future <- scen$mangrove_area *(1+0.01*scen$area_change_cum)
                    scen$loss <- 1
                    scen$loss[which(scen$mangrove_area_future<0)] <- 0
                    
                    agg_aloss_neg <- aggregate(mangrove_area_future~year+loss+R5,data=scen,FUN="sum")
                    num_levels <- length(unique(agg_aloss_neg$R5))
                    agg_aloss_neg$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total <- aggregate(mangrove_area_future~year,data=scen,FUN="sum")
                    
                    # ggplot(agg_aloss_neg)+
                    # geom_bar(aes(x=year,y=mangrove_area_future,
                    #     #fill=factor(loss)),stat="identity")+
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Mangrove Area (km2)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    # scale_fill_manual(values = color_vector) +
                    # scale_color_manual(values = color_vector)+
                    # geom_hline(aes(yintercept=0),linetype="dashed")+
                    # geom_line(data=agg_aloss_neg_total,aes(x=year,y=mangrove_area_future),color="indianred",size=1.5)+
                    # theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    #ggsave("Figures/Draft/Projection_Loss_Gain_RCP85.png",dpi=600)

                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    ggplot(agg_aloss_neg)+
                    #geom_bar(aes(x=year,y=100*mangrove_area_future/sum2020 ,
                        #fill=factor(loss)),stat="identity")+
                    #    fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Mangrove Area Change(%)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future/sum2020),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    ggsave("Figures/Draft/Projection_Area_Threshold_RCP85_percent.png",dpi=600)


                    glimpse(scen)
                    mcn_2020 <- mcn%>%filter(year==2020)
                    scen_2100 <- merge(scen2100,mcn_2020,by="gridcell_id",suffixes=c("2100","2020"),all=T)
                    glimpse(scen_2100)
                    ggplot(scen_2100,aes(x=logGDPpc/logGDPpc_country,y=mangrove_area_future/mangrove_area2020,col=R52020))+geom_point()
                ## Bins (end)
            ## Area Loss Projections 

            ## GAPS Projections (start)
                
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
                    
                    timeseries_gaps_per_area <-ggplot(gaps_per_area,aes(x=(year),y=gaps_per_area_cumulative))+
                    geom_line(aes(x=(year),y=gaps_per_area_cumulative,group=gridcell_id,color=R5),alpha=0.2)+#+ylim(c(-1,1))+
                    theme_bw()+guides(color="none")+#ylim(c(-7.5,20))+
                        scale_color_manual(values = color_vector) + xlab("Year")+ylab("Area Loss (%)")

                    timeseries_gaps_per_area

                    gaps_per_area_2100 <- gaps_per_area %>% filter(year==2100)
                    
                    min(gaps_per_area$gaps_per_area_cumulative)
                    max(gaps_per_area$gaps_per_area_cumulative)
                        
                    
                    
                    boxplot_gaps <- ggplot(gaps_per_area[which(!is.na(gaps_per_area$R5)),])+
                        geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=gaps_per_area_cumulative), width = 0.2)+
                        geom_boxplot(aes(fill=factor(R5),y=gaps_per_area_cumulative),outlier.shape = NA, width = 0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        theme_void()+#ylim(c(-7.5,20))+
                        scale_fill_manual(values = color_vector) +
                        scale_color_manual(values = color_vector)+
                        guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                        #guides(fill="none",color="none")
                    boxplot_arealoss_onlyCC 

                    ggarrange(timeseries_gaps_per_area,boxplot_gaps,widths=c(3,1),
                                align="hv")
                    
                    ggarrange(boxplot_arealoss+theme_bw(),boxplot_gaps+theme_bw(),common.legend=TRUE)
                    ggsave("Figures/Draft/UpdateThis.png")

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
                    #scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    gaps_per_area_scen$loss <- 1
                    gaps_per_area_scen$loss[which(gaps_per_area_scen$gaps_per_area_future<0)] <- 0
                    
                    agg_aloss_neg_gaps <- aggregate(gaps_per_area_future~year+loss+R5,data=gaps_per_area_scen,FUN="median",na.rm=TRUE)
                    num_levels <- length(unique(agg_aloss_neg_gaps$R5))
                    agg_aloss_neg_gaps$color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    glimpse(gaps_per_area_scen)
                    agg_aloss_neg_total_gaps <- gaps_per_area_scen %>%
                        group_by(year) %>%
                        mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                        summarise(gaps_per_area_future = sum(gaps_per_area_future * weight, na.rm = TRUE)) %>% as.data.frame()

                    #agg_aloss_neg_total_gaps <- aggregate(gaps_per_area_future~year,data=gaps_per_area_scen,FUN="median",na.rm=TRUE)
                    
                    weighted_means_h <- read.csv("Data/output/weighted_means_h.csv")
                    agg_aloss_neg_total_gaps$change_holesperha <- agg_aloss_neg_total_gaps$gaps_per_area_future*100
                    agg_aloss_neg_total_gaps$change_holesperha_wrt1996 <- agg_aloss_neg_total_gaps$change_holesperha - weighted_means_h$change_holesoerha_wrt1996[weighted_means_h$year==2020]
                    glimpse(agg_aloss_neg_total_gaps)

                    # agg_aloss_neg_total_gaps <- gaps_per_area_scen %>%
                    #     mutate(normalized_area = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                    #     mutate(weighted_gaps = gaps_per_area_future * normalized_area) %>%
                    #     group_by(year) %>%
                    #     summarise(weighted_sum = sum(weighted_gaps, na.rm = TRUE)) %>% as.data.frame()
                    
                    ggplot()+
                    #geom_bar(aes(x=year,y=gaps_per_area_future,
                        #fill=factor(loss)),stat="identity")+
                     #   fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Gaps per ha")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total_gaps,aes(x=year,y=change_holesperha_wrt1996),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))+
                    geom_line(data=weighted_means_h,aes(x=year,y=change_holesoerha_wrt1996))
                    #ggsave("Figures/Draft/Projection_Loss_Gain_RCP85.png",dpi=600)


                                        # First, let's create a new variable to represent the 10-year periods
                    agg_aloss_neg_total_gaps$decade <- cut(agg_aloss_neg_total_gaps$year, breaks = seq(2020, 2100, by = 10), include.lowest = TRUE, labels = FALSE)
                    agg_aloss_neg_total_gaps_d <- aggregate(change_holesperha_wrt1996~decade,data=agg_aloss_neg_total_gaps,FUN="mean")

                    # Now, let's plot
                    # df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
                        # ggplot(df, aes(trt, outcome)) +
                        #     geom_col_pattern(
                        #         aes(fill = trt, pattern_density = trt),
                        #         colour          = 'black', 
                        #         pattern         = 'circle'
                        #     ) +
                        #     theme_bw() +
                        #     labs(title = "Aesthetic Mapping of 'trt' to Density") + 
                        #     theme(legend.key.size = unit(1.5, 'cm')) + 
                        #     scale_pattern_density_manual(values = c(a = 0.1, b=0.3, c=0.5))

                    agg_aloss_neg_total_gaps_d
                    
                    ggplot(agg_aloss_neg_total_gaps, aes(x = (year), y = change_holesperha_wrt1996)) +
                    geom_col_pattern(
                        aes(#fill=decade,
                        pattern_density=(change_holesperha_wrt1996)),
                        pattern = "circle",
                        fill=NA,
                        #pattern_density = abs(agg_aloss_neg_total_gaps_d$change_holesperha),  # use the absolute value to ensure the density is non-negative
                        #pattern_args = list(radius = 0.1),
                        #fill = "blue",
                        color = NA
                    ) +
                    #scale_y_reverse(limits = c(0, -6000)) +
                    labs(x = "Decade", y = "Change in holes per hectare wrt 1996")#+ 
                    
                    agg_aloss_neg_total_gaps <- merge(agg_aloss_neg_total_f,agg_aloss_neg_total_gaps,by="year")
                    
                    ggplot(agg_aloss_neg_total_f)+ geom_line(aes(x=(year),y=area_change_wrt_1996,color=color),size=1,stat="identity")+
                        #ggplot(agg_aloss_neg_total_capped)+ geom_line(aes(x=(year),y=mangrove_area_future_loss_capped),stat="identity")+
                                xlab("Year")+ylab("Area Change (km2)")+
                                labs(fill=guide_legend("Region"))+
                            geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=area_change_wrt_1996,color=color),size=1,linetype="dashed")+
                            geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                                aes(x=year,y=cumsum(area_change),color=color),size=1)+
                            geom_col_pattern(data=agg_aloss_neg_total_gaps,aes(x = year, y = area_change_wrt_1996,pattern_density=(change_holesperha_wrt1996)),pattern = "circle",fill=NA, color = NA) +
                                scale_x_break(c(1998.8,2006))+ 
                                scale_x_break(c(2010.5,2015))+ 
                                guides(fill=guide_legend(reverse = TRUE))+ 
                            geom_hline(aes(yintercept=0),linetype="dashed")+
                                scale_color_manual(values = c("Historical"="gray30","Climate + Socioeconomic" = "darkblue","Climate"="indianred")) +
                                guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Forcings"))+
                                theme_bw()+ #theme(legend.position="bottom")+
                                scale_x_continuous(breaks = c(1996,2008,2020,2050,2100))


                    
                    sum2020 <- sum(mcn_2020$holes,na.rm=T)/sum(mcn_2020$mangrove_area,na.rm=T)
                    ggplot(agg_aloss_neg_gaps)+
                    geom_bar(aes(x=year,y=100*gaps_per_area_future/sum2020 ,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Gaps Increase (%)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total_gaps,aes(x=year,y=100*gaps_per_area_future/sum2020),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    #ggsave("Figures/Draft/Projection_Gaps_RCP85_SSP5_percent.png",dpi=600)

                    ## Figures

                        agg_aloss_neg_total_gaps 
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        summary_dfs_total_median <- read.csv("Data/Fig_Vals/Past_All_Median.csv")
                        glimpse(summary_dfs_total_median)
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_median$holes_d_change)[which(summary_dfs_total_median$year==2020)]
                        glimpse(agg_aloss_neg_total_gaps)
                        agg_aloss_neg_total_gaps$gaps_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_gaps$gaps_per_area_future


                        #bars_sumsum_area_short_past_projection <- 
                        ggplot(agg_aloss_neg_total_gaps)+
                                geom_line(aes(x=(year),y=gaps_change_wrt_1996))+
                                xlab("Year")+ylab("Area Change \n(km2)")+
                                labs(fill=guide_legend("Region"))+
                                #scale_fill_manual(values = color_vector)+
                                geom_hline(aes(yintercept=0),linetype="dashed")+
                                #geom_line(data=agg_aloss_neg_total_gaps,
                                #    aes(x=year,y=gaps_change_wrt_1996),size=1.5)+
                                theme_bw() + 
                                geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                                aes(x=year,y=cumsum(holes_d_change),color=factor(color)),size=1.5)+
                                scale_x_break(c(1998.8,2006))+ 
                                scale_x_break(c(2010.5,2015))+ 
                                guides(fill=guide_legend(reverse = TRUE))+ 
                                scale_color_manual(values = c(" " = "indianred")) +
                                guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))

                    ## Figures
                ## All forcings  (end)
            ## GAPS Projections (start)

            ## Fractal Projections (start)
                
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
                #sal <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_sal_85.csv")
                mcn <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                #mcn2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("gridcell_id","Mean_Salinity"))]
                #names(mcn2020)[2] <- "sal2020"
                #sal <- merge(sal,mcn2020,by="gridcell_id",all=T)
                #sal$salinity <- sal$sal2020 * (1+sal$Salinity_perc/100)
                #glimpse(sal)
                glimpse(scen)
                hist(sal$Salinity_perc)
                
                ggplot(scen[which(scen$year<2024),],aes(x=temp_85,y=pop_ssp5))+geom_point()
                
                scen <- scen[which(scen$year >2024),]
                
                scen <- scen %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(
                            perc_inc_pop5 = (pop_ssp5 /lag(pop_ssp5)-1)*100,
                            temp_inc_85 = (temp_85 -lag(temp_85)),
                            preci_inc_85 = (preci_85 -lag(preci_85))
                             )%>% ungroup() %>%  arrange(gridcell_id,year)
                
                ggplot(scen[which(scen$year<2027),],aes(x=temp_inc_85,y=perc_inc_pop5))+geom_point()

                #scen$gridcell_id[which(scen$perc_inc_pop5[which(scen$year<2027)] ==max(scen$perc_inc_pop5[which(scen$year<2026)],na.rm=T))]
                #scen$pop_ssp5[which(scen$gridcell_id==1)]
                ggplot(scen)+geom_line(aes(x=year,y=pop_ssp5,group=gridcell_id))
                
                load("Models/Round1/pref_model_pafrac_ssthot.RData")
                summary(pref_model_pafrac_ssthot)
                coef_temp <- summary(pref_model_pafrac_ssthot)$coef[2]
                coef_preci <- summary(pref_model_pafrac_ssthot)$coef[3]
                coef_pop <- summary(pref_model_pafrac_ssthot)$coef[5]

               
                
                ## All forcings  (start)
                    scen$change_pafrac_perc <-  (scen$temp_inc_85*coef_temp) + (scen$preci_inc_85*coef_preci)  + (scen$perc_inc_pop5*coef_pop) 

                    scen$change_pafrac_perc <- ifelse(is.na(scen$change_pafrac_perc), 0, scen$change_pafrac_perc)
                        
                    
                    
                    scen <- scen %>% filter(year>2023)
                    
                    pafrac <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(change_pafrac_perc_cumulative = cumsum(change_pafrac_perc)) %>%
                            ungroup() 
                    
                    mcn_c <- mcn[,which(names(mcn) %in% c("gridcell_id","countrycode","R5"))]
                        mcn_c <- mcn_c[complete.cases(mcn_c),]
                        c1 <- aggregate(countrycode~gridcell_id,FUN="first",data=mcn_c)
                        r1 <- aggregate(R5~gridcell_id,FUN="first",data=mcn_c)
                        c2 <- data.frame(gridcell_id=c1$gridcell_id,countrycode=c1$countrycode,R5=r1$R5)
                        pafrac <- merge(c2,pafrac ,by="gridcell_id",all=T) 
                    glimpse(pafrac )
                    
                    library(scico)
                    
                    # Get the number of levels in your factor variable
                    num_levels <- length(unique(pafrac$R5))

                    # Create a vector of colors from the scico palette
                    color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    timeseries_pafrac <-ggplot(pafrac,aes(x=(year),y=change_pafrac_perc_cumulative))+
                    geom_line(aes(x=(year),y=change_pafrac_perc_cumulative,group=gridcell_id,color=R5),alpha=0.2)+#+ylim(c(-1,1))+
                    theme_bw()+guides(color="none")+#ylim(c(-7.5,20))+
                        scale_color_manual(values = color_vector) + xlab("Year")+ylab("Increase in Pafrac (%)")

                    timeseries_pafrac

                    pafrac_2100 <- pafrac %>% filter(year==2100)
                    
                    min(pafrac$change_pafrac_perc_cumulative)
                    max(pafrac$change_pafrac_perc_cumulative)
                        
                    
                    
                    boxplot_gaps <- ggplot(pafrac[which(!is.na(pafrac$R5)),])+
                        geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=change_pafrac_perc_cumulative), width = 0.2)+
                        geom_boxplot(aes(fill=factor(R5),y=change_pafrac_perc_cumulative),outlier.shape = NA, width = 0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        theme_void()+#ylim(c(-7.5,20))+
                        scale_fill_manual(values = color_vector) +
                        scale_color_manual(values = color_vector)+
                        guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                        #guides(fill="none",color="none")
                    

                    ggarrange(timeseries_pafrac,boxplot_gaps,widths=c(3,1),
                                align="hv")

                    ggsave("Figures/Draft/PAFRAC_Projeciton_SSP5-RCP85.png",dpi=600)

                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("pafrac","gridcell_id","mangrove_area"))]
                    mean(mcn_2020$pafrac,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("pafrac","gridcell_id","mangrove_area"))]
                    mean(mcn_1996$pafrac,na.rm=T)

                    pafrac_scen<- merge(pafrac,mcn_2020,by=c("gridcell_id"),all=T)
                    glimpse(pafrac_scen)
                    pafrac_scen$pafrac_future <- pafrac_scen$pafrac * (100+ pafrac_scen$change_pafrac_perc_cumulative)/100

                    #glimpse(gaps_per_area_scen)
                    #max(gaps_per_area_scen$gaps_per_area2020)
                    #gaps_per_area_scen <- gaps_per_area_scen[which(is.finite(gaps_per_area_scen$gaps_per_area2020) & !is.na(gaps_per_area_scen$gaps_per_area2020)),]
                    #gaps_per_area_scen$gaps_per_area_future <- gaps_per_area_scen$gaps_per_area2020 *(0.01*gaps_per_area_scen$gaps_per_area_cumulative)
                    #scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    pafrac_scen$loss <- 1
                    pafrac_scen$loss[which( pafrac_scen$pafrac_future<0)] <- 0
                    
                    agg_aloss_neg <- aggregate(pafrac_future~year+loss+R5,data=pafrac_scen,FUN="mean",na.rm=TRUE)
                    num_levels <- length(unique(agg_aloss_neg$R5))
                    agg_aloss_neg$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total <- aggregate(pafrac_future~year,data=pafrac_scen,FUN="mean",na.rm=TRUE)
                    
                    ggplot(agg_aloss_neg)+
                    geom_bar(aes(x=year,y=pafrac_future,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity",position="dodge")+
                    xlab("Year")+ylab("Vegetation Gaps (holes/km2)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=pafrac_future),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    #ggsave("Figures/Draft/Projection_Loss_Gain_RCP85.png",dpi=600)

                    
                    sum2020 <- mean(mcn_2020$pafrac,na.rm=T)
                    ggplot(agg_aloss_neg)+
                    geom_bar(aes(x=year,y=100*pafrac_future/sum2020 ,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity",position="dodge")+
                    xlab("Year")+ylab("Gaps Increase (%)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*gaps_per_area_future/sum2020),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    ggsave("Figures/Draft/Projection_Fractal_RCP85_SSP5_percent.png",dpi=600)
                ## All forcings  (end)
            ## Fractal Projections (start)

            ## Patches Projections (start)
                
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
                sal <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_sal_85.csv")
                mcn <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                ssps_country <- read.csv("C:\\Users\\basti\\Box\\Data\\SSPs\\SspDb_country_data_2013-06-12.csv")
                ## SSPs
                    names(ssps_country)
                    unique(ssps_country$SCENARIO)
                    scens <- c("SSP5_v9_130325" ,"SSP5_v9_130424" ,"SSP5_v9_130115")
                    
                    glimpse(ssps_country)
                    countries <- unique(mcn$countrycode)
                    vars <- c("GDP|PPP","Population")
                    ssps_country <- ssps_country[which(ssps_country$SCENARIO %in% scens & 
                                                        ssps_country$REGION %in% countries &
                                                        ssps_country$VARIABLE %in% vars),]
                    long_ssps_country <- ssps_country %>%
                        pivot_longer(cols = starts_with("X"), 
                                    names_to = "Year", 
                                    values_to = "Value")

                        # Convert the year column to numeric removing the 'X' at the beginning
                        long_ssps_country$Year <- as.numeric(str_remove(long_ssps_country$Year, "X"))
                        long_ssps_country <- long_ssps_country %>%
                            spread(key = VARIABLE, value = Value)

                    glimpse(long_ssps_country)

                    mean_ssps_country <- long_ssps_country %>%
                        group_by(REGION, Year) %>%
                        summarise(mean_GDP = mean(`GDP|PPP`, na.rm = TRUE),
                                    mean_Population = mean(Population, na.rm = TRUE))
                    glimpse(mean_ssps_country)

                    mean_ssps_country <- mean_ssps_country %>%
                    arrange(REGION, Year)

                    # Fill in missing years and interpolate GDP|PPP and Population
                    mean_ssps_country_interpolated <- mean_ssps_country %>%
                        group_by(REGION) %>%
                        complete(Year = full_seq(Year, 1)) %>%
                        mutate(mean_GDP = na.approx(mean_GDP, na.rm = FALSE), 
                                mean_Population = na.approx(mean_Population, na.rm = FALSE))

                    mean_ssps_country_interpolated <- mean_ssps_country_interpolated %>% filter(Year>2020)
                    
                    ggplot(mean_ssps_country_interpolated[which(mean_ssps_country_interpolated$Year>2000),])+
                    geom_point(aes(x=Year,y=mean_GDP))
                    
                    
                    mcn_c <- mcn[,which(names(mcn) %in% c("gridcell_id","countrycode","R5"))]
                    mcn_c <- mcn_c[complete.cases(mcn_c),]
                    c1 <- aggregate(countrycode~gridcell_id,FUN="first",data=mcn_c)
                    r1 <- aggregate(R5~gridcell_id,FUN="first",data=mcn_c)
                    c2 <- data.frame(gridcell_id=c1$gridcell_id,countrycode=c1$countrycode,R5=r1$R5)
                    scen <- merge(c2,scen ,by="gridcell_id",all=T) 
                    glimpse(scen)
                    glimpse(mean_ssps_country_interpolated)
                    names(mean_ssps_country_interpolated) <- c("countrycode","year","GDP_Country_ssp5","Pop_Country_ssp5")
                    scen <- merge(scen,mean_ssps_country_interpolated,by=c("countrycode","year"))
                    glimpse(scen)



                ## SSPs
                
                mcn2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("gridcell_id","Mean_Salinity"))]
                names(mcn2020)[2] <- "sal2020"
                sal <- merge(sal,mcn2020,by="gridcell_id",all=T)
                sal$salinity <- sal$sal2020 * (1+sal$Salinity_perc/100)
                glimpse(sal)
                glimpse(scen)
                
                sal <- sal %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(
                            perc_inc_sal = (salinity /lag(salinity)-1)*100
                             )%>% ungroup() %>%  arrange(gridcell_id,year)
                
                scen <- merge(scen,sal,by=c("gridcell_id","year"))
                
                glimpse(scen)
                
                load("Models/Round1/pref_model_np_ssthot.RData")
                summary(pref_model_np_ssthot)
                coef_sal <- summary(pref_model_np_ssthot)$coef[4]
                coef_pop <- summary(pref_model_np_ssthot)$coef[5]
                coef_gdppc_c <- summary(pref_model_np_ssthot)$coef[8]
                coef_gdppc_c2 <- summary(pref_model_np_ssthot)$coef[9]

               
                
                ## All forcings  (start)
                    scen$GDPpc_c<-(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5)
                    scen$logGDPpc_c<-log(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5)
                    scen$marg_GDPpc <-  coef_gdppc_c + 2*coef_gdppc_c2*(log(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5))
                    hist(scen$marg_GDPpc)

                       
                    
                    scen <- scen %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(
                            perc_inc_pop5 = (pop_ssp5 /lag(pop_ssp5)-1)*100,
                            #temp_inc_85 = (temp_85 -lag(temp_85)),
                            perc_inc_GDPpc_c = (GDPpc_c/lag(GDPpc_c)-1)*100
                             )%>% ungroup() %>%  arrange(gridcell_id,year)

                    scen$np_change <- scen$perc_inc_sal*coef_sal #+ coef_pop*scen$perc_inc_pop5  + scen$marg_GDPpc*scen$perc_inc_GDPpc_c
                    hist(scen$np_change)

                np_per_area <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(np_per_area_cumulative = cumsum(np_change)) %>%
                            ungroup() 
                    
                    mcn_c <- mcn[,which(names(mcn) %in% c("gridcell_id","countrycode","R5"))]
                        mcn_c <- mcn_c[complete.cases(mcn_c),]
                        c1 <- aggregate(countrycode~gridcell_id,FUN="first",data=mcn_c)
                        r1 <- aggregate(R5~gridcell_id,FUN="first",data=mcn_c)
                        c2 <- data.frame(gridcell_id=c1$gridcell_id,countrycode=c1$countrycode,R5=r1$R5)
                        gaps_per_area<- merge(c2,gaps_per_area,by="gridcell_id",all=T) 
                    glimpse(gaps_per_area)
                    
                    library(scico)
                    
                    # Get the number of levels in your factor variable
                    num_levels <- length(unique(gaps_per_area$R5))

                    # Create a vector of colors from the scico palette
                    color_vector <- scico(n = num_levels, palette = "batlow")
                    
                    timeseries_np_per_area <-ggplot(np_per_area,aes(x=(year),y=np_per_area_cumulative))+
                    geom_line(aes(x=(year),y=np_per_area_cumulative,group=gridcell_id,color=R5),alpha=0.2)+#+ylim(c(-1,1))+
                    theme_bw()+guides(color="none")+#ylim(c(-7.5,20))+
                        scale_color_manual(values = color_vector) + xlab("Year")+ylab("Area Loss (%)")

                    timeseries_np_per_area

                    gaps_per_area_2100 <- gaps_per_area %>% filter(year==2100)
                    
                    min(gaps_per_area$gaps_per_area_cumulative)
                    max(gaps_per_area$gaps_per_area_cumulative)
                        
                    
                    
                    boxplot_gaps <- ggplot(gaps_per_area[which(!is.na(gaps_per_area$R5)),])+
                        geom_boxplot(aes(color=factor(R5),fill=factor(R5),y=gaps_per_area_cumulative), width = 0.2)+
                        geom_boxplot(aes(fill=factor(R5),y=gaps_per_area_cumulative),outlier.shape = NA, width = 0.2)+
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        theme_void()+#ylim(c(-7.5,20))+
                        scale_fill_manual(values = color_vector) +
                        scale_color_manual(values = color_vector)+
                        guides(fill=guide_legend("Region"),color=guide_legend("Region"))  #+#+scale_color_scico()+
                        #guides(fill="none",color="none")
                    boxplot_arealoss_onlyCC 

                    ggarrange(timeseries_gaps_per_area,boxplot_gaps,widths=c(3,1),
                                align="hv")

                    ggsave("Figures/Draft/GAPS_Projeciton_RCP85.png",dpi=600)

                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("holes","gridcell_id","mangrove_area"))]
                    sum(mcn_2020$holes,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("holes","gridcell_id","mangrove_area"))]
                    sum(mcn_1996$holes,na.rm=T)

                    gaps_per_area_scen<- merge(gaps_per_area,mcn_2020,by=c("gridcell_id"),all=T)
                    gaps_per_area_scen$gaps_per_area2020 <- gaps_per_area_scen$holes / gaps_per_area_scen$mangrove_area
                    glimpse(gaps_per_area_scen)
                    max(gaps_per_area_scen$gaps_per_area2020)
                    gaps_per_area_scen <- gaps_per_area_scen[which(is.finite(gaps_per_area_scen$gaps_per_area2020) & !is.na(gaps_per_area_scen$gaps_per_area2020)),]
                    gaps_per_area_scen$gaps_per_area_future <- gaps_per_area_scen$gaps_per_area2020 *(0.01*gaps_per_area_scen$gaps_per_area_cumulative)
                    #scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    gaps_per_area_scen$loss <- 1
                    gaps_per_area_scen$loss[which(gaps_per_area_scen$gaps_per_area_future<0)] <- 0
                    
                    agg_aloss_neg <- aggregate(gaps_per_area_future~year+loss+R5,data=gaps_per_area_scen,FUN="mean",na.rm=TRUE)
                    num_levels <- length(unique(agg_aloss_neg$R5))
                    agg_aloss_neg$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total <- aggregate(gaps_per_area_future~year,data=gaps_per_area_scen,FUN="mean",na.rm=TRUE)
                    
                    ggplot(agg_aloss_neg)+
                    geom_bar(aes(x=year,y=gaps_per_area_future,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Vegetation Gaps (holes/km2)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=gaps_per_area_future),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    #ggsave("Figures/Draft/Projection_Loss_Gain_RCP85.png",dpi=600)

                    
                    sum2020 <- sum(mcn_2020$holes,na.rm=T)/sum(mcn_2020$mangrove_area,na.rm=T)
                    ggplot(agg_aloss_neg)+
                    geom_bar(aes(x=year,y=100*gaps_per_area_future/sum2020 ,
                        #fill=factor(loss)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Gaps Increase (%)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*gaps_per_area_future/sum2020),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    ggsave("Figures/Draft/Projection_Gaps_RCP85_SSP5_percent.png",dpi=600)
                ## All forcings  (end)
            ## Patches Projections (start)



            ## Area Loss Projections 

        ## Area Loss
    ## Distribution Plots
## Plot before and after (end)

## Project Area Loss
            load(file="Models/Round1/pref_area_model.RData") 
            summary(model_area_ssthot)
            c_ssth <- summary(model_area_ssthot)$coef[1]
            c_ssth2 <- summary(model_area_ssthot)$coef[2]
            c_logGDPpc <- summary(model_area_ssthot)$coef[3]
            c_logGDPpc2 <- summary(model_area_ssthot)$coef[4]
            ## Forcing Projections (start)
## Project Area Loss


## Read Coefficients

