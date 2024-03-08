
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

## Plot Bins
ggplot(mcn, aes(x=sst_hottest,y=b33_34C,color=sst))+geom_point()

## Plot Bins

## Plots SST

    
    ggplot(mcn) +
        geom_point(aes(y = mhw_dur, x = year, color = "Hotwave"), alpha = 0.1) +
        geom_point(aes(y = mcw_dur, x = year, color = "Coldwave"), alpha = 0.1) +
        #geom_smooth(aes(y = mcw_dur, x = year), method="lm", color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year), method="lm",color = "brown2",se=TRUE,level=0.99) +
        geom_smooth(aes(y = mcw_dur, x = year), color = "cyan4",se=TRUE,level=0.99) +
        geom_smooth(aes(y = mhw_dur, x = year),color = "brown2",se=TRUE,level=0.99) +
        scale_color_manual(values = c("Hotwave" = "brown2", "Coldwave" = "cyan4"))+
        scale_y_continuous(trans = "log") +
        labs(y = "Duration (days)", x = "Year", title = "Marine hot and cold waves duration by year", color = "Type") +
        theme_bw()
        #ggsave("Figures/MHW_MCW_duration_nonlinear.png")

    
    
    ggplot(mcn) +
        geom_density_ridges(aes(x = mhw_dur, y = as.factor(year), fill = "Hotwave"), alpha = 0.5) +
        geom_density_ridges(aes(x = mcw_dur, y = as.factor(year), fill = "Coldwave"), alpha = 0.5) +
        scale_x_continuous(trans = "log") +
        scale_fill_manual(values = c("Hotwave" = "brown2", "Coldwave" = "cyan4")) +
        labs(x = "Duration (days)", y = "Year", title = "Marine hot and cold waves duration by year",, fill = "Type") +
        theme_bw()
        #ggsave("Figures/MHW_MCW_duration_ridges.png")

    m1 <- felm(mhw_int_anom~year+sst|gridcell_id|0|0,data=mcn)
    summary(m1)
    m2 <- felm(mcw_int_anom~year+sst|gridcell_id|0|0,data=mcn)
    summary(m2)
    m2$coefficients

    ggplot(mcn) +
        geom_line(aes(x=year,color = sst, y = mhw_int_anom,group = gridcell_id))
    
    hotwaves_plot <- ggplot(mcn) +
        geom_line(aes(x = year, color = sst, y = mhw_int_anom, group = gridcell_id)) +
        scale_color_viridis_c(option = "plasma") +
        #geom_smooth(aes( x = year, y = mhw_int_anom),method="lm",color="black")+
        #geom_text(aes( x = 2010, y = 1.5, label="Slope = 0.0016***"))+
        coord_cartesian() +
        ggtitle("Marine hotwaves intensity")+
        theme_bw()+ylab("Temperature anomaly (C)")

    coldwaves_plot <- ggplot(mcn) +
        geom_line(aes(color = sst, y = mcw_int_anom, group = gridcell_id, x = year))+
        scale_color_viridis_c() +  
        #geom_smooth(aes( x = year, y = mcw_int_anom),method="lm",color="black")+
        #geom_text(aes( x = 2010, y = -1.5, label="Slope = 0.0007***"))+
        coord_cartesian() +
        ggtitle("Marine coldwaves intensity")+
        theme_bw()+ylab("Temperature anomaly (C)")

    library('ggpubr')
    ggarrange(hotwaves_plot,coldwaves_plot,ncol=1)

    #ggsave("Figures/MHW_MCW_anomalies.png")
    stargazer(m1,m2,type="text")

    glimpse(mcn)
    count_mhw <- aggregate(mhw~year,data=mcn,FUN="sum")
    names(count_mhw)[2] <- "count"
    count_mhw$type="MHW"
    
    count_mcw <- aggregate(mcw~year,data=mcn,FUN="sum")
    names(count_mcw)[2] <- "count"
    count_mcw$type="MCW"
    count_mw <- rbind(count_mhw,count_mcw)
    #levels(factor(count_mw$type))
    #count_mw$type <- factor(count_mw$type)
    #levels(count_mw$type) <- relevel(count_mw$type,levels=c("MCW","MHW"),ref="MHW")
    #count_mw$type <- fct_rev(count_mw$type)
    
    ggplot(data=count_mw)+
    geom_bar(aes(x=year,y=count,fill=fct_rev(type)),stat="identity")+
    theme_bw()+ 
    guides(fill=guide_legend(title="Type"))+
    scale_fill_manual(values=c("#c7457c","#2b9089"))
    #ggsave("Figures/MHW_MCW_count.png")


    ggplot(data=count_mw)+
    geom_line(aes(x=year,y=100*count/1533,color=fct_rev(type)),stat="identity", position="dodge")+
    theme_bw()+ 
    ylab("Percent of sites")+
    guides(color=guide_legend(title="Type"))+
    scale_color_manual(values=c("#c7457c","#2b9089"))
    #ggsave("Figures/MHW_MCW_percent.png")

    
    aggregate(count~type,data=count_mw,FUN="sum")

    glimpse(mcn)
    
    ggplot(mcn)+
    geom_point(aes(x=(mean_temp+temp_anom),y=(sst),color=factor(mhw)))+
    theme_minimal()+
    geom_text(data=mcn[which(mcn$year==2015),],aes(x=(mean_temp+temp_anom),y=sst+0.3,label=countrycode))+
    xlab("Annual mean surface temperature")+ylab("SST")+
    guides(color=guide_legend(title="Year with MHW"))
    #ggsave("Figures/SST_Temp_MHW.png",dpi=600)

    ggplot(mcn)+
    geom_point(aes(x=(temp_anom),y=sst,color=factor(mhw)))+
    theme_minimal()

    ggplot(mcn)+
    geom_point(aes(x=(temp_anom),y=(mhw_int_anom),color=sst,group=gridcell_id))+
    scale_color_viridis_c(option = "plasma") +
    theme_minimal()+xlab("Annual mean surface temperature anomaly")+ylab("Anomaly during MHW")+
    guides(color=guide_legend("Annual mean SST"))
    #ggsave("Figures/Anomalies.png",dpi=600)

    model_sst <- felm(mhw_int_anom~temp_anom|gridcell_id|0|0,data=mcn)
    summary(model_sst)
    
    model_sst <- felm(mhw_int_anom~temp_anom+Mean_Precipitation|gridcell_id|0|0,data=mcn)
    summary(model_sst)
    stargazer(model_sst,type="text")

    ggplot(mcn)+
    geom_point(aes(x=(spei),y=(mhw_int_anom),color=sst,group=gridcell_id))+
    scale_color_viridis_c(option = "plasma") +
    theme_minimal()+xlab("Annual SPEI")+ylab("Anomaly during MHW")+
    guides(color=guide_legend("Annual mean SST"))

    model_sst <- felm(mhw_int_anom~temp_anom+Mean_Precipitation+spei|year+gridcell_id|0|0,data=mcn)
    summary(model_sst)
    stargazer(model_sst,type="text")

    ggplot(mcn)+
    geom_point(aes(x=(Mean_Precipitation),y=(sst),color=sst,group=gridcell_id))+
    scale_color_viridis_c(option = "plasma") +
    theme_minimal()+xlab("Annual mean Precipitation")+ylab("Anomaly during MHW")+
    guides(color=guide_legend("Annual mean SST"))
    
    model_sst <- felm(mhw_int_anom~I(temp_anom)+Mean_Precipitation|gridcell_id|0|0,data=mcn)
    summary(model_sst)
    stargazer(model_sst,type="text")
    

    
    

    ggplot(mcn)+
    geom_point(aes(x=(mean_temp+temp_anom),y=mhw_int,color=factor(mhw)))+
    theme_minimal()

## Plots SST (end)
## Plots NTL (start)

    glimpse(mcn)
    
    
    glimpse(mcn)
    
    

    

    filtered_mcn <- mcn %>%
    group_by(gridcell_id) %>%
    filter(sum(which(abs(ntl_change)>1))<1) #filtering out any gridcell with ntl changes larger than 10

    filtered_mcn$ntl[which(filtered_mcn$gridcell_id==1200)]
    filtered_mcn$ntl_change[which(filtered_mcn$gridcell_id==1200)]
    sum(filtered_mcn$ntl[which(filtered_mcn$gridcell_id==1200)]==0)
    sum(is.na(filtered_mcn$ntl[which(filtered_mcn$gridcell_id==1200)]))

    filtered_mcn <- filtered_mcn %>%
    group_by(gridcell_id) %>%
    filter(sum(is.na(ntl))<19) #filtering out any gridcell with ntl changes larger than 10

    filtered_mcn <- filtered_mcn %>%
    group_by(gridcell_id) %>%
    filter(sum((ntl==0),na.rm=TRUE)<1) 

    #filtered_mcn <- ungroup(filtered_mcn)

    #sum(which(abs(filtered_mcn$ntl_change[which(filtered_mcn$gridcell_id==2)])>10))
    
    
    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=log(ntl),group=gridcell_id,color=sensor),alpha=0.2)+
    #coord_cartesian(xlim=c(2013,2021))+
    theme_bw()+
    ylab("Log of NTL (nWatts/cm^2)")
    ggsave("Figures/NTL_filtered_sensors.png")

    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=ntl_change,group=gridcell_id,color=R5),alpha=0.2)+
    #coord_cartesian(xlim=c(2014,2021))+
    theme_bw()+
    ylab("NTL annual change")
    #ggsave("Figures/NTL_change_filtered_sensors.png")

    
    ntl_R5 <- aggregate(ntl_change~R5+year,data=filtered_mcn,FUN="median")

    ntl_R52 <- aggregate(ntl~R5+year,data=filtered_mcn,FUN="mean")
    ntl_R5$ntl <- ntl_R52$ntl

    filtered_mcn$countrycode[which(filtered_mcn$ntl %in% aggregate(ntl~R5,data=filtered_mcn[which(filtered_mcn$year==2020),],FUN="max")[,2])]
    aggregate(ntl~R5,data=filtered_mcn,FUN="max")
    
    ggplot(ntl_R52)+
    geom_histogram(aes(x=year,y=log(ntl),fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("NTL")+ggtitle("Average NTL by region")
    #ggsave("Figures/NTL_by_reigon.png")

    ggplot(ntl_R5[which(ntl_R5$year %in% c(1996,2020)),])+
    geom_histogram(aes(x=factor(year),y=ntl_change,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("NTL growth")+ggtitle("Average NTL growth by region")
    
    #ggsave("Figures/NTLchange_by_reigon.png")

    ggplot(mcn[which(mcn$year==2020),])+
    geom_histogram(aes(x=ntl_change,fill=R5),bins=7,position="dodge")+
    xlim(c(-1,1))+
    theme_bw()+
    xlab("NTL annual change in 2020")+
    ylab("Count")
    #ggsave("Figures/NTL_change_2020.png")

    ggplot(mcn[which(mcn$year %in% c(2015,2020)),])+
    geom_histogram(aes(x=ntl_change,fill=factor(year)),bins=15,position="dodge")+
    xlim(c(-1,1))+
    theme_bw()+
    xlab("NTL annual change")+
    ylab("Count")
    #ggsave("Figures/NTL_change_20152020.png")
        

    library(lfe)
    glimpse(filtered_mcn)
    
    model1 <- felm(log(ntl)~year:R5|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$ntl>0),])
    summary(model1)
    stargazer(model1,type="text")

    ggplot(filtered_mcn)+
    #ggplot(mcn_g)+
    geom_line(aes(x=year,y=(ntl_change),group=gridcell_id,color=income_grp),alpha=0.2)+
    #geom_smooth(aes(x=year,y=(ntl_change)),method="lm")+
    #scale_color_viridis_c(option = "plasma") +  
    #geom_line(aes(x=year,y=(ntl_change),group=gridcell_id,color=income_grp),alpha=0.2)+
    #ylim(c(1,16))+
    coord_cartesian(xlim=c(2014,2021))+
    guides(color="none")+
    theme_bw()+
    ylab("NTL annual change (%)")

    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=(ntl_change),group=gridcell_id,color=income_grp),alpha=0.02)+
    geom_smooth(aes(x=year,y=(ntl_change)),method="lm")+
    coord_cartesian(xlim=c(2014,2021),ylim=c(0,0.1))+
    #scale_y_continuous(trans="log")+
    guides(color="none")+
    theme_bw()+
    ylab("NTL annual change (%)")
    
    #ggsave("Figures/NTLchange_filtered_lm.png")
    model2 <- felm(ntl_change~year:R5+I(GDP/Population)|gridcell_id+countrycode|0|gridcell_id,data=filtered_mcn[which(filtered_mcn$ntl>0),])
    summary(model2)
    stargazer(model2,type="text")


## Plots NTL (start)

## Plots Mangroves (start)

    glimpse(filtered_mcn)

    #filtered_mcn <- mcn %>%
    #group_by(gridcell_id) %>%
    #filter(sum(which(abs(annual_area_change)>1))<1) #filtering out any gridcell with ntl changes larger than 10


    change_area <- ggplot(mcn)+
    geom_line(aes(x=year,y=annual_area_change,group=gridcell_id,color=R5),alpha=0.2)+
    ylab("Area change")+
    coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+theme_bw()

    change_area
    
    #model3 <- felm(annual_area_change~year|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    #summary(model3)

    mcn$mangrove_area[which(mcn$year==1996)]
    mcn$year <- as.double(mcn$year)
    
    ## Plot Area timeseries (start)
    
        glimpse(mcn)
        mcn_grouped <- mcn %>%
        arrange(gridcell_id,year)  %>% group_by(gridcell_id)%>%
        filter(year %in% c(2007,2020)) %>%
        mutate(long_change =mangrove_area / dplyr::lag(mangrove_area)-1)
        mcn_grouped_area <- mcn_grouped
        q_area <- quantile(mcn_grouped$long_change,probs = c(0.25,0.75), na.rm = TRUE)
        id_low_area <- mcn_grouped$gridcell_id[which(mcn_grouped$long_change < q_area[1])]
        id_high_area <- mcn_grouped$gridcell_id[which(mcn_grouped$long_change > q_area[2])]
        mcn$qarea <- "med"
        mcn$qarea[which(mcn$gridcell_id %in% id_low_area)] <- "low"
        mcn$qarea[which(mcn$gridcell_id %in% id_high_area)] <- "high"

        area_mcn_sum <- aggregate(mangrove_area~year+qarea,FUN="sum",data=mcn[which(mcn$year>2006),])
        area_mcn_sum_temp <- aggregate(sst~year+qarea,FUN="mean",data=mcn[which(mcn$year %in% unique(area_mcn_sum$year)),])
        area_mcn_sum$sst <- area_mcn_sum_temp$sst
        glimpse(area_mcn_sum)

        ggplot(area_mcn_sum)+
        geom_point(aes(x=sst,y=mangrove_area),alpha=0.2)
        
        plot_sum_area <- ggplot(area_mcn_sum)+
        geom_point(aes(x=year,y=mangrove_area,col=factor(qarea)))+
        #scale_color_scico(palette="hawaii")+
        geom_smooth(aes(x=year,y=mangrove_area,col=factor(qarea)))+
        theme_bw()+
        ylab("Log Mangrove area")
        plot_sum_area

        a07 <- area_mcn_sum[which(area_mcn_sum$year==2007),c(2,3)]
        names(a07)[2] <- "a07"
        area_mcn_sum <- merge(area_mcn_sum,a07,by="qarea",all=T)
        area_mcn_sum$area_perc <- 100*area_mcn_sum$mangrove_area/area_mcn_sum$a07

        plot_sum_area <- ggplot(area_mcn_sum)+
        geom_point(aes(x=year,y=area_perc,col=factor(qarea)))+
        #scale_color_scico(palette="hawaii")+
        geom_smooth(aes(x=year,y=area_perc,col=factor(qarea)))+
        theme_bw()+
        ylab("Log Mangrove area")
        plot_sum_area

    ## Plot Area timeseries (end)

    ## Plot Area timeseries (start)
    
        glimpse(mcn)
        mcn_grouped <- mcn %>%
        arrange(gridcell_id,year)  %>% group_by(gridcell_id)%>%
        filter(year %in% c(2007,2020)) %>%
        mutate(long_changenp =np / dplyr::lag(np)-1)
        glimpse(mcn_grouped)
        mcn_grouped_np <- mcn_grouped
        q_np <- quantile(mcn_grouped$long_changenp,probs = c(0.25,0.75), na.rm = TRUE)
        id_low_np <- mcn_grouped$gridcell_id[which(mcn_grouped$long_changenp < q_np[1])]
        id_high_np <- mcn_grouped$gridcell_id[which(mcn_grouped$long_changenp > q_np[2])]
        mcn$q_np <- "med"
        mcn$q_np[which(mcn$gridcell_id %in% id_low_np)] <- "low"
        mcn$q_np[which(mcn$gridcell_id %in% id_high_np)] <- "high"

        np_mcn_sum <- aggregate(np~year+q_np,FUN="sum",data=mcn[which(mcn$year>2006),])
        np_mcn_sum_temp <- aggregate(sst~year+q_np,FUN="mean",data=mcn[which(mcn$year %in% unique(np_mcn_sum$year)),])
        np_mcn_sum$sst <- np_mcn_sum_temp$sst
        glimpse(np_mcn_sum)

        ggplot(np_mcn_sum)+
        geom_point(aes(x=sst,y=np),alpha=0.2)
        
        plot_sum_np <- ggplot(np_mcn_sum)+
        geom_point(aes(x=year,y=np,col=factor(q_np)))+
        #scale_color_scico(palette="hawaii")+
        geom_smooth(aes(x=year,y=np,col=factor(q_np)))+
        theme_bw()+
        ylab("Log Mangrove area")
        plot_sum_np

        a07 <- np_mcn_sum[which(np_mcn_sum$year==2007),c(2,3)]
        names(a07)[2] <- "a07"
        np_mcn_sum <- merge(np_mcn_sum,a07,by="q_np",all=T)
        np_mcn_sum$np_perc <- 100*np_mcn_sum$np/np_mcn_sum$a07

        plot_sum_np <- ggplot(np_mcn_sum)+
        geom_point(aes(x=year,y=np_perc,col=factor(q_np)))+
        #scale_color_scico(palette="hawaii")+
        geom_smooth(aes(x=year,y=np_perc,col=factor(q_np)))+
        theme_bw()+
        ylab("Log Mangrove area")
        plot_sum_np



        



        mcn_grouped_area$long_changenp <- mcn_grouped_np$long_changenp

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
        
        ## Calculating Area and NP percentiles (start)
            gpkg_file <- "C:/Users/basti/Box/Data/Oceans/Mangroves/Nightlights/grid_nighlights_spatial.gpkg"
            gridcell_data <- st_read(gpkg_file)
            gridcell_sf <- st_as_sf(gridcell_data)
            #glimpse(gridcell_sf)
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

    ## Plot individual study unit high fragmentation low area loss (start)
        low_arealoss_high_fragmentation <- unique(mcn$gridcell_id[which(mcn$qarea =="33 - 66%" & mcn$q_np =="66 - 100%" )])
        low_arealoss_high_fragmentation <- unique(mcn$gridcell_id[which(mcn$qarea =="66 - 100%" & mcn$q_np =="66 - 100%" )])
        glimpse(mcn_grouped_area)
        m2 <- mcn_grouped_area[which(mcn_grouped_area$gridcell_id %in% low_arealoss_high_fragmentation),]
        glimpse(m2)
        m2$Longitude[which(m2$long_changenp==max(m2$long_changenp,na.rm=TRUE) )]        
        m2$Latitude[which(m2$long_changenp==max(m2$long_changenp,na.rm=TRUE) )]

        m3 <- cbind(m2$Longitude,m2$Latitude,m2$mangrove_area,m2$long_changenp)
        m3 <- m3[order(m3[,3]),]
        m3
        
        library(raster)
        r07 <- raster::raster("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_2007_gtiff//gmw_v3_2007/GMW_S03W045_2007_v3.tif")
        r20 <- raster::raster("C:\\Users\\basti\\Box\\Data\\Oceans\\Mangroves\\mangrove_watch\\rasters\\gmw_v3_2020_gtiff//gmw_v3_2020/GMW_S03W045_2020_v3.tif")
        #here

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
        map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 11)
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
                geom_raster(data = df_p07[which(!is.na(df_p07$patches)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_p07$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2007")+theme_map()+
                coord_sf(xlim = c(-44.9, -44.4), ylim = c(-3, -3.4))
            map_patch_07
            
            df_p20$color <- factor(df_p20$patch, levels = unique(df_p20$patch), 
                                labels = pal_scico[1:(dim(patch_counts20)[1])])
                                
            map_patch_20 <- ggmap(map) +
                geom_raster(data = df_p20[which(!is.na(df_p20$patches)),], aes(x = x, y = y, fill = color), na.rm = TRUE, hjust = 0, vjust = 0) +
                scale_fill_manual(values = levels(df_p20$color),guide="none") +    
                coord_quickmap()+
                ggtitle("2020")+
                coord_sf(xlim = c(-44.9, -44.4), ylim = c(-3, -3.4))+theme_map()
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
            ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft\\patches_2007_2020.png",g)

            g <- arrangeGrob(map_patch_07,map_patch_20,patch_area_bar,leg_plot, ncol=4,nrow=1,widths=c(5,5,2,1))
            ggsave(file="C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\patches_2007_2020.png",g)



        ymin <- -3
        ymax <- -2
        xmin <- 133
        xmax <- 134
        e <- raster::extent(c(xmin, xmax, ymin, ymax))  # Replace xmin, xmax, ymin, ymax with the desired coordinates
        raster::extent(r20)
        plot(r20)
        # Crop the raster
        r20_zoomed <- crop(r20, e)

        # Now you can plot the zoomed raster
        plot(r20_zoomed)

        bbox <- c(left = xmin, bottom = ymin, right = xmax, top =ymax)
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
        ggtitle("2020")
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
    ## Plot individual study unit high fragmentation low area loss (end)

    mcn$holes_density <- mcn$holes / mcn$mangrove_area
    mcn$np_density <- mcn$np / mcn$mangrove_area
    
    no_country_id <- unique(mcn$gridcell_id[which(is.na(mcn$R5) & mcn$mangrove_area >0)])
    
    vars <- c("holes", "mangrove_area","np")
        
    mcn_c_sum <-  mcn %>%
            group_by(countrycode,year,R5) %>%
            summarise(across(all_of(vars), sum, na.rm = FALSE))
    
    vars <- c("pafrac")   
    mcn_c_median <-  mcn %>%
            group_by(countrycode,year,R5) %>%
            summarise(across(all_of(vars), mean, na.rm = FALSE))
    mcn_c <- merge(mcn_c_sum,mcn_c_median,by=c("year","countrycode","R5"))
    glimpse(mcn_c)


    ggplot(mcn_c)+geom_point(aes(x=year,y=pafrac))

    mcn_c$holes_density <- mcn_c$holes / mcn_c$mangrove_area
    mcn_c$np_density <- mcn_c$np / mcn_c$mangrove_area
    mcn_c$patch_size <- mcn_c$mangrove_area / mcn_c$np
    
    mcn_c <- mcn_c %>%
        group_by(countrycode) %>%
        arrange(year) %>%
        mutate(holes_change = holes / dplyr::lag(na.locf(holes, na.rm = FALSE)-1),
               area_change = mangrove_area - dplyr::lag(na.locf(mangrove_area, na.rm = FALSE)),
               holes_d_change = holes_density - dplyr::lag(na.locf(holes_density, na.rm = FALSE)),
               patch_size_change = patch_size / dplyr::lag(na.locf(patch_size, na.rm = FALSE)-1) ,
               np_change = np / dplyr::lag(na.locf(np, na.rm = FALSE)-1) ,
               pafrac_change = pafrac / dplyr::lag(na.locf(pafrac, na.rm = FALSE)-1)  ,
               np_d_change = np_density / dplyr::lag(na.locf(np_density, na.rm = FALSE))-1) %>%
        ungroup() %>%  
        arrange(countrycode, year)
        glimpse(mcn_c)

        mcn_c <- mcn_c %>%
        mutate(holes_pos = ifelse(holes_change >= 0, 1, 0),
               area_pos = ifelse(area_change >= 0, 1, 0),
               holes_d_pos = ifelse(holes_d_change >= 0, 1, 0),
               patch_size_pos = ifelse(patch_size_change >= 0, 1, 0),
               np_pos = ifelse(np_change >= 0, 1, 0),
               pafrac_pos = ifelse(pafrac_change >= 0, 1, 0),
               np_d_pos = ifelse(np_d_change >= 0, 1, 0))



    # specify variables and corresponding pos flags
        vars <- c("holes_change", "area_change","holes_d_change", "patch_size_change", "np_change", "pafrac_change", "np_d_change")
        pos_flags <- c("holes_pos","area_pos", "holes_d_pos", "patch_size_pos", "np_pos", "pafrac_pos", "np_d_pos")

        mcn_c <- mcn_c[which(!is.na(mcn_c$R5)),]
        install.packages("spatstat.utils")
library(spatstat.utils)
        # loop over each variable/flag pair
        for(i in seq_along(vars)){
        # get variable and flag names
        var <- vars[i]
        pos_flag <- pos_flags[i]
        
        # create summary and store in list
        summary_dfs_median_df <- mcn_c %>%
            group_by(R5, year, !!sym(pos_flag)) %>%
            summarise(across(all_of(var), median, na.rm = TRUE)) %>% as.data.frame()
        names(summary_dfs_median_df)[3] <- "pos"
        
        summary_dfs_sum_df <- mcn_c %>%
            group_by(R5, year, !!sym(pos_flag)) %>%
            summarise(across(all_of(var), sum, na.rm = TRUE)) %>% as.data.frame()
        names(summary_dfs_sum_df)[3] <- "pos"
        
        summary_dfs_total_median_df <- mcn_c %>%
            group_by(year) %>%
            summarise(across(all_of(var), median, na.rm = TRUE)) %>% as.data.frame()
        
        summary_dfs_total_sum_df <- mcn_c %>%
            group_by(year) %>%
            summarise(across(all_of(var), sum, na.rm = TRUE)) %>% as.data.frame()
            if(i==1){
                summary_dfs_median <- summary_dfs_median_df
                summary_dfs_sum <- summary_dfs_sum_df
                summary_dfs_total_median <- summary_dfs_total_median_df
                summary_dfs_total_sum <- summary_dfs_total_sum_df
            } else{
                summary_dfs_median <- merge(summary_dfs_median,summary_dfs_median_df,by=c("year","R5","pos"),all=TRUE)
                summary_dfs_sum <- merge(summary_dfs_sum,summary_dfs_sum_df,by=c("year","R5","pos"),all=TRUE)
                summary_dfs_total_median <- merge(summary_dfs_total_median,summary_dfs_total_median_df,by=c("year"),all=TRUE)
                summary_dfs_total_sum <- merge(summary_dfs_total_sum,summary_dfs_total_sum_df,by=c("year"),all=TRUE)
            }
        }

        glimpse(summary_dfs_median)
        glimpse(summary_dfs_median_df)
        glimpse(summary_dfs_total_sum)
        sum(mcn_c$area_change,na.rm=T)


        #ag_r5_pos <- merge(summary_dfs_median,summary_dfs_sum,by=c("year","R5"),suffixes=c("_median","_sum"))
        #ag_total <- merge(summary_dfs_total_median,summary_dfs_total_sum,by=c("year"),suffixes=c("_median","_sum"))
        #glimpse(ag_total)
        #glimpse(ag_r5_pos)

        y_i <- c(2016 ,2017 ,2018 ,2019)
        y_i <- c(1996,ag_total$year[which(ag_total$holes_change_median!=0)] )#[1] 2007 2008 2009 2016 2017 2018 2019
        
 
        library(ggbreak) 
        library(scico) 
                    
                    summary_dfs_sum <- summary_dfs_sum[which(summary_dfs_sum$R5!="REF"),]
                    num_levels <- length(unique(summary_dfs_sum$R5))
                    color_vector <- scico(n = num_levels, palette = "batlow")

        ggplot(summary_dfs_sum[which(summary_dfs_sum$year %in%y_i ),])+
                    geom_bar(aes(x=(year),y=area_change,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Area Change (km2)")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    #scale_fill_manual(values = color_vector) +
                    #scale_color_manual(values = color_vector)+
                    scale_fill_manual(values = color_vector)+
                    #scale_fill_scico_d()+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                        aes(x=year,y=cumsum(area_change)),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE)) + 
                    scale_x_break(c(1998.5,2006))+ 
                    scale_x_break(c(2010.5,2015))
        ggsave("Figures/Draft/Area_CountryLevel.png",dpi=600)

        
            
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
    mcn$holes_density <- mcn$holes / mcn$mangrove_area
    glimpse(mcn)
    
    mcn$np_density <- mcn$np / mcn$mangrove_area
    mcn <- mcn %>%
        group_by(gridcell_id) %>%
        arrange(year) %>%
        mutate(holes_change = holes - dplyr::lag(na.locf(holes, na.rm = FALSE)-1),
               area_change = mangrove_area - dplyr::lag(na.locf(mangrove_area, na.rm = FALSE)),
               holes_d_change = holes_density - dplyr::lag(na.locf(holes_density, na.rm = FALSE)),
               patch_size_change = patch_size / dplyr::lag(na.locf(patch_size, na.rm = FALSE)-1) ,
               np_change = np / dplyr::lag(na.locf(np, na.rm = FALSE)-1) ,
               pafrac_change = pafrac / dplyr::lag(na.locf(pafrac, na.rm = FALSE)-1)  ,
               np_d_change = np_density / dplyr::lag(na.locf(np_density, na.rm = FALSE))-1) %>%
        ungroup() %>%  
        arrange(gridcell_id, year)
        
    my <- aggregate(mangrove_area~year,FUN="sum",data=mcn)
    weighted_means_h <- mcn %>%
                        group_by(year) %>%
                        mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                        summarise(holes_weighted_mean = sum(holes * weight, na.rm = TRUE)) %>% as.data.frame()
    weighted_means_h <- merge(weighted_means_h,my,by="year")
    
    weighted_means_h$holes_density <- weighted_means_h$holes_weighted_mean / weighted_means_h$mangrove_area
    weighted_means_h$holes_perha <- weighted_means_h$holes_density*100
    ggplot(weighted_means_h,aes(x=year,y=holes_perha))+geom_line()

    holes_perha1996 <- weighted_means_h$holes_perha[weighted_means_h$year==1996]
    weighted_means_h$change_holesoerha_wrt1996 <- weighted_means_h$holes_perha-holes_perha1996
    ggplot(weighted_means_h,aes(x=year,y=(change_holesoerha_wrt1996)))+geom_line()
    write.csv(weighted_means_h,"Data/output/weighted_means_h.csv")

    weighted_means_h_R5 <- mcn %>%
                        group_by(R5) %>%
                        mutate(weight = mangrove_area / sum(mangrove_area, na.rm = TRUE)) %>%
                        summarise(holes_weighted_mean = sum(holes * weight, na.rm = TRUE)) %>% as.data.frame()
    my <- aggregate(mangrove_area~year,FUN="sum",data=mcn)
    weighted_means_h_R5 <- merge(weighted_means_h,my,by="year")

        # Calculate the 0.01 and 0.99 quantiles
        lower_limit <- quantile(mcn$holes_d_change, 0.001, na.rm = TRUE)
        upper_limit <- quantile(mcn$holes_d_change, 0.999, na.rm = TRUE)

        # Remove outliers
        mcn <- mcn[mcn$holes_d_change > lower_limit & mcn$holes_d_change < upper_limit, ]

        glimpse(mcn)
        aggregate(mangrove_area~year,FUN="sum",data=mcn)

    mcn <- mcn %>%
        mutate(holes_pos = ifelse(holes_change >= 0, 1, 0),
               area_pos = ifelse(area_change >= 0, 1, 0),
               holes_d_pos = ifelse(holes_d_change >= 0, 1, 0),
               patch_size_pos = ifelse(patch_size_change >= 0, 1, 0),
               np_pos = ifelse(np_change >= 0, 1, 0),
               pafrac_pos = ifelse(pafrac_change >= 0, 1, 0),
               np_d_pos = ifelse(np_d_change >= 0, 1, 0))

        # specify variables and corresponding pos flags
        vars <- c("holes_change", "area_change","holes_d_change", "patch_size_change", "np_change", "pafrac_change", "np_d_change")
        pos_flags <- c("holes_pos","area_pos", "holes_d_pos", "patch_size_pos", "np_pos", "pafrac_pos", "np_d_pos")

        # initialize an empty list to store each summary dataframe
        summary_dfs_median <- list()
        summary_dfs_sum <- list()
        summary_dfs_total_median <- list()
        summary_dfs_total_sum <- list()

        summ_median_holes <- mcn %>%
                group_by("year") %>%
                summarise(holes_d_, median, na.rm = TRUE) %>% as.data.frame()
            names(summary_dfs_median_df)[3] <- "pos"


        for(i in seq_along(vars)){
            # get variable and flag names
            var <- vars[i]
            pos_flag <- pos_flags[i]
            
            # create summary and store in list
            summary_dfs_median_df <- mcn %>%
                group_by(R5, year, !!sym(pos_flag)) %>%
                summarise(across(all_of(var), median, na.rm = TRUE)) %>% as.data.frame()
            names(summary_dfs_median_df)[3] <- "pos"
            
            summary_dfs_sum_df <- mcn %>%
                group_by(R5, year, !!sym(pos_flag)) %>%
                summarise(across(all_of(var), sum, na.rm = TRUE)) %>% as.data.frame()
            names(summary_dfs_sum_df)[3] <- "pos"
            
            summary_dfs_total_median_df <- mcn %>%
                group_by(year) %>%
                summarise(across(all_of(var), median, na.rm = TRUE)) %>% as.data.frame()
            
            summary_dfs_total_sum_df <- mcn %>%
                group_by(year) %>%
                summarise(across(all_of(var), sum, na.rm = TRUE)) %>% as.data.frame()
                if(i==1){
                    summary_dfs_median <- summary_dfs_median_df
                    summary_dfs_sum <- summary_dfs_sum_df
                    summary_dfs_total_median <- summary_dfs_total_median_df
                    summary_dfs_total_sum <- summary_dfs_total_sum_df
                } else{
                    summary_dfs_median <- merge(summary_dfs_median,summary_dfs_median_df,by=c("year","R5","pos"),all=TRUE)
                    summary_dfs_sum <- merge(summary_dfs_sum,summary_dfs_sum_df,by=c("year","R5","pos"),all=TRUE)
                    summary_dfs_total_median <- merge(summary_dfs_total_median,summary_dfs_total_median_df,by=c("year"),all=TRUE)
                    summary_dfs_total_sum <- merge(summary_dfs_total_sum,summary_dfs_total_sum_df,by=c("year"),all=TRUE)
                }
        }


        glimpse(summary_dfs_median)
        glimpse(summary_dfs_median_df)
        glimpse(summary_dfs_total_sum)
        sum(mcn_c$area_change,na.rm=T)


        #ag_r5_pos <- merge(summary_dfs_median,summary_dfs_sum,by=c("year","R5"),suffixes=c("_median","_sum"))
        #ag_total <- merge(summary_dfs_total_median,summary_dfs_total_sum,by=c("year"),suffixes=c("_median","_sum"))
        #glimpse(ag_total)
        #glimpse(ag_r5_pos)

        y_i <- c(2016 ,2017 ,2018 ,2019)
        y_i <- c(1996,ag_total$year[which(ag_total$holes_change_median!=0)] )#[1] 2007 2008 2009 2016 2017 2018 2019
        
 
        library(ggbreak) 
        library(scico) 
                    
                    summary_dfs_sum <- summary_dfs_sum[which(summary_dfs_sum$R5!="REF"),]
                    color_vector <- scico(n = 5, palette = "batlow")

        
        y_i <- c(1996,ag_total$year[which(ag_total$holes_change_median!=0)] )#[1] 2007 2008 2009 2016 2017 2018 2019
        #y_i <- c(2016 ,2017 ,2018 ,2019,2020)
         
        
        summary_dfs_sum$color <- " "
        summary_dfs_total_sum$color <- " "
        summary_dfs_sum <- summary_dfs_sum[which(!is.na(summary_dfs_sum$R5)),]

        summary_dfs_total_sum <- merge(summary_dfs_total_sum,data.frame(year=1996,area_change=0),by=c("year","area_change"),all=T)

        bars_sumsum_area_short <- ggplot(summary_dfs_sum[which(summary_dfs_sum$year %in%y_i ),])+
                    geom_bar(aes(x=(year),y=area_change,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Area Change \n(km2)")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                        aes(x=year,y=cumsum(area_change),color=factor(color)),size=1.5)+
                    theme_bw() + 
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))+ 
                    guides(fill=guide_legend(reverse = TRUE))+ 
                    scale_color_manual(values = c(" " = "indianred")) +
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
        bars_sumsum_area_short
        
        
        # bars_sumsum_holes_short <- ggplot(summary_dfs_sum[which(summary_dfs_sum$year %in%y_i ),])+
        #             geom_bar(aes(x=(year),y=holes_change,
        #                 fill=factor(R5)),stat="identity")+
        #             xlab("Year")+ylab("Number of Gaps")+
        #             labs(fill=guide_legend("Region"))+
        #             scale_fill_manual(values = color_vector)+
        #             geom_hline(aes(yintercept=0),linetype="dashed")+
        #             geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
        #                 aes(x=year,y=cumsum(holes_change),color=factor(color)),size=1.5)+
        #             theme_bw() +
        #             scale_color_manual(values = c(" " = "indianred")) +
        #             scale_x_break(c(1998.5,2006))+ 
        #             scale_x_break(c(2010.5,2015))+ 
        #             guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))
        # bars_sumsum_holes_short
        
        # library(cowplot)

        # plot_grid(bars_sumsum_area_short, bars_sumsum_holes_short)

        # library(patchwork)

        # bars_sumsum_area_short + bars_sumsum_holes_short


        # ggarrange(bars_sumsum_area_short,bars_sumsum_holes_short)
        # ggsave("Figures/Draft/Bars_Line_Historial_SHORT.png",dpi=600)

        summary_dfs_total_sum[which(summary_dfs_total_sum$year==1996),-c(1)] <- 0
        summary_dfs_total_median
        
        summary_dfs_total_sum$color <-" "
        bars_sumsum_holes_short <- ggplot(summary_dfs_median[which(summary_dfs_median$year %in%y_i ),])+
                    geom_bar(aes(x=(year),y=holes_d_change,
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Number of \nGaps per Area")+
                    labs(fill=guide_legend("Region"))+
                    scale_fill_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    #geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],aes(x=year,y=cumsum(holes_d_change),color=factor(color)),size=1.5)+
                    geom_line(data=summary_dfs_total_median[which(summary_dfs_total_median$year %in% y_i),],aes(x=year,y=cumsum(holes_d_change),color=factor(color)),size=1.5)+
                    scale_color_manual(values = c(" " = "indianred")) +
                    guides(fill=guide_legend(reverse = TRUE),color=guide_legend(title="Net Cumulative \nChange"))+
                    theme_bw() +theme(legend.position="bottom")+
                    scale_x_break(c(1998.8,2006))+ 
                    scale_x_break(c(2010.5,2015))
                    #scale_x_continuous(breaks = c(1998.8, 2006, 2010.5, 2015))
                    bars_sumsum_holes_short
        leg <- get_legend(bars_sumsum_holes_short)
        
        ((bars_sumsum_area_short+ theme(legend.position = "none")) /(( bars_sumsum_holes_short)+ theme(legend.position = "none"))/(leg)) +
                plot_layout(heights=c(3,3,1))

                write.csv(summary_dfs_total_sum,"Data/Fig_Vals/Past_Area_Change.csv")
                write.csv(summary_dfs_total_median,"Data/Fig_Vals/Past_All_Median.csv")
                
                cumsum(summary_dfs_total_median$holes_d_change)
  #ggsave("Figures/Draft/Bar_CumLine_Historical_Long.png",dpi=600)

summary_dfs_total_sum

        combined_df_median <- ungroup(bind_rows(summary_dfs_median))
        combined_df_sum <- ungroup(bind_rows(summary_dfs_sum))
        
        combined_df_total_median<- Reduce(function(df1, df2) full_join(df1, df2, by = c("year")), summary_dfs_total_median)
        combined_df_total_sum<- Reduce(function(df1, df2) full_join(df1, df2, by = c("year")), summary_dfs_total_sum)

        ag_r5_pos <- merge(combined_df_median,combined_df_sum,by=c("year","R5"),suffixes=c("_median","_sum"))
        ag_total <- merge(combined_df_total_median,combined_df_total_sum,by=c("year"),suffixes=c("_median","_sum"))
        glimpse(ag_total)

        y_i <- c(2016 ,2017 ,2018 ,2019)
        y_i <- c(1996,ag_total$year[which(ag_total$holes_change_median!=0)] )#[1] 2007 2008 2009 2016 2017 2018 2019
        
 
                        library(ggbreak) 
        ggplot(ag_r5_pos[which(ag_r5_pos$year %in%y_i),])+
                    geom_bar(aes(x=(year),y=patch_size_change_median,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Holes Change")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    #scale_fill_manual(values = color_vector) +
                    #scale_color_manual(values = color_vector)+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=ag_total[which(ag_total$year %in% y_i),],
                        aes(x=year,y=cumsum(patch_size_change_median)),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE)) + 
                    scale_x_break(c(1997,2006))+ 
                    scale_x_break(c(2010,2015))
        
        y_i <- ag_total$year[which(ag_total$holes_change_median!=0)]
        y_i <- c( 2008 ,2009,2016 ,2017 ,2018 ,2019)
        y_i <- c(2016 ,2017 ,2018 ,2019)
        ggplot(ag_r5_pos[which(ag_r5_pos$year %in%y_i),])+
                    geom_bar(aes(x=(year),y=patch_size_change_median,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Gaps")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    #scale_fill_manual(values = color_vector) +
                    #scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=ag_total[which(ag_total$year %in% y_i),],
                        aes(x=(year),y=patch_size_change_median),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
        
        y_i <- ag_total$year[which(ag_total$holes_change_median!=0)]
        y_i <- c( 2008 ,2009,2016 ,2017 ,2018 ,2019)
        y_i <- c(2016 ,2017 ,2018 ,2019)
        ggplot(ag_r5_pos[which(ag_r5_pos$year %in%y_i),])+
                    geom_bar(aes(x=(year),y=pafrac_change_median,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Gaps")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    #scale_fill_manual(values = color_vector) +
                    #scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=ag_total[which(ag_total$year %in% y_i),],
                        aes(x=(year),y=pafrac_change_median),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))

        y_i <- ag_total$year[which(ag_total$holes_change_median!=0)]
        y_i <- c( 2008 ,2009,2016 ,2017 ,2018 ,2019)
        y_i <- c(2016 ,2017 ,2018 ,2019)
        ggplot(ag_r5_pos[which(ag_r5_pos$year %in%y_i),])+
                    geom_bar(aes(x=(year),y=area_change_sum,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Gaps")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    #scale_fill_manual(values = color_vector) +
                    #scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=ag_total[which(ag_total$year %in% y_i),],
                        aes(x=(year),y=cumsum(area_change_sum)),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))

        sum(mcn$mangrove_area[which(mcn$year==1996)],na.rm=T)
        sum(mcn$mangrove_area[which(mcn$year==2020)],na.rm=T) - sum(mcn$mangrove_area[which(mcn$year==1996)],na.rm=T)


        #         # join all dataframes in the list into one dataframe
        # combined_df <- Reduce(function(df1, df2) full_join(df1, df2, by = c("R5", "year")), summary_dfs)


mcn %>%
        group_by(R5, year, holes_pos) %>%
        summarise(across(all_of(mean_cols), mean, na.rm = TRUE),  # mean for specified columns
                    across(all_of(sum_cols), sum, na.rm = TRUE))   # sum for specified columns

    
    mcn$holes_pos <- 1
    mcn$holes_pos[which(mcn$holes_d_change<0)]<-0
    
    ag_mcn_holes <- aggregate(holes_density_change~R5+year+holes_pos,data=mcn,FUN="median")
    agg_aloss_neg_total <- aggregate(holes_density_change~year,data=mcn,FUN="median")

    ggplot(ag_mcn_holes)+
                    geom_bar(aes(x=year,y=holes_density_change,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Gaps")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=holes_density_change),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
    
    mcn <- mcn %>%
        group_by(gridcell_id) %>%
        arrange(year) %>%
        mutate(holes_change = holes - dplyr::lag(na.locf(holes, na.rm = FALSE)),
               holes_d_change = holes_density - dplyr::lag(na.locf(holes_density, na.rm = FALSE)),
               patch_size_change = patch_size - dplyr::lag(na.locf(patch_size, na.rm = FALSE)) ,
               np_change = np - dplyr::lag(na.locf(np, na.rm = FALSE)) ,
               pafrac_change = pafrac - dplyr::lag(na.locf(pafrac, na.rm = FALSE))  ,
               np_d_change = np_density - dplyr::lag(na.locf(np_density, na.rm = FALSE))) %>%
        ungroup() %>%  
        arrange(gridcell_id, year)
    
    mcn$holes_pos <- 1
    mcn$holes_pos[which(mcn$holes_change<0)]<-0
    
    ag_mcn_holes <- aggregate(holes_change~R5+year+holes_pos,data=mcn,FUN="sum")
    agg_aloss_neg_total <- aggregate(holes_change~year,data=mcn,FUN="sum")

    ggplot(ag_mcn_holes)+
                    geom_bar(aes(x=year,y=holes_change,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Gaps")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=holes_change),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))

    mcn <- mcn %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(patch_size_change = patch_size - dplyr::lag(patch_size))%>% ungroup() %>%  arrange(gridcell_id,year)
    
    mcn$holes_pos <- 1
    mcn$holes_pos[which(mcn$patch_size_change<0)]<-0
    
    ag_mcn_holes <- aggregate(patch_size_change~R5+year+holes_pos,data=mcn,FUN="mean")
    agg_aloss_neg_total <- aggregate(patch_size_change~year,data=mcn,FUN="mean")

    ggplot(ag_mcn_holes)+
                    geom_bar(aes(x=year,y=patch_size_change,
                        #fill=factor(holes_pos)),stat="identity")+
                        fill=factor(R5)),stat="identity")+
                    xlab("Year")+ylab("Change in Gaps")+
                    labs(fill=guide_legend("Region"))+
                    #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    scale_fill_manual(values = color_vector) +
                    scale_color_manual(values = color_vector)+
                    #geom_hline(aes(yintercept=0),linetype="dashed")+
                    geom_line(data=agg_aloss_neg_total,aes(x=year,y=patch_size_change),color="indianred",size=1.5)+
                    theme_bw()+ guides(fill=guide_legend(reverse = TRUE))


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

    np_mcn_sum <- aggregate(np~year,FUN="sum",data=mcn)
    np_mcn_sum$mangrove_area <- area_mcn_sum$mangrove_area
    glimpse(np_mcn_sum)
    ggplot(np_mcn_sum)+
    geom_point(aes(x=year,y=np/mangrove_area),alpha=0.2)+
    geom_smooth(aes(x=year,y=np/mangrove_area))+
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



    np_ag <- aggregate(np~R5+year+countrycode,data=mcn,FUN="sum")
    glimpse(np_ag)
    np_ag <- aggregate(np~R5+year,data=np_ag,FUN="mean")
    np2015 <- np_ag[which(np_ag$year %in% c(2015)),]
    np2020 <- np_ag[which(np_ag$year %in% c(2020)),]
    np2015$np2020 <- np2020$np
    np2015$dif <- 100*(np2015$np2020 - np2015$np)/np2015$np

    ggplot(np2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Number of patches change (%)")+ggtitle("Mangrove patches change (2015 to 2020)")
    #ggsave("Figures/Mangrove_np_change.png")

    glimpse(mcn)
    np_ag <- aggregate(patch_size~R5+year+countrycode,data=mcn,FUN="mean")
    glimpse(np_ag)
    np_ag <- aggregate(patch_size~R5+year,data=np_ag,FUN="mean")
    np2015 <- np_ag[which(np_ag$year %in% c(2015)),]
    np2020 <- np_ag[which(np_ag$year %in% c(2020)),]
    np2015$patch_size2020 <- np2020$patch_size
    np2015$dif <- 100*(np2015$patch_size2020 - np2015$patch_size)/np2015$patch_size

    ggplot(np2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Number of patches change (%)")+ggtitle("Mangrove patches change (2015 to 2020)")
    ggsave("Figures/Mangrove_patchsize_change.png")

    ggplot(mcn)+
    geom_line(aes(x=year,y=(np),group=gridcell_id,color=R5),alpha=0.2)+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()+
    ylab("Log Mangrove area")
    ggsave("Figures/Mangrove_np.png")

    glimpse(mcn)
    ggplot(mcn)+
    geom_line(aes(x=year,y=log(patch_size),group=gridcell_id,color=R5),alpha=0.2)+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()+
    ylab("Log Patch size")
    ggsave("Figures/Mangrove_patchsize.png")

    np_ag <- aggregate(patch_size~R5+year,data=mcn,FUN="mean")
    glimpse(np_ag)
    np2015 <- np_ag[which(np_ag$year %in% c(2015)),]
    np2020 <- np_ag[which(np_ag$year %in% c(2020)),]
    np2015$patch_size2020 <- np2020$patch_size
    np2015$dif <- 100*(np2015$patch_size2020 - np2015$patch_size)/np2015$patch_size

    ggplot(np2015)+
    geom_histogram(aes(x=factor(year),y=dif,fill=R5),stat="identity",bins=7,position="dodge")+
    theme_bw()+
    xlab("Year")+
    ylab("Number of patches change (%)")+ggtitle("Mangrove patches change (2015 to 2020)")
    #ggsave("Figures/Mangrove_np_change.png")


    #table(mcn$annual_area_change)
    model3 <- felm(log(mangrove_area)~year|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model3)

    change_np <- ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=annual_np_change,group=gridcell_id,color=continent),alpha=0.2)+
    coord_cartesian(xlim=c(2016,2020))+
    ylab("Fragmentation change")+
    theme_bw()
    
    model4 <- felm(annual_np_change~year|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model4)

    ggarrange(change_area,change_np,common.legend=T,legend="bottom")
    ggsave("Figures/Mangrove_change.png")

    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=(np),group=gridcell_id,color=continent),alpha=0.2)+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()

    ggplot(filtered_mcn)+
    geom_line(aes(x=year,y=log(mangrove_area/np),group=gridcell_id,color=continent),alpha=0.2)+
    coord_cartesian(xlim=c(2016,2020))+
    theme_bw()
    
    model5 <- felm(log(np)~sst+I(sst^2)+year+log(mangrove_area)|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model5)

    filtered_mcn$patch_area <- filtered_mcn$mangrove_area/filtered_mcn$np


    ggplot(mcn)+
    geom_line(aes(x=(sst),y=annual_np_change,group=gridcell_id,color=continent),alpha=0.2)+
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


