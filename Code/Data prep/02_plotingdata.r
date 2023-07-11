
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
    
    model3 <- felm(annual_area_change~year|gridcell_id|0|0,data=filtered_mcn[which(filtered_mcn$mangrove_area>0),])
    summary(model3)

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
        library(cowplot)
        
        #install.packages("biscale", dependencies = TRUE)
        library(biscale)
        library(sf)
        library("rnaturalearth")
        library("rnaturalearthdata")
        library(mapproj)
        
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
                
        # map <- ggplot() +
        #     geom_sf(data = data, mapping = aes(fill = bi_class, geometry=geom), 
        #     color = "white", size = 0.001, show.legend = FALSE) +
        #     bi_scale_fill(pal = "GrPink",
        #                     rotate_pal=TRUE,
        #                     #flip_axes=TRUE, 
        #                     dim = 3) +
        #     labs(
        #         title = "Magrove Change"#,
        #         #subtitle = "Gray Pink (GrPink) Palette"
        #     ) +
        #     bi_theme()
        # map

        

        # finalPlot <- ggdraw() +
        # draw_plot(map, 0, 0, 1, 1) +
        # draw_plot(legend, 0.1, .3, 0.2, 0.2)

        # finalPlot

        # ## Get BaseMAP
        #     library("ggmap")
        #     bbox <- c(left = -180, bottom = -40, right = 180, top =31)
        #     map <- get_stamenmap(bbox, maptype = "terrain-background", zoom = 4)
        #     ggmap(map)

        #     # Define a function to fix the bbox to be in EPSG:3857
        #     # ggmap_bbox <- function(map) {
        #     # if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
        #     # # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
        #     # # and set the names to what sf::st_bbox expects:
        #     # map_bbox <- setNames(unlist(attr(map, "bb")), 
        #     #                     c("ymin", "xmin", "ymax", "xmax"))

        #     # # Coonvert the bbox to an sf polygon, transform it to 3857, 
        #     # # and convert back to a bbox (convoluted, but it works)
        #     # bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

        #     # # Overwrite the bbox of the ggmap object with the transformed coordinates 
        #     # attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
        #     # attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
        #     # attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
        #     # attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
        #     # map
        #     # }

        #     # # Use the function:
        #     # map <- ggmap_bbox(map)
            
        # ## Get BaseMAP
            
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

            ggsave("Figures/Draft/Map_Change.png",dpi=2000)


            finalPlot <- ggdraw() +
                    draw_plot(mangrove_map_basemap, 0, 0, 1, 1) +
                    draw_plot(legend, x=0.14, y=.25, width = 0.2, height = 0.3)
            blank_plot <- ggplot() + theme_void()
            
            ggarrange(finalPlot,ggarrange(blank_plot,plot_sum_np,plot_sum_area,ncol=3,widths=c(1,3,3)),nrow=2,heights=c(3,2))
            ggsave("Figures/Draft/Fig_1_Map.png",dpi=600)


            mangrove_map_basemap <- ggplot()+
            #geom_sf(data = sea, fill = "lightblue") +
            geom_sf(data =world, color = "black", fill = "black")+
            geom_sf(data = data, mapping = aes(fill = bi_class, geometry=geom), 
            color = "white", size = 0, show.legend = FALSE, inherit.aes = FALSE) + 
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
            
            ggarrange(finalPlot,ggarrange(blank_plot,plot_sum_np,plot_sum_area,ncol=3,widths=c(1,3,3)),nrow=2,heights=c(3,2))
            ggsave("Figures/Draft/Fig_1_Map_whiteborder.png",dpi=600)

        
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
            ##Figure with distribution of classes (end)
            
    ## Plot dimension changes in map (end)

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


