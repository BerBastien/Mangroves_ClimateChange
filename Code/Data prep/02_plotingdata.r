
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
    
    glimpse(mcn)
    mcn_grouped <- mcn %>%
    arrange(gridcell_id,year)  %>% group_by(gridcell_id)%>%
    filter(year %in% c(1996,2020)) %>%
    mutate(long_change =mangrove_area / dplyr::lag(mangrove_area)-1)
    
    glimpse(mcn_grouped)
    

    area_mcn_sum <- aggregate(mangrove_area~year+R5,FUN="sum",data=mcn)
    glimpse(area_mcn_sum)

    
    plot_sum_area <- ggplot(area_mcn_sum)+
    geom_point(aes(x=year,y=mangrove_area),alpha=0.2)+
    geom_smooth(aes(x=year,y=mangrove_area,col=R5))+
    #coord_cartesian(xlim=c(2004,2020))+
    #coord_cartesian(ylim=c(-1,1),xlim=c(2016,2020))+
    #coord_cartesian(xlim=c(1995,2020),ylim=c(2,2.5))+
    theme_bw()+
    ylab("Log Mangrove area")

    gaps_mcn_sum <- aggregate(holes~year+R5,FUN="sum",data=mcn)
    glimpse(gaps_mcn_sum)
    gaps_mcn_sum$mangrove_area <- area_mcn_sum$mangrove_area
    
    plot_sum_gaps <- ggplot(gaps_mcn_sum)+
    geom_point(aes(x=year,y=holes/mangrove_area),alpha=0.8)+
    geom_smooth(aes(x=year,y=holes/mangrove_area,color=R5))+
    #coord_cartesian(xlim=c(2007,2020),ylim=)+
    theme_bw()

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


