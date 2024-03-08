
libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe")

lapply(libraries, library, character.only = TRUE)

mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
source("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Code\\Helper_Functions.R")
mcn_2020 <- mcn %>% filter(year == 2020)
glimpse(mcn)
mcn <- mcn %>% mutate(hot_bin = b31_32C+b32_33C+b33_34C+b34_35C+b35_36C+b36_37C+b37_38C)
ggplot(mcn %>% filter(mean_mangrove_area>0),aes(x=mean_mangrove_area,y=annual_area_change,group=gridcell_id,col=temp))+geom_point()#+xlim(c(2007,2020))
mcn$sst_hot_demeaned
hist(mcn$mean_mangrove_area)
### Model 1 and Diagnosis (Start)
    subset_mcn1 <- mcn[which(mcn$mean_mangrove_area>0 & 
                                                    #mcn$ntl>0 &
                                                    !is.na(mcn$countrycode) &
                                                    mcn$year>2014 &
                                                    mcn$pop>0 &
                                                    #mcn$gap_density>0  & 
                                                    #is.finite(mcn$annual_gap_density_change) & 
                                                    is.finite(mcn$logGDPpc_country) & 
                                                    is.finite(mcn$logGDPpc) & 
                                                    #mcn$mean_mangrove_area>0 & 
                                                    is.finite(mcn$annual_area_change) & mcn$annual_area_change>quantile(mcn$annual_area_change,0.01,na.rm=TRUE)& mcn$annual_area_change<quantile(mcn$annual_area_change,0.99,na.rm=TRUE)&
                                                    is.finite(mcn$logSal) & 
                                                    is.finite(mcn$gap_density) & 
                                                    is.finite(mcn$lag_gap_density) & 
                                                    is.finite(mcn$lag2_gap_density) & 
                                                    is.finite(mcn$lag3_gap_density) & 
                                                    is.finite(mcn$Mean_Precipitation) & 
                                                    is.finite(mcn$sst)  & 
                                                    is.finite(mcn$sst_hottest)  & 
                                                    is.finite(mcn$pop) 
                                                    #mcn$year>2012
                                                    ),]
    model_area1 <- 
                #felm(annual_area_change~
                #felm(mangrove_area~
                felm(log(mangrove_area)~
                #hot_bin+
                #felm(annual_area_change~
                #log(holes_size/holes)+
                #log(holes/mangrove_area)+
                #lag(holes)+
                #log(mangrove_area)+
                temp + I(temp^2)+
                #sst + I(sst^2)+
                sst_hottest + I(sst_hottest^2) +
                preci + I(preci^2)+ #I(Mean_Precipitation^2) +
                #Mean_Salinity + I(Mean_Salinity^2)+
                #mhw_int_anom+
                #Mean_Precipitation*logGDPpc+
                #Mean_Precipitation*logPop+
                logSal + #I(logSal^2)  + 
                #log(Sum_GDP_50km/Population_Count_50km)+
                #I(log(Sum_GDP_50km)/log(GDP))+
                #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                #log(Population_Count_50km)+ 
                #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                #log(GDP/Population)sst_hottest + I(sst_hottest^2)+
                #I(Mean_Precipitation^2) + Mean_Precipitation +
                #log(Mean_Salinity)+
                #factor(rich)*log(Sum_GDP_50km/Population_Count_50km)+ #
                #I(log(GDP/Population))+ 
                #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                #log(pop)+ I(log(pop)^2)+
                #+ 
                logGDPpc + I(logGDPpc^2)+
                #logPop + #factor(rich)*logGDPpc+factor(rich)*I(logGDPpc^2)+#I(logGDPpc^3)+
                #factor(rich)*logPop+factor(rich)*I(logPop^2)+#I(logPop^3)
                #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                #income:log(ntl) + income:I(log(ntl)^2)
                #log(ntl) + I(log(ntl)^2) + #log(GDP_country) + 
                I(logGDPpc/logGDPpc_country)+
                logGDPpc_country + I(logGDPpc_country^2)+ #I(logGDPpc_country^3)+
                #log(mangrove_area)+
                #gap_density+I(gap_density^2)+
                lag_gap_density+I(lag_gap_density^2)
                #lag2_gap_density+I(lag2_gap_density^2)+
                #lag3_gap_density+I(lag3_gap_density^2)#+
                #log(mangrove_area)#+
                #year:countrycode+
                #R5:year + 
                #income:year
                #year+I(year^2)
                |countrycode+year|0|gridcell_id,
                data=subset_mcn1,
                weights=(subset_mcn1$mangrove_area+1))
            summary(model_area1)

            model <- model_area1
            data <- subset_mcn1
            filename <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Area_Model\\Experiments\\Diagnostic_Model1_logArea_gap2_logmangroveareaPOS.png"
            variable <- log(subset_mcn1$mangrove_area)
            #variable <- subset_mcn1$annual_area_change
            #model_diagnostics(model, data, variable)
            adjust_and_plot(model, data, variable, filename,save=FALSE)
    
### Model 1 and Diagnosis (End)
##Plots SST Hot
        qgdp <- quantile(mcn$logGDPpc_country[which(mcn$mangrove_area>0 & mcn$year==2018)],0.66,na.rm=T)
        crich <- unique(mcn$countrycode[which(mcn$logGDPpc_country>qgdp)])
        mcn$rich <- 0
        mcn$rich[which(mcn$countrycode %in% crich)] <- 1
        mcn$logPop <- log(mcn$Population_Count)
        #mcn$lag_gap_density <- mcn$lag_holes / mcn$mangrove_area
        glimpse(mcn)
        
        
        model_area_notrends <- felm(I(log(mangrove_area))~
            sst_hottest + I(sst_hottest^2)+
            Mean_Precipitation +I(Mean_Precipitation^2) + 
           #log(Mean_Salinity) +  
           gap_density +  I(gap_density^2)+
           lag_gap_density+ I(lag_gap_density^2)+
        #log(lag2_gap_density)+ I(log(lag2_gap_density^2))+ 
        #log(lag3_gap_density)+ I(log(lag3_gap_density^2))+
            #I(lag_holes/mangrove_area)+#I(lag_holes^2)+
                logGDPpc+I(logGDPpc^2)+#+I(GDP_country/Population_country)
                year:countrycode + 
                                R5:year + 
                                income:year
            |gridcell_id+year |0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc) & (mcn$lag_gap_density>0) 
                #& (mcn$lag2_gap_density>0) & (mcn$lag3_gap_density>0)
                ),],
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc) & 
            (mcn$lag_gap_density>0) #& (mcn$lag2_gap_density>0)& (mcn$lag3_gap_density>0)
            )]+1)
        )
        
        summary(model_area_notrends)
        
        ggplot(data=mcn %>% filter(lag_gap_density>0),aes(y=log(mangrove_area),x=(lag_gap_density)))+
        geom_point()












        mcn$log_lag_holes <- log(mcn$lag_holes)
        
        model_area_ssthot2 <- felm(I(log(mangrove_area))~
            sst_hottest + I(sst_hottest^2)+
            Mean_Precipitation +I(Mean_Precipitation^2) + 
                logGDPpc+I(logGDPpc^2) + 
                #lag_holes + I(lag_holes^2)+
                log_lag_holes + I(log_lag_holes^2)+
                year:countrycode + 
                R5:year + 
                income:year
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)& is.finite(mcn$log_lag_holes)),],
        #weights=(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc))])
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc) & is.finite(mcn$log_lag_holes))]+1)
        )
        
        summary(model_area_ssthot2)
        ggplot(mcn)+
        geom_point(aes(x=log(mangrove_area),y=log(lag_holes)))
        geom_point(aes(x=log(mangrove_area),y=log(lag_holes_size)))
        
        sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area Loss")
            sq_estimate_preci_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"Mean_Precipitation","Area Loss")
            sq_estimate_pop_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"logPop","Area Loss")
            sq_estimate_gdp_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"logGDPpc","Area Loss")
            # ggplot(sq_estimate_pop_area)+geom_point(aes(x=temp,y=gestimated))
            # ggplot(sq_estimate_gdp_area)+geom_point(aes(x=temp,y=gestimated))

            sq_estimate_pop_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_pop_area$significant[which(sq_estimate_pop_area$p_value<0.05)] <- "Significant (p < 0.05)"
            sq_estimate_gdp_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_gdp_area$significant[which(sq_estimate_gdp_area$p_value<0.05)] <- "Significant (p < 0.05)"
            sq_estimate_sst_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_sst_area$significant[which(sq_estimate_sst_area$p_value<0.05)] <- "Significant (p < 0.05)"
            sq_estimate_preci_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_preci_area$significant[which(sq_estimate_preci_area$p_value<0.05)] <- "Significant (p < 0.05)"
        # save(sq_estimate_sst_area,file="Models/Round1/sq_estimate_sst_area.RData") #here
        #     save(sq_estimate_preci_area,file="Models/Round1/sq_estimate_preci_area.RData") 
        #     save(sq_estimate_gdp_area,file="Models/Round1/sq_estimate_gdp_area.RData") 
        #     save(sq_estimate_pop_area,file="Models/Round1/sq_estimate_pop_area.RData") 
            
