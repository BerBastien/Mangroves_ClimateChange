libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe")

lapply(libraries, library, character.only = TRUE)

setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
mcn$GDPpc_country <- (mcn$GDP_country/mcn$Population_country)
mcn$logGDPpc_country <- log(mcn$GDPpc_country)
mcn_2020 <- mcn %>% filter(year == 2020)
## Model Area        
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
            #|gridcell_id + year+ countrycode+R5|0|gridcell_id,
            |year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],
        #weights=(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc))])
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc))]+1)
        )
        
        summary(model_area_ssthot)

        model_area_base <- model_area_ssthot

        
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
            
