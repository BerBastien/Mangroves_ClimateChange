libraries <- c( "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe","stargazer","textreadr")
install.packages(libraries)
install.packages("rvest")
library("rvest")
lapply(libraries, library, character.only = TRUE)

setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023_buffers.csv")
mcn$GDPpc_country <- (mcn$GDP_country/mcn$Population_country)
subset_mcn <- mcn[,c("b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", "b36_37C")]
mcn$hot_bin <- rowSums(subset_mcn, na.rm = TRUE)
subset_mcn <- mcn[,c("b8_9C", "b9_10C", "b10_11C", "b11_12C", "b12_13C", "b13_14C","b14_15C","b15_16C","b16_17C","b17_18C")]
subset_mcn <- mcn[,c("b8_9C", "b9_10C", "b10_11C", "b11_12C", "b12_13C")]
mcn$cold_bin <- rowSums(subset_mcn, na.rm = TRUE)
mcn$logGDPpc_country <- log(mcn$GDPpc_country)
mcn$abs_lat <- abs(mcn$Latitude)
mcn_2020 <- mcn %>% filter(year == 2020)
mcn$disc <- 0
mcn$disc[mcn$year<2013] <- 1
df_f <- mcn %>% filter(
                               
                                mean_mangrove_area>1 & 
                                #mean_mangrove_area>0.1 &
                                #lag_holes>0 & 
                                #np>0 &
                                #lag_gap_density>0 & 
                                #gap_density>0 &
                                #year > 2010 & 
                                #year > 2000 & 
                                #year <2014 &
                                #is.finite(logGDPpc_country) &
                                #is.finite(pafrac) &
                                is.finite(lag_gap_density_avg) &
                                #gap_density <50 &
                                #holes_size>0 &
                                #ntl > 0 &
                                is.finite(logGDPpc)# & 
                                #is.finite(mcw_int)
                                #abs(Latitude)>20
                                )
        df_f$lag_gap_density <- df_f$lag_gap_density_avg
                                unique(mcn$year)
                                unique(df_f$year)

    model_area_temp <- felm(log(mangrove_area) ~ 
                                        temp+I(temp^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + #factor(disc)+
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
    summary(model_area_temp)


    model_area_sst <- felm(log(mangrove_area) ~ 
                                        sst+I(sst^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
    summary(model_area_sst)
                
    model_area6 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + #factor(disc)+
                                        year:countrycode
                                        |gridcell_id  + year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6)
                
    reg_table_custom(model_area_temp,model_area_sst,model_area6,
                                title_table="Table S1: Mean and Extremes", dep.var.labels="Log Mangrove Area", 
                                name="Tables/Draft3/Table1_Temp_SST_SSTh.html",
                                sci=F, replacements=replacements_list)


    reg_table_custom(model_area_temp,model_area_sst,model_area6,model_area6_mcw_int,model_area_bins,
                                title_table="Table 1: Area Models", dep.var.labels="Log Mangrove Area", 
                                name="Tables/Draft3/TableS1_Mean_and_Extremes.html",
                                sci=F, replacements=replacements_list)

        model_area6_discontinuity <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2)  + factor(disc) +
                                        year:countrycode
                                        |gridcell_id   |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                                        
        model_area6_discontinuity_trend <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + factor(disc)*year+
                                        year:countrycode
                                        |gridcell_id   |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                
    reg_table_custom(model_area6,model_area6_discontinuity,model_area6_discontinuity_trend,
                                title_table="Table S5: Discontinuity", dep.var.labels="Log Mangrove Area", 
                                name="Tables/Draft3/Table5_discontinuity.html",
                                sci=F, replacements=replacements_list)
ghbsh
ksdndjk

smwkodlwd


    glimpse(df_f)
    model_area_gdp <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logSumGDP + I(logSumGDP^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
    summary(model_area_gdp)

    model_area_pop <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logPop + I(logPop^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
    summary(model_area_pop)

    model_area_ntl <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        ntl:sensor + sensor:I(ntl^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
    summary(model_area_ntl)

    reg_table_custom(model_area6,model_area_gdp,model_area_pop,model_area_ntl,
                                title_table="Table 2: Socioeconomic metrics", dep.var.labels="Log Mangrove Area", 
                                name="Tables/Draft3/Table1_GDP_Pop_NTL.html",
                                sci=F, replacements=replacements_list)
    ## Extreme Warming Variables (start)
        ### SST Hottest Month (start)

                model_area <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2) +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year|0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area)             
                model_area2 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2) +
                                        logGDPpc_country + I(logGDPpc_country^2) +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area2)
                model_area3 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+
                                        logGDPpc +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area3)
                model_area4 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2)+
                                        logPop + I(logPop^2)+
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area4)
                model_area5 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logPop*rich +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area5)
                
                model_area6 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6)
                model_area_ssthot <- model_area6
                #save(model_area_ssthot,file="Models/Round2/pref_area_model.RData")

                model_area6_noW <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f)

                model_area6_logSal <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        logSal+
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_logSal)
                summary(model_area6)
              
                reg_table_custom(model_area,model_area2,model_area3,model_area4,model_area6,
                                title_table="Table 1: Area Model", dep.var.labels="Log Mangrove Area", 
                                name="Tables/Draft3/Table1_sstHottest.html",
                                sci=F, replacements=replacements_list)
                #replace_strings_in_html("Tables/AreaModel/Table1_sstHottest.html", replacements_list) 
                reg_table_custom_2(model_area6, model_area6_noW,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_mcw_2007_2020_noW.html")
        ### SST Hottest Month (end)

        ## Other extremes (start)
        
                model_area6_mcw_int <- felm(log(mangrove_area) ~ 
                                        mcw_int:mcw_freq + 
                                        mhw_int:mhw_freq + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_int)

                model_area_bins <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    preci+I(preci^2)+
                                    logGDPpc + I(logGDPpc^2) +
                                    lag_gap_density + I((lag_gap_density)^2)+year:countrycode
                                    |gridcell_id  +  year|0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                summary(model_area_bins)

                reg_table_custom(model_area6_mcw_int,model_area_bins,
                                title_table="Table 3: Other Measures of Extremes", dep.var.labels="Log Mangrove Area", 
                                name="Tables/Draft3/Table_OtherExtremes.html",
                                sci=F, replacements=replacements_list)

        ## Other extremes (end)
   