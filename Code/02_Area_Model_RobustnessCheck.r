libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe")
lapply(libraries, library, character.only = TRUE)
setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
#mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023_buffers.csv")
#mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023.csv")
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023_buffers.csv")
glimpse(mcn)
subset_mcn <- mcn[,c("b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", "b36_37C")]
mcn$hot_bin <- rowSums(subset_mcn, na.rm = TRUE)
subset_mcn <- mcn[,c("b8_9C", "b9_10C", "b10_11C", "b11_12C", "b12_13C", "b13_14C","b14_15C","b15_16C","b16_17C","b17_18C")]
mcn$cold_bin <- rowSums(subset_mcn, na.rm = TRUE)
mcn$abs_lat <- abs(mcn$Latitude)
mcn_2020 <- mcn %>% filter(year == 2020)

df_f <- mcn %>% filter(
                               
                                mean_mangrove_area>1 & 
                                #lag_holes>0 & 
                                #mangrove_area> 1 & 
                                #np>0 &
                                #lag_gap_density>0 & 
                                #gap_density>0 &
                                #year > 2014 & 
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
        df_f$high_lat <- 1
        df_f$high_lat[df_f$abs_lat<20] <- 0
        df_f$low_lat <- 1
        df_f$low_lat[df_f$abs_lat>20] <- 0
        df_f$subtropical <- 1
        df_f$subtropical[df_f$abs_lat>23] <- 0
        df_f$extratropical <- 1
        df_f$extratropical[df_f$abs_lat<23] <- 0
        df_f$mhw_FREQ <- df_f$mhw_freq
        df_f$mhw_DUR <- df_f$mhw_dur
        df_f$mhw_FREQ[df_f$mhw==0] <- 0
        df_f$mhw_DUR[df_f$mhw==0] <- 0
        #df_f$preci <- df_f$Mean_Precipitation
                                
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
                reg_table_custom(model_area6,
                                title_table="Table 1: Area Model", dep.var.labels="Log Mangrove Area", 
                                name="Tables/AreaModel/Table_PrefferedModel.html",
                                sci=T, replacements=replacements_list)
                

                model_area6_noW <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f)

                model_area6_noGaps <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_noGaps)
                # save(model_area6_noGaps,file="Models/Round2/pref_area_model_noGaps.RData")
                # sq_estimate_sst_area <- sqest(df_f,model_area6_noGaps,"sst_hottest","Area Loss")
                # sq_estimate_gdp_area <- sqest(df_f,model_area6_noGaps,"logGDPpc","Area Loss")
                # save(sq_estimate_gdp_area,file="Models/Round2/sq_estimate_gdp_area_nogaps.RData") 
                # save(sq_estimate_sst_area,file="Models/Round2/sq_estimate_sst_area_nogaps.RData") 

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
                                name="Tables/AreaModel/Table1_sstHottest.html",
                                sci=T, replacements=replacements_list)
                #replace_strings_in_html("Tables/AreaModel/Table1_sstHottest.html", replacements_list) 
                #reg_table_custom_2(model_area6, model_area6_noW,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_mcw_2007_2020_noW.html")
        ### SST Hottest Month (end)

        ## MCW (start)
            
                

                model_area6_mcw_int <- felm(log(mangrove_area) ~ 
                                        mcw_int + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_int)

                model_area6_mcw_int_anom <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_int_anom)

                model_area6_mcw_freq <- felm(log(mangrove_area) ~ 
                                        mcw_freq + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_freq)

                model_area6_mcw_freq <- felm(log(mangrove_area) ~ 
                                        mcw_int:I(abs_lat^2) + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_freq)

                model_area6_mcw_freq_extratropical <- felm(log(mangrove_area) ~ #queda
                                        mcw_freq*extratropical + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2)+year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_freq_extratropical)


                df_f$mcw_int_anom_neg <- -df_f$mcw_int_anom
                model_area6_mcw_freq_extratropical_all <- felm(log(mangrove_area) ~ 
                                        #mcw_int_anom*extratropical+ mcw_freq*extratropical + #mcw_dur*extratropical +
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2)+year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_freq_extratropical_all)


                df_f$mcw_int_anom_neg <- -df_f$mcw_int_anom
                model_area6_mcw_freq_extratropical_all <- felm(log(mangrove_area) ~ 
                                        #mhw_int_anom*extratropical+ mhw_freq*extratropical + #mcw_dur*extratropical +
                                        mhw_int_anom:mhw_freq + 
                                        mcw_int_anom_neg:mcw_freq+
                                        #mhw_int_anom:mhw_freq:subtropical + 
                                        #mcw_int_anom_neg:mcw_freq:extratropical+
                                        
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2)+year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_freq_extratropical_all)
                reg_table_custom(model_area6_mcw_freq_extratropical_all,
                                title_table="Table 3: Area Model Marine HeatWaves and ColdWaves", dep.var.labels="Log Mangrove Area", 
                                name="Tables/AreaModel/Table_MCW_MHW.html",
                                sci=T, replacements=replacements_list)

                model_area6_cold_extratropical <- felm(log(mangrove_area) ~ 
                                        #cold_bin+#*extratropical + #mcw_dur*extratropical +
                                        #b8_9C + b9_10C + b10_11C + b11_12C + b12_13C + b13_14C+ b14_15C + b15_16C + b16_17C + b17_18C+ b18_19C + b19_20C + b20_21C + b21_22C+ b22_23C+ b23_24C+ b24_25C+ b25_26C+ b26_27C+ b27_28C+ b28_29C+ b29_30C+ b30_31C+ b31_32C+ b32_33C+ b33_34C+ b34_35C+b35_36C+b36_37C+b37_38C+
                                        #cold_bin + 
                                        hot_bin + #I(hot_bin^2)+
                                        #hot_bin+#*extratropical+
                                        #sst_hottest + I(sst_hottest^2) +
                                        #temp + I(temp^2)+
                                        #anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C+anom_bn7_n6C + anom_bn6_n5C+ anom_bn5_n4C +anom_bn4_n3C+anom_bn3_n2C+ anom_bn2_n1C +anom_bn1_0C+anom_b0_1C +anom_b1_2C +anom_b2_3C +anom_b3_4C +anom_b4_5C +anom_b5_6C+anom_b6_7C+anom_b7_8C+
                                        #anom_bn10_n9C*abs_lat +anom_bn9_n8C*abs_lat +anom_bn8_n7C*abs_lat+anom_bn7_n6C*abs_lat + anom_bn6_n5C*abs_lat+ anom_bn5_n4C*abs_lat +anom_bn4_n3C*abs_lat+anom_bn3_n2C*abs_lat+ anom_bn2_n1C*abs_lat +anom_bn1_0C*abs_lat+anom_b0_1C*abs_lat +anom_b1_2C*abs_lat +anom_b2_3C*abs_lat +anom_b3_4C*abs_lat +anom_b4_5C*abs_lat +anom_b5_6C*abs_lat+anom_b6_7C*abs_lat+anom_b7_8C*abs_lat+
                                        #anom_bn10_n9C*extratropical +anom_bn9_n8C*extratropical +anom_bn8_n7C*extratropical+anom_bn7_n6C*extratropical + anom_bn6_n5C*extratropical+ anom_bn5_n4C*extratropical +anom_bn4_n3C*extratropical+anom_bn3_n2C*extratropical+ anom_bn2_n1C*extratropical +anom_bn1_0C*extratropical+anom_b0_1C*extratropical +anom_b1_2C*extratropical +anom_b2_3C*extratropical +anom_b3_4C*extratropical +anom_b4_5C*extratropical +anom_b5_6C*extratropical+anom_b6_7C*extratropical+anom_b7_8C*extratropical+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2)+
                                        year:countrycode #+ I(year^2):countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_cold_extratropical)
                reg_table_custom(model_area6_cold_extratropical,
                                title_table="Table 2: Area Model Days Above 30", dep.var.labels="Log Mangrove Area", 
                                name="Tables/AreaModel/Table_DaysAbove.html",
                                sci=T, replacements=replacements_list)
                
                hist(df_f$anom_bn3_n2C)
                glimpse(df_f)

                model_area6_mcw_freq_all <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom+ mcw_freq+ #mcw_dur+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2)+year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_freq_all)
                #ggplot(df_f,aes(x=mcw_freq,y=mcw_dur))+geom_point()
                
                reg_table_custom_2(model_area6_mcw_freq_all,model_area6_mcw_freq_extratropical_all,title_table="Area Model with Marine ColdWaves", dep.var.labels="Log Mangrove Area", name="output_coldwaves_coldbin_2007_2020.html")
           
                
                #hist(df_f$mcw_freq)
                #hist(df_f$mcw_int_anom)

                model_area6_mcw_int_freq <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom + mcw_freq + 
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area6_mcw_int_freq)

                
                model_area_mcw <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom*abs_lat+
                                        #mcw_int_anom:high_lat+
                                        #mhw_int_anom+#:low_lat+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2) +
                                        lag_gap_density + I((lag_gap_density)^2) 
                                        |gridcell_id  +  year + countrycode |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area_mcw)

                model_area_mcw <- felm(log(mangrove_area) ~ 
                                        #mcw_int_anom*abs_lat+
                                        mhw_int_anom+#*high_lat+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2) +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year|0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area_mcw)

                
                model_area_mcw2 <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2) +
                                        logGDPpc_country + I(logGDPpc_country^2) +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area_mcw2)

                model_area_mcw3 <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom+
                                        preci+I(preci^2)+
                                        logGDPpc +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area_mcw3)

                model_area_mcw4 <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom+
                                        preci+I(preci^2)+
                                        logGDPpc + I(logGDPpc^2)+
                                        logPop + I(logPop^2)+
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area_mcw4)


                model_area_mcw5 <- felm(log(mangrove_area) ~ 
                                        mcw_int_anom+
                                        preci+I(preci^2)+                                   
                                        logPop*rich +
                                        lag_gap_density + I((lag_gap_density)^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
                summary(model_area_mcw5)

                model_area_mcw6 <- felm(log(mangrove_area) ~ 
                                        mcw_int+#I(mcw_int^2)+
                                        mhw_int+#I(mhw_int^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))

                model_area_mcw6_noW <- felm(log(mangrove_area) ~ 
                                        mcw_int+#I(mcw_int^2)+
                                        mhw_int+#I(mhw_int^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f)

                summary(model_area_mcw6)
                reg_table_custom_2(model_area_mcw,model_area_mcw2,model_area_mcw3,model_area_mcw6,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_mcw_2007_2020.html")
            
        ## MCW (end)

        ## Bins (start)    
            model_area_bins <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    preci+I(preci^2)+
                                    logGDPpc + I(logGDPpc^2) +
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year|0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                        
            model_area_bins2 <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    preci+I(preci^2)+
                                    logGDPpc + I(logGDPpc^2) +
                                    logGDPpc_country + I(logGDPpc_country^2) +
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            
            model_area_bins3 <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    preci+I(preci^2)+
                                    logGDPpc +
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            

            model_area_bins4 <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    preci+I(preci^2)+
                                    logGDPpc + I(logGDPpc^2)+
                                    logPop + I(logPop^2)+
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
  

            model_area_bins5 <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    preci+I(preci^2)+                                   
                                    logPop*rich +
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))


            model_area_bins6 <- felm(log(mangrove_area) ~ 
                                    cold_bin+hot_bin+
                                    #sst_hottest + I(sst_hottest^2)+
                                    preci+I(preci^2)+                                   
                                    logGDPpc + I(logGDPpc^2)+
                                    lag_gap_density + I((lag_gap_density)^2)# + 
                                    #year:countrycode
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            summary(model_area_bins6)
          
            reg_table_custom_2(model_area_bins,model_area_bins2,model_area_bins3,model_area_bins6,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_bins_2007_2020.html")
            
        
        ## Bins (end)
    ## Extreme Warming Variables (end)


    ### Other measurements of Human Perturbation
                model_area6 <- felm(log(mangrove_area) ~ 
                                    sst_hottest+I(sst_hottest^2)+
                                    preci+I(preci^2)+                                   
                                    logGDPpc + I(logGDPpc^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                summary(model_area6)
                model_area6_ntl <- felm(log(mangrove_area) ~ 
                                    sst_hottest+I(sst_hottest^2)+
                                    preci+I(preci^2)+                                   
                                    sensor:log(ntl) + sensor:I(log(ntl)^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode                                   
                                   |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                summary(model_area6_ntl)

                model_area6_ntl2 <- felm(log(mangrove_area) ~ 
                                    sst_hottest+I(sst_hottest^2)+
                                    preci+I(preci^2)+                                   
                                    #sensor*log(ntl) + sensor*I(log(ntl)^2)+
                                    sensor*(ntl) + sensor*I(ntl^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode                                   
                                   |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                summary(model_area6_ntl2)

                model_area6_gdp_pop <- felm(log(mangrove_area) ~ 
                                    sst_hottest+I(sst_hottest^2)+
                                    preci+I(preci^2)+                      
                                    logPop + I(logPop^2)+                        
                                    log(Sum_GDP)+I(log(Sum_GDP)^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode                                   
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                summary(model_area6_gdp_pop)

                model_area6_gdp50_pop <- felm(log(mangrove_area) ~ 
                                    sst_hottest+I(sst_hottest^2)+
                                    preci+I(preci^2)+                      
                                    logPop + I(logPop^2)+                        
                                    log(Sum_GDP_50km)+I(log(Sum_GDP_50km)^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode                                   
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                summary(model_area6_gdp50_pop)
                reg_table_custom_2(model_area6,model_area6_ntl,model_area6_gdp_pop,model_area6_gdp50_pop,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_HumanDisruption_2015_2020.html")
            ## Other Measurements of Human Perturbation
    ### Other measurements of Human Perturbation
    
    ## Other measurements of ecosystem dynamics (start)
        model_area6 <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I((lag_gap_density)^2) + 
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        summary(model_area6)

        model_area6_nogap <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+ #lag(temp)+
                                        #lag_gap_density + I((lag_gap_density)^2) + 
                                        factor(year):countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        summary(model_area6_nogap)

        model_area6_nogap_pafrac <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        #lag_gap_density + I((lag_gap_density)^2) + 
                                        log(pafrac)+I(log(pafrac)^2)+
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        summary(model_area6_nogap_pafrac)

        model_area6_patch <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        log(patch_density_avg)+I(log(patch_density_avg)^2)+
                                        year:countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        summary(model_area6_pafrac)

        reg_table_custom_2(model_area6_nogap,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="Tables/AreaModel/output_pafrac_patch_2007_2020_nogap.html")
        
        model_x <- lm(log(pafrac) ~ factor(year) + factor(gridcell_id) , data = df_f %>% filter(!is.na(pafrac) & !is.na(gap_density)))
        model_y <- lm(log(gap_density) ~ factor(year) + factor(gridcell_id) , data = df_f%>% filter(!is.na(pafrac) & !is.na(gap_density)))

        resid_x <- residuals(model_x)
        resid_y <- residuals(model_y)
        glimpse(resid_x)
        glimpse(resid_y)
        
        cor_resid <- cor(resid_x, resid_y)
        df_f2 <- df_f%>% filter(!is.na(pafrac) & !is.na(gap_density))
        cor(df_f2$pafrac, df_f2$gap_density)
        print(cor_resid)


        model_x <- lm(log(patch_density_avg) ~ factor(year) + factor(gridcell_id) , data = df_f %>% filter(!is.na(pafrac) & !is.na(gap_density)))
        model_y <- lm(log(gap_density) ~ factor(year) + factor(gridcell_id) , data = df_f%>% filter(!is.na(pafrac) & !is.na(gap_density)))

        resid_x <- residuals(model_x)
        resid_y <- residuals(model_y)
        glimpse(resid_x)
        glimpse(resid_y)
        
        cor_resid <- cor(resid_x, resid_y)
        print(cor_resid)
        

        
        summary(model_gap_pafrac)
        ggplot(df_f,aes(x=pafrac,y=lag_gap_density))+geom_point()
        ggplot(df_f,aes(x=log(pafrac),y=log(mangrove_area)))+geom_point()
    ## Other measurements of ecosystem dynamics (end)


    ### Mean Variables (T, SST) (start)


        ## Temp (start)
            # model_area_t <- felm(log(mangrove_area) ~ 
            #                         temp+I(temp^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc + I(logGDPpc^2) +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year|0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_t)

            
            # model_area_t2 <- felm(log(mangrove_area) ~ 
            #                         temp+I(temp^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc + I(logGDPpc^2) +
            #                         logGDPpc_country + I(logGDPpc_country^2) +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_t2)

            # model_area_t3 <- felm(log(mangrove_area) ~ 
            #                         temp+I(temp^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_t3)

            # model_area_t4 <- felm(log(mangrove_area) ~ 
            #                         temp+I(temp^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc + I(logGDPpc^2)+
            #                         logPop + I(logPop^2)+
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_t4)

            # model_area_t5 <- felm(log(mangrove_area) ~ 
            #                         sst_hottest+I(sst_hottest^2)+
            #                         preci+I(preci^2)+                                   
            #                         logPop*rich +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_t5)

            model_area_t6 <- felm(log(mangrove_area) ~ 
                                    temp+I(temp^2)+
                                    preci+I(preci^2)+                                   
                                    logGDPpc + I(logGDPpc^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            summary(model_area_t6)
            reg_table_custom_2(model_area_t,model_area_t2,model_area_t3,model_area_t6,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_t_2007_2020.html")
        ## Temp (end)

        ## SST (start)


            # model_area_sst <- felm(log(mangrove_area) ~ 
            #                         sst+I(sst^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc + I(logGDPpc^2) +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year|0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_sst)

            
            # model_area_sst2 <- felm(log(mangrove_area) ~ 
            #                         sst+I(sst^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc + I(logGDPpc^2) +
            #                         logGDPpc_country + I(logGDPpc_country^2) +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_sst2)

            # model_area_sst3 <- felm(log(mangrove_area) ~ 
            #                         sst+I(sst^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_sst3)

            # model_area_sst4 <- felm(log(mangrove_area) ~ 
            #                         sst+I(sst^2)+
            #                         preci+I(preci^2)+
            #                         logGDPpc + I(logGDPpc^2)+
            #                         logPop + I(logPop^2)+
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_sst4)

            # model_area_sst5 <- felm(log(mangrove_area) ~ 
            #                         sst+I(sst^2)+
            #                         preci+I(preci^2)+                                   
            #                         logPop*rich +
            #                         lag_gap_density + I((lag_gap_density)^2)
            #                         |gridcell_id  +  year |0|gridcell_id,data=df_f,
            #                         weights=log(df_f$mangrove_area+1))
            # summary(model_area_sst5)

            model_area_sst6 <- felm(log(mangrove_area) ~ 
                                    sst+I(sst^2)+
                                    preci+I(preci^2)+                                   
                                    logGDPpc + I(logGDPpc^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            summary(model_area_sst6)
            reg_table_custom_2(model_area_t6,model_area_sst6,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_sstandt_2007_2020.html")

            model_area_sst6 <- felm(log(mangrove_area) ~ 
                                    sst+ I(sst^2)+#temp+ I(temp^2)+
                                    #sst_hottest + I(sst_hottest^2)+
                                    #preci+I(preci^2)+                                   
                                    logGDPpc + I(logGDPpc^2)+
                                    #lag_gap_density + I((lag_gap_density)^2) + 
                                    logSal+
                                    year:countrycode
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            summary(model_area_sst6)
        ## SST (end)
    ### Mean Variables (T, SST) (end)



        ## Salinity (start)

            model_area_logSal <- felm(log(mangrove_area) ~ 
                                    sst + I(sst^2)+
                                    preci+I(preci^2)+
                                    logSal+
                                    logGDPpc + I(logGDPpc^2) +
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year|0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
                        
            model_area_logSal2 <- felm(log(mangrove_area) ~ 
                                    sst + I(sst^2)+
                                    preci+I(preci^2)+logSal+
                                    logGDPpc + I(logGDPpc^2) +
                                    logGDPpc_country + I(logGDPpc_country^2) +
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            
            model_area_logSal3 <- felm(log(mangrove_area) ~ 
                                    sst + I(sst^2)+
                                    preci+I(preci^2)+
                                    logGDPpc +logSal+
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
            

            model_area_logSal4 <- felm(log(mangrove_area) ~ 
                                    sst + I(sst^2)+
                                    preci+I(preci^2)+logSal+
                                    logGDPpc + I(logGDPpc^2)+
                                    logPop + I(logPop^2)+
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
  

            model_area_logSal5 <- felm(log(mangrove_area) ~ 
                                    sst + I(sst^2)+
                                    preci+I(preci^2)+                                   
                                    logPop*rich +logSal+
                                    lag_gap_density + I((lag_gap_density)^2)
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))


            model_area_logSal6 <- felm(log(mangrove_area) ~ 
                                    sst + I(sst^2)+
                                    preci+I(preci^2)+           logSal+                        
                                    logGDPpc + I(logGDPpc^2)+
                                    lag_gap_density + I((lag_gap_density)^2) + 
                                    year:countrycode
                                    |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                    weights=log(df_f$mangrove_area+1))
          
            reg_table_custom_2(model_area_logSal,model_area_logSal2,model_area_logSal6,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_logSal_2007_2020.html")


        ## Salinity (end)
            
            reg_table_custom_2(model_area6,model_area_bins6,model_area_mcw6,title_table="Area Model", dep.var.labels="Log Mangrove Area", name="output_all3_2007_2020.html")


            unique(df_f$year)
            unique(mcn$year)

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

            #ggsave("Figures/Sept/Model_Area_GDPEffect.png",dpi=600)


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

