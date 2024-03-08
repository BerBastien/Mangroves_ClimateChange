#SSP1


        ## Read Data (start)

        setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
        gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
        
                        scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all_ssps.csv")
                        glimpse(scen)
                        load("Models/Round2/pref_area_model.RData")
                        load(file="Models/Round2/sq_estimate_sst_area.RData") 
                        

                        coef_ssth <- summary(model_area_ssthot)$coef[1]
                        coef_ssth2 <- summary(model_area_ssthot)$coef[2]

                        # Extract the variance-covariance matrix
                        vcov_matrix <- vcov(model_area_ssthot)

                        # Extract variances and covariance
                        var_coef1 <- vcov_matrix["sst_hottest", "sst_hottest"]
                        var_coef2 <- vcov_matrix["I(sst_hottest^2)", "I(sst_hottest^2)"]
                        cov_coef1_coef2 <- vcov_matrix["sst_hottest", "I(sst_hottest^2)"]
                        # Calculate the variance of the derived coefficient
                        var_damage <- var_coef1 + (2 * scen$sst_hot_70)^2 * var_coef2 + 2 * 1 * (2 * scen$sst_hot_70) * cov_coef1_coef2
                        # Calculate the standard error for the derived coefficient
                        se_damage <- sqrt(var_damage)

                        coef_p <- summary(model_area_ssthot)$coef[3]
                        coef_p2 <- summary(model_area_ssthot)$coef[4]
                        coef_gdp <- summary(model_area_ssthot)$coef[5]
                        coef_gdp2 <- summary(model_area_ssthot)$coef[6]

                        
                        
                        scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","sst_hottest","logGDPpc"),by="gridcell_id",all=TRUE)
                        scen$sst_hot_70_capped <- matrixStats::rowMins(cbind(scen$sst_hot_70,rep(max(exp(scen$sst_hottest),na.rm=TRUE),times=length(scen$sst_hot_70))))
                        scen$marg_ssth_area <-  coef_ssth + 2*coef_ssth2*scen$sst_hot_70
                        alpha <- 0.1  # for a 95% CI
                        z_value <- qnorm(1 - alpha/2) 
                        scen$marg_ssth_area_lower_limit <- scen$marg_ssth_area - z_value * se_damage
                        scen$marg_ssth_area_upper_limit <- scen$marg_ssth_area + z_value * se_damage


                        var_coef1 <- vcov_matrix["logGDPpc", "logGDPpc"]
                        var_coef2 <- vcov_matrix["I(logGDPpc^2)", "I(logGDPpc^2)"]
                        cov_coef1_coef2 <- vcov_matrix["logGDPpc", "I(logGDPpc^2)"]
                        #scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                        # Calculate the variance of the derived coefficient
                        var_damage <- var_coef1 + (2 * log(scen$gdppc1))^2 * var_coef2 + 2 * 1 * (2 * log(scen$gdppc1)) * cov_coef1_coef2
                        # Calculate the standard error for the derived coefficient
                        se_damage <- sqrt(var_damage)
                        # scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                        # ggplot(scen)+geom_point(aes(x=log(gdppc1),y=logGDPpc))
                        # glimpse(scen)
                        # #scen$gdppc1_capped <-matrixStats::rowMins(cbind(scen$gdppc1,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc1))))
                        scen$gdppc1_capped <- matrixStats::rowMins(cbind(scen$gdppc1,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc1))))
                        #max((scen$gdppc1[which(is.finite(scen$gdppc1))]),na.rm=TRUE)
                        
                        #ggplot(scen)+geom_point(aes(x=log(gdppc1),y=log(gdppc1_capped)))
                        
                        scen$marg_gdppc_area <-  coef_gdp + 2*coef_gdp2* log(scen$gdppc1_capped)   #log(scen$gdppc1)   
                        scen$marg_gdppc_area_lower_limit <- scen$marg_gdppc_area - z_value * se_damage
                        scen$marg_gdppc_area_upper_limit <- scen$marg_gdppc_area + z_value * se_damage


                        scen <- scen[which(is.finite(scen$POP_SSP1) & scen$POP_SSP1>10),]
                        scen <- scen[which(is.finite(scen$gdppc1) & !is.na(scen$gdppc1)),]
                        
                        ## Only Climate Change (start)
                        scen$arealoss_perc_onlyCC <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)
                        scen$arealoss_perc_onlyCC <- ifelse(is.na(scen$arealoss_perc_onlyCC), 0, scen$arealoss_perc_onlyCC)
                        scen$arealoss_perc_onlyCC_lower_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                        scen$arealoss_perc_onlyCC_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_lower_limit), 0, scen$arealoss_perc_onlyCC_lower_limit)
                        scen$arealoss_perc_onlyCC_upper_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                        scen$arealoss_perc_onlyCC_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_upper_limit), 0, scen$arealoss_perc_onlyCC_upper_limit)
                        
                        scen_arealoss_perc_onlyCC <- scen %>%
                                arrange(gridcell_id, year) %>%
                                group_by(gridcell_id) %>%
                                mutate(arealoss_perc_cumulative_onlyCC = cumsum(arealoss_perc_onlyCC), 
                                        arealoss_perc_cumulative_onlyCC_lower_limit = cumsum(arealoss_perc_onlyCC_lower_limit),
                                        arealoss_perc_cumulative_onlyCC_upper_limit = cumsum(arealoss_perc_onlyCC_upper_limit)) %>%
                                ungroup() 
                        
                        timeseries_arealoss_onlyCC_countrylevel <- scen_arealoss_perc_onlyCC %>% group_by(countrycode) %>%
                                                                        summarise()
                        

                        scen_arealoss_perc_onlyCC_2100 <- scen_arealoss_perc_onlyCC %>% filter(year==2100)
                        mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                        mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                        
                        scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                        names(mcn_1996)[2] <-  "mangrove_area1996"
                        names(mcn_2020)[2] <-  "mangrove_area2020"
                        scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                        scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_1996,by=c("gridcell_id"),all=T)
                        scen_arealoss_perc_onlyCC$mangrove_area_future_loss <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                        scen_arealoss_perc_onlyCC$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_upper_limit)
                        scen_arealoss_perc_onlyCC$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_lower_limit)
                        scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                        scen_arealoss_perc_onlyCC$loss <- 1
                        scen_arealoss_perc_onlyCC$loss[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss<0)] <- 0

                        scen_arealoss_perc_onlyCC$pos <- 1
                        scen_arealoss_perc_onlyCC$pos[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss>0)] <- 0
                        
                        scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC %>% filter(!is.na(gridcell_id))
                        glimpse(scen_arealoss_perc_onlyCC)
                        write.csv(scen_arealoss_perc_onlyCC, file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP170.csv")
                        
                        agg_aloss_neg_onlyCC <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        agg_aloss_neg_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        agg_aloss_neg_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        #num_levels <- length(unique(agg_aloss_neg$R5))
                        #agg_aloss_neg_onlyCC$color_vector <- scico(n = num_levels, palette = "batlow")
                        agg_aloss_neg_total_onlyCC <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        agg_aloss_neg_total_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        agg_aloss_neg_total_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_upper_limit,by="year")
                        agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_lower_limit,by="year")
                        glimpse(agg_aloss_neg_total_onlyCC)
                        agg_aloss_neg_country_onlyCC <- aggregate(mangrove_area_future_loss~year+countrycode,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        # Get world data
                        world_data <- ne_countries(scale = "medium", returnclass = "sf")

                        glimpse(scen_arealoss_perc_onlyCC)
                        scen_arealoss_perc_onlyCC_countrycode <- scen_arealoss_perc_onlyCC %>% group_by(countrycode,year) %>% 
                                                                summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                                mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                                mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                                mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                                mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                                R5=first(R5))  %>% ungroup() %>%
                                                                mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                                mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                                mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                                mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                        write.csv(scen_arealoss_perc_onlyCC_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyCC_SSP170.csv")
                        
                        
                                summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                                glimpse(summary_dfs_total_sum)
                                level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                                glimpse(agg_aloss_neg_total_onlyCC)
                                agg_aloss_neg_total_onlyCC$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss
                                agg_aloss_neg_total_onlyCC$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_upper_limit
                                agg_aloss_neg_total_onlyCC$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_lower_limit

                                
                                write.csv(agg_aloss_neg_total_onlyCC, file="Results\\Area\\Proj_Area_Perc_Total_OnlyCC_SSP170.csv")


                        
                        ## New Figure (end)
                        ## Only Climate Change (end)

                        ## Only GDP (start)
                        scen <- scen %>% filter(year>2025)
                        scen$arealoss_perc_onlygdp <-  (scen$perc_inc_gdppc1*scen$marg_gdppc_area)
                        scen$arealoss_perc_onlygdp <- ifelse(is.na(scen$arealoss_perc_onlygdp), 0, scen$arealoss_perc_onlygdp)
                        scen$arealoss_perc_onlygdp_lower_limit <-  (scen$perc_inc_gdppc1*scen$marg_gdppc_area_lower_limit)
                        scen$arealoss_perc_onlygdp_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_lower_limit), 0, scen$arealoss_perc_onlygdp_lower_limit)
                        scen$arealoss_perc_onlygdp_upper_limit <-  (scen$perc_inc_gdppc1*scen$marg_gdppc_area_upper_limit)
                        scen$arealoss_perc_onlygdp_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_upper_limit), 0, scen$arealoss_perc_onlygdp_upper_limit)
                        
                        scen_arealoss_perc_onlygdp <- scen %>%
                                arrange(gridcell_id, year) %>%
                                group_by(gridcell_id) %>%
                                mutate(arealoss_perc_cumulative_onlygdp = cumsum(arealoss_perc_onlygdp), 
                                        arealoss_perc_cumulative_onlygdp_lower_limit = cumsum(arealoss_perc_onlygdp_lower_limit),
                                        arealoss_perc_cumulative_onlygdp_upper_limit = cumsum(arealoss_perc_onlygdp_upper_limit)) %>%
                                ungroup() 
                        
                        scen_arealoss_perc_onlygdp_2100 <- scen_arealoss_perc_onlygdp %>% filter(year==2100)
                        
                        mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                        sum(mcn_2020$mangrove_area,na.rm=T)
                        mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                        sum(mcn_1996$mangrove_area,na.rm=T)

                        scen_arealoss_perc_onlygdp<- merge(scen_arealoss_perc_onlygdp,mcn_2020,by=c("gridcell_id"),all=T)
                        sum(mcn_2020$mangrove_area,na.rm=T)

                        
                        scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                        scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_2020 %>% dplyr::select("gridcell_id","mangrove_area"),by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                        scen_arealoss_perc_onlygdp$max_loss <- abs(scen_arealoss_perc_onlygdp$mangrove_area1996 - scen_arealoss_perc_onlygdp$mangrove_area2020)
                        

                        scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp),scen_arealoss_perc_onlygdp$max_loss))
                        scen_arealoss_perc_onlygdp$mangrove_area_future_loss_upper_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_upper_limit),scen_arealoss_perc_onlygdp$max_loss))
                        scen_arealoss_perc_onlygdp$mangrove_area_future_loss_lower_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_lower_limit),scen_arealoss_perc_onlygdp$max_loss))
                        #scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- 
                        scen_arealoss_perc_onlygdp$mangrove_area_future <- scen_arealoss_perc_onlygdp$mangrove_area *(1-0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp)
                        scen_arealoss_perc_onlygdp$loss <- 1
                        scen_arealoss_perc_onlygdp$loss[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss<0)] <- 0
                        
                        scen_arealoss_perc_onlygdp$pos <- 1
                        scen_arealoss_perc_onlygdp$pos[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss>0)] <- 0
                        
                        agg_aloss_neg_onlygdp <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        agg_aloss_neg_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        agg_aloss_neg_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        #num_levels <- length(unique(agg_aloss_neg$R5))
                        #agg_aloss_neg_onlygdp$color_vector <- scico(n = num_levels, palette = "batlow")
                        glimpse(scen_arealoss_perc_onlygdp)
                        scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp %>% filter(!is.na(gridcell_id))
                        write.csv(scen_arealoss_perc_onlygdp, file="Results\\Area\\Proj_Area_Perc_OnlyGDP_SSP170.csv") 
                        agg_aloss_neg_total_onlygdp <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        agg_aloss_neg_total_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        agg_aloss_neg_total_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_upper_limit,by="year")
                        agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_lower_limit,by="year")
                        glimpse(agg_aloss_neg_total_onlygdp)
                        #write.csv(agg_aloss_neg_total_onlygdp,file="Results\\Area\\agg_aloss_neg_total_onlygdp.csv") 
                        scen_arealoss_perc_onlygdp_countrycode <- scen_arealoss_perc_onlygdp %>% group_by(countrycode,year) %>% 
                                                                summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                                mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                                mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                                mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                                mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                                R5=first(R5))  %>% ungroup() %>%
                                                                mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                                mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                                mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                                mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                        write.csv(scen_arealoss_perc_onlygdp_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyGDP_SSP170.csv") 
                        glimpse(scen_arealoss_perc_onlygdp_countrycode)
                        
                        
                        sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                                
                                agg_aloss_pos_onlygdp <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                                summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                                level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                                agg_aloss_neg_total_onlygdp$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss
                                agg_aloss_neg_total_onlygdp$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_upper_limit
                                agg_aloss_neg_total_onlygdp$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_lower_limit

                                #glimpse(mcn_2020)
                                #merge(mcn_2020,mcn_1996,by=c("gridcell_id","year"))

                                
                                
                                write.csv(agg_aloss_neg_total_onlygdp, file="Results\\Area\\Proj_Area_Perc_Total_OnlyGDP_SSP170.csv")


                        
                        ## New Figure (end)
                        ## Only GDP (end)

                        ## Both: Climate and Socioeconomic (start)
                        scen <- scen %>% filter(year>2025)
                        scen$arealoss_perc_both <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)+(scen$perc_inc_gdppc1*scen$marg_gdppc_area)
                        scen$arealoss_perc_both <- ifelse(is.na(scen$arealoss_perc_both), 0, scen$arealoss_perc_both)
                        scen$arealoss_perc_both_lower_limit <-  (scen$perc_inc_gdppc1*scen$marg_gdppc_area_lower_limit) + (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                        scen$arealoss_perc_both_lower_limit <- ifelse(is.na(scen$arealoss_perc_both_lower_limit), 0, scen$arealoss_perc_both_lower_limit)
                        scen$arealoss_perc_both_upper_limit <-  (scen$perc_inc_gdppc1*scen$marg_gdppc_area_upper_limit)+ (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                        scen$arealoss_perc_both_upper_limit <- ifelse(is.na(scen$arealoss_perc_both_upper_limit), 0, scen$arealoss_perc_both_upper_limit)
                        
                        scen_arealoss_perc_both <- scen %>%
                                arrange(gridcell_id, year) %>%
                                group_by(gridcell_id) %>%
                                mutate(arealoss_perc_cumulative_both = cumsum(arealoss_perc_both), 
                                        arealoss_perc_cumulative_both_lower_limit = cumsum(arealoss_perc_both_lower_limit),
                                        arealoss_perc_cumulative_both_upper_limit = cumsum(arealoss_perc_both_upper_limit)) %>%
                                ungroup() 
                        glimpse(scen_arealoss_perc_both)

                        scen_arealoss_perc_both_2100 <- scen_arealoss_perc_both %>% filter(year==2100)
                        scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                        scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                        scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==1996) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                        scen_arealoss_perc_both$max_loss <- abs(scen_arealoss_perc_both$mangrove_area1996 - scen_arealoss_perc_both$mangrove_area2020)

                        
                        scen_arealoss_perc_both$mangrove_area_future_loss <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both)
                        scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_upper_limit)
                        scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_lower_limit)

                        scen_arealoss_perc_both$mangrove_area_future_loss <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss,scen_arealoss_perc_both$max_loss))
                        scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit,scen_arealoss_perc_both$max_loss))
                        scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit,scen_arealoss_perc_both$max_loss))
                        glimpse(scen_arealoss_perc_both)
                        scen_arealoss_perc_both <- scen_arealoss_perc_both %>% filter(!is.na(gridcell_id))
                        write.csv(scen_arealoss_perc_both, file="Results\\Area\\Proj_Area_Perc_both_SSP170.csv") 
                        
                        agg_aloss_neg_total_both <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_both,FUN="sum")
                        agg_aloss_neg_total_both_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                        agg_aloss_neg_total_both_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                        agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_upper_limit,by="year")
                        agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_lower_limit,by="year")
                        glimpse(agg_aloss_neg_total_both)
                        scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                                                                summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                                mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                                mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                                mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                                mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                                R5=first(R5))  %>% ungroup() %>%
                                                                mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                                mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                                mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                                mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                        write.csv(scen_arealoss_perc_both_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_both_SSP170.csv")
                        glimpse(scen_arealoss_perc_both_countrycode)
                        
                        # scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                        #                                         summarise(area= sum(mangrove_area_future,na.rm=TRUE), 
                        #                                         perc_area = mean(arealoss_perc_cumulative_both,na.rm=TRUE))
                        #glimpse(scen_arealoss_perc_both_countrycode)
                        
                


                        ## New Figure Pas + Future (start)area_change_wrt_1996_lower_limit
                                #agg_aloss_neg_total_both
                                #glimpse(agg_aloss_pos_both)
                                summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                                glimpse(summary_dfs_total_sum)
                                level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                                glimpse(agg_aloss_neg_total_both)
                                agg_aloss_neg_total_both$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss
                                agg_aloss_neg_total_both$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_upper_limit
                                agg_aloss_neg_total_both$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_lower_limit

                                
                                
                                #write.csv(summary_dfs_total_sum,file="Results\\Area\\summary_dfs_total_sum.csv")
                                write.csv(agg_aloss_neg_total_both, file="Results\\Area\\Proj_Area_Perc_Total_both_SSP170.csv")


                        
                        ## New Figure (end)
                        ## Both: Climate and Socioeconomic (end)
## SSP1 

## SSP2
        
## Read Data (start)
    setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
    
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all_ssps.csv")
                glimpse(scen)
                load("Models/Round2/pref_area_model.RData")
                load(file="Models/Round2/sq_estimate_sst_area.RData") 
                

                coef_ssth <- summary(model_area_ssthot)$coef[1]
                coef_ssth2 <- summary(model_area_ssthot)$coef[2]

                # Extract the variance-covariance matrix
                vcov_matrix <- vcov(model_area_ssthot)

                # Extract variances and covariance
                var_coef1 <- vcov_matrix["sst_hottest", "sst_hottest"]
                var_coef2 <- vcov_matrix["I(sst_hottest^2)", "I(sst_hottest^2)"]
                cov_coef1_coef2 <- vcov_matrix["sst_hottest", "I(sst_hottest^2)"]
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * scen$sst_hot_70)^2 * var_coef2 + 2 * 1 * (2 * scen$sst_hot_70) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)

                coef_p <- summary(model_area_ssthot)$coef[3]
                coef_p2 <- summary(model_area_ssthot)$coef[4]
                coef_gdp <- summary(model_area_ssthot)$coef[5]
                coef_gdp2 <- summary(model_area_ssthot)$coef[6]

                
                
                scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","sst_hottest","logGDPpc"),by="gridcell_id",all=TRUE)
                scen$sst_hot_70_capped <- matrixStats::rowMins(cbind(scen$sst_hot_70,rep(max(exp(scen$sst_hottest),na.rm=TRUE),times=length(scen$sst_hot_70))))
                scen$marg_ssth_area <-  coef_ssth + 2*coef_ssth2*scen$sst_hot_70
                alpha <- 0.1  # for a 95% CI
                z_value <- qnorm(1 - alpha/2) 
                scen$marg_ssth_area_lower_limit <- scen$marg_ssth_area - z_value * se_damage
                scen$marg_ssth_area_upper_limit <- scen$marg_ssth_area + z_value * se_damage


                var_coef1 <- vcov_matrix["logGDPpc", "logGDPpc"]
                var_coef2 <- vcov_matrix["I(logGDPpc^2)", "I(logGDPpc^2)"]
                cov_coef1_coef2 <- vcov_matrix["logGDPpc", "I(logGDPpc^2)"]
                #scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * log(scen$gdppc2))^2 * var_coef2 + 2 * 1 * (2 * log(scen$gdppc2)) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)
                # scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                # ggplot(scen)+geom_point(aes(x=log(gdppc2),y=logGDPpc))
                # glimpse(scen)
                # #scen$gdppc2_capped <-matrixStats::rowMins(cbind(scen$gdppc2,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc2))))
                scen$gdppc2_capped <- matrixStats::rowMins(cbind(scen$gdppc2,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc2))))
                #max((scen$gdppc2[which(is.finite(scen$gdppc2))]),na.rm=TRUE)
                
                #ggplot(scen)+geom_point(aes(x=log(gdppc2),y=log(gdppc2_capped)))
                
                scen$marg_gdppc_area <-  coef_gdp + 2*coef_gdp2* log(scen$gdppc2_capped)   #log(scen$gdppc2)   
                scen$marg_gdppc_area_lower_limit <- scen$marg_gdppc_area - z_value * se_damage
                scen$marg_gdppc_area_upper_limit <- scen$marg_gdppc_area + z_value * se_damage


                scen <- scen[which(is.finite(scen$POP_SSP2) & scen$POP_SSP2>10),]
                scen <- scen[which(is.finite(scen$gdppc2) & !is.na(scen$gdppc2)),]
                
                ## Only Climate Change (start)
                    scen$arealoss_perc_onlyCC <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)
                    scen$arealoss_perc_onlyCC <- ifelse(is.na(scen$arealoss_perc_onlyCC), 0, scen$arealoss_perc_onlyCC)
                    scen$arealoss_perc_onlyCC_lower_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_onlyCC_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_lower_limit), 0, scen$arealoss_perc_onlyCC_lower_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_upper_limit), 0, scen$arealoss_perc_onlyCC_upper_limit)
                    
                    scen_arealoss_perc_onlyCC <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlyCC = cumsum(arealoss_perc_onlyCC), 
                                    arealoss_perc_cumulative_onlyCC_lower_limit = cumsum(arealoss_perc_onlyCC_lower_limit),
                                    arealoss_perc_cumulative_onlyCC_upper_limit = cumsum(arealoss_perc_onlyCC_upper_limit)) %>%
                            ungroup() 
                    
                    timeseries_arealoss_onlyCC_countrylevel <- scen_arealoss_perc_onlyCC %>% group_by(countrycode) %>%
                                                                summarise()
                    

                    scen_arealoss_perc_onlyCC_2100 <- scen_arealoss_perc_onlyCC %>% filter(year==2100)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    names(mcn_1996)[2] <-  "mangrove_area1996"
                    names(mcn_2020)[2] <-  "mangrove_area2020"
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_1996,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_upper_limit)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_lower_limit)
                    scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$loss <- 1
                    scen_arealoss_perc_onlyCC$loss[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss<0)] <- 0

                    scen_arealoss_perc_onlyCC$pos <- 1
                    scen_arealoss_perc_onlyCC$pos[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss>0)] <- 0
                    
                    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC %>% filter(!is.na(gridcell_id))
                    glimpse(scen_arealoss_perc_onlyCC)
                    write.csv(scen_arealoss_perc_onlyCC, file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP270.csv")
                    
                    agg_aloss_neg_onlyCC <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    #num_levels <- length(unique(agg_aloss_neg$R5))
                    #agg_aloss_neg_onlyCC$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total_onlyCC <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_upper_limit,by="year")
                    agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_onlyCC)
                    agg_aloss_neg_country_onlyCC <- aggregate(mangrove_area_future_loss~year+countrycode,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    # Get world data
                    world_data <- ne_countries(scale = "medium", returnclass = "sf")

                    glimpse(scen_arealoss_perc_onlyCC)
                    scen_arealoss_perc_onlyCC_countrycode <- scen_arealoss_perc_onlyCC %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_onlyCC_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyCC_SSP270.csv")
                    
                    
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_onlyCC)
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_lower_limit

                        
                        write.csv(agg_aloss_neg_total_onlyCC, file="Results\\Area\\Proj_Area_Perc_Total_OnlyCC_SSP270.csv")


                    
                    ## New Figure (end)
                ## Only Climate Change (end)

                ## Only GDP (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_onlygdp <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area)
                    scen$arealoss_perc_onlygdp <- ifelse(is.na(scen$arealoss_perc_onlygdp), 0, scen$arealoss_perc_onlygdp)
                    scen$arealoss_perc_onlygdp_lower_limit <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area_lower_limit)
                    scen$arealoss_perc_onlygdp_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_lower_limit), 0, scen$arealoss_perc_onlygdp_lower_limit)
                    scen$arealoss_perc_onlygdp_upper_limit <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area_upper_limit)
                    scen$arealoss_perc_onlygdp_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_upper_limit), 0, scen$arealoss_perc_onlygdp_upper_limit)
                    
                    scen_arealoss_perc_onlygdp <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlygdp = cumsum(arealoss_perc_onlygdp), 
                                    arealoss_perc_cumulative_onlygdp_lower_limit = cumsum(arealoss_perc_onlygdp_lower_limit),
                                    arealoss_perc_cumulative_onlygdp_upper_limit = cumsum(arealoss_perc_onlygdp_upper_limit)) %>%
                            ungroup() 
                    
                    scen_arealoss_perc_onlygdp_2100 <- scen_arealoss_perc_onlygdp %>% filter(year==2100)
                    
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)

                    scen_arealoss_perc_onlygdp<- merge(scen_arealoss_perc_onlygdp,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)

                    
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_2020 %>% dplyr::select("gridcell_id","mangrove_area"),by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_onlygdp$max_loss <- abs(scen_arealoss_perc_onlygdp$mangrove_area1996 - scen_arealoss_perc_onlygdp$mangrove_area2020)
                    

                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_upper_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_upper_limit),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_lower_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_lower_limit),scen_arealoss_perc_onlygdp$max_loss))
                    #scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- 
                    scen_arealoss_perc_onlygdp$mangrove_area_future <- scen_arealoss_perc_onlygdp$mangrove_area *(1-0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp)
                    scen_arealoss_perc_onlygdp$loss <- 1
                    scen_arealoss_perc_onlygdp$loss[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss<0)] <- 0
                    
                    scen_arealoss_perc_onlygdp$pos <- 1
                    scen_arealoss_perc_onlygdp$pos[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss>0)] <- 0
                    
                    agg_aloss_neg_onlygdp <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    #num_levels <- length(unique(agg_aloss_neg$R5))
                    #agg_aloss_neg_onlygdp$color_vector <- scico(n = num_levels, palette = "batlow")
                    glimpse(scen_arealoss_perc_onlygdp)
                    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp %>% filter(!is.na(gridcell_id))
                    write.csv(scen_arealoss_perc_onlygdp, file="Results\\Area\\Proj_Area_Perc_OnlyGDP_SSP270.csv") 
                    agg_aloss_neg_total_onlygdp <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_upper_limit,by="year")
                    agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_onlygdp)
                    #write.csv(agg_aloss_neg_total_onlygdp,file="Results\\Area\\agg_aloss_neg_total_onlygdp.csv") 
                    scen_arealoss_perc_onlygdp_countrycode <- scen_arealoss_perc_onlygdp %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_onlygdp_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyGDP_SSP270.csv") 
                    glimpse(scen_arealoss_perc_onlygdp_countrycode)
                    
                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                        
                        agg_aloss_pos_onlygdp <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_lower_limit

                        #glimpse(mcn_2020)
                        #merge(mcn_2020,mcn_1996,by=c("gridcell_id","year"))

                        
                        
                        write.csv(agg_aloss_neg_total_onlygdp, file="Results\\Area\\Proj_Area_Perc_Total_OnlyGDP_SSP270.csv")


                    
                    ## New Figure (end)
                ## Only GDP (end)

                ## Both: Climate and Socioeconomic (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_both <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)+(scen$perc_inc_gdppc2*scen$marg_gdppc_area)
                    scen$arealoss_perc_both <- ifelse(is.na(scen$arealoss_perc_both), 0, scen$arealoss_perc_both)
                    scen$arealoss_perc_both_lower_limit <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area_lower_limit) + (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_both_lower_limit <- ifelse(is.na(scen$arealoss_perc_both_lower_limit), 0, scen$arealoss_perc_both_lower_limit)
                    scen$arealoss_perc_both_upper_limit <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area_upper_limit)+ (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_both_upper_limit <- ifelse(is.na(scen$arealoss_perc_both_upper_limit), 0, scen$arealoss_perc_both_upper_limit)
                    
                    scen_arealoss_perc_both <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_both = cumsum(arealoss_perc_both), 
                                    arealoss_perc_cumulative_both_lower_limit = cumsum(arealoss_perc_both_lower_limit),
                                    arealoss_perc_cumulative_both_upper_limit = cumsum(arealoss_perc_both_upper_limit)) %>%
                            ungroup() 
                    glimpse(scen_arealoss_perc_both)

                    scen_arealoss_perc_both_2100 <- scen_arealoss_perc_both %>% filter(year==2100)
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==1996) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_both$max_loss <- abs(scen_arealoss_perc_both$mangrove_area1996 - scen_arealoss_perc_both$mangrove_area2020)

                    
                    scen_arealoss_perc_both$mangrove_area_future_loss <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both)
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_upper_limit)
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_lower_limit)

                    scen_arealoss_perc_both$mangrove_area_future_loss <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit,scen_arealoss_perc_both$max_loss))
                    glimpse(scen_arealoss_perc_both)
                    scen_arealoss_perc_both <- scen_arealoss_perc_both %>% filter(!is.na(gridcell_id))
                    write.csv(scen_arealoss_perc_both, file="Results\\Area\\Proj_Area_Perc_both_SSP270.csv") 
                    
                    agg_aloss_neg_total_both <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_upper_limit,by="year")
                    agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_both)
                    scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_both_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_both_SSP270.csv")
                    glimpse(scen_arealoss_perc_both_countrycode)
                    
                    # scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                    #                                         summarise(area= sum(mangrove_area_future,na.rm=TRUE), 
                    #                                         perc_area = mean(arealoss_perc_cumulative_both,na.rm=TRUE))
                    #glimpse(scen_arealoss_perc_both_countrycode)
                    
               


                    ## New Figure Pas + Future (start)area_change_wrt_1996_lower_limit
                        #agg_aloss_neg_total_both
                        #glimpse(agg_aloss_pos_both)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_both)
                        agg_aloss_neg_total_both$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss
                        agg_aloss_neg_total_both$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_both$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_lower_limit

                        
                        
                        #write.csv(summary_dfs_total_sum,file="Results\\Area\\summary_dfs_total_sum.csv")
                        write.csv(agg_aloss_neg_total_both, file="Results\\Area\\Proj_Area_Perc_Total_both_SSP270.csv")


                    
                    ## New Figure (end)
                ## Both: Climate and Socioeconomic (end)
## SSP2

## SSP4
        
## Read Data (start)
    setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
    
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all_ssps.csv")
                glimpse(scen)
                load("Models/Round2/pref_area_model.RData")
                load(file="Models/Round2/sq_estimate_sst_area.RData") 
                

                coef_ssth <- summary(model_area_ssthot)$coef[1]
                coef_ssth2 <- summary(model_area_ssthot)$coef[2]

                # Extract the variance-covariance matrix
                vcov_matrix <- vcov(model_area_ssthot)

                # Extract variances and covariance
                var_coef1 <- vcov_matrix["sst_hottest", "sst_hottest"]
                var_coef2 <- vcov_matrix["I(sst_hottest^2)", "I(sst_hottest^2)"]
                cov_coef1_coef2 <- vcov_matrix["sst_hottest", "I(sst_hottest^2)"]
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * scen$sst_hot_70)^2 * var_coef2 + 2 * 1 * (2 * scen$sst_hot_70) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)

                coef_p <- summary(model_area_ssthot)$coef[3]
                coef_p2 <- summary(model_area_ssthot)$coef[4]
                coef_gdp <- summary(model_area_ssthot)$coef[5]
                coef_gdp2 <- summary(model_area_ssthot)$coef[6]

                
                
                scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","sst_hottest","logGDPpc"),by="gridcell_id",all=TRUE)
                scen$sst_hot_70_capped <- matrixStats::rowMins(cbind(scen$sst_hot_70,rep(max(exp(scen$sst_hottest),na.rm=TRUE),times=length(scen$sst_hot_70))))
                scen$marg_ssth_area <-  coef_ssth + 2*coef_ssth2*scen$sst_hot_70
                alpha <- 0.1  # for a 95% CI
                z_value <- qnorm(1 - alpha/2) 
                scen$marg_ssth_area_lower_limit <- scen$marg_ssth_area - z_value * se_damage
                scen$marg_ssth_area_upper_limit <- scen$marg_ssth_area + z_value * se_damage


                var_coef1 <- vcov_matrix["logGDPpc", "logGDPpc"]
                var_coef2 <- vcov_matrix["I(logGDPpc^2)", "I(logGDPpc^2)"]
                cov_coef1_coef2 <- vcov_matrix["logGDPpc", "I(logGDPpc^2)"]
                #scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * log(scen$gdppc4))^2 * var_coef2 + 2 * 1 * (2 * log(scen$gdppc4)) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)
                # scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                # ggplot(scen)+geom_point(aes(x=log(gdppc4),y=logGDPpc))
                # glimpse(scen)
                # #scen$gdppc4_capped <-matrixStats::rowMins(cbind(scen$gdppc4,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc4))))
                scen$gdppc4_capped <- matrixStats::rowMins(cbind(scen$gdppc4,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc4))))
                #max((scen$gdppc4[which(is.finite(scen$gdppc4))]),na.rm=TRUE)
                
                #ggplot(scen)+geom_point(aes(x=log(gdppc4),y=log(gdppc4_capped)))
                
                scen$marg_gdppc_area <-  coef_gdp + 2*coef_gdp2* log(scen$gdppc4_capped)   #log(scen$gdppc4)   
                scen$marg_gdppc_area_lower_limit <- scen$marg_gdppc_area - z_value * se_damage
                scen$marg_gdppc_area_upper_limit <- scen$marg_gdppc_area + z_value * se_damage


                scen <- scen[which(is.finite(scen$POP_SSP4) & scen$POP_SSP4>10),]
                scen <- scen[which(is.finite(scen$gdppc4) & !is.na(scen$gdppc4)),]
                
                ## Only Climate Change (start)
                    scen$arealoss_perc_onlyCC <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)
                    scen$arealoss_perc_onlyCC <- ifelse(is.na(scen$arealoss_perc_onlyCC), 0, scen$arealoss_perc_onlyCC)
                    scen$arealoss_perc_onlyCC_lower_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_onlyCC_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_lower_limit), 0, scen$arealoss_perc_onlyCC_lower_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_upper_limit), 0, scen$arealoss_perc_onlyCC_upper_limit)
                    
                    scen_arealoss_perc_onlyCC <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlyCC = cumsum(arealoss_perc_onlyCC), 
                                    arealoss_perc_cumulative_onlyCC_lower_limit = cumsum(arealoss_perc_onlyCC_lower_limit),
                                    arealoss_perc_cumulative_onlyCC_upper_limit = cumsum(arealoss_perc_onlyCC_upper_limit)) %>%
                            ungroup() 
                    
                    timeseries_arealoss_onlyCC_countrylevel <- scen_arealoss_perc_onlyCC %>% group_by(countrycode) %>%
                                                                summarise()
                    

                    scen_arealoss_perc_onlyCC_2100 <- scen_arealoss_perc_onlyCC %>% filter(year==2100)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    names(mcn_1996)[2] <-  "mangrove_area1996"
                    names(mcn_2020)[2] <-  "mangrove_area2020"
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_1996,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_upper_limit)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_lower_limit)
                    scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$loss <- 1
                    scen_arealoss_perc_onlyCC$loss[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss<0)] <- 0

                    scen_arealoss_perc_onlyCC$pos <- 1
                    scen_arealoss_perc_onlyCC$pos[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss>0)] <- 0
                    
                    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC %>% filter(!is.na(gridcell_id))
                    glimpse(scen_arealoss_perc_onlyCC)
                    write.csv(scen_arealoss_perc_onlyCC, file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP470.csv")
                    
                    agg_aloss_neg_onlyCC <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    #num_levels <- length(unique(agg_aloss_neg$R5))
                    #agg_aloss_neg_onlyCC$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total_onlyCC <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_upper_limit,by="year")
                    agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_onlyCC)
                    agg_aloss_neg_country_onlyCC <- aggregate(mangrove_area_future_loss~year+countrycode,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    # Get world data
                    world_data <- ne_countries(scale = "medium", returnclass = "sf")

                    glimpse(scen_arealoss_perc_onlyCC)
                    scen_arealoss_perc_onlyCC_countrycode <- scen_arealoss_perc_onlyCC %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_onlyCC_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyCC_SSP470.csv")
                    
                    
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_onlyCC)
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_lower_limit

                        
                        write.csv(agg_aloss_neg_total_onlyCC, file="Results\\Area\\Proj_Area_Perc_Total_OnlyCC_SSP470.csv")


                    
                    ## New Figure (end)
                ## Only Climate Change (end)

                ## Only GDP (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_onlygdp <-  (scen$perc_inc_gdppc4*scen$marg_gdppc_area)
                    scen$arealoss_perc_onlygdp <- ifelse(is.na(scen$arealoss_perc_onlygdp), 0, scen$arealoss_perc_onlygdp)
                    scen$arealoss_perc_onlygdp_lower_limit <-  (scen$perc_inc_gdppc4*scen$marg_gdppc_area_lower_limit)
                    scen$arealoss_perc_onlygdp_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_lower_limit), 0, scen$arealoss_perc_onlygdp_lower_limit)
                    scen$arealoss_perc_onlygdp_upper_limit <-  (scen$perc_inc_gdppc4*scen$marg_gdppc_area_upper_limit)
                    scen$arealoss_perc_onlygdp_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_upper_limit), 0, scen$arealoss_perc_onlygdp_upper_limit)
                    
                    scen_arealoss_perc_onlygdp <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlygdp = cumsum(arealoss_perc_onlygdp), 
                                    arealoss_perc_cumulative_onlygdp_lower_limit = cumsum(arealoss_perc_onlygdp_lower_limit),
                                    arealoss_perc_cumulative_onlygdp_upper_limit = cumsum(arealoss_perc_onlygdp_upper_limit)) %>%
                            ungroup() 
                    
                    scen_arealoss_perc_onlygdp_2100 <- scen_arealoss_perc_onlygdp %>% filter(year==2100)
                    
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)

                    scen_arealoss_perc_onlygdp<- merge(scen_arealoss_perc_onlygdp,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)

                    
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_2020 %>% dplyr::select("gridcell_id","mangrove_area"),by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_onlygdp$max_loss <- abs(scen_arealoss_perc_onlygdp$mangrove_area1996 - scen_arealoss_perc_onlygdp$mangrove_area2020)
                    

                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_upper_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_upper_limit),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_lower_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_lower_limit),scen_arealoss_perc_onlygdp$max_loss))
                    #scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- 
                    scen_arealoss_perc_onlygdp$mangrove_area_future <- scen_arealoss_perc_onlygdp$mangrove_area *(1-0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp)
                    scen_arealoss_perc_onlygdp$loss <- 1
                    scen_arealoss_perc_onlygdp$loss[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss<0)] <- 0
                    
                    scen_arealoss_perc_onlygdp$pos <- 1
                    scen_arealoss_perc_onlygdp$pos[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss>0)] <- 0
                    
                    agg_aloss_neg_onlygdp <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    #num_levels <- length(unique(agg_aloss_neg$R5))
                    #agg_aloss_neg_onlygdp$color_vector <- scico(n = num_levels, palette = "batlow")
                    glimpse(scen_arealoss_perc_onlygdp)
                    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp %>% filter(!is.na(gridcell_id))
                    write.csv(scen_arealoss_perc_onlygdp, file="Results\\Area\\Proj_Area_Perc_OnlyGDP_SSP470.csv") 
                    agg_aloss_neg_total_onlygdp <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_upper_limit,by="year")
                    agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_onlygdp)
                    #write.csv(agg_aloss_neg_total_onlygdp,file="Results\\Area\\agg_aloss_neg_total_onlygdp.csv") 
                    scen_arealoss_perc_onlygdp_countrycode <- scen_arealoss_perc_onlygdp %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_onlygdp_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyGDP_SSP470.csv") 
                    glimpse(scen_arealoss_perc_onlygdp_countrycode)
                    
                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                        
                        agg_aloss_pos_onlygdp <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_lower_limit

                        #glimpse(mcn_2020)
                        #merge(mcn_2020,mcn_1996,by=c("gridcell_id","year"))

                        
                        
                        write.csv(agg_aloss_neg_total_onlygdp, file="Results\\Area\\Proj_Area_Perc_Total_OnlyGDP_SSP470.csv")


                    
                    ## New Figure (end)
                ## Only GDP (end)

                ## Both: Climate and Socioeconomic (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_both <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)+(scen$perc_inc_gdppc4*scen$marg_gdppc_area)
                    scen$arealoss_perc_both <- ifelse(is.na(scen$arealoss_perc_both), 0, scen$arealoss_perc_both)
                    scen$arealoss_perc_both_lower_limit <-  (scen$perc_inc_gdppc4*scen$marg_gdppc_area_lower_limit) + (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_both_lower_limit <- ifelse(is.na(scen$arealoss_perc_both_lower_limit), 0, scen$arealoss_perc_both_lower_limit)
                    scen$arealoss_perc_both_upper_limit <-  (scen$perc_inc_gdppc4*scen$marg_gdppc_area_upper_limit)+ (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_both_upper_limit <- ifelse(is.na(scen$arealoss_perc_both_upper_limit), 0, scen$arealoss_perc_both_upper_limit)
                    
                    scen_arealoss_perc_both <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_both = cumsum(arealoss_perc_both), 
                                    arealoss_perc_cumulative_both_lower_limit = cumsum(arealoss_perc_both_lower_limit),
                                    arealoss_perc_cumulative_both_upper_limit = cumsum(arealoss_perc_both_upper_limit)) %>%
                            ungroup() 
                    glimpse(scen_arealoss_perc_both)

                    scen_arealoss_perc_both_2100 <- scen_arealoss_perc_both %>% filter(year==2100)
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==1996) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_both$max_loss <- abs(scen_arealoss_perc_both$mangrove_area1996 - scen_arealoss_perc_both$mangrove_area2020)

                    
                    scen_arealoss_perc_both$mangrove_area_future_loss <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both)
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_upper_limit)
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_lower_limit)

                    scen_arealoss_perc_both$mangrove_area_future_loss <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit,scen_arealoss_perc_both$max_loss))
                    glimpse(scen_arealoss_perc_both)
                    scen_arealoss_perc_both <- scen_arealoss_perc_both %>% filter(!is.na(gridcell_id))
                    write.csv(scen_arealoss_perc_both, file="Results\\Area\\Proj_Area_Perc_both_SSP470.csv") 
                    
                    agg_aloss_neg_total_both <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_upper_limit,by="year")
                    agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_both)
                    scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_both_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_both_SSP470.csv")
                    glimpse(scen_arealoss_perc_both_countrycode)
                    
                    # scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                    #                                         summarise(area= sum(mangrove_area_future,na.rm=TRUE), 
                    #                                         perc_area = mean(arealoss_perc_cumulative_both,na.rm=TRUE))
                    #glimpse(scen_arealoss_perc_both_countrycode)
                    
               


                    ## New Figure Pas + Future (start)area_change_wrt_1996_lower_limit
                        #agg_aloss_neg_total_both
                        #glimpse(agg_aloss_pos_both)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_both)
                        agg_aloss_neg_total_both$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss
                        agg_aloss_neg_total_both$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_both$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_lower_limit

                        
                        
                        #write.csv(summary_dfs_total_sum,file="Results\\Area\\summary_dfs_total_sum.csv")
                        write.csv(agg_aloss_neg_total_both, file="Results\\Area\\Proj_Area_Perc_Total_both_SSP470.csv")


                    
                    ## New Figure (end)
                ## Both: Climate and Socioeconomic (end)
## SSP4


## SSP5
        
## Read Data (start)
    setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
    
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all_ssps.csv")
                glimpse(scen)
                load("Models/Round2/pref_area_model.RData")
                load(file="Models/Round2/sq_estimate_sst_area.RData") 
                

                coef_ssth <- summary(model_area_ssthot)$coef[1]
                coef_ssth2 <- summary(model_area_ssthot)$coef[2]

                # Extract the variance-covariance matrix
                vcov_matrix <- vcov(model_area_ssthot)

                # Extract variances and covariance
                var_coef1 <- vcov_matrix["sst_hottest", "sst_hottest"]
                var_coef2 <- vcov_matrix["I(sst_hottest^2)", "I(sst_hottest^2)"]
                cov_coef1_coef2 <- vcov_matrix["sst_hottest", "I(sst_hottest^2)"]
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * scen$sst_hot_70)^2 * var_coef2 + 2 * 1 * (2 * scen$sst_hot_70) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)

                coef_p <- summary(model_area_ssthot)$coef[3]
                coef_p2 <- summary(model_area_ssthot)$coef[4]
                coef_gdp <- summary(model_area_ssthot)$coef[5]
                coef_gdp2 <- summary(model_area_ssthot)$coef[6]

                
                
                scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","sst_hottest","logGDPpc"),by="gridcell_id",all=TRUE)
                scen$sst_hot_70_capped <- matrixStats::rowMins(cbind(scen$sst_hot_70,rep(max(exp(scen$sst_hottest),na.rm=TRUE),times=length(scen$sst_hot_70))))
                scen$marg_ssth_area <-  coef_ssth + 2*coef_ssth2*scen$sst_hot_70
                alpha <- 0.1  # for a 95% CI
                z_value <- qnorm(1 - alpha/2) 
                scen$marg_ssth_area_lower_limit <- scen$marg_ssth_area - z_value * se_damage
                scen$marg_ssth_area_upper_limit <- scen$marg_ssth_area + z_value * se_damage


                var_coef1 <- vcov_matrix["logGDPpc", "logGDPpc"]
                var_coef2 <- vcov_matrix["I(logGDPpc^2)", "I(logGDPpc^2)"]
                cov_coef1_coef2 <- vcov_matrix["logGDPpc", "I(logGDPpc^2)"]
                #scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * log(scen$gdppc5))^2 * var_coef2 + 2 * 1 * (2 * log(scen$gdppc5)) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)
                # scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                # ggplot(scen)+geom_point(aes(x=log(gdppc5),y=logGDPpc))
                # glimpse(scen)
                # #scen$gdppc5_capped <-matrixStats::rowMins(cbind(scen$gdppc5,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc5))))
                scen$gdppc5_capped <- matrixStats::rowMins(cbind(scen$gdppc5,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc5))))
                #max((scen$gdppc5[which(is.finite(scen$gdppc5))]),na.rm=TRUE)
                
                #ggplot(scen)+geom_point(aes(x=log(gdppc5),y=log(gdppc5_capped)))
                
                scen$marg_gdppc_area <-  coef_gdp + 2*coef_gdp2* log(scen$gdppc5_capped)   #log(scen$gdppc5)   
                scen$marg_gdppc_area_lower_limit <- scen$marg_gdppc_area - z_value * se_damage
                scen$marg_gdppc_area_upper_limit <- scen$marg_gdppc_area + z_value * se_damage


                scen <- scen[which(is.finite(scen$POP_SSP5) & scen$POP_SSP5>10),]
                scen <- scen[which(is.finite(scen$gdppc5) & !is.na(scen$gdppc5)),]
                
                ## Only Climate Change (start)
                    scen$arealoss_perc_onlyCC <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)
                    scen$arealoss_perc_onlyCC <- ifelse(is.na(scen$arealoss_perc_onlyCC), 0, scen$arealoss_perc_onlyCC)
                    scen$arealoss_perc_onlyCC_lower_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_onlyCC_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_lower_limit), 0, scen$arealoss_perc_onlyCC_lower_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_upper_limit), 0, scen$arealoss_perc_onlyCC_upper_limit)
                    
                    scen_arealoss_perc_onlyCC <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlyCC = cumsum(arealoss_perc_onlyCC), 
                                    arealoss_perc_cumulative_onlyCC_lower_limit = cumsum(arealoss_perc_onlyCC_lower_limit),
                                    arealoss_perc_cumulative_onlyCC_upper_limit = cumsum(arealoss_perc_onlyCC_upper_limit)) %>%
                            ungroup() 
                    
                    timeseries_arealoss_onlyCC_countrylevel <- scen_arealoss_perc_onlyCC %>% group_by(countrycode) %>%
                                                                summarise()
                    

                    scen_arealoss_perc_onlyCC_2100 <- scen_arealoss_perc_onlyCC %>% filter(year==2100)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    names(mcn_1996)[2] <-  "mangrove_area1996"
                    names(mcn_2020)[2] <-  "mangrove_area2020"
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_1996,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_upper_limit)
                    scen_arealoss_perc_onlyCC$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_onlyCC$mangrove_area *(0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC_lower_limit)
                    scen_arealoss_perc_onlyCC$mangrove_area_future <- scen_arealoss_perc_onlyCC$mangrove_area *(1-0.01*scen_arealoss_perc_onlyCC$arealoss_perc_cumulative_onlyCC)
                    scen_arealoss_perc_onlyCC$loss <- 1
                    scen_arealoss_perc_onlyCC$loss[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss<0)] <- 0

                    scen_arealoss_perc_onlyCC$pos <- 1
                    scen_arealoss_perc_onlyCC$pos[which(scen_arealoss_perc_onlyCC$mangrove_area_future_loss>0)] <- 0
                    
                    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC %>% filter(!is.na(gridcell_id))
                    glimpse(scen_arealoss_perc_onlyCC)
                    write.csv(scen_arealoss_perc_onlyCC, file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP570.csv")
                    
                    agg_aloss_neg_onlyCC <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    #num_levels <- length(unique(agg_aloss_neg$R5))
                    #agg_aloss_neg_onlyCC$color_vector <- scico(n = num_levels, palette = "batlow")
                    agg_aloss_neg_total_onlyCC <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_upper_limit,by="year")
                    agg_aloss_neg_total_onlyCC <- merge(agg_aloss_neg_total_onlyCC,agg_aloss_neg_total_onlyCC_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_onlyCC)
                    agg_aloss_neg_country_onlyCC <- aggregate(mangrove_area_future_loss~year+countrycode,data=scen_arealoss_perc_onlyCC,FUN="sum")
                    # Get world data
                    world_data <- ne_countries(scale = "medium", returnclass = "sf")

                    glimpse(scen_arealoss_perc_onlyCC)
                    scen_arealoss_perc_onlyCC_countrycode <- scen_arealoss_perc_onlyCC %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_onlyCC_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyCC_SSP570.csv")
                    
                    
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_onlyCC)
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_lower_limit

                        
                        write.csv(agg_aloss_neg_total_onlyCC, file="Results\\Area\\Proj_Area_Perc_Total_OnlyCC_SSP570.csv")


                    
                    ## New Figure (end)
                ## Only Climate Change (end)

                ## Only GDP (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_onlygdp <-  (scen$perc_inc_gdppc5*scen$marg_gdppc_area)
                    scen$arealoss_perc_onlygdp <- ifelse(is.na(scen$arealoss_perc_onlygdp), 0, scen$arealoss_perc_onlygdp)
                    scen$arealoss_perc_onlygdp_lower_limit <-  (scen$perc_inc_gdppc5*scen$marg_gdppc_area_lower_limit)
                    scen$arealoss_perc_onlygdp_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_lower_limit), 0, scen$arealoss_perc_onlygdp_lower_limit)
                    scen$arealoss_perc_onlygdp_upper_limit <-  (scen$perc_inc_gdppc5*scen$marg_gdppc_area_upper_limit)
                    scen$arealoss_perc_onlygdp_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlygdp_upper_limit), 0, scen$arealoss_perc_onlygdp_upper_limit)
                    
                    scen_arealoss_perc_onlygdp <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlygdp = cumsum(arealoss_perc_onlygdp), 
                                    arealoss_perc_cumulative_onlygdp_lower_limit = cumsum(arealoss_perc_onlygdp_lower_limit),
                                    arealoss_perc_cumulative_onlygdp_upper_limit = cumsum(arealoss_perc_onlygdp_upper_limit)) %>%
                            ungroup() 
                    
                    scen_arealoss_perc_onlygdp_2100 <- scen_arealoss_perc_onlygdp %>% filter(year==2100)
                    
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)

                    scen_arealoss_perc_onlygdp<- merge(scen_arealoss_perc_onlygdp,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)

                    
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_2020 %>% dplyr::select("gridcell_id","mangrove_area"),by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_onlygdp$max_loss <- abs(scen_arealoss_perc_onlygdp$mangrove_area1996 - scen_arealoss_perc_onlygdp$mangrove_area2020)
                    

                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_upper_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_upper_limit),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_lower_limit <- matrixStats::rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_lower_limit),scen_arealoss_perc_onlygdp$max_loss))
                    #scen_arealoss_perc_onlygdp$mangrove_area_future_loss <- 
                    scen_arealoss_perc_onlygdp$mangrove_area_future <- scen_arealoss_perc_onlygdp$mangrove_area *(1-0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp)
                    scen_arealoss_perc_onlygdp$loss <- 1
                    scen_arealoss_perc_onlygdp$loss[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss<0)] <- 0
                    
                    scen_arealoss_perc_onlygdp$pos <- 1
                    scen_arealoss_perc_onlygdp$pos[which(scen_arealoss_perc_onlygdp$mangrove_area_future_loss>0)] <- 0
                    
                    agg_aloss_neg_onlygdp <- aggregate(mangrove_area_future_loss~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year+loss+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    #num_levels <- length(unique(agg_aloss_neg$R5))
                    #agg_aloss_neg_onlygdp$color_vector <- scico(n = num_levels, palette = "batlow")
                    glimpse(scen_arealoss_perc_onlygdp)
                    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp %>% filter(!is.na(gridcell_id))
                    write.csv(scen_arealoss_perc_onlygdp, file="Results\\Area\\Proj_Area_Perc_OnlyGDP_SSP570.csv") 
                    agg_aloss_neg_total_onlygdp <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_onlygdp,FUN="sum")
                    agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_upper_limit,by="year")
                    agg_aloss_neg_total_onlygdp <- merge(agg_aloss_neg_total_onlygdp,agg_aloss_neg_total_onlygdp_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_onlygdp)
                    #write.csv(agg_aloss_neg_total_onlygdp,file="Results\\Area\\agg_aloss_neg_total_onlygdp.csv") 
                    scen_arealoss_perc_onlygdp_countrycode <- scen_arealoss_perc_onlygdp %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_onlygdp_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_OnlyGDP_SSP570.csv") 
                    glimpse(scen_arealoss_perc_onlygdp_countrycode)
                    
                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                        
                        agg_aloss_pos_onlygdp <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_lower_limit

                        #glimpse(mcn_2020)
                        #merge(mcn_2020,mcn_1996,by=c("gridcell_id","year"))

                        
                        
                        write.csv(agg_aloss_neg_total_onlygdp, file="Results\\Area\\Proj_Area_Perc_Total_OnlyGDP_SSP570.csv")


                    
                    ## New Figure (end)
                ## Only GDP (end)

                ## Both: Climate and Socioeconomic (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_both <-  (100*scen$delta_sst_hot_70*scen$marg_ssth_area)+(scen$perc_inc_gdppc5*scen$marg_gdppc_area)
                    scen$arealoss_perc_both <- ifelse(is.na(scen$arealoss_perc_both), 0, scen$arealoss_perc_both)
                    scen$arealoss_perc_both_lower_limit <-  (scen$perc_inc_gdppc5*scen$marg_gdppc_area_lower_limit) + (100*scen$delta_sst_hot_70*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_both_lower_limit <- ifelse(is.na(scen$arealoss_perc_both_lower_limit), 0, scen$arealoss_perc_both_lower_limit)
                    scen$arealoss_perc_both_upper_limit <-  (scen$perc_inc_gdppc5*scen$marg_gdppc_area_upper_limit)+ (100*scen$delta_sst_hot_70*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_both_upper_limit <- ifelse(is.na(scen$arealoss_perc_both_upper_limit), 0, scen$arealoss_perc_both_upper_limit)
                    
                    scen_arealoss_perc_both <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_both = cumsum(arealoss_perc_both), 
                                    arealoss_perc_cumulative_both_lower_limit = cumsum(arealoss_perc_both_lower_limit),
                                    arealoss_perc_cumulative_both_upper_limit = cumsum(arealoss_perc_both_upper_limit)) %>%
                            ungroup() 
                    glimpse(scen_arealoss_perc_both)

                    scen_arealoss_perc_both_2100 <- scen_arealoss_perc_both %>% filter(year==2100)
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==1996) %>% dplyr::select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_both$max_loss <- abs(scen_arealoss_perc_both$mangrove_area1996 - scen_arealoss_perc_both$mangrove_area2020)

                    
                    scen_arealoss_perc_both$mangrove_area_future_loss <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both)
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_upper_limit)
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_lower_limit)

                    scen_arealoss_perc_both$mangrove_area_future_loss <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <-matrixStats::rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit,scen_arealoss_perc_both$max_loss))
                    glimpse(scen_arealoss_perc_both)
                    scen_arealoss_perc_both <- scen_arealoss_perc_both %>% filter(!is.na(gridcell_id))
                    write.csv(scen_arealoss_perc_both, file="Results\\Area\\Proj_Area_Perc_both_SSP570.csv") 
                    
                    agg_aloss_neg_total_both <- aggregate(mangrove_area_future_loss~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both_lower_limit <- aggregate(mangrove_area_future_loss_lower_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both_upper_limit <- aggregate(mangrove_area_future_loss_upper_limit~year,data=scen_arealoss_perc_both,FUN="sum")
                    agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_upper_limit,by="year")
                    agg_aloss_neg_total_both <- merge(agg_aloss_neg_total_both,agg_aloss_neg_total_both_lower_limit,by="year")
                    glimpse(agg_aloss_neg_total_both)
                    scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                                                            summarise(mangrove_area_future_loss_lower_limit= sum(mangrove_area_future_loss_lower_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss_upper_limit= sum(mangrove_area_future_loss_upper_limit,na.rm=TRUE),
                                                            mangrove_area_future_loss= sum(mangrove_area_future_loss,na.rm=TRUE),
                                                            mangrove_area1996 = sum(mangrove_area1996,na.rm=TRUE),
                                                            mangrove_area2020 = sum(mangrove_area2020,na.rm=TRUE),
                                                            R5=first(R5))  %>% ungroup() %>%
                                                            mutate(level_2020_loss = mangrove_area2020 - mangrove_area1996) %>%
                                                            mutate(mangrove_area_future_loss_lower_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_lower_limit,
                                                            mangrove_area_future_loss_upper_limit_wrt1996= level_2020_loss+mangrove_area_future_loss_upper_limit,
                                                            mangrove_area_future_loss_wrt1996= mangrove_area_future_loss+level_2020_loss)
                    write.csv(scen_arealoss_perc_both_countrycode, file="Results\\Area\\Proj_Area_Perc_Country_both_SSP570.csv")
                    glimpse(scen_arealoss_perc_both_countrycode)
                    
                    # scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                    #                                         summarise(area= sum(mangrove_area_future,na.rm=TRUE), 
                    #                                         perc_area = mean(arealoss_perc_cumulative_both,na.rm=TRUE))
                    #glimpse(scen_arealoss_perc_both_countrycode)
                    
               


                    ## New Figure Pas + Future (start)area_change_wrt_1996_lower_limit
                        #agg_aloss_neg_total_both
                        #glimpse(agg_aloss_pos_both)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_both)
                        agg_aloss_neg_total_both$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss
                        agg_aloss_neg_total_both$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_both$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_both$mangrove_area_future_loss_lower_limit

                        
                        
                        #write.csv(summary_dfs_total_sum,file="Results\\Area\\summary_dfs_total_sum.csv")
                        write.csv(agg_aloss_neg_total_both, file="Results\\Area\\Proj_Area_Perc_Total_both_SSP570.csv")


                    
                    ## New Figure (end)
                ## Both: Climate and Socioeconomic (end)
## SSP5