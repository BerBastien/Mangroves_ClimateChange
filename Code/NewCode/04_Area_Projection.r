## Make Projections
library("ggplot2")
library("ggpubr")
library("scico")
library("jtools")
library("scales")
library("rnaturalearth")
library("countrycode")
library("WDI")

## Read Data (start)
    setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
## Read Data (end)

## Plot before and after (start)
    ## Distribution Plots 
        ## Gaps
            #load data
                
                #load("Models/Round1/sq_estimate_sst_holes.RData")
                #load("Models/Round1/sss_available.RData")


    

            ## Area Loss Projections (start)
                
                scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all.csv")
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
                var_damage <- var_coef1 + (2 * scen$sst_hot_85)^2 * var_coef2 + 2 * 1 * (2 * scen$sst_hot_85) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)

                coef_p <- summary(model_area_ssthot)$coef[3]
                coef_p2 <- summary(model_area_ssthot)$coef[4]
                coef_gdp <- summary(model_area_ssthot)$coef[5]
                coef_gdp2 <- summary(model_area_ssthot)$coef[6]

                scen$marg_ssth_area <-  coef_ssth + 2*coef_ssth2*scen$sst_hot_85
                alpha <- 0.1  # for a 95% CI
                z_value <- qnorm(1 - alpha/2) 
                scen$marg_ssth_area_lower_limit <- scen$marg_ssth_area - z_value * se_damage
                scen$marg_ssth_area_upper_limit <- scen$marg_ssth_area + z_value * se_damage


                var_coef1 <- vcov_matrix["logGDPpc", "logGDPpc"]
                var_coef2 <- vcov_matrix["I(logGDPpc^2)", "I(logGDPpc^2)"]
                cov_coef1_coef2 <- vcov_matrix["logGDPpc", "I(logGDPpc^2)"]
                # Calculate the variance of the derived coefficient
                var_damage <- var_coef1 + (2 * log(scen$gdppc2))^2 * var_coef2 + 2 * 1 * (2 * log(scen$gdppc2)) * cov_coef1_coef2
                # Calculate the standard error for the derived coefficient
                se_damage <- sqrt(var_damage)
                scen <- merge(scen,mcn %>% filter(year==2020) %>% select("gridcell_id","logGDPpc"),by="gridcell_id",all=TRUE)
                scen$gdppc2_capped <- rowMins(cbind(scen$gdppc2,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc2))))
                
                ggplot(scen)+geom_point(aes(x=log(gdppc2),y=log(gdppc2_capped)))
                
                scen$marg_gdppc_area <-  coef_gdp + 2*coef_gdp2* log(scen$gdppc2_capped)   #log(scen$gdppc2)   
                scen$marg_gdppc_area_lower_limit <- scen$marg_gdppc_area - z_value * se_damage
                scen$marg_gdppc_area_upper_limit <- scen$marg_gdppc_area + z_value * se_damage


                scen <- scen[which(is.finite(scen$pop_ssp2) & scen$pop_ssp2>10),]
                scen <- scen[which(is.finite(scen$gdppc2) & !is.na(scen$gdppc2)),]
                
                ## Only Climate Change (start)
                    scen$arealoss_perc_onlyCC <-  (100*scen$delta_sst_hot*scen$marg_ssth_area)
                    scen$arealoss_perc_onlyCC <- ifelse(is.na(scen$arealoss_perc_onlyCC), 0, scen$arealoss_perc_onlyCC)
                    scen$arealoss_perc_onlyCC_lower_limit <-  (100*scen$delta_sst_hot*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_onlyCC_lower_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_lower_limit), 0, scen$arealoss_perc_onlyCC_lower_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <-  (100*scen$delta_sst_hot*scen$marg_ssth_area_upper_limit)
                    scen$arealoss_perc_onlyCC_upper_limit <- ifelse(is.na(scen$arealoss_perc_onlyCC_upper_limit), 0, scen$arealoss_perc_onlyCC_upper_limit)
                    
                    scen_arealoss_perc_onlyCC <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative_onlyCC = cumsum(arealoss_perc_onlyCC), 
                                    arealoss_perc_cumulative_onlyCC_lower_limit = cumsum(arealoss_perc_onlyCC_lower_limit),
                                    arealoss_perc_cumulative_onlyCC_upper_limit = cumsum(arealoss_perc_onlyCC_upper_limit)) %>%
                            ungroup() 
                    glimpse(scen_arealoss_perc_onlyCC)

                    timeseries_arealoss_onlyCC_countrylevel <- scen_arealoss_perc_onlyCC %>% group_by(countrycode) %>%
                                                                summarise()
                    glimpse(timeseries_arealoss_onlyCC_countrylevel)


                    scen_arealoss_perc_onlyCC_2100 <- scen_arealoss_perc_onlyCC %>% filter(year==2100)
                    
                    min(scen_arealoss_perc_onlyCC_2100$arealoss_perc_cumulative_onlyCC)
                    max(scen_arealoss_perc_onlyCC_2100$arealoss_perc_cumulative_onlyCC)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)
                    glimpse(mcn_1996)
                    #names(mcn_1996)[2] <-  "mangrove_area1996"

                    
                    
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    names(mcn_1996)[2] <-  "mangrove_area1996"
                    names(mcn_2020)[2] <-  "mangrove_area2020"
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_2020,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_1996,by=c("gridcell_id"),all=T)
                    #scen_arealoss_perc_onlyCC<- merge(scen_arealoss_perc_onlyCC,mcn_1996,by=c("gridcell_id"),all=T)
                    
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    glimpse(scen_arealoss_perc_onlyCC)
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
                    write.csv(scen_arealoss_perc_onlyCC, file="Results\\Area\\scen_arealoss_perc_onlyCC.csv")
                    
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
                    write.csv(scen_arealoss_perc_onlyCC_countrycode, file="Results\\Area\\scen_arealoss_perc_onlyCC_countrycode_ssp2_cappedArea_cappedEffect.csv")
                    glimpse(scen_arealoss_perc_onlyCC_countrycode)
                    
                    # Filter your data for the year 2100
                    agg_aloss_2100 <- agg_aloss_neg_country_onlyCC %>% filter(year == 2100)
                    map_aloss_neg_country_onlyCC <- left_join(world_data, agg_aloss_2100, by = c("iso_a3" = "countrycode"))

                    glimpse(agg_aloss_neg_country_onlyCC)

                    ## Economic Loss By Country
                    val <- 193844 * 100 #value in dollars per hectare converted to dollars per km
                    agg_aloss_neg_country_onlyCC <- agg_aloss_neg_country_onlyCC %>%
                    mutate(economic_loss = mangrove_area_future_loss * val)
                    agg_aloss_neg_country_onlyCC <- agg_aloss_neg_country_onlyCC %>%
                    mutate(discounted_loss = economic_loss / (1.02)^(year - 2023))
                    agg_loss_by_country <- agg_aloss_neg_country_onlyCC %>%
                    group_by(countrycode) %>%
                    summarize(total_loss_2023PV = sum(discounted_loss, na.rm = TRUE))
                    # Assuming you have a data frame like this:
                    # gdp_2022 <- data.frame(countrycode = c(...), gdp = c(...))

                    # Fetching GDP (in current US$) for the year 2022
                    library("WDI")
                    library("countrycode")
                    gdp_2022 <- WDI(indicator = "NY.GDP.MKTP.CD", start = 2022, end = 2022)
                    gdp_2022 <- gdp_2022 %>%
                    select(iso2c, NY.GDP.MKTP.CD)
                    colnames(gdp_2022) <- c("countrycode", "gdp")
                    gdp_2022$countrycode <- countrycode(gdp_2022$countrycode,origin="iso2c",destination="iso3c")
                    gdp_2022$countryname <- countrycode(gdp_2022$countrycode,origin="iso3c",destination="country.name")

                    agg_loss_by_country <- left_join(agg_loss_by_country, gdp_2022, by = "countrycode")
                    agg_loss_by_country <- agg_loss_by_country %>%
                    mutate(loss_as_percent_GDP = (total_loss_2023PV / gdp) * 100)



                    #save(map_aloss_neg_country_onlyCC,file="Results\\Area\\map_aloss_neg_country_onlyCC.Rda")
                    #write.csv(agg_aloss_neg_total_onlyCC,file="Results\\Area\\agg_aloss_neg_total_onlyCC.csv") 
                    #write.csv(scen_arealoss_perc_onlyCC,file="Results\\Area\\scen_arealoss_perc_onlyCC.csv")
                    #write.csv(scen_arealoss_perc_onlyCC_2100,file="Results\\Area\\scen_arealoss_perc_onlyCC_2100.csv") 
                    #write.csv(agg_aloss_neg_onlyCC,file="Results\\Area\\agg_aloss_neg_onlyCC.csv") 
                    #write.csv(agg_aloss_neg_country_onlyCC,file="Results\\Area\\agg_aloss_neg_country_onlyCC.csv") 
                    #write.csv(agg_loss_by_country,file="Results\\Area\\agg_loss_by_country.csv")
                    scen_arealoss_perc_onlyCC_countrycode <- scen_arealoss_perc_onlyCC %>% group_by(countrycode,year) %>% 
                                                            summarise(area= sum(mangrove_area_future,na.rm=TRUE), 
                                                            perc_area = mean(arealoss_perc_cumulative_onlyCC,na.rm=TRUE))
                    glimpse(scen_arealoss_perc_onlyCC_countrycode)
                    # ggplot(scen_arealoss_perc_onlyCC_countrycode)+
                    # geom_line(aes(x=year,y=(perc_area),group=countrycode))
                    #write.csv(scen_arealoss_perc_onlyCC_countrycode,"Data/output/scen_arealoss_perc_onlyCC_countrycode.csv")

                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    ggplot(agg_aloss_neg_onlyCC)+
                    # geom_bar(aes(x=year,y=100*mangrove_area_future_loss/sum2020 ,
                    #     #fill=factor(loss)),stat="identity")+
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Mangrove Area Loss (%)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    # scale_fill_manual(values = color_vector) +
                    # scale_color_manual(values = color_vector)+
                    # geom_hline(aes(yintercept=0),linetype="dashed")+
                    # geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future_loss/sum2020),color="indianred",size=1.5)+
                    # theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    # ggsave("Figures/Draft/Projection_Loss_Gain_RCP85_percent.png",dpi=600)


                    ## New Figure Pas + Future (start)
                        
                        agg_aloss_pos_onlyCC <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlyCC,FUN="sum")
                        glimpse(agg_aloss_pos_onlyCC)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_onlyCC)
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlyCC$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlyCC$mangrove_area_future_loss_lower_limit

                        
                        #write.csv(agg_aloss_neg_total_onlyCC, file="Results\\Area\\agg_aloss_neg_total_onlyCC_ssp2_cappedArea_cappedEffect.csv")
                        #write.csv(summary_dfs_total_sum,file="Results\\Area\\summary_dfs_total_sum.csv")
                        #write.csv(agg_aloss_neg_total_onlyCC, file="Results\\Area\\agg_aloss_neg_total_onlyCC.csv")


                    
                    ## New Figure (end)
                ## Only Climate Change (end)

                ## Only GDP (start)
                    ggplot(scen %>% filter(year > 2025))+geom_point(aes(y=perc_inc_gdppc2,x=year))
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
                    glimpse(scen_arealoss_perc_onlygdp)

                    scen_arealoss_perc_onlygdp_2100 <- scen_arealoss_perc_onlygdp %>% filter(year==2100)
                    
                    min(scen_arealoss_perc_onlygdp_2100$arealoss_perc_cumulative_onlygdp)
                    max(scen_arealoss_perc_onlygdp_2100$arealoss_perc_cumulative_onlygdp)
                    mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_2020$mangrove_area,na.rm=T)
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    sum(mcn_1996$mangrove_area,na.rm=T)

                    scen_arealoss_perc_onlygdp<- merge(scen_arealoss_perc_onlygdp,mcn_2020,by=c("gridcell_id"),all=T)
                    sum(mcn_2020$mangrove_area,na.rm=T)

                    
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_onlygdp <- merge(scen_arealoss_perc_onlygdp,mcn_2020 %>% select("gridcell_id","mangrove_area"),by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_onlygdp$max_loss <- abs(scen_arealoss_perc_onlygdp$mangrove_area1996 - scen_arealoss_perc_onlygdp$mangrove_area2020)
                    #gain_since_1996 <- unique(scen_arealoss_perc_onlygdp$gridcell_id[which(scen_arealoss_perc_onlygdp$max_loss<0)])
                    
                    #install.packages("matrixStats")
                    library(matrixStats)
                    #scen_arealoss_perc_onlygdp$mangrove_area_future_capped <- rowMaxs(cbind(scen_arealoss_perc_onlygdp$mangrove_area_future,scen_arealoss_perc_onlygdp$mangrove_area1996))


                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss <-  rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_upper_limit <-  rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_upper_limit),scen_arealoss_perc_onlygdp$max_loss))
                    scen_arealoss_perc_onlygdp$mangrove_area_future_loss_lower_limit <-  rowMins(cbind(scen_arealoss_perc_onlygdp$mangrove_area *(0.01*scen_arealoss_perc_onlygdp$arealoss_perc_cumulative_onlygdp_lower_limit),scen_arealoss_perc_onlygdp$max_loss))
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
                    write.csv(scen_arealoss_perc_onlygdp, file="Results\\Area\\scen_arealoss_perc_onlygdp.csv")
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
                    write.csv(scen_arealoss_perc_onlygdp_countrycode, file="Results\\Area\\scen_arealoss_perc_onlygdp_countrycode_ssp2_cappedArea_cappedEffect.csv")
                    glimpse(scen_arealoss_perc_onlygdp_countrycode)
                    
                    
                    sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    #ggplot(agg_aloss_neg_onlygdp)+
                    # geom_bar(aes(x=year,y=100*mangrove_area_future_loss/sum2020 ,
                    #     #fill=factor(loss)),stat="identity")+
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Mangrove Area Loss (%)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    # scale_fill_manual(values = color_vector) +
                    # scale_color_manual(values = color_vector)+
                    # geom_hline(aes(yintercept=0),linetype="dashed")+
                    # geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future_loss/sum2020),color="indianred",size=1.5)+
                    # theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    # ggsave("Figures/Draft/Projection_Loss_Gain_RCP85_percent.png",dpi=600)


                    ## New Figure Pas + Future (start)
                        
                        agg_aloss_pos_onlygdp <- aggregate(mangrove_area_future_loss~year+pos+R5,data=scen_arealoss_perc_onlygdp,FUN="sum")
                        glimpse(agg_aloss_pos_onlygdp)
                        summary_dfs_total_sum <- read.csv("Data/Fig_Vals/Past_Area_Change.csv")
                        glimpse(summary_dfs_total_sum)
                        level_2020_loss  <-cumsum(summary_dfs_total_sum$area_change)[which(summary_dfs_total_sum$year==2020)]
                        glimpse(agg_aloss_neg_total_onlygdp)
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996 <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_upper_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_upper_limit
                        agg_aloss_neg_total_onlygdp$area_change_wrt_1996_lower_limit <- level_2020_loss + agg_aloss_neg_total_onlygdp$mangrove_area_future_loss_lower_limit

                        #glimpse(mcn_2020)
                        #merge(mcn_2020,mcn_1996,by=c("gridcell_id","year"))

                        
                        
                        write.csv(agg_aloss_neg_total_onlygdp, file="Results\\Area\\agg_aloss_neg_total_onlygdp_ssp2_cappedArea_cappedEffect.csv")


                    
                    ## New Figure (end)
                ## Only GDP (end)

                ## Both: Climate and Socioeconomic (start)
                    scen <- scen %>% filter(year>2025)
                    scen$arealoss_perc_both <-  (100*scen$delta_sst_hot*scen$marg_ssth_area)+(scen$perc_inc_gdppc2*scen$marg_gdppc_area)
                    scen$arealoss_perc_both <- ifelse(is.na(scen$arealoss_perc_both), 0, scen$arealoss_perc_both)
                    scen$arealoss_perc_both_lower_limit <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area_lower_limit) + (100*scen$delta_sst_hot*scen$marg_ssth_area_lower_limit)
                    scen$arealoss_perc_both_lower_limit <- ifelse(is.na(scen$arealoss_perc_both_lower_limit), 0, scen$arealoss_perc_both_lower_limit)
                    scen$arealoss_perc_both_upper_limit <-  (scen$perc_inc_gdppc2*scen$marg_gdppc_area_upper_limit)+ (100*scen$delta_sst_hot*scen$marg_ssth_area_upper_limit)
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
                    #mcn_2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    #mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    #scen_arealoss_perc_both<- merge(scen_arealoss_perc_both,mcn_2020,by=c("gridcell_id"),all=T)
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==2020) %>% select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","2020"))
                    scen_arealoss_perc_both <- merge(scen_arealoss_perc_both,mcn %>% filter(year==1996) %>% select("gridcell_id","mangrove_area") ,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    scen_arealoss_perc_both$max_loss <- abs(scen_arealoss_perc_both$mangrove_area1996 - scen_arealoss_perc_both$mangrove_area2020)

                    
                    scen_arealoss_perc_both$mangrove_area_future_loss <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both)
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_upper_limit)
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <- scen_arealoss_perc_both$mangrove_area *(0.01*scen_arealoss_perc_both$arealoss_perc_cumulative_both_lower_limit)

                    scen_arealoss_perc_both$mangrove_area_future_loss <- rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit <- rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_upper_limit,scen_arealoss_perc_both$max_loss))
                    scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit <- rowMins(cbind(scen_arealoss_perc_both$mangrove_area_future_loss_lower_limit,scen_arealoss_perc_both$max_loss))
                    glimpse(scen_arealoss_perc_both)
                    scen_arealoss_perc_both <- scen_arealoss_perc_both %>% filter(!is.na(gridcell_id))
                    #write.csv(scen_arealoss_perc_both, file="Results\\Area\\scen_arealoss_perc_both.csv")
                    
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
                    write.csv(scen_arealoss_perc_both_countrycode, file="Results\\Area\\scen_arealoss_perc_both_countrycode_ssp2_cappedArea_cappedEffect.csv")
                    glimpse(scen_arealoss_perc_both_countrycode)
                    
                    # scen_arealoss_perc_both_countrycode <- scen_arealoss_perc_both %>% group_by(countrycode,year) %>% 
                    #                                         summarise(area= sum(mangrove_area_future,na.rm=TRUE), 
                    #                                         perc_area = mean(arealoss_perc_cumulative_both,na.rm=TRUE))
                    #glimpse(scen_arealoss_perc_both_countrycode)
                    
                    
                    #sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    #ggplot(agg_aloss_neg_both)+
                    # geom_bar(aes(x=year,y=100*mangrove_area_future_loss/sum2020 ,
                    #     #fill=factor(loss)),stat="identity")+
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Mangrove Area Loss (%)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    # scale_fill_manual(values = color_vector) +
                    # scale_color_manual(values = color_vector)+
                    # geom_hline(aes(yintercept=0),linetype="dashed")+
                    # geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future_loss/sum2020),color="indianred",size=1.5)+
                    # theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    
                    # ggsave("Figures/Draft/Projection_Loss_Gain_RCP85_percent.png",dpi=600)


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
                        write.csv(agg_aloss_neg_total_both, file="Results\\Area\\agg_aloss_neg_total_both_ssp2_cappedArea_cappedEffect.csv")


                    
                    ## New Figure (end)
                ## Both: Climate and Socioeconomic (end)


                ## Climate Change + GDP (start)
                
                    scen$arealoss_perc <- (scen$delta_sst_hot*scen$marg_ssth_area)+(scen$perc_inc_gdppc2*scen$marg_gdppc_area)

                    scen$arealoss_perc <- ifelse(is.na(scen$arealoss_perc), 0, scen$arealoss_perc)
                    scen_arealoss_perc <- scen %>%
                            arrange(gridcell_id, year) %>%
                            group_by(gridcell_id) %>%
                            mutate(arealoss_perc_cumulative = cumsum(arealoss_perc)) %>%
                            ungroup() 
                    

                    scen_arealoss_perc_2100 <- scen_arealoss_perc %>% filter(year==2100)

                    glimpse(scen_arealoss_perc)
                    #scen_arealoss_perc$mangrove_area_future_loss <- scen_arealoss_perc$mangrove_area *(0.01*scen_arealoss_perc$arealoss_perc_cumulative)
                    #scen_arealoss_perc$mangrove_area_future <- scen_arealoss_perc$mangrove_area *(1-0.01*scen_arealoss_perc$arealoss_perc_cumulative)
                    #scen_arealoss_perc$loss <- 1
                    #scen_arealoss_perc$loss[which(scen_arealoss_perc$mangrove_area_future_loss<0)] <- 0
                    
                    #scen_arealoss_perc$pos <- 1
                    #scen_arealoss_perc$pos[which(scen_arealoss_perc$mangrove_area_future_loss>0)] <- 0

                    mcn <-read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                    mcn_1996 <- mcn[which(mcn$year==1996),which(names(mcn) %in% c("mangrove_area","gridcell_id"))]
                    scen_arealoss_perc <- merge(scen_arealoss_perc,mcn_1996,by=c("gridcell_id"),all=T,suffixes=c("","1996"))
                    glimpse(scen_arealoss_perc)

                    #install.packages("matrixStats")
                    library(matrixStats)
                    #scen_arealoss_perc$mangrove_area_future_capped <- rowMins(cbind(scen_arealoss_perc$mangrove_area_future,scen_arealoss_perc$mangrove_area1996))
                    #hist(scen_arealoss_perc$mangrove_area_future_capped)
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
                    
                    # ggplot(agg_aloss_neg)+
                    # geom_bar(aes(x=year,y=mangrove_area_future_loss,
                    #     #fill=factor(loss)),stat="identity")+
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Mangrove Area Loss (km2)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    # scale_fill_manual(values = color_vector) +
                    # scale_color_manual(values = color_vector)+
                    # geom_hline(aes(yintercept=0),linetype="dashed")+
                    # geom_line(data=agg_aloss_neg_total,aes(x=year,y=mangrove_area_future_loss),color="indianred",size=1.5)+
                    # theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    # ggsave("Figures/Draft/Projection_Loss_Gain_SSP2.png",dpi=600)

                    
                    # sum2020 <- sum(mcn_2020$mangrove_area,na.rm=T)
                    # ggplot(agg_aloss_neg)+
                    # geom_bar(aes(x=year,y=100*mangrove_area_future_loss/sum2020 ,
                    #     #fill=factor(loss)),stat="identity")+
                    #     fill=factor(R5)),stat="identity")+
                    # xlab("Year")+ylab("Mangrove Area Loss (%)")+
                    # labs(fill=guide_legend("Region"))+
                    # #scale_fill_manual(values=c("#20719e","#be6635"),labels=c("Gain","Loss"))+
                    # scale_fill_manual(values = color_vector) +
                    # scale_color_manual(values = color_vector)+
                    # geom_hline(aes(yintercept=0),linetype="dashed")+
                    # geom_line(data=agg_aloss_neg_total,aes(x=year,y=100*mangrove_area_future_loss/sum2020),color="indianred",size=1.5)+
                    # theme_bw()+ guides(fill=guide_legend(reverse = TRUE))
                    # ggsave("Figures/Draft/Projection_Loss_SSP2RCP85.png",dpi=600)

                    #glimpse(scen_arealoss_perc)
                    #write.csv(scen_arealoss_perc,"Data/output/scen_arealoss_perc.csv")
                    #ggplot(scen_arealoss_perc,aes(x=log(pop_ssp2),y=mangrove_area_future_loss))+geom_point()


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
                            geom_line(data=summary_dfs_total_sum,
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
                    agg_aloss_neg_total_gaps$gaps_perha <- agg_aloss_neg_total_gaps$change_holesperha +weighted_means_h$holes_perha[weighted_means_h$year==2020]
                    glimpse(agg_aloss_neg_total_gaps)
                    glimpse(weighted_means_h)

                    gaps_and_year <- rbind(data.frame(year=weighted_means_h$year,gaps_perha = weighted_means_h$holes_perha),
                        data.frame(year=agg_aloss_neg_total_gaps$year,gaps_perha = agg_aloss_neg_total_gaps$gaps_perha))
                    gaps_and_year <- gaps_and_year %>%
                                    complete(year = full_seq(year, 1)) %>%
                                    mutate(gaps_perha = ifelse(is.na(gaps_perha), 
                                                                approx(year, gaps_perha, xout = year)$y, 
                                                                gaps_perha)) %>% as.data.frame()

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
                    
                    area_year_hist_future <- rbind(data.frame(year=summary_dfs_total_sum$year,area=cumsum(summary_dfs_total_sum$area_change)),
                                                    data.frame(year=agg_aloss_neg_total_f$year,area=agg_aloss_neg_total_f$area_change_wrt_1996))
                    area_year_hist_future <- area_year_hist_future[which(!is.na(area_year_hist_future$year)),]
                    area_year_hist_future <- area_year_hist_future %>%
                                    complete(year = full_seq(year, 1)) %>%
                                    mutate(area = ifelse(is.na(area), 
                                                                approx(year, area, xout = year)$y, 
                                                                area)) %>% as.data.frame()
                    
                    gaps_and_year_area <- merge(area_year_hist_future,gaps_and_year,by="year",all=T)
                    gaps_and_year_area$spacing <- 1.1- gaps_and_year_area$gaps_perha / max(gaps_and_year_area$gaps_perha)

                    ggplot(agg_aloss_neg_total_f)+ geom_line(aes(x=(year),y=area_change_wrt_1996,color=color),size=1,stat="identity")+
                        #ggplot(agg_aloss_neg_total_capped)+ geom_line(aes(x=(year),y=mangrove_area_future_loss_capped),stat="identity")+
                                xlab("Year")+ylab("Mangrove Area \nRelative to 1996 (km2)")+
                                labs(fill=guide_legend("Region"))+
                            geom_line(data=agg_aloss_neg_total_onlyCC,aes(x=year,y=area_change_wrt_1996,color=color),size=1,linetype="dashed")+
                            geom_line(data=summary_dfs_total_sum[which(summary_dfs_total_sum$year %in% y_i),],
                                aes(x=year,y=cumsum(area_change),color=color),size=1)+
                            geom_col_pattern(data=gaps_and_year_area,aes(x = year, y = area,
                                #pattern_density=(gaps_perha),
                                pattern_spacing=spacing),
                                pattern = "circle",
                                pattern_fill = "black",fill=NA, color = NA,pattern_density=0.05) +
                                #scale_x_break(c(1998.8,2006))+ 
                                #scale_x_break(c(2010.5,2015))+ 
                                #guides(fill=guide_legend(reverse = TRUE))+ 
                            geom_hline(aes(yintercept=0),linetype="dashed")+
                                scale_color_manual(values = c("Historical"="gray30","Climate + Socioeconomic" = "darkblue","Climate"="indianred")) +
                                guides(pattern_spacing=guide_legend(title="Non-vegetated \nGaps per ha"),fill=guide_legend(reverse = FALSE),color=guide_legend(title="Forcings"),pattern_density=guide_legend(title="Non-vegetated \nGaps per ha"))+
                                theme_bw()+ #theme(legend.position="bottom")+
                                scale_pattern_spacing_continuous(breaks = c(0.11,0.3,1),
                                         labels = c("100","50", "10"))+
                                scale_x_continuous(breaks = c(1996,2008,2020,2050,2100))+xlim(1996,2100)
                    ggsave("Figures/Draft/Projection_Holes_Area_spacing.png",dpi=600)


                    
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

