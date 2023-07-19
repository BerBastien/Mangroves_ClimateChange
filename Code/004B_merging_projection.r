## Make Projections


## Read Data (start)
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    ssps <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\gdp_pop_ssp.csv")
    sst_bin_p <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_bins_projection85.csv")
    sst_hot_p <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_hot_projection85.csv")
    temp_85 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\temp_85.csv")
    preci_85 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\preci_85.csv")
    sst_p <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_projection85.csv")
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")

    glimpse(ssps)
    glimpse(sst_p)
    glimpse(preci_85)
    names(sst_p)[2] <- "gridcell_id"
    names(sst_hot_p)[2] <- "gridcell_id"
    names(sst_bin_p)[2] <- "gridcell_id"

    ## Interpoalte SSPs
        # load the library
        library(tidyverse)

        # calculate compound annual growth rate (CAGR)
        # calculate compound annual growth rate (CAGR)
            
            glimpse(mcn)
            glimpse(ssps)
    

            comp_ssps <- data.frame(gdp_2020 = mcn$Sum_GDP_50km[which(mcn$year==2020)],
            gdp_2025 = ssps$Sum_GDP_SSP2[which(ssps$year==2025)]
            )

            glimpse(comp_ssps)

            ggplot(comp_ssps)+geom_point(aes(x=gdp_2020,y=gdp_2025))            
            
            ssp0 <- data.frame(Year=2020,ID=mcn$gridcell_id[which(mcn$year==2020)],
            Sum_GDP_SSP2=mcn$Sum_GDP_50km[which(mcn$year==2020)],Sum_GDP_SSP5=mcn$Sum_GDP_50km[which(mcn$year==2020)],
            Sum_pop_SSP2=mcn$Population_Count_50km[which(mcn$year==2020)],Sum_pop_SSP5=mcn$Population_Count_50km[which(mcn$year==2020)],
            Longitude = ssps$Longitude[which(ssps$Year==2025)], Latitude = ssps$Latitude[which(ssps$Year==2025)])

            ssps <- rbind(ssp0,ssps)

            which(ssps$Sum_pop_SSP2[ssps$year==2020]==0 & ssps$Sum_pop_SSP2[ssps$year==2025]>0)
            which(ssps$Sum_pop_SSP2[ssps$year==2020]>0 & ssps$Sum_pop_SSP2[ssps$year==2025]==0)


            names(ssps)[1] <- "year"
            names(ssps)[2] <- "gridcell_id"
            
            ssps <- ssps %>%
            arrange(gridcell_id, year) %>%
            group_by(gridcell_id) %>%
            mutate(
                CAGR_GDP_SSP2 = (lead(Sum_GDP_SSP2) / Sum_GDP_SSP2) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP5 = (lead(Sum_GDP_SSP5) / Sum_GDP_SSP5) ^ (1 / (lead(year) - year)) - 1,
                CAGR_pop_SSP2 = (lead(Sum_pop_SSP2) / Sum_pop_SSP2) ^ (1 / (lead(year) - year)) - 1,
                CAGR_pop_SSP5 = (lead(Sum_pop_SSP5) / Sum_pop_SSP5) ^ (1 / (lead(year) - year)) - 1
            ) 

            # fill in NA growth rates with previous value
          

            # use the growth rate to interpolate
            ssps_interpolate <- ssps %>%
            group_by(gridcell_id) %>%
            complete(year = seq(min(year), max(year), by = 1)) %>%
            fill(CAGR_GDP_SSP2, CAGR_GDP_SSP5, CAGR_pop_SSP2, CAGR_pop_SSP5, Latitude, Longitude, .direction = "downup") %>%
            mutate(
                Sum_GDP_SSP2 = if_else(is.na(Sum_GDP_SSP2), lag(Sum_GDP_SSP2) * (1 + CAGR_GDP_SSP2), Sum_GDP_SSP2),
                Sum_GDP_SSP5 = if_else(is.na(Sum_GDP_SSP5), lag(Sum_GDP_SSP5) * (1 + CAGR_GDP_SSP5), Sum_GDP_SSP5),
                Sum_pop_SSP2 = if_else(is.na(Sum_pop_SSP2), lag(Sum_pop_SSP2) * (1 + CAGR_pop_SSP2), Sum_pop_SSP2),
                Sum_pop_SSP5 = if_else(is.na(Sum_pop_SSP5), lag(Sum_pop_SSP5) * (1 + CAGR_pop_SSP5), Sum_pop_SSP5)
            )%>%
            mutate(
                Sum_GDP_SSP2 = if_else(is.na(Sum_GDP_SSP2), lag(Sum_GDP_SSP2) * (1 + CAGR_GDP_SSP2), Sum_GDP_SSP2),
                Sum_GDP_SSP5 = if_else(is.na(Sum_GDP_SSP5), lag(Sum_GDP_SSP5) * (1 + CAGR_GDP_SSP5), Sum_GDP_SSP5),
                Sum_pop_SSP2 = if_else(is.na(Sum_pop_SSP2), lag(Sum_pop_SSP2) * (1 + CAGR_pop_SSP2), Sum_pop_SSP2),
                Sum_pop_SSP5 = if_else(is.na(Sum_pop_SSP5), lag(Sum_pop_SSP5) * (1 + CAGR_pop_SSP5), Sum_pop_SSP5)
            )%>%
            mutate(
                Sum_GDP_SSP2 = if_else(is.na(Sum_GDP_SSP2), lag(Sum_GDP_SSP2) * (1 + CAGR_GDP_SSP2), Sum_GDP_SSP2),
                Sum_GDP_SSP5 = if_else(is.na(Sum_GDP_SSP5), lag(Sum_GDP_SSP5) * (1 + CAGR_GDP_SSP5), Sum_GDP_SSP5),
                Sum_pop_SSP2 = if_else(is.na(Sum_pop_SSP2), lag(Sum_pop_SSP2) * (1 + CAGR_pop_SSP2), Sum_pop_SSP2),
                Sum_pop_SSP5 = if_else(is.na(Sum_pop_SSP5), lag(Sum_pop_SSP5) * (1 + CAGR_pop_SSP5), Sum_pop_SSP5)
            )%>%
            mutate(
                Sum_GDP_SSP2 = if_else(is.na(Sum_GDP_SSP2), lag(Sum_GDP_SSP2) * (1 + CAGR_GDP_SSP2), Sum_GDP_SSP2),
                Sum_GDP_SSP5 = if_else(is.na(Sum_GDP_SSP5), lag(Sum_GDP_SSP5) * (1 + CAGR_GDP_SSP5), Sum_GDP_SSP5),
                Sum_pop_SSP2 = if_else(is.na(Sum_pop_SSP2), lag(Sum_pop_SSP2) * (1 + CAGR_pop_SSP2), Sum_pop_SSP2),
                Sum_pop_SSP5 = if_else(is.na(Sum_pop_SSP5), lag(Sum_pop_SSP5) * (1 + CAGR_pop_SSP5), Sum_pop_SSP5)
            )

            print(ssps_interpolate)

            as.data.frame(ssps_interpolate[which(ssps_interpolate$gridcell_id==3),])
            
            ggplot(ssps_interpolate)+geom_point(aes(x=year,y=log(Sum_GDP_SSP2),group=gridcell_id),alpha=0.5)+
            xlim(c(2020,2030))

            ggplot(ssps)+geom_line(aes(x=year,y=log(Sum_pop_SSP2),group=gridcell_id),alpha=0.5)+
            xlim(c(2020,2030))


        
            glimpse(ssps)
            ssps <- as.data.frame(ssps)
            
    ## Interpolate SSPS

    ## Merge with Climate Data (start)
        names(sst_p)[-c(1:3)] <- paste0(names(sst_p)[-c(1:3)],"_85")
        names(sst_bin_p)[-c(1:3)] <- paste0(names(sst_bin_p)[-c(1:3)],"_85")
        names(sst_hot_p)[-c(1:3)] <- paste0(names(sst_hot_p)[-c(1:3)],"_85")
        scen <- merge(ssps_interpolate,sst_p,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,sst_hot_p,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,sst_bin_p,by=c("gridcell_id","year"),all=T)

        glimpse(scen)

        ggplot(scen)+geom_point(aes(x=sst_85,y=log(Sum_GDP_SSP2),group=gridcell_id))

        scen <- scen[,-which(names(scen) %in% c("X.x","X.y","X","NA._85","CAGR_GDP_SSP2" ,"CAGR_GDP_SSP5" ,"CAGR_pop_SSP2" ,"CAGR_pop_SSP5"))]
        names(scen)[3:6] <- c("gdp_ssp2","gdp_ssp5","pop_ssp2","pop_ssp5")
        glimpse(ssps_interpolate)
        glimpse(scen)

        
        scen <- merge(scen,temp_85,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,preci_85,by=c("gridcell_id","year"),all=T)
        scen <- scen[,-which(names(scen) %in% c("X.x","X.y","X","NA._85","CAGR_GDP_SSP2" ,"CAGR_GDP_SSP5" ,"CAGR_pop_SSP2" ,"CAGR_pop_SSP5"))]
        names(scen)[c(56,57)] <- c("temp_85"  ,"preci_85")

        
        write.csv(scen,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")

    ## Merge with Climate Data (end)

        scen <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
        glimpse(scen)
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

                    scen$GDPpc_c_ssp5<-(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5)
                    scen$logGDPpc_c_ssp5<-log(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5)
        
        #Perc_inc 
        scen$gdppc2 <- scen$gdp_ssp2/scen$pop_ssp2
        scen$gdppc5 <- scen$gdp_ssp5/scen$pop_ssp5
        scen$days31_85 <- scen$b31_32C_85 + scen$b32_33C_85+scen$b33_34C_85 + 
                        scen$b34_35C_85+ scen$b35_36C_85+ scen$b36_37C_85+ 
                        scen$b37_38C_85 + scen$b39_40C_85+ scen$b40_41C_85+ 
                        scen$b41_42C_85+ scen$b42_43C_85 + scen$b43_44C_85+ scen$b44_45C_85
        scen$days32_85 <- scen$b32_33C_85+scen$b33_34C_85 + 
                        scen$b34_35C_85+ scen$b35_36C_85+ scen$b36_37C_85+ 
                        scen$b37_38C_85 + scen$b39_40C_85+ scen$b40_41C_85+ 
                        scen$b41_42C_85+ scen$b42_43C_85 + scen$b43_44C_85+ scen$b44_45C_85
        scen <- scen %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(delta_sst_hot = sst_hot_85 - lag(sst_hot_85),
                            perc_inc_gdp5 = (gdp_ssp5 /lag(gdp_ssp5)-1)*100,
                            perc_inc_gdp2 = (gdp_ssp2 /lag(gdp_ssp2)-1)*100,
                            perc_inc_gdppc5 = (gdppc5 /lag(gdppc5)-1)*100,
                            perc_inc_gdppc2 = (gdppc2 /lag(gdppc2)-1)*100,
                            perc_inc_pop2 = (pop_ssp2 /lag(pop_ssp2)-1)*100,
                            perc_inc_pop5 = (pop_ssp5 /lag(pop_ssp5)-1)*100,
                            temp_inc_85 = (temp_85 -lag(temp_85)),
                            preci_inc_85 = (preci_85 -lag(preci_85)),
                            days32_inc_85 = (days32_85 -lag(days32_85)),
                            days31_inc_85 = (days31_85 -lag(days31_85)),
                            perc_inc_GDPpc_c = (GDPpc_c_ssp5/lag(GDPpc_c_ssp5)-1)*100
                             )%>% ungroup() %>%  arrange(gridcell_id,year)

        
        
        
        sal <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_sal_85.csv")
        mcn2020 <- mcn[which(mcn$year==2020),which(names(mcn) %in% c("gridcell_id","Mean_Salinity"))]
                names(mcn2020)[2] <- "sal2020"
                sal <- merge(sal,mcn2020,by="gridcell_id",all=T)
                sal$salinity <- sal$sal2020 * (1+sal$Salinity_perc/100)
        sal <- sal %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(
                            perc_inc_sal = (salinity /lag(salinity)-1)*100
                             )%>% ungroup() %>%  arrange(gridcell_id,year)
        
                scen <- merge(scen,sal,by=c("gridcell_id","year"),all=T)
        glimpse(scen)
        
        write.csv(scen,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all.csv")
        
## Read Data (end)

## Plot before and after (start)
    ## Distribution Plots 
    ## Distribution Plots
## Plot before and after (end)


## Read Coefficients

