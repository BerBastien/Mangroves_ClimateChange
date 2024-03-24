## Make Projections


## Read Data (start)
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    ssps <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\gdp_pop_ssp.csv")
    ssps_pop <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\Population_all_SSPs_2020_2100.csv")
    ssps_gdp <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\GDP_all_SSPs_2020_2100.csv")
    sst_bin_p <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_bins_projection85.csv")
    sst_hot_p <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_hot_projection85.csv")
    sst_hot_p_70 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_hot_projection70.csv")
    temp_85 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\temp_85.csv")
    preci_85 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\preci_85.csv")
    sst_p <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\sst_projection85.csv")
    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv") %>% filter(!is.na(gridcell_id))
    glimpse(mcn)
    glimpse(ssps)
    glimpse(ssps_pop)
    glimpse(ssps_gdp)
    ssps <- merge(ssps_gdp,ssps_pop %>% dplyr::select(-c("Longitude","Latitude")),by=c("Year","ID"))
    glimpse(ssps)
    glimpse(sst_p)
    glimpse(preci_85)
    names(ssps)[1] <- "year"
    names(ssps)[2] <- "gridcell_id"
    names(sst_p)[2] <- "gridcell_id"
    names(sst_hot_p)[2] <- "gridcell_id"
    names(sst_hot_p_70)[2] <- "gridcell_id"
    names(sst_bin_p)[2] <- "gridcell_id"

    ## Interpoalte SSPs
        # load the library
        library(tidyverse)

        # calculate compound annual growth rate (CAGR)
        # calculate compound annual growth rate (CAGR)
            
            glimpse(mcn)
            glimpse(ssps)
    

            comp_ssps <- data.frame(gdp_2020 = mcn$Population_Count[which(mcn$year==2020)],
            gdp_2025 = ssps$POP_SSP2[which(ssps$year==2020)]
            )

            glimpse(comp_ssps)

            ggplot(comp_ssps)+geom_point(aes(x=gdp_2020,y=gdp_2025))            
            
            ssp0 <- data.frame(year=2020,gridcell_id=mcn$gridcell_id[which(mcn$year==2020)],
            GDP_SSP1=mcn$Sum_GDP[which(mcn$year==2020)],
GDP_SSP1_50km  = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP1_100km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP1_500km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP2=mcn$Sum_GDP[which(mcn$year==2020)],
GDP_SSP2_50km  = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP2_100km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP2_500km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP3=mcn$Sum_GDP[which(mcn$year==2020)],
GDP_SSP3_50km  = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP3_100km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP3_500km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP4=mcn$Sum_GDP[which(mcn$year==2020)],
GDP_SSP4_50km  = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP4_100km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP4_500km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP5=mcn$Sum_GDP[which(mcn$year==2020)],
GDP_SSP5_50km  = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP5_100km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
GDP_SSP5_500km = mcn$Sum_GDP_50km[which(mcn$year==2020)],
Longitude       = ssps$Longitude[which(ssps$year==2025)],
Latitude        = ssps$Latitude[which(ssps$year==2025)],
POP_SSP1        = mcn$Population_Count[which(mcn$year==2020)],
POP_SSP1_50km   = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP1_100km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP1_500km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP2= mcn$Population_Count[which(mcn$year==2020)],
POP_SSP2_50km   = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP2_100km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP2_500km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP3= mcn$Population_Count[which(mcn$year==2020)],
POP_SSP3_50km   = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP3_100km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP3_500km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP4= mcn$Population_Count[which(mcn$year==2020)],
POP_SSP4_50km   = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP4_100km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP4_500km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP5= mcn$Population_Count[which(mcn$year==2020)],
POP_SSP5_50km   = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP5_100km  = mcn$Population_Count_50km[which(mcn$year==2020)],
POP_SSP5_500km  = mcn$Population_Count_50km[which(mcn$year==2020)])

            ssps <- rbind(ssp0,ssps)
            glimpse(ssps)

            which(ssps$Sum_pop_SSP2[ssps$year==2020]==0 & ssps$Sum_pop_SSP2[ssps$year==2025]>0)
            which(ssps$Sum_pop_SSP2[ssps$year==2020]>0 & ssps$Sum_pop_SSP2[ssps$year==2025]==0)


            names(ssps)[1] <- "year"
            names(ssps)[2] <- "gridcell_id"
            
            ssps <- ssps %>%
            arrange(gridcell_id, year) %>%
            group_by(gridcell_id) %>%
            mutate(
                CAGR_GDP_SSP1 = (lead(GDP_SSP1) / GDP_SSP1) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP1_50km = (lead(GDP_SSP1_50km) / GDP_SSP1_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP1_100km = (lead(GDP_SSP1_100km) / GDP_SSP1_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP1_500km = (lead(GDP_SSP1_500km) / GDP_SSP1_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP2 = (lead(GDP_SSP2) / GDP_SSP2) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP2_50km = (lead(GDP_SSP2_50km) / GDP_SSP2_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP2_100km = (lead(GDP_SSP2_100km) / GDP_SSP2_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP2_500km = (lead(GDP_SSP2_500km) / GDP_SSP2_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP3 = (lead(GDP_SSP3) / GDP_SSP3) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP3_50km = (lead(GDP_SSP3_50km) / GDP_SSP3_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP3_100km = (lead(GDP_SSP3_100km) / GDP_SSP3_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP3_500km = (lead(GDP_SSP3_500km) / GDP_SSP3_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP4 = (lead(GDP_SSP4) / GDP_SSP4) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP4_50km = (lead(GDP_SSP4_50km) / GDP_SSP4_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP4_100km = (lead(GDP_SSP4_100km) / GDP_SSP4_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP4_500km = (lead(GDP_SSP4_500km) / GDP_SSP4_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP5 = (lead(GDP_SSP5) / GDP_SSP5) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP5_50km = (lead(GDP_SSP5_50km) / GDP_SSP5_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP5_100km = (lead(GDP_SSP5_100km) / GDP_SSP5_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_GDP_SSP5_500km = (lead(GDP_SSP5_500km) / GDP_SSP5_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP1 = (lead(POP_SSP1) / POP_SSP1) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP1_50km = (lead(POP_SSP1_50km) / POP_SSP1_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP1_100km = (lead(POP_SSP1_100km) / POP_SSP1_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP1_500km = (lead(POP_SSP1_500km) / POP_SSP1_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP2 = (lead(POP_SSP2) / POP_SSP2) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP2_50km = (lead(POP_SSP2_50km) / POP_SSP2_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP2_100km = (lead(POP_SSP2_100km) / POP_SSP2_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP2_500km = (lead(POP_SSP2_500km) / POP_SSP2_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP3 = (lead(POP_SSP3) / POP_SSP3) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP3_50km = (lead(POP_SSP3_50km) / POP_SSP3_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP3_100km = (lead(POP_SSP3_100km) / POP_SSP3_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP3_500km = (lead(POP_SSP3_500km) / POP_SSP3_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP4 = (lead(POP_SSP4) / POP_SSP4) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP4_50km = (lead(POP_SSP4_50km) / POP_SSP4_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP4_100km = (lead(POP_SSP4_100km) / POP_SSP4_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP4_500km = (lead(POP_SSP4_500km) / POP_SSP4_500km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP5 = (lead(POP_SSP5) / POP_SSP5) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP5_50km = (lead(POP_SSP5_50km) / POP_SSP5_50km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP5_100km = (lead(POP_SSP5_100km) / POP_SSP5_100km) ^ (1 / (lead(year) - year)) - 1,
                CAGR_POP_SSP5_500km = (lead(POP_SSP5_500km) / POP_SSP5_500km) ^ (1 / (lead(year) - year)) - 1
            ) 

            # fill in NA growth rates with previous value
          

            # use the growth rate to interpolate
            ssps_interpolate <- ssps %>%
            group_by(gridcell_id) %>%
            complete(year = seq(min(year), max(year), by = 1)) %>%
            fill(CAGR_GDP_SSP1, CAGR_GDP_SSP1_50km,
                CAGR_GDP_SSP1_100km,CAGR_GDP_SSP1_500km, CAGR_GDP_SSP2,
                CAGR_GDP_SSP2_50km,CAGR_GDP_SSP2_100km, CAGR_GDP_SSP2_500km,CAGR_GDP_SSP3,
                CAGR_GDP_SSP3_50km, CAGR_GDP_SSP3_100km,CAGR_GDP_SSP3_500km,CAGR_GDP_SSP4, 
                CAGR_GDP_SSP4_50km,CAGR_GDP_SSP4_100km,CAGR_GDP_SSP4_500km, CAGR_GDP_SSP5,
                CAGR_GDP_SSP5_50km,CAGR_GDP_SSP5_100km, CAGR_GDP_SSP5_500km,CAGR_POP_SSP1,
                CAGR_POP_SSP1_50km, CAGR_POP_SSP1_100km,CAGR_POP_SSP1_500km,CAGR_POP_SSP2, 
                CAGR_POP_SSP2_50km,CAGR_POP_SSP2_100km,CAGR_POP_SSP2_500km, CAGR_POP_SSP3,
                CAGR_POP_SSP3_50km,CAGR_POP_SSP3_100km, CAGR_POP_SSP3_500km,CAGR_POP_SSP4,
                CAGR_POP_SSP4_50km, CAGR_POP_SSP4_100km,CAGR_POP_SSP4_500km,CAGR_POP_SSP5, 
                CAGR_POP_SSP5_50km,CAGR_POP_SSP5_100km,CAGR_POP_SSP5_500km,
                Latitude, Longitude, .direction = "downup") %>%
            mutate(
                GDP_SSP1 = if_else(is.na(GDP_SSP1), lag(GDP_SSP1) * (1 + CAGR_GDP_SSP1), GDP_SSP1),
                GDP_SSP1_50km = if_else(is.na(GDP_SSP1_50km), lag(GDP_SSP1_50km) * (1 + CAGR_GDP_SSP1_50km), GDP_SSP1_50km),
                GDP_SSP1_100km = if_else(is.na(GDP_SSP1_100km), lag(GDP_SSP1_100km) * (1 + CAGR_GDP_SSP1_100km), GDP_SSP1_100km),
                GDP_SSP1_500km = if_else(is.na(GDP_SSP1_500km), lag(GDP_SSP1_500km) * (1 + CAGR_GDP_SSP1_500km), GDP_SSP1_500km),
                GDP_SSP2 = if_else(is.na(GDP_SSP2), lag(GDP_SSP2) * (1 + CAGR_GDP_SSP2), GDP_SSP2),
                GDP_SSP2_50km = if_else(is.na(GDP_SSP2_50km), lag(GDP_SSP2_50km) * (1 + CAGR_GDP_SSP2_50km), GDP_SSP2_50km),
                GDP_SSP2_100km = if_else(is.na(GDP_SSP2_100km), lag(GDP_SSP2_100km) * (1 + CAGR_GDP_SSP2_100km), GDP_SSP2_100km),
                GDP_SSP2_500km = if_else(is.na(GDP_SSP2_500km), lag(GDP_SSP2_500km) * (1 + CAGR_GDP_SSP2_500km), GDP_SSP2_500km),
                GDP_SSP3 = if_else(is.na(GDP_SSP3), lag(GDP_SSP3) * (1 + CAGR_GDP_SSP3), GDP_SSP3),
                GDP_SSP3_50km = if_else(is.na(GDP_SSP3_50km), lag(GDP_SSP3_50km) * (1 + CAGR_GDP_SSP3_50km), GDP_SSP3_50km),
                GDP_SSP3_100km = if_else(is.na(GDP_SSP3_100km), lag(GDP_SSP3_100km) * (1 + CAGR_GDP_SSP3_100km), GDP_SSP3_100km),
                GDP_SSP3_500km = if_else(is.na(GDP_SSP3_500km), lag(GDP_SSP3_500km) * (1 + CAGR_GDP_SSP3_500km), GDP_SSP3_500km),
                GDP_SSP4 = if_else(is.na(GDP_SSP4), lag(GDP_SSP4) * (1 + CAGR_GDP_SSP4), GDP_SSP4),
                GDP_SSP4_50km = if_else(is.na(GDP_SSP4_50km), lag(GDP_SSP4_50km) * (1 + CAGR_GDP_SSP4_50km), GDP_SSP4_50km),
                GDP_SSP4_100km = if_else(is.na(GDP_SSP4_100km), lag(GDP_SSP4_100km) * (1 + CAGR_GDP_SSP4_100km), GDP_SSP4_100km),
                GDP_SSP4_500km = if_else(is.na(GDP_SSP4_500km), lag(GDP_SSP4_500km) * (1 + CAGR_GDP_SSP4_500km), GDP_SSP4_500km),
                GDP_SSP5 = if_else(is.na(GDP_SSP5), lag(GDP_SSP5) * (1 + CAGR_GDP_SSP5), GDP_SSP5),
                GDP_SSP5_50km = if_else(is.na(GDP_SSP5_50km), lag(GDP_SSP5_50km) * (1 + CAGR_GDP_SSP5_50km), GDP_SSP5_50km),
                GDP_SSP5_100km = if_else(is.na(GDP_SSP5_100km), lag(GDP_SSP5_100km) * (1 + CAGR_GDP_SSP5_100km), GDP_SSP5_100km),
                GDP_SSP5_500km = if_else(is.na(GDP_SSP5_500km), lag(GDP_SSP5_500km) * (1 + CAGR_GDP_SSP5_500km), GDP_SSP5_500km),
                POP_SSP1 = if_else(is.na(POP_SSP1), lag(POP_SSP1) * (1 + CAGR_POP_SSP1), POP_SSP1),
                POP_SSP1_50km = if_else(is.na(POP_SSP1_50km), lag(POP_SSP1_50km) * (1 + CAGR_POP_SSP1_50km), POP_SSP1_50km),
                POP_SSP1_100km = if_else(is.na(POP_SSP1_100km), lag(POP_SSP1_100km) * (1 + CAGR_POP_SSP1_100km), POP_SSP1_100km),
                POP_SSP1_500km = if_else(is.na(POP_SSP1_500km), lag(POP_SSP1_500km) * (1 + CAGR_POP_SSP1_500km), POP_SSP1_500km),
                POP_SSP2 = if_else(is.na(POP_SSP2), lag(POP_SSP2) * (1 + CAGR_POP_SSP2), POP_SSP2),
                POP_SSP2_50km = if_else(is.na(POP_SSP2_50km), lag(POP_SSP2_50km) * (1 + CAGR_POP_SSP2_50km), POP_SSP2_50km),
                POP_SSP2_100km = if_else(is.na(POP_SSP2_100km), lag(POP_SSP2_100km) * (1 + CAGR_POP_SSP2_100km), POP_SSP2_100km),
                POP_SSP2_500km = if_else(is.na(POP_SSP2_500km), lag(POP_SSP2_500km) * (1 + CAGR_POP_SSP2_500km), POP_SSP2_500km),
                POP_SSP3 = if_else(is.na(POP_SSP3), lag(POP_SSP3) * (1 + CAGR_POP_SSP3), POP_SSP3),
                POP_SSP3_50km = if_else(is.na(POP_SSP3_50km), lag(POP_SSP3_50km) * (1 + CAGR_POP_SSP3_50km), POP_SSP3_50km),
                POP_SSP3_100km = if_else(is.na(POP_SSP3_100km), lag(POP_SSP3_100km) * (1 + CAGR_POP_SSP3_100km), POP_SSP3_100km),
                POP_SSP3_500km = if_else(is.na(POP_SSP3_500km), lag(POP_SSP3_500km) * (1 + CAGR_POP_SSP3_500km), POP_SSP3_500km),
                POP_SSP4 = if_else(is.na(POP_SSP4), lag(POP_SSP4) * (1 + CAGR_POP_SSP4), POP_SSP4),
                POP_SSP4_50km = if_else(is.na(POP_SSP4_50km), lag(POP_SSP4_50km) * (1 + CAGR_POP_SSP4_50km), POP_SSP4_50km),
                POP_SSP4_100km = if_else(is.na(POP_SSP4_100km), lag(POP_SSP4_100km) * (1 + CAGR_POP_SSP4_100km), POP_SSP4_100km),
                POP_SSP4_500km = if_else(is.na(POP_SSP4_500km), lag(POP_SSP4_500km) * (1 + CAGR_POP_SSP4_500km), POP_SSP4_500km),
                POP_SSP5 = if_else(is.na(POP_SSP5), lag(POP_SSP5) * (1 + CAGR_POP_SSP5), POP_SSP5),
                POP_SSP5_50km = if_else(is.na(POP_SSP5_50km), lag(POP_SSP5_50km) * (1 + CAGR_POP_SSP5_50km), POP_SSP5_50km),
                POP_SSP5_100km = if_else(is.na(POP_SSP5_100km), lag(POP_SSP5_100km) * (1 + CAGR_POP_SSP5_100km), POP_SSP5_100km),
                POP_SSP5_500km = if_else(is.na(POP_SSP5_500km), lag(POP_SSP5_500km) * (1 + CAGR_POP_SSP5_500km), POP_SSP5_500km)
            )%>%
            mutate(
                
                GDP_SSP1 = if_else(is.na(GDP_SSP1), lag(GDP_SSP1) * (1 + CAGR_GDP_SSP1), GDP_SSP1),
                GDP_SSP1_50km = if_else(is.na(GDP_SSP1_50km), lag(GDP_SSP1_50km) * (1 + CAGR_GDP_SSP1_50km), GDP_SSP1_50km),
                GDP_SSP1_100km = if_else(is.na(GDP_SSP1_100km), lag(GDP_SSP1_100km) * (1 + CAGR_GDP_SSP1_100km), GDP_SSP1_100km),
                GDP_SSP1_500km = if_else(is.na(GDP_SSP1_500km), lag(GDP_SSP1_500km) * (1 + CAGR_GDP_SSP1_500km), GDP_SSP1_500km),
                GDP_SSP2 = if_else(is.na(GDP_SSP2), lag(GDP_SSP2) * (1 + CAGR_GDP_SSP2), GDP_SSP2),
                GDP_SSP2_50km = if_else(is.na(GDP_SSP2_50km), lag(GDP_SSP2_50km) * (1 + CAGR_GDP_SSP2_50km), GDP_SSP2_50km),
                GDP_SSP2_100km = if_else(is.na(GDP_SSP2_100km), lag(GDP_SSP2_100km) * (1 + CAGR_GDP_SSP2_100km), GDP_SSP2_100km),
                GDP_SSP2_500km = if_else(is.na(GDP_SSP2_500km), lag(GDP_SSP2_500km) * (1 + CAGR_GDP_SSP2_500km), GDP_SSP2_500km),
                GDP_SSP3 = if_else(is.na(GDP_SSP3), lag(GDP_SSP3) * (1 + CAGR_GDP_SSP3), GDP_SSP3),
                GDP_SSP3_50km = if_else(is.na(GDP_SSP3_50km), lag(GDP_SSP3_50km) * (1 + CAGR_GDP_SSP3_50km), GDP_SSP3_50km),
                GDP_SSP3_100km = if_else(is.na(GDP_SSP3_100km), lag(GDP_SSP3_100km) * (1 + CAGR_GDP_SSP3_100km), GDP_SSP3_100km),
                GDP_SSP3_500km = if_else(is.na(GDP_SSP3_500km), lag(GDP_SSP3_500km) * (1 + CAGR_GDP_SSP3_500km), GDP_SSP3_500km),
                GDP_SSP4 = if_else(is.na(GDP_SSP4), lag(GDP_SSP4) * (1 + CAGR_GDP_SSP4), GDP_SSP4),
                GDP_SSP4_50km = if_else(is.na(GDP_SSP4_50km), lag(GDP_SSP4_50km) * (1 + CAGR_GDP_SSP4_50km), GDP_SSP4_50km),
                GDP_SSP4_100km = if_else(is.na(GDP_SSP4_100km), lag(GDP_SSP4_100km) * (1 + CAGR_GDP_SSP4_100km), GDP_SSP4_100km),
                GDP_SSP4_500km = if_else(is.na(GDP_SSP4_500km), lag(GDP_SSP4_500km) * (1 + CAGR_GDP_SSP4_500km), GDP_SSP4_500km),
                GDP_SSP5 = if_else(is.na(GDP_SSP5), lag(GDP_SSP5) * (1 + CAGR_GDP_SSP5), GDP_SSP5),
                GDP_SSP5_50km = if_else(is.na(GDP_SSP5_50km), lag(GDP_SSP5_50km) * (1 + CAGR_GDP_SSP5_50km), GDP_SSP5_50km),
                GDP_SSP5_100km = if_else(is.na(GDP_SSP5_100km), lag(GDP_SSP5_100km) * (1 + CAGR_GDP_SSP5_100km), GDP_SSP5_100km),
                GDP_SSP5_500km = if_else(is.na(GDP_SSP5_500km), lag(GDP_SSP5_500km) * (1 + CAGR_GDP_SSP5_500km), GDP_SSP5_500km),
                POP_SSP1 = if_else(is.na(POP_SSP1), lag(POP_SSP1) * (1 + CAGR_POP_SSP1), POP_SSP1),
                POP_SSP1_50km = if_else(is.na(POP_SSP1_50km), lag(POP_SSP1_50km) * (1 + CAGR_POP_SSP1_50km), POP_SSP1_50km),
                POP_SSP1_100km = if_else(is.na(POP_SSP1_100km), lag(POP_SSP1_100km) * (1 + CAGR_POP_SSP1_100km), POP_SSP1_100km),
                POP_SSP1_500km = if_else(is.na(POP_SSP1_500km), lag(POP_SSP1_500km) * (1 + CAGR_POP_SSP1_500km), POP_SSP1_500km),
                POP_SSP2 = if_else(is.na(POP_SSP2), lag(POP_SSP2) * (1 + CAGR_POP_SSP2), POP_SSP2),
                POP_SSP2_50km = if_else(is.na(POP_SSP2_50km), lag(POP_SSP2_50km) * (1 + CAGR_POP_SSP2_50km), POP_SSP2_50km),
                POP_SSP2_100km = if_else(is.na(POP_SSP2_100km), lag(POP_SSP2_100km) * (1 + CAGR_POP_SSP2_100km), POP_SSP2_100km),
                POP_SSP2_500km = if_else(is.na(POP_SSP2_500km), lag(POP_SSP2_500km) * (1 + CAGR_POP_SSP2_500km), POP_SSP2_500km),
                POP_SSP3 = if_else(is.na(POP_SSP3), lag(POP_SSP3) * (1 + CAGR_POP_SSP3), POP_SSP3),
                POP_SSP3_50km = if_else(is.na(POP_SSP3_50km), lag(POP_SSP3_50km) * (1 + CAGR_POP_SSP3_50km), POP_SSP3_50km),
                POP_SSP3_100km = if_else(is.na(POP_SSP3_100km), lag(POP_SSP3_100km) * (1 + CAGR_POP_SSP3_100km), POP_SSP3_100km),
                POP_SSP3_500km = if_else(is.na(POP_SSP3_500km), lag(POP_SSP3_500km) * (1 + CAGR_POP_SSP3_500km), POP_SSP3_500km),
                POP_SSP4 = if_else(is.na(POP_SSP4), lag(POP_SSP4) * (1 + CAGR_POP_SSP4), POP_SSP4),
                POP_SSP4_50km = if_else(is.na(POP_SSP4_50km), lag(POP_SSP4_50km) * (1 + CAGR_POP_SSP4_50km), POP_SSP4_50km),
                POP_SSP4_100km = if_else(is.na(POP_SSP4_100km), lag(POP_SSP4_100km) * (1 + CAGR_POP_SSP4_100km), POP_SSP4_100km),
                POP_SSP4_500km = if_else(is.na(POP_SSP4_500km), lag(POP_SSP4_500km) * (1 + CAGR_POP_SSP4_500km), POP_SSP4_500km),
                POP_SSP5 = if_else(is.na(POP_SSP5), lag(POP_SSP5) * (1 + CAGR_POP_SSP5), POP_SSP5),
                POP_SSP5_50km = if_else(is.na(POP_SSP5_50km), lag(POP_SSP5_50km) * (1 + CAGR_POP_SSP5_50km), POP_SSP5_50km),
                POP_SSP5_100km = if_else(is.na(POP_SSP5_100km), lag(POP_SSP5_100km) * (1 + CAGR_POP_SSP5_100km), POP_SSP5_100km),
                POP_SSP5_500km = if_else(is.na(POP_SSP5_500km), lag(POP_SSP5_500km) * (1 + CAGR_POP_SSP5_500km), POP_SSP5_500km)
            )%>%
            mutate(
                
                GDP_SSP1 = if_else(is.na(GDP_SSP1), lag(GDP_SSP1) * (1 + CAGR_GDP_SSP1), GDP_SSP1),
                GDP_SSP1_50km = if_else(is.na(GDP_SSP1_50km), lag(GDP_SSP1_50km) * (1 + CAGR_GDP_SSP1_50km), GDP_SSP1_50km),
                GDP_SSP1_100km = if_else(is.na(GDP_SSP1_100km), lag(GDP_SSP1_100km) * (1 + CAGR_GDP_SSP1_100km), GDP_SSP1_100km),
                GDP_SSP1_500km = if_else(is.na(GDP_SSP1_500km), lag(GDP_SSP1_500km) * (1 + CAGR_GDP_SSP1_500km), GDP_SSP1_500km),
                GDP_SSP2 = if_else(is.na(GDP_SSP2), lag(GDP_SSP2) * (1 + CAGR_GDP_SSP2), GDP_SSP2),
                GDP_SSP2_50km = if_else(is.na(GDP_SSP2_50km), lag(GDP_SSP2_50km) * (1 + CAGR_GDP_SSP2_50km), GDP_SSP2_50km),
                GDP_SSP2_100km = if_else(is.na(GDP_SSP2_100km), lag(GDP_SSP2_100km) * (1 + CAGR_GDP_SSP2_100km), GDP_SSP2_100km),
                GDP_SSP2_500km = if_else(is.na(GDP_SSP2_500km), lag(GDP_SSP2_500km) * (1 + CAGR_GDP_SSP2_500km), GDP_SSP2_500km),
                GDP_SSP3 = if_else(is.na(GDP_SSP3), lag(GDP_SSP3) * (1 + CAGR_GDP_SSP3), GDP_SSP3),
                GDP_SSP3_50km = if_else(is.na(GDP_SSP3_50km), lag(GDP_SSP3_50km) * (1 + CAGR_GDP_SSP3_50km), GDP_SSP3_50km),
                GDP_SSP3_100km = if_else(is.na(GDP_SSP3_100km), lag(GDP_SSP3_100km) * (1 + CAGR_GDP_SSP3_100km), GDP_SSP3_100km),
                GDP_SSP3_500km = if_else(is.na(GDP_SSP3_500km), lag(GDP_SSP3_500km) * (1 + CAGR_GDP_SSP3_500km), GDP_SSP3_500km),
                GDP_SSP4 = if_else(is.na(GDP_SSP4), lag(GDP_SSP4) * (1 + CAGR_GDP_SSP4), GDP_SSP4),
                GDP_SSP4_50km = if_else(is.na(GDP_SSP4_50km), lag(GDP_SSP4_50km) * (1 + CAGR_GDP_SSP4_50km), GDP_SSP4_50km),
                GDP_SSP4_100km = if_else(is.na(GDP_SSP4_100km), lag(GDP_SSP4_100km) * (1 + CAGR_GDP_SSP4_100km), GDP_SSP4_100km),
                GDP_SSP4_500km = if_else(is.na(GDP_SSP4_500km), lag(GDP_SSP4_500km) * (1 + CAGR_GDP_SSP4_500km), GDP_SSP4_500km),
                GDP_SSP5 = if_else(is.na(GDP_SSP5), lag(GDP_SSP5) * (1 + CAGR_GDP_SSP5), GDP_SSP5),
                GDP_SSP5_50km = if_else(is.na(GDP_SSP5_50km), lag(GDP_SSP5_50km) * (1 + CAGR_GDP_SSP5_50km), GDP_SSP5_50km),
                GDP_SSP5_100km = if_else(is.na(GDP_SSP5_100km), lag(GDP_SSP5_100km) * (1 + CAGR_GDP_SSP5_100km), GDP_SSP5_100km),
                GDP_SSP5_500km = if_else(is.na(GDP_SSP5_500km), lag(GDP_SSP5_500km) * (1 + CAGR_GDP_SSP5_500km), GDP_SSP5_500km),
                POP_SSP1 = if_else(is.na(POP_SSP1), lag(POP_SSP1) * (1 + CAGR_POP_SSP1), POP_SSP1),
                POP_SSP1_50km = if_else(is.na(POP_SSP1_50km), lag(POP_SSP1_50km) * (1 + CAGR_POP_SSP1_50km), POP_SSP1_50km),
                POP_SSP1_100km = if_else(is.na(POP_SSP1_100km), lag(POP_SSP1_100km) * (1 + CAGR_POP_SSP1_100km), POP_SSP1_100km),
                POP_SSP1_500km = if_else(is.na(POP_SSP1_500km), lag(POP_SSP1_500km) * (1 + CAGR_POP_SSP1_500km), POP_SSP1_500km),
                POP_SSP2 = if_else(is.na(POP_SSP2), lag(POP_SSP2) * (1 + CAGR_POP_SSP2), POP_SSP2),
                POP_SSP2_50km = if_else(is.na(POP_SSP2_50km), lag(POP_SSP2_50km) * (1 + CAGR_POP_SSP2_50km), POP_SSP2_50km),
                POP_SSP2_100km = if_else(is.na(POP_SSP2_100km), lag(POP_SSP2_100km) * (1 + CAGR_POP_SSP2_100km), POP_SSP2_100km),
                POP_SSP2_500km = if_else(is.na(POP_SSP2_500km), lag(POP_SSP2_500km) * (1 + CAGR_POP_SSP2_500km), POP_SSP2_500km),
                POP_SSP3 = if_else(is.na(POP_SSP3), lag(POP_SSP3) * (1 + CAGR_POP_SSP3), POP_SSP3),
                POP_SSP3_50km = if_else(is.na(POP_SSP3_50km), lag(POP_SSP3_50km) * (1 + CAGR_POP_SSP3_50km), POP_SSP3_50km),
                POP_SSP3_100km = if_else(is.na(POP_SSP3_100km), lag(POP_SSP3_100km) * (1 + CAGR_POP_SSP3_100km), POP_SSP3_100km),
                POP_SSP3_500km = if_else(is.na(POP_SSP3_500km), lag(POP_SSP3_500km) * (1 + CAGR_POP_SSP3_500km), POP_SSP3_500km),
                POP_SSP4 = if_else(is.na(POP_SSP4), lag(POP_SSP4) * (1 + CAGR_POP_SSP4), POP_SSP4),
                POP_SSP4_50km = if_else(is.na(POP_SSP4_50km), lag(POP_SSP4_50km) * (1 + CAGR_POP_SSP4_50km), POP_SSP4_50km),
                POP_SSP4_100km = if_else(is.na(POP_SSP4_100km), lag(POP_SSP4_100km) * (1 + CAGR_POP_SSP4_100km), POP_SSP4_100km),
                POP_SSP4_500km = if_else(is.na(POP_SSP4_500km), lag(POP_SSP4_500km) * (1 + CAGR_POP_SSP4_500km), POP_SSP4_500km),
                POP_SSP5 = if_else(is.na(POP_SSP5), lag(POP_SSP5) * (1 + CAGR_POP_SSP5), POP_SSP5),
                POP_SSP5_50km = if_else(is.na(POP_SSP5_50km), lag(POP_SSP5_50km) * (1 + CAGR_POP_SSP5_50km), POP_SSP5_50km),
                POP_SSP5_100km = if_else(is.na(POP_SSP5_100km), lag(POP_SSP5_100km) * (1 + CAGR_POP_SSP5_100km), POP_SSP5_100km),
                POP_SSP5_500km = if_else(is.na(POP_SSP5_500km), lag(POP_SSP5_500km) * (1 + CAGR_POP_SSP5_500km), POP_SSP5_500km)
            )%>%
            mutate(
                
                GDP_SSP1 = if_else(is.na(GDP_SSP1), lag(GDP_SSP1) * (1 + CAGR_GDP_SSP1), GDP_SSP1),
                GDP_SSP1_50km = if_else(is.na(GDP_SSP1_50km), lag(GDP_SSP1_50km) * (1 + CAGR_GDP_SSP1_50km), GDP_SSP1_50km),
                GDP_SSP1_100km = if_else(is.na(GDP_SSP1_100km), lag(GDP_SSP1_100km) * (1 + CAGR_GDP_SSP1_100km), GDP_SSP1_100km),
                GDP_SSP1_500km = if_else(is.na(GDP_SSP1_500km), lag(GDP_SSP1_500km) * (1 + CAGR_GDP_SSP1_500km), GDP_SSP1_500km),
                GDP_SSP2 = if_else(is.na(GDP_SSP2), lag(GDP_SSP2) * (1 + CAGR_GDP_SSP2), GDP_SSP2),
                GDP_SSP2_50km = if_else(is.na(GDP_SSP2_50km), lag(GDP_SSP2_50km) * (1 + CAGR_GDP_SSP2_50km), GDP_SSP2_50km),
                GDP_SSP2_100km = if_else(is.na(GDP_SSP2_100km), lag(GDP_SSP2_100km) * (1 + CAGR_GDP_SSP2_100km), GDP_SSP2_100km),
                GDP_SSP2_500km = if_else(is.na(GDP_SSP2_500km), lag(GDP_SSP2_500km) * (1 + CAGR_GDP_SSP2_500km), GDP_SSP2_500km),
                GDP_SSP3 = if_else(is.na(GDP_SSP3), lag(GDP_SSP3) * (1 + CAGR_GDP_SSP3), GDP_SSP3),
                GDP_SSP3_50km = if_else(is.na(GDP_SSP3_50km), lag(GDP_SSP3_50km) * (1 + CAGR_GDP_SSP3_50km), GDP_SSP3_50km),
                GDP_SSP3_100km = if_else(is.na(GDP_SSP3_100km), lag(GDP_SSP3_100km) * (1 + CAGR_GDP_SSP3_100km), GDP_SSP3_100km),
                GDP_SSP3_500km = if_else(is.na(GDP_SSP3_500km), lag(GDP_SSP3_500km) * (1 + CAGR_GDP_SSP3_500km), GDP_SSP3_500km),
                GDP_SSP4 = if_else(is.na(GDP_SSP4), lag(GDP_SSP4) * (1 + CAGR_GDP_SSP4), GDP_SSP4),
                GDP_SSP4_50km = if_else(is.na(GDP_SSP4_50km), lag(GDP_SSP4_50km) * (1 + CAGR_GDP_SSP4_50km), GDP_SSP4_50km),
                GDP_SSP4_100km = if_else(is.na(GDP_SSP4_100km), lag(GDP_SSP4_100km) * (1 + CAGR_GDP_SSP4_100km), GDP_SSP4_100km),
                GDP_SSP4_500km = if_else(is.na(GDP_SSP4_500km), lag(GDP_SSP4_500km) * (1 + CAGR_GDP_SSP4_500km), GDP_SSP4_500km),
                GDP_SSP5 = if_else(is.na(GDP_SSP5), lag(GDP_SSP5) * (1 + CAGR_GDP_SSP5), GDP_SSP5),
                GDP_SSP5_50km = if_else(is.na(GDP_SSP5_50km), lag(GDP_SSP5_50km) * (1 + CAGR_GDP_SSP5_50km), GDP_SSP5_50km),
                GDP_SSP5_100km = if_else(is.na(GDP_SSP5_100km), lag(GDP_SSP5_100km) * (1 + CAGR_GDP_SSP5_100km), GDP_SSP5_100km),
                GDP_SSP5_500km = if_else(is.na(GDP_SSP5_500km), lag(GDP_SSP5_500km) * (1 + CAGR_GDP_SSP5_500km), GDP_SSP5_500km),
                POP_SSP1 = if_else(is.na(POP_SSP1), lag(POP_SSP1) * (1 + CAGR_POP_SSP1), POP_SSP1),
                POP_SSP1_50km = if_else(is.na(POP_SSP1_50km), lag(POP_SSP1_50km) * (1 + CAGR_POP_SSP1_50km), POP_SSP1_50km),
                POP_SSP1_100km = if_else(is.na(POP_SSP1_100km), lag(POP_SSP1_100km) * (1 + CAGR_POP_SSP1_100km), POP_SSP1_100km),
                POP_SSP1_500km = if_else(is.na(POP_SSP1_500km), lag(POP_SSP1_500km) * (1 + CAGR_POP_SSP1_500km), POP_SSP1_500km),
                POP_SSP2 = if_else(is.na(POP_SSP2), lag(POP_SSP2) * (1 + CAGR_POP_SSP2), POP_SSP2),
                POP_SSP2_50km = if_else(is.na(POP_SSP2_50km), lag(POP_SSP2_50km) * (1 + CAGR_POP_SSP2_50km), POP_SSP2_50km),
                POP_SSP2_100km = if_else(is.na(POP_SSP2_100km), lag(POP_SSP2_100km) * (1 + CAGR_POP_SSP2_100km), POP_SSP2_100km),
                POP_SSP2_500km = if_else(is.na(POP_SSP2_500km), lag(POP_SSP2_500km) * (1 + CAGR_POP_SSP2_500km), POP_SSP2_500km),
                POP_SSP3 = if_else(is.na(POP_SSP3), lag(POP_SSP3) * (1 + CAGR_POP_SSP3), POP_SSP3),
                POP_SSP3_50km = if_else(is.na(POP_SSP3_50km), lag(POP_SSP3_50km) * (1 + CAGR_POP_SSP3_50km), POP_SSP3_50km),
                POP_SSP3_100km = if_else(is.na(POP_SSP3_100km), lag(POP_SSP3_100km) * (1 + CAGR_POP_SSP3_100km), POP_SSP3_100km),
                POP_SSP3_500km = if_else(is.na(POP_SSP3_500km), lag(POP_SSP3_500km) * (1 + CAGR_POP_SSP3_500km), POP_SSP3_500km),
                POP_SSP4 = if_else(is.na(POP_SSP4), lag(POP_SSP4) * (1 + CAGR_POP_SSP4), POP_SSP4),
                POP_SSP4_50km = if_else(is.na(POP_SSP4_50km), lag(POP_SSP4_50km) * (1 + CAGR_POP_SSP4_50km), POP_SSP4_50km),
                POP_SSP4_100km = if_else(is.na(POP_SSP4_100km), lag(POP_SSP4_100km) * (1 + CAGR_POP_SSP4_100km), POP_SSP4_100km),
                POP_SSP4_500km = if_else(is.na(POP_SSP4_500km), lag(POP_SSP4_500km) * (1 + CAGR_POP_SSP4_500km), POP_SSP4_500km),
                POP_SSP5 = if_else(is.na(POP_SSP5), lag(POP_SSP5) * (1 + CAGR_POP_SSP5), POP_SSP5),
                POP_SSP5_50km = if_else(is.na(POP_SSP5_50km), lag(POP_SSP5_50km) * (1 + CAGR_POP_SSP5_50km), POP_SSP5_50km),
                POP_SSP5_100km = if_else(is.na(POP_SSP5_100km), lag(POP_SSP5_100km) * (1 + CAGR_POP_SSP5_100km), POP_SSP5_100km),
                POP_SSP5_500km = if_else(is.na(POP_SSP5_500km), lag(POP_SSP5_500km) * (1 + CAGR_POP_SSP5_500km), POP_SSP5_500km)
            )

            print(ssps_interpolate)

            as.data.frame(ssps_interpolate[which(ssps_interpolate$gridcell_id==3),])
            
            ggplot(ssps_interpolate)+geom_point(aes(x=year,y=log(GDP_SSP2),group=gridcell_id),alpha=0.5)+
            xlim(c(2020,2030))

            ggplot(ssps)+geom_line(aes(x=year,y=log(POP_SSP2),group=gridcell_id),alpha=0.5)+
            xlim(c(2020,2030))


        
            glimpse(ssps)
            ssps <- as.data.frame(ssps)
            
    ## Interpolate SSPS

    ## Merge with Climate Data (start)
        names(sst_p)[-c(1:3)] <- paste0(names(sst_p)[-c(1:3)],"_85")
        names(sst_bin_p)[-c(1:3)] <- paste0(names(sst_bin_p)[-c(1:3)],"_85")
        names(sst_hot_p)[-c(1:3)] <- paste0(names(sst_hot_p)[-c(1:3)],"_85")
        names(sst_hot_p_70)[-c(1:3)] <- paste0(names(sst_hot_p_70)[-c(1:3)],"_70")
        scen <- merge(ssps_interpolate,sst_p,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,sst_hot_p,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,sst_hot_p_70,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,sst_bin_p,by=c("gridcell_id","year"),all=T)

        glimpse(scen)

       
        additional_columns <- c("X.x", "X.y", "X", "NA._85")

        # Combine the indices of CAGR columns and the additional columns
        columns_to_remove <- c(grep("^CAGR", names(scen)), which(names(scen) %in% additional_columns))

        # Drop these columns from the data frame
        scen <- scen[,-columns_to_remove]
        #scen <- scen[,-which(names(scen) %in% c("X.x","X.y","X","NA._85","CAGR_GDP_SSP2" ,"CAGR_GDP_SSP5" ,"CAGR_pop_SSP2" ,"CAGR_pop_SSP5"))]
        #names(scen)[3:6] <- c("GDP_SSP2","GDP_SSP5","POP_SSP2","POP_SSP5")
        glimpse(ssps_interpolate)
        glimpse(scen)

        
        scen <- merge(scen,temp_85,by=c("gridcell_id","year"),all=T)
        scen <- merge(scen,preci_85,by=c("gridcell_id","year"),all=T)
        scen <- scen[,-which(names(scen) %in% c("X.x","X.y","X"))]
        names(scen)[c(93,94)] <- c("temp_85"  ,"preci_85")

        
        write.csv(scen,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_gridcells.csv")

    ## Merge with Climate Data (end)

        scen <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_gridcells.csv")
        glimpse(scen)
        ssps_country <- read.csv("C:\\Users\\basti\\Box\\Data\\SSPs\\SspDb_country_data_2013-06-12.csv")
        ssps_country_pop <- read.csv("C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_pop.csv")
        ssps_country_pop$id <- paste0(ssps_country_pop$ISO3,ssps_country_pop$year,ssps_country_pop$scenario)
        ssps_country_gdp <- read.csv("C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv")
        ssps_country_gdp$id <- paste0(ssps_country_gdp$ISO3,ssps_country_gdp$year,ssps_country_gdp$scenario)
        ssps <- merge(ssps_country_pop,ssps_country_gdp %>% dplyr::select(id,GDP.billion2005USDperYear),by="id",all=T)
        glimpse(ssps)        


        ssps_1 <- ssps %>% filter(scenario=="SSP1") %>% dplyr::select(ISO3,year,Pop.million,GDP.billion2005USDperYear)
        names(ssps_1) <- c("countrycode","year","Pop_Country_ssp1","GDP_Country_ssp1")
        ssps_2 <- ssps %>% filter(scenario=="SSP2") %>% dplyr::select(ISO3,year,Pop.million,GDP.billion2005USDperYear)
        names(ssps_2) <- c("countrycode","year","Pop_Country_ssp2","GDP_Country_ssp2")
        ssps_3 <- ssps %>% filter(scenario=="SSP3") %>% dplyr::select(ISO3,year,Pop.million,GDP.billion2005USDperYear)
        names(ssps_3) <- c("countrycode","year","Pop_Country_ssp3","GDP_Country_ssp3")
        ssps_4 <- ssps %>% filter(scenario=="SSP4") %>% dplyr::select(ISO3,year,Pop.million,GDP.billion2005USDperYear)
        names(ssps_4) <- c("countrycode","year","Pop_Country_ssp4","GDP_Country_ssp4")
        ssps_5 <- ssps %>% filter(scenario=="SSP5") %>% dplyr::select(ISO3,year,Pop.million,GDP.billion2005USDperYear)
        names(ssps_5) <- c("countrycode","year","Pop_Country_ssp5","GDP_Country_ssp5")

        ssps_c_all <- merge(ssps_1,ssps_2,by=c("year","countrycode"))
        ssps_c_all <- merge(ssps_c_all,ssps_3,by=c("year","countrycode"))
        ssps_c_all <- merge(ssps_c_all,ssps_4,by=c("year","countrycode"))
        ssps_c_all <- merge(ssps_c_all,ssps_5,by=c("year","countrycode"))
        glimpse(ssps_c_all)        
        
        # library(dplyr)

        # ssps_wide <- ssps %>%
        # # Spread the data to wide format, separate columns for Pop and GDP for each SSP
        # pivot_wider(names_from = scenario,
        #             values_from = c(Pop.million, GDP.billion2005USDperYear),
        #             names_glue = "{scenario}_{.value}") %>%
        # # Optional: Sort the data by ISO3 and year for better readability
        # arrange(ISO3, year)
        # # Check the result
        # glimpse(ssps_wide)

        # glimpse(ssps_wide)

        # glimpse(ssps_country_pop)
        # glimpse(ssps_country_gdp)
        # unique(ssps_country_pop$scenario)
        # unique(ssps_country_pop$ISO3)
        #         ## SSPs Old
        #             names(ssps_country)
        #             unique(ssps_country$SCENARIO)
        #             scens <- c("SSP5_v9_130325" ,"SSP5_v9_130424" ,"SSP5_v9_130115")
                    
        #             glimpse(ssps_country)
        #             countries <- unique(mcn$countrycode)
        #             vars <- c("GDP|PPP","Population") #GDP in billion 2005 USD . Pop in million
        #             ssps_country <- ssps_country[which(ssps_country$SCENARIO %in% scens & 
        #                                                 ssps_country$REGION %in% countries &
        #                                                 ssps_country$VARIABLE %in% vars),]
        #             long_ssps_country <- ssps_country %>%
        #                 pivot_longer(cols = starts_with("X"), 
        #                             names_to = "Year", 
        #                             values_to = "Value")

        #                 # Convert the year column to numeric removing the 'X' at the beginning
        #                 long_ssps_country$Year <- as.numeric(str_remove(long_ssps_country$Year, "X"))
        #                 long_ssps_country <- long_ssps_country %>%
        #                     spread(key = VARIABLE, value = Value)

        #             glimpse(long_ssps_country)

        #             mean_ssps_country <- long_ssps_country %>%
        #                 group_by(REGION, Year) %>%
        #                 summarise(mean_GDP = mean(`GDP|PPP`, na.rm = TRUE),
        #                             mean_Population = mean(Population, na.rm = TRUE))
        #             glimpse(mean_ssps_country)

        #             mean_ssps_country <- mean_ssps_country %>%
        #             arrange(REGION, Year)

        #             # Fill in missing years and interpolate GDP|PPP and Population
        #             mean_ssps_country_interpolated <- mean_ssps_country %>%
        #                 group_by(REGION) %>%
        #                 complete(Year = full_seq(Year, 1)) %>%
        #                 mutate(mean_GDP = na.approx(mean_GDP, na.rm = FALSE), 
        #                         mean_Population = na.approx(mean_Population, na.rm = FALSE))

        #             mean_ssps_country_interpolated <- mean_ssps_country_interpolated %>% filter(Year>2020)
                    
        #             ggplot(mean_ssps_country_interpolated[which(mean_ssps_country_interpolated$Year>2000),])+
        #             geom_point(aes(x=Year,y=mean_GDP))
                    
        #         ## SSPs Old
                    
                     mcn_c <- mcn[,which(names(mcn) %in% c("gridcell_id","countrycode","R5"))]
                     mcn_c <- mcn_c[complete.cases(mcn_c),]
                     c1 <- aggregate(countrycode~gridcell_id,FUN="first",data=mcn_c)
                     r1 <- aggregate(R5~gridcell_id,FUN="first",data=mcn_c)
                     c2 <- data.frame(gridcell_id=c1$gridcell_id,countrycode=c1$countrycode,R5=r1$R5)
                     scen <- merge(c2,scen ,by="gridcell_id",all=T) 
        #             glimpse(scen)
        #             glimpse(mean_ssps_country_interpolated)
        #             names(mean_ssps_country_interpolated) <- c("countrycode","year","GDP_Country_ssp5","Pop_Country_ssp5")
        #             scen <- merge(scen,mean_ssps_country_interpolated,by=c("countrycode","year"))
        #             glimpse(scen)

        #             scen$GDPpc_c_ssp5<-(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5)
        #             scen$logGDPpc_c_ssp5<-log(scen$GDP_Country_ssp5/scen$Pop_Country_ssp5)
        
        #Perc_inc 

        scen <- merge(scen,ssps_c_all,by=c("countrycode","year"))
        glimpse(scen)
        glimpse(ssps_c_all)
        scen$gdppc1 <- scen$GDP_SSP1/scen$POP_SSP1 #GDP
        scen$gdppc2 <- scen$GDP_SSP2/scen$POP_SSP2
        scen$gdppc3 <- scen$GDP_SSP3/scen$POP_SSP3
        scen$gdppc4 <- scen$GDP_SSP4/scen$POP_SSP4
        scen$gdppc5 <- scen$GDP_SSP5/scen$POP_SSP5
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
                            delta_sst_hot_70 = sst_hot_70 - lag(sst_hot_70),
                            perc_inc_gdp1 = (GDP_SSP1 /lag(GDP_SSP1)-1)*100,
                            perc_inc_gdp2 = (GDP_SSP2 /lag(GDP_SSP2)-1)*100,
                            perc_inc_gdp3 = (GDP_SSP3 /lag(GDP_SSP3)-1)*100,
                            perc_inc_gdp4 = (GDP_SSP4 /lag(GDP_SSP4)-1)*100,
                            perc_inc_gdp5 = (GDP_SSP5 /lag(GDP_SSP5)-1)*100,
                            perc_inc_gdppc1 = (gdppc1 /lag(gdppc1)-1)*100,
                            perc_inc_gdppc2 = (gdppc2 /lag(gdppc2)-1)*100,
                            perc_inc_gdppc3 = (gdppc3 /lag(gdppc3)-1)*100,
                            perc_inc_gdppc4 = (gdppc4 /lag(gdppc4)-1)*100,
                            perc_inc_gdppc5 = (gdppc5 /lag(gdppc5)-1)*100,
                            perc_inc_pop2 = (POP_SSP2 /lag(POP_SSP2)-1)*100,
                            perc_inc_pop1 = (POP_SSP1 /lag(POP_SSP1)-1)*100,
                            perc_inc_pop3 = (POP_SSP3 /lag(POP_SSP3)-1)*100,
                            perc_inc_pop4 = (POP_SSP4 /lag(POP_SSP4)-1)*100,
                            perc_inc_pop5 = (POP_SSP5 /lag(POP_SSP5)-1)*100,
                            temp_inc_85 = (temp_85 -lag(temp_85)),
                            preci_inc_85 = (preci_85 -lag(preci_85)),
                            days32_inc_85 = (days32_85 -lag(days32_85)),
                            days31_inc_85 = (days31_85 -lag(days31_85)),
                            perc_inc_GDPpc_c = (GDP_Country_ssp3/lag(GDP_Country_ssp3)-1)*100
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
        
        write.csv(scen,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_all_ssps.csv")
        
## Read Data (end)

