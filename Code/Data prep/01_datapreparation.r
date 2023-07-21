#Merge all data


# Load libraries
library(sf)      # For working with spatial data in .gpkg format
library(ncdf4)   # For reading netCDF files
library(tidyverse)
#install.packages("WDI")
library(WDI)

# Set the path to your data files
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    ntl_df <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nightlights.csv")
    ntl_df_pre2013 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\nightlights_mangroves_pre2013.csv")
    #mhw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw.csv")
    mhw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw_full.csv")
    #mcw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw.csv")
    mcw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw_full.csv")
    mhw_toolbox <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw_toolbox.csv")
    mcw_toolbox <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw_toolbox.csv")
    mangrove_df <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches.csv")
    mangrove_df_pre2015 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches_pre2015.csv")
    mangrove_df_full <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches_Jul10.csv")
    prcip <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\precip.csv")
    temp_anom <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\temp_grid.csv")
    temp_clim <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\climgrid.csv")
    countries <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nightlights_countries.csv")
    wealth <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\WorldBank_WealthAccounts_2018USD.csv')
    gdp <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\GDP_2015USD_19952018.csv')
    population  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\Pop1995_2018.csv')
    regions  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\r5regions.csv')
    spei  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\spei.csv')
    socioecon  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\yearly_gdp_pop.csv')
    sst_bins_or  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins.csv')
    #sst_bins_or  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_or2.csv')
    sst_bins  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_full.csv')
    #sst_bins  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_full_raw.csv')
    #sst_anom_bins  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_full_raw.csv')
    sst_anom_bins  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_full.csv')
    #sst_anom_bins  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins.csv')
    sst_anom_bins_hr01  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_hr01.csv')
    #sst_anom_bins_mr05  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_mr05.csv')
    sst_anom_bins_mr05  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_anom_bins_mr05_full.csv')
    sst_bins_mr05  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_bins_mr05_full.csv')
    salinity  <- read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\salinity.csv')
    sst_hottest <-read.csv('C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\sst_hottest.csv') 

## Parenthesis to plot sst bins
    glimpse(sst_bins_or)
    glimpse(sst_bins)

    long_sst_bins <- sst_bins %>%
  pivot_longer(cols = b8_9C:b37_38C, names_to = "days", values_to = "bin_count")
    glimpse(long_sst_bins)

    long_sst_bins_or <- sst_bins_or %>%
  pivot_longer(cols = b8_9C:b37_38C, names_to = "days", values_to = "bin_count")
    glimpse(long_sst_bins_or)
    names(long_sst_bins_or)[5] <- "bin_count_or"

    merged_bins <- merge(long_sst_bins,long_sst_bins_or,by=c("gridcell_id","year","days"),all=T)
    glimpse(merged_bins)
    ggplot(merged_bins[which(merged_bins$year==2000),],aes(x=bin_count,y=bin_count_or))+geom_point()

    aggregate(bin_count_or ~ year + gridcell_id,FUN="sum",data=merged_bins[which(merged_bins$year==2000),])
    aggregate(bin_count ~ year + gridcell_id,FUN="sum",data=merged_bins[which(merged_bins$year==2000),])

    empty_gridcell <- unique(merged_bins$gridcell_id[which(merged_bins$year==2000 &  is.na(merged_bins$bin_count_or) & !is.na(merged_bins$bin_count))])
    empty_gridcell
    
    ggplot(merged_bins[which(merged_bins$year==2000 & merged_bins$gridcell_id == 997),],aes(x=bin_count,y=bin_count_or))+geom_point()
    ggplot(merged_bins[which(merged_bins$year==2000 & merged_bins$gridcell_id == 200),],aes(x=bin_count,y=bin_count_or))+geom_point()
    ggplot(merged_bins[which(merged_bins$year==2000 & merged_bins$gridcell_id == 38),],aes(x=bin_count,y=bin_count_or))+geom_point()
    
## Parenthesis to plot sst bins

    

# Read files
    #mhw
    glimpse(mhw)
    glimpse(mcw)
    names(mhw)[2] <- "gridcell_id"
    names(mcw)[2] <- "gridcell_id"
    mhw_df <- merge(mhw,mcw,by=c("gridcell_id","year"))
    glimpse(mhw_df)
    mhw_df_toolbox <- merge(mhw_toolbox,mcw_toolbox,by=c("gridcell_id","year"))
    glimpse(mhw_df_toolbox)
    mhw_df <- mhw_df[,-which(names(mhw_df) %in% c("X.x","sst.y","X.y"))]
    mhw_df_toolbox  <- mhw_df_toolbox [,-which(names(mhw_df_toolbox ) %in% c("X.x","sst.y","X.y"))]
    names(mhw_df)[which(names(mhw_df)=="sst.x")] <- "sst"
    
    names(mhw_df_toolbox)[c(3:8)] <- paste0( names(mhw_df_toolbox)[c(3:8)],"_toolbox")
   
    #ntl
    ntl_df <- ntl_df %>%
        pivot_longer(cols = starts_with("X"),
                    names_to = "year",
                    values_to = "ntl")

        # Print the long format dataframe
        glimpse(ntl_df)
        ntl_df$year <- as.numeric(gsub("X", "", ntl_df$year))
        names(ntl_df)[1] <- "gridcell_id"
        ntl_df$sensor <- "VIIRS"  
        
        glimpse(ntl_df_pre2013)
        ntl_df_pre2013 <- ntl_df_pre2013[,-c(1)]
        names(ntl_df_pre2013) <- c("gridcell_id","id","year","ntl")
        ntl_df_pre2013$sensor <- "DMSP"    
    countries <- countries[,c(1,11:15)]
    names(countries)[1] <- "gridcell_id"

    ntl_df <- rbind(ntl_df,ntl_df_pre2013)
    mcn <- merge(ntl_df,mhw_df,by=c("gridcell_id","year"),all=TRUE)
    glimpse(mcn)
    mcn <- merge(mcn,mhw_df_toolbox,by=c("gridcell_id","year"),all=TRUE)
    

    glimpse(sst_bins)
    
    glimpse(mcn)
    
    any(table(mcn$unique_id)>1)
    mcn <- merge(mcn,sst_bins,by=c("gridcell_id","year"),all=TRUE)

    
    glimpse(sst_anom_bins)
    mcn <- merge(mcn,sst_anom_bins,by=c("gridcell_id","year"),all=TRUE)
    mcn <- mcn[,-which(names(mcn) %in% c("X.x","X.y"))]

    mcn$unique_id <- paste0(mcn$gridcell_id,mcn$year)
    any(table(mcn$unique_id)>1)
    

    #names(mangrove_df)[2] <- "gridcell_id"
    #names(mangrove_df_pre2015)[2] <- "gridcell_id"
    #glimpse(mangrove_df)
    #glimpse(mangrove_df_pre2015)
    #mangrove_df <- rbind(mangrove_df,mangrove_df_pre2015)

    
    names(mangrove_df_full)[2] <- "gridcell_id"
    mangrove_df <- mangrove_df_full
    # Group the data by gridcell_id
        df_mangroves_grouped <- mangrove_df %>%
        arrange(gridcell_id,year)  %>% group_by(gridcell_id)
         # Ensure the data is sorted by year

        glimpse(df_mangroves_grouped)
        
        # Calculate the annual change in mangrove cover percentage
        df_mangroves_change <- df_mangroves_grouped %>%
        mutate(annual_area_change =mangrove_area / dplyr::lag(mangrove_area)-1)

        glimpse(df_mangroves_change)

        df_mangroves_change <- df_mangroves_change %>%
        mutate(annual_np_change =np / dplyr::lag(np)-1)

        df_mangroves_change$patch_size <- df_mangroves_change$mangrove_area / df_mangroves_change$np
        
        df_mangroves_change <- df_mangroves_change %>%
        mutate(annual_patchsize_change = patch_size / dplyr::lag(patch_size)-1)

        df_mangroves_change <- df_mangroves_change %>%
        mutate(annual_holes_change = holes / dplyr::lag(holes)-1)

        # Print the dataset with annual change
        glimpse(df_mangroves_change)

        df_mangroves_change[which(df_mangroves_change$year %in% c(1996,2007,2015)),which(names(df_mangroves_change) %in% c("annual_area_change","annual_np_change","annual_patchsize_change"))] <-NA #Because the data has gaps


        
        df_mangroves_change$unique_id <- paste0(df_mangroves_change$gridcell_id,df_mangroves_change$year)
        any(table(mcn$unique_id)>1)

        df_mangroves_change <- data.frame(df_mangroves_change)
        
        mcn0 <- mcn
        mcn <- merge(mcn0,df_mangroves_change,by=c("gridcell_id","year"),all=TRUE)
        mcn$unique_id <- paste0(mcn$gridcell_id,mcn$year)
        any(table(mcn$unique_id)>1)
        glimpse(mcn)
        
        glimpse(countries)
        names(countries)[1] <- "gridcell_id"
        countries$unique <- paste0(countries$gridcell_id,countries$sovereignt)
        any(table(countries$unique)>1)
        table(countries$unique)[(table(countries$unique)>1)]
        countries_unique <- countries %>% distinct(gridcell_id, .keep_all = TRUE)
        any(table(countries_unique$unique)>1)

    glimpse(countries_unique)  
    #countries_unique <- countries_unique[,-c(2:10)]
    mcn2 <- merge(mcn,countries_unique,by=c("gridcell_id"),all.x=TRUE)
    mcn2$unique_id <- paste0(mcn2$gridcell_id,mcn2$year)
    any(table(mcn2$unique_id)>1)
    table(mcn2$unique_id)[(table(mcn2$unique_id)>1)]
        
##


    glimpse(mcn2)

    glimpse(prcip) 
    names(prcip)[2] <- "gridcell_id"
    names(prcip)[3] <- "year"
    mcn2 <- merge(mcn2,prcip,by=c("gridcell_id","year"),all=TRUE)
    

    glimpse(temp_anom )
    names(temp_anom)[2] <- "gridcell_id"
    names(temp_anom)[3] <- "year"
    mcn2 <- merge(mcn2,temp_anom,by=c("gridcell_id","year"),all=TRUE)
    
    glimpse(temp_clim )
    names(temp_clim)[3] <- "gridcell_id"
    mcn2 <- merge(mcn2,temp_clim,by=c("gridcell_id"),all.x=TRUE)

    glimpse(mcn2)   
    
    mcn2 <- mcn2[,-which(names(mcn2) %in% c("X","unique_id.x","X.x","unique_id.y","unique_id","unique","X.y"))] 
    mcn2$temp <- mcn2$mean_temp + mcn2$temp_anom

    ## World Bank
        df <- WDI(country = "all", indicator = c("NY.GDP.MKTP.PP.KD", "SP.POP.TOTL","NY.GDP.PCAP.PP.KD"), start = 1990, end = 2022) #2017 PPP constant 2017
        glimpse(df)
        names(df)[c(3,5,6,7)] <- c("countrycode","GDP_country","Population_country","GDPpc_country")

        # colnames(wealth) <- c("Country.Name" ,"Country.Code",   "Series.Name" , "Series.Code",
        # 1995:2018)

        # years <- c(1995:2018)
        # for (i in 1:length(years)){
        #     yeari <- years[i]
        #     w2 <- wealth[,which(colnames(wealth) %in% 
        #         c("Country.Name","Country.Code","Series.Name",yeari))]
        #     w3 <- reshape(w2, idvar=c("Country.Name" ,"Country.Code"), 
        #         timevar="Series.Name", direction="wide")
        #     w3 <- w3[,c(1,2,3,4,34,37,38,39,41,47,49,51,53)]
        #     w3[,(dim(w3)[2]+1)] <- yeari
        #     colnames(w3) <- c("countryname","countrycode", "N","H","Nagg","Nfisheries","NforestES","NforestT","Nmangroves","Npa","Foreign", "K","TotalWealth","year")
        #     if (i==1){Wealth <- w3} else {
        #         Wealth <- rbind(Wealth,w3)
        #     }
        # }

        # colnames(gdp) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        # for (i in 1:length(years)){
        #     yeari <- years[i]
        #     gdp2 <- gdp[,which(colnames(gdp) %in% 
        #         c("countryname","countrycode","seriesname",yeari))]
            
        #     gdp3 <- reshape(gdp2, idvar=c("countryname","countrycode"), 
        #         timevar="seriesname", direction="wide")
        #     gdp3 <- gdp3[,c(1:3)]
        #     gdp3[,4] <- yeari
        #     colnames(gdp3) <- c("countryname","countrycode","GDP","year")
        #     if (i==1){Gdp <- gdp3} else {
        #         Gdp <- rbind(Gdp,gdp3)
        #     }
        # }
        
        # wealth <- merge(Wealth,Gdp, by = c("countrycode","year"),all=TRUE)
        # wealth <- wealth[-c(which(wealth[,1]=="")),] #no data in these rows
        # glimpse(wealth)
        # wealth$GDP <- as.numeric(as.character(wealth$GDP)) * 1.06 #converting 2015 to 2018 usd
        # colnames(wealth)[3] <- "countryname"
        

        
        # colnames(population) <-  c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        # years <- c(1995:2018)
        # for (i in 1:length(years)){
        #     yeari <- years[i]
        #     w2 <- population[,which(colnames(population) %in% 
        #         c("countryname", "countrycode", "seriesname",yeari))]
        #     w3 <- reshape(w2, idvar=c("countryname", "countrycode"), 
        #         timevar="seriesname", direction="wide")
        #     w3[,4] <- yeari
        #     colnames(w3) <- c("countryname","countrycode","Population","year")
        #     if (i==1){Population <- w3} else {
        #         Population  <- rbind(Population,w3)
        #     }
        # }
        # wealth_data <- merge(wealth,Population,all=TRUE,by=c("countrycode","year"))
        # wdata_1995_2018 <- wealth_data

        # wealth_data$H <- as.numeric(as.character(wealth_data$H))
        # wealth_data$K <- as.numeric(as.character(wealth_data$K))
        # wealth_data$N <- as.numeric(as.character(wealth_data$N))
        # wealth_data$NforestES <- as.numeric(as.character(wealth_data$NforestES))
        # wealth_data$NforestT <- as.numeric(as.character(wealth_data$NforestT))
        # wealth_data$Nmangroves <- as.numeric(as.character(wealth_data$Nmangroves))
        # wealth_data$Nfisheries <- as.numeric(as.character(wealth_data$Nfisheries))
        # wealth_data$Nagg <- as.numeric(as.character(wealth_data$Nagg))
        # wealth_data$Foreign <- as.numeric(as.character(wealth_data$Foreign))
        # wealth_data$Npa <- as.numeric(as.character(wealth_data$Npa))
        # wealth_data$TotalWealth <- as.numeric(as.character(wealth_data$TotalWealth))
        # wealth_data$Population <- as.numeric(as.character(wealth_data$Population))
        
        # wealth_data <- wealth_data[,-which(names(wealth_data) %in% c("countryname.x","countryname.y","NA","source"))]
        # table(wealth_data$countrycode)
        # glimpse(wealth_data)

         library("countrycode")
         mcn2$countrycode <- countrycode(mcn2$sovereignt,origin="country.name",destination="iso3c")
        mcn3 <- merge(mcn2,df,by=c("countrycode","year"),all=TRUE)

        glimpse(mcn3)
        #mcn3 <- mcn3[which(mcn3$countrycode!=""),]
    ## World Bank

    #Grid GDP and Pop
        glimpse(socioecon)
        names(socioecon)[c(1,2)] <- c("year","gridcell_id")
        mcn3 <- merge(mcn3,socioecon, by=c("year","gridcell_id"),all=TRUE)
        glimpse(mcn3)


    #Grid GDP and Pop

    #NEW variables
    mcn3$mhw <- mcn3$mhw_int
    mcn3$mhw[!is.na(mcn3$mhw)] <- 1
    mcn3$mhw[is.na(mcn3$mhw)] <- 0
    mcn3$mhw_INT <- mcn3$mhw_int
    mcn3$mhw_INT_anom <- mcn3$mhw_int_anom
    mcn3$mhw_INT_freq <- mcn3$mhw_int_freq
    mcn3$mhw_INT_dur <- mcn3$mhw_int_dur
    mcn3$mhw_INT[mcn3$mhw==0] <- 0
    mcn3$mhw_INT_anom[mcn3$mhw==0] <- 0
    mcn3$mhw_INT_freq[mcn3$mhw==0] <- 0
    mcn3$mhw_INT_dur[mcn3$mhw==0] <- 0
    #mcn3$mhw_int_anom[mcn3$mhw==0] <- 0
    #mcn3$mhw_int_freq[mcn3$mhw==0] <- 0
    #mcn3$mhw_int_dur[mcn3$mhw==0] <- 0
    
    mcn3$mcw <- mcn3$mcw_int
    mcn3$mcw[!is.na(mcn3$mcw)] <- 1
    mcn3$mcw[is.na(mcn3$mcw)] <- 0
    mcn3$mcw_INT <- mcn3$mcw_int
    mcn3$mcw_INT_anom <- mcn3$mcw_int_anom
    mcn3$mcw_INT_freq <- mcn3$mcw_int_freq
    mcn3$mcw_INT_dur <- mcn3$mcw_int_dur
    mcn3$mcw_INT[mcn3$mcw==0] <- 0
    mcn3$mcw_INT_anom[mcn3$mcw==0] <- 0
    mcn3$mcw_INT_freq[mcn3$mcw==0] <- 0
    mcn3$mcw_INT_dur[mcn3$mcw==0] <- 0
    #mcn3$mcw_int[mcn3$mcw==0] <- 0
    #mcn3$mcw_int_anom[mcn3$mcw==0] <- 0
    #mcn3$mcw_int_freq[mcn3$mcw==0] <- 0
    #mcn3$mcw_int_dur[mcn3$mcw==0] <- 0

    mcn3 <- mcn3 %>%
    group_by(gridcell_id) %>%
    arrange(gridcell_id,year) %>%
    mutate(ntl_change =ntl / dplyr::lag(ntl)-1) 
    max(mcn3$year[which(mcn3$sensor=="DMSP")])
    mcn3$ntl_change[which(mcn3$year==2013)] <- NA

    
    mcn3$year <- as.integer(mcn3$year)

    glimpse(mcn3)
    glimpse(regions)
    names(regions) <- c("R5","countrycode")
    regions$R5 <- as.character(gsub("R5", "", regions$R5))
    mcn3 <- merge(mcn3,regions,by="countrycode",all.x=TRUE)

    glimpse(spei) 
    names(spei)[2] <- "gridcell_id"
    names(spei)[3] <- "year"
    names(spei)[4] <- "spei"
    mcn3 <- merge(mcn3,spei,by=c("gridcell_id","year"),all=TRUE)
    mcn3 <- mcn3[,-which(names(mcn3) %in% c("X","unique_id.x","X.x","unique_id.y","unique_id","unique","X.y"))] 

    glimpse(mcn3)
    glimpse(salinity)
    salinity <- salinity[,-which(names(salinity)=="X")]
    mcn4 <- merge(mcn3,salinity,by=c("gridcell_id","year"),all=TRUE)    
    glimpse(mcn4)

    mcn4 <- mcn4 %>%
    group_by(gridcell_id) %>%
    arrange(gridcell_id,year) %>%
    mutate(salinity_change =Mean_Salinity/ dplyr::lag(Mean_Salinity)-1) 


    mcn4 <- mcn4 %>%
    group_by(gridcell_id) %>%
    arrange(gridcell_id,year) %>%
    mutate(gdp_change =Sum_GDP / dplyr::lag(Sum_GDP)-1) 


    
    hot_50 <- quantile(mcn4$sst, probs=c(0.5),na.rm=TRUE)
    mean_sst_id <- aggregate(sst ~ gridcell_id, FUN="mean", data=mcn4)
    mean_sst_id$hot_location_alldata <- 1
    mean_sst_id$hot_location_alldata[which(mean_sst_id$sst<hot_50)] <- 0
    mcn4 <- merge(mcn4,mean_sst_id[,c(1,3)],by="gridcell_id",all=TRUE)

    
    hot_50 <- quantile(mcn4$sst[which(mcn4$year==2020)], probs=c(0.5),na.rm=TRUE)
    mean_sst_id <- aggregate(sst ~ gridcell_id, FUN="mean", data=mcn4[which(mcn4$year==2020),])
    mean_sst_id$hot_location_2020 <- 1
    mean_sst_id$hot_location_2020[which(mean_sst_id$sst<hot_50)] <- 0
    mcn4 <- merge(mcn4,mean_sst_id[,c(1,3)],by="gridcell_id",all=TRUE)
    
    hot_50 <- quantile(mcn4$sst[which(mcn4$year==2000)], probs=c(0.5),na.rm=TRUE)
    mean_sst_id <- aggregate(sst ~ gridcell_id, FUN="mean", data=mcn4[which(mcn4$year==2000),])
    mean_sst_id$hot_location_2000 <- 1
    mean_sst_id$hot_location_2000[which(mean_sst_id$sst<hot_50)] <- 0
    mcn4 <- merge(mcn4,mean_sst_id[,c(1,3)],by="gridcell_id",all=TRUE)
       
    
    meanGDP <- aggregate(GDP_country ~countrycode,data=mcn4[which(mcn4$mangrove_area>0),],FUN="mean")
    rich_50 <- quantile(meanGDP$GDP_country, probs=c(0.5),na.rm=TRUE)
    mean_gdp_id <- aggregate(GDP_country ~ gridcell_id, FUN="mean", data=mcn4)
    mean_gdp_id$rich <- 1
    mean_gdp_id$rich[which(mean_gdp_id$GDP_country<rich_50)] <- 0
    mcn4 <- merge(mcn4,mean_gdp_id[,c(1,3)],by="gridcell_id",all=TRUE)

    
    #unique(mcn$countrycode[which(mcn$rich==1 & mcn$income!="high")] )

    
    mcn4$logGDPpc <- log(mcn4$Sum_GDP_50km/mcn4$Population_Count_50km)
    mcn4$logGDPpc_country <- log(mcn4$GDPpc_country)
    meanlogGDPpc <- aggregate(logGDPpc_country ~countrycode,data=mcn4[which(mcn4$mangrove_area>0),],FUN="mean")
    qinc <- quantile(meanlogGDPpc$logGDPpc_country , probs=c(0.33,0.66),na.rm=TRUE)
    c_low <- meanlogGDPpc$countrycode[which(meanlogGDPpc$logGDPpc_country < qinc[1])]
    c_high <- meanlogGDPpc$countrycode[which(meanlogGDPpc$logGDPpc_country > qinc[2])]
    mcn4$income <- "med"    
    mcn4$income[which(mcn4$countrycode %in% c_low)] <- "low"
    mcn4$income[which(mcn4$countrycode %in% c_high)] <- "high"


    glimpse(mcn4)
    glimpse(mcn4) 
    #glimpse(sst_anom_bins_hr01)
    #mcn4 <- merge(mcn4,sst_anom_bins_hr01,by=c("gridcell_id","year"),all=TRUE)
    #mcn4 <- merge(mcn4,sst_anom_bins_mr05,by=c("gridcell_id","year"),all=TRUE)
    
    #mcn4 <- mcn4[,-which(names(mcn4) %in% c("X","X.x","X.y"))] 

    
    mcn4$logGDPpc <- log(mcn4$Sum_GDP_50km/mcn4$Population_Count_50km)
    
    glimpse(sst_hottest)
    glimpse(mcn4)
    names(sst_hottest)[2] <- "gridcell_id"
    mcn4 <- merge(mcn4,sst_hottest,by=c("gridcell_id","year"),all=T)

    table(sst_hottest$year)
    mean_hottest <- aggregate(sst_hottest~gridcell_id,FUN="mean",data=sst_hottest[which(sst_hottest$year %in% c(1990:2022)),])
    glimpse(mean_hottest)
    names(mean_hottest)[2] <- "sst_hottest_mean0020"

    quantile(mean_hottest$sst_hottest_mean0020,probs=0.5)
    mean_hottest$hot_month <- "hot"
    mean_hottest$hot_month[which(mean_hottest$sst_hottest_mean0020<quantile(mean_hottest$sst_hottest_mean0020,probs=0.5))] <- "cold"

    
    mcn4 <- merge(mcn4,mean_hottest,by="gridcell_id",all=T)
    #mcn4 <- mcn4[,-which(names(mcn4) %in% c("X","X.x","X.y"))] 
    glimpse(mcn4)

    mcn4$logSumGDP <- log(mcn4$Sum_GDP_50km)
    mcn4$logPop <- log(mcn4$Population_Count_50km)
    mcn4$logGDPpc <- log(mcn4$Sum_GDP_50km/mcn4$Population_Count_50km)

 #   write.csv(mcn4,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
#mcn4 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")

mcn2020 <- mcn4 %>% filter(year==2020) %>% filter(mangrove_area>0) %>% filter(is.na(R5))
                unique(mcn2020$countrycode)
                unique(paste0(mcn2020$Latitude,",",mcn2020$Longitude))
                mcn4$latlot <- paste0(mcn4$Latitude,",",mcn4$Longitude)
                mcn2020$latlot <- paste0(mcn2020$Latitude,",",mcn2020$Longitude)
                names_latlon <- c("-19.1319486654551,-159.5"="ASIA","-17.1319435042391,-150.5"="ASIA",
                   "22.8683898685419,-89.5"= "LAM","14.8683770788442,-82.5000000000003"="LAM",
                    "13.8683707489465,-81.5000000000002"="LAM","12.8683635684406,-81.5"="LAM",
                    "25.8683733492814,-79.5000000000002"="OECD",
                    "20.8683938205675,-79.5000000000003"="LAM","11.8683555978666,-67.5"="LAM",
                    "11.8683555978665,-66.4999999999998"="LAM","11.8683555978666,-64.5000000000001"="LAM",
                    "16.8683869453379,40.4999999999999"="MAF","-22.1319475018277,40.4999999999996"="MAF",
                    "10.8683468979047,45.5"="MAF","-9.13188579406375,46.4999999999998"="MAF","-10.1318956159673,47.4999999999999"="MAF",
                    "-5.13184149328199,53.4999999999998"="MAF","-6.13185319777108,53.4999999999998"="MAF",
                    "6.86830602287207,72.5000000000002"="ASIA")

mcn2020$R5[mcn2020$latlot %in% names(names_latlon)] <- names_latlon[match(mcn2020$latlot[mcn2020$latlot %in% names(names_latlon)], names(names_latlon))]
mcn2020$R5[which(is.na(mcn2020$R5))] <- "ASIA"

newR5 <- data.frame(gridcell_id = mcn2020$gridcell_id, R5new = mcn2020$R5)
mcn4 <- merge(mcn4,newR5,by="gridcell_id",all=TRUE)
glimpse(mcn4)
mcn4$R5[which(is.na(mcn4$R5))] <- mcn4$R5new[which(is.na(mcn4$R5))] 
# Check the dataframe
head(mcn4)

glimpse(mcn4)



