#Merge all data
libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", "ncdf4",
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", "WDI",
               "ggpubr", "mapproj","tidyr")

lapply(libraries, library, character.only = TRUE)

# Set the path to your data files
    gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
    ntl_df <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nightlights.csv")
    ntl_df_pre2013 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\nightlights_mangroves_pre2013.csv")
    #mhw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw.csv")
    mhw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw_full.csv")
    mangroves_gdp_buffers <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangroves_gdp_buffers.csv")
    #mcw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw.csv")
    mcw <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw_full.csv")
    mhw_toolbox <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mhw_toolbox.csv")
    mcw_toolbox <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mcw_toolbox.csv")
    mangrove_df <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches.csv")
    mangrove_df_pre2015 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches_pre2015.csv")
    #mangrove_df_full <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches_Jul10.csv") 
    mangrove_df_full <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\mangrove_cover_patches_aug04.csv")
    prcip <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\precip.csv")
    temp_anom <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\temp_grid.csv")
    temp_clim <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\climgrid.csv")
    countries <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nightlights_countries.csv")
    glimpse(countries)
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


# Read files
    names(mhw)[2] <- "gridcell_id"
    names(mcw)[2] <- "gridcell_id"
    mhw_df <- merge(mhw,mcw,by=c("gridcell_id","year"))
    mhw_df_toolbox <- merge(mhw_toolbox,mcw_toolbox,by=c("gridcell_id","year"))
    mhw_df <- mhw_df[,-which(names(mhw_df) %in% c("X.x","sst.y","X.y"))]
    mhw_df_toolbox  <- mhw_df_toolbox [,-which(names(mhw_df_toolbox ) %in% c("X.x","sst.y","X.y"))]
    names(mhw_df)[which(names(mhw_df)=="sst.x")] <- "sst"    
    names(mhw_df_toolbox)[c(3:8)] <- paste0( names(mhw_df_toolbox)[c(3:8)],"_toolbox")
   
    #ntl
    ntl_df <- ntl_df %>%
        pivot_longer(cols = starts_with("X"),
                    names_to = "year",
                    values_to = "ntl")
        ntl_df$year <- as.numeric(gsub("X", "", ntl_df$year))
        names(ntl_df)[1] <- "gridcell_id"
        ntl_df$sensor <- "VIIRS"  

        ntl_df_pre2013 <- ntl_df_pre2013[,-c(1)]
        names(ntl_df_pre2013) <- c("gridcell_id","id","year","ntl")
        ntl_df_pre2013$sensor <- "DMSP"    
    countries <- countries[,c(1,11:15)]
    names(countries)[1] <- "gridcell_id"

    ntl_df <- rbind(ntl_df,ntl_df_pre2013)
    mcn <- merge(ntl_df,mhw_df,by=c("gridcell_id","year"),all=TRUE)
    mcn <- merge(mcn,mhw_df_toolbox,by=c("gridcell_id","year"),all=TRUE)
    
    
    any(table(mcn$unique_id)>1)
    mcn <- merge(mcn,sst_bins,by=c("gridcell_id","year"),all=TRUE)

    
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

        mangrove_df$holes <- (mangrove_df$holes + 2)
        mangrove_df$holes_size <- mangrove_df$h_area / mangrove_df$holes
        #hist(mangrove_df$holes_size)
        mangrove_df$gap_density <- (mangrove_df$holes) / mangrove_df$mangrove_area
        
        # mangrove_df %>% filter(is.finite(gap_density)) %>% select(gap_density) %>% min()
        # mangrove_df %>% filter(is.finite(holes )) %>% select(holes) %>% hist()
        # mangrove_df %>% filter(is.finite(holes), holes <0 ) %>% group_by(gridcell_id) %>% slice(1) %>% select(mangrove_area,year,gridcell_id)
        
        # ggplot(mangrove_df[which(mangrove_df$holes<0),])+
        # geom_point(aes(x=holes,y=mangrove_area))
        # #mangrove_df %>% filter(is.finite(holes )) %>% select(mangrove_area) %>% min()
        
        # min(mangrove_df$gap_density,na.rm=TRUE)
        
        df_mangroves_change <- mangrove_df %>%
        arrange(gridcell_id,year)  %>% group_by(gridcell_id) %>%
        mutate(holes_size  = h_area/ holes,
                patch_size = mangrove_area / np,
                lag_pafrac=dplyr::lag(pafrac),
                lag_np=dplyr::lag(np),
                lag_holes=dplyr::lag(holes),
                lag_holes_size=lag(holes_size),
                lag_gap_density=lag(gap_density),
                lag2_gap_density=lag(gap_density,2),
                lag3_gap_density=lag(gap_density,3)) %>% 
        mutate(annual_area_change =mangrove_area / dplyr::lag(mangrove_area)-1,
                annual_np_change =np / dplyr::lag(np)-1,
                annual_patchsize_change = patch_size / dplyr::lag(patch_size)-1,
                annual_gap_density_change = gap_density / dplyr::lag(gap_density)-1,
                annual_lag_gap_density_change = lag_gap_density / dplyr::lag(lag_gap_density)-1,
                annual_lag2_gap_density_change = lag2_gap_density / dplyr::lag(lag2_gap_density)-1,
                annual_holes_change = holes / dplyr::lag(holes)-1,
                annual_holesize_change = holes_size / dplyr::lag(holes_size)-1)
        
        # # Calculate the annual change in mangrove cover percentage
        # df_mangroves_change <- df_mangroves_grouped %>%
        # mutate(annual_area_change =mangrove_area / dplyr::lag(mangrove_area)-1)

        # glimpse(df_mangroves_change)

        # df_mangroves_change <- df_mangroves_change %>%
        # mutate(annual_np_change =np / dplyr::lag(np)-1)

        # df_mangroves_change$patch_size <- df_mangroves_change$mangrove_area / df_mangroves_change$np
        
        # df_mangroves_change <- df_mangroves_change %>%
        # mutate(annual_patchsize_change = patch_size / dplyr::lag(patch_size)-1)

        #df_mangroves_change <- df_mangroves_change %>%
        #mutate(annual_holes_change = holes / dplyr::lag(holes)-1)

        # Print the dataset with annual change
        glimpse(df_mangroves_change)

        df_mangroves_change[which(df_mangroves_change$year %in% c(1996,2007,2015)),which(names(df_mangroves_change) %in% c("annual_area_change","annual_np_change","annual_patchsize_change","annual_lag3_gap_density_change","annual_lag2_gap_density_change","annual_lag_gap_density_change","annual_gap_density_change","annual_holesize_change"))] <-NA #Because the data has gaps


        
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
    mcn3$mhw_FREQ <- mcn3$mhw_freq
    mcn3$mhw_DUR <- mcn3$mhw_dur
    mcn3$mhw_INT[mcn3$mhw==0] <- 0
    mcn3$mhw_INT_anom[mcn3$mhw==0] <- 0
    mcn3$mhw_FREQ[mcn3$mhw==0] <- 0
    mcn3$mhw_DUR[mcn3$mhw==0] <- 0
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
    #glimpse(sst_anom_bins_hr01)
    #mcn4 <- merge(mcn4,sst_anom_bins_hr01,by=c("gridcell_id","year"),all=TRUE)
    #mcn4 <- merge(mcn4,sst_anom_bins_mr05,by=c("gridcell_id","year"),all=TRUE)
    
    #mcn4 <- mcn4[,-which(names(mcn4) %in% c("X","X.x","X.y"))] 

    
    mcn4$logGDPpc <- log(mcn4$Sum_GDP/mcn4$Population_Count)
    mcn4$logGDPpc_50 <- log(mcn4$Sum_GDP_50km/mcn4$Population_Count_50km)
    
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

    mean_dry <- aggregate(Mean_Precipitation~gridcell_id,FUN="mean",data=mcn4[which(mcn4$year %in% c(1990:2022)),])
    glimpse(mean_dry)
    names(mean_dry)[2] <- "preci_mean0020"
    q_dry <- quantile(mean_dry$preci_mean0020,probs=0.5)
    mean_dry$dry <- 1
    mean_dry$dry[which(mean_dry$preci_mean0020>q_dry)] <- 0
    mcn4 <- merge(mcn4,mean_dry,by="gridcell_id",all=T)

    #mcn4 <- mcn4[,-which(names(mcn4) %in% c("X","X.x","X.y"))] 
    glimpse(mcn4)

    mcn4$logSumGDP_50 <- log(mcn4$Sum_GDP_50km)
    mcn4$logSumGDP <- log(mcn4$Sum_GDP)
    mcn4$logPop <- log(mcn4$Population_Count)
    mcn4$logPop_50 <- log(mcn4$Population_Count_50km)
    mcn4$logGDPpc <- log(mcn4$Sum_GDP/mcn4$Population_Count)

  #
#mcn4 <- write.csv(mcn4,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023.csv")

mcn2020 <- mcn4 %>% filter(year==2020) %>% filter(mangrove_area>0) %>% filter(is.na(R5))
                unique(mcn2020$countrycode)
                unique(paste0(mcn2020$Latitude,",",mcn2020$Longitude))
                mcn4$latlot <- paste0(mcn4$Latitude,",",mcn4$Longitude)
                mcn2020$latlot <- paste0(mcn2020$Latitude,",",mcn2020$Longitude)
        mcn2020$latlot[which(is.na(mcn2020$R5))] 
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
# ggplot(mcn4) + geom_point(aes(x=log(holes),y=log(h_area/holes),color=year,size=mangrove_area))

# ggplot(mcn4) + geom_point(aes(x=annual_holes_change,y=annual_area_change,color=year,size=mangrove_area))+xlim(c(-1,1))+ylim(c(-1,1))


#mcn4 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_aug2023.csv")

mcn4<- mcn4 %>%
  group_by(gridcell_id) %>%
  mutate(mangrove_area_demeaned = mangrove_area - mean(mangrove_area, na.rm = TRUE),
  sst_hot_demeaned = sst_hottest - mean(sst_hottest, na.rm = TRUE),
  log_mangrove_area_demeaned = log(mangrove_area) - mean(log(mangrove_area), na.rm = TRUE),
  mean_log_mangrove_area = mean(log(mangrove_area), na.rm = TRUE),
  mean_mangrove_area = mean((mangrove_area), na.rm = TRUE)) %>%
  ungroup()
mcn4$logSal <- log(mcn4$Mean_Salinity)
mcn4$preci <- mcn4$Mean_Precipitation
mcn4$pop_50 <- mcn4$Population_Count_50km
mcn4$pop <- mcn4$Population_Count
mcn4$patch_density_avg <- mcn4$np/mcn4$mean_mangrove_area
mcn4$gap_density_avg <- mcn4$holes/mcn4$mean_mangrove_area
mcn4$lag_patch_density_avg <- mcn4$lag_np/mcn4$mean_mangrove_area
mcn4$lag_gap_density_avg <- mcn4$lag_holes/mcn4$mean_mangrove_area


#write.csv(mcn4,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023.csv")
#mcn4 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
glimpse(mcn4)
glimpse(mangroves_gdp_buffers)



reshaped_df <- mangroves_gdp_buffers %>%
  unite("buffer_col", "buffer", sep = "buffer") %>% # Create column names like "gdp_bufferX"
  spread(key = buffer_col, value = gdp) # Convert to wide format

# View the reshaped dataframe
head(reshaped_df)
names(reshaped_df)[4:9] <- paste0("gdp",names(reshaped_df)[4:9])
names(reshaped_df)[2] <- "gridcell_id"

df_collapsed <- reshaped_df %>%
  # Convert NA values to 0
  mutate(across(c(gdp0, gdp10, gdp100, gdp300, gdp50, gdp500), ~replace(., is.na(.), 0))) %>%
  # Group by gridcell_id and year and sum other columns
  group_by(gridcell_id, year) %>%
  summarise(
    gdp0 = sum(gdp0),
    gdp10 = sum(gdp10),
    gdp50 = sum(gdp50),
    gdp100 = sum(gdp100),
    gdp300 = sum(gdp300),
    gdp500 = sum(gdp500),
    .groups = "drop" # Drop grouping for final result
  ) %>%
  # Convert 0 values back to NA
  mutate(across(c(gdp0, gdp10, gdp50, gdp100, gdp300, gdp500), ~replace(., . == 0, NA)))

glimpse(df_collapsed)
df_collapsed %>% filter(gridcell_id==1)
mcn5 <- merge(mcn4,df_collapsed,by=c("year","gridcell_id"),all=TRUE)
glimpse(mcn5)
ggplot(mcn5,aes(x=Sum_GDP_50km,y=gdp50))+geom_point()+geom_abline()

write.csv(mcn5,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023_buffers.csv")

mcn <- mcn5
non_char_df <- mcn %>% 
  select(where(~ !is.character(.))) %>%  # Select non-character columns
  names() %>%                            # Get column names
  setdiff(., c("year", "countrycode"))   # Remove 'year' and 'countrycode' from the names


char_df <- mcn %>% 
  select_if(~ is.character(.)) %>% names()%>%                            # Get column names
  setdiff(., c("year", "countrycode"))   # Remove 'year' and 'countrycode' from the names


mcn_country <- mcn %>% filter(!is.na(countrycode)) %>%
  group_by(countrycode, year) %>%
  mutate(weights = exp(mean_mangrove_area) / sum(exp(mean_mangrove_area), na.rm=TRUE)) %>%
  summarise(
    mangrove_area = sum(mangrove_area, na.rm=TRUE),
    across(all_of(non_char_df), ~sum(. * weights, na.rm=TRUE)),
    across(all_of(char_df), first)
  ) %>%
  ungroup()

write.csv(mcn_country,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mcn_country_15sept2023.csv")
