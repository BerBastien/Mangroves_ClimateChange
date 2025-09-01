library("readxl")
metareg_es_prov <- read_excel("Data\\ValueFunctions.xlsx",sheet="Provisioning")
metareg_es_cult <- read_excel("Data\\ValueFunctions.xlsx",sheet="Cultural")
metareg_es_regu <- read_excel("Data\\ValueFunctions.xlsx",sheet="Regulating")

b0_prov <- metareg_es_prov[27,2]
b_ln.mangrove.ha_prov <- metareg_es_prov[28,2]
b_ln.gdp.pcap_prov <- metareg_es_prov[29,2]
b_ln.pop_prov <- metareg_es_prov[30,2]

b0_cult <- metareg_es_cult[27,2]
b_ln.mangrove.ha_cult <- metareg_es_cult[28,2]
b_ln.gdp.pcap_cult <- metareg_es_cult[29,2]
b_ln.pop_cult<- metareg_es_cult[30,2]

b0_regu <- metareg_es_regu[27,2]
b_ln.mangrove.ha_regu <- metareg_es_regu[28,2]
b_ln.gdp.pcap_regu <- metareg_es_regu[29,2]
b_ln.pop_regu <- metareg_es_regu[30,2]


    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP570_Sept2024.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Onlygdp_SSP570_Sept2024.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP570_Sept2024.csv")
    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)

    glimpse(scen_allforcings)
    library("dplyr")
    scen_allforcings %>% filter(year==2030,countrycode=="MEX") %>% dplyr::select("gdppc2")
    
    grid_with_countries_ALL <- read.csv("Data/grid_with_specific_countries_FINAL.csv")
    glimpse(grid_with_countries_ALL)

    scen_allforcings <- scen_allforcings %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_allforcings$countrycode_old <-scen_allforcings$countrycode
    scen_allforcings$countrycode <- scen_allforcings$iso_a3

    any(scen_allforcings$countrycode=="NLD")
    any(scen_allforcings$countrycode_old=="NLD")

    types <- c("provision", "cultural","regulation")
    b0_values <- list(b0_prov, b0_cult, b0_regu)
    b_ln.mangrove.ha_values <- list(b_ln.mangrove.ha_prov, b_ln.mangrove.ha_cult, b_ln.mangrove.ha_regu)
    b_ln.gdp.pcap_values <- list(b_ln.gdp.pcap_prov, b_ln.gdp.pcap_cult, b_ln.gdp.pcap_regu)
    b_ln.pop_values <- list(b_ln.pop_prov, b_ln.pop_cult, b_ln.pop_regu)
    list_of_dfs <- list()
    for (i in seq_along(types)) {
        temp_df <- scen_allforcings
        #temp_df$dummy_ES <- values[[i]]
        temp_df$b0 <- as.double(b0_values[[i]][[1]])
        temp_df$b_ln.mangrove.ha <- as.double(b_ln.mangrove.ha_values[[i]][[1]])
        temp_df$b_ln.gdp.pcap <- as.double(b_ln.gdp.pcap_values[[i]][[1]])
        temp_df$b_ln.pop <- as.double(b_ln.pop_values[[i]][[1]])
        temp_df$type <- types[i]
        list_of_dfs[[i]] <- temp_df
    }

    scen_allforcings_allES <- do.call(rbind, list_of_dfs)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 +
                                                b_ln.gdp.pcap*log(gdppc5) + 
                                                b_ln.pop * log(POP_SSP5_50km) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100) 
    
    
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp370_Sept2024.csv")
    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc3 > 0)
    
    benefit_perha_val <- scen_allforcings_allES %>% filter(!is.na(benefits_perha) & is.finite(benefits_perha) & year==2026 & forcing=="both") %>% select(benefits_perha) %>% unlist()
    
    scen_allforcings_allES %>% group_by(type) %>% filter(!is.na(benefits_perha) & 
                            is.finite(benefits_perha) & year==2026 & forcing=="both") %>% 
                            #select(benefits_perha) %>% unlist()
                            summarise(mean_benefit_perha = mean(benefits_perha, na.rm=TRUE),
                                        median_benefit_perha = median(benefits_perha, na.rm=TRUE))
    ggplot(scen_allforcings_allES %>% filter(year %in% c(2020+seq(1:16)*5),countrycode=="IND",type=="cultural")) + 
    geom_point(aes(x=mangrove_area_future_loss*100,y=benefit_change/10^6,shape=forcing,color=log(GDP_SSP5/POP_SSP5))) + theme_bw()+
    ylab("Benefit Change (million USD)") + 
    labs(color="Log GDP",shape="Forcing")+xlab("Mangrove Area Change (ha)")+
    scale_color_scico()


    


        both_df <- scen_allforcings_allES %>% 
        filter(forcing == "both")

        onlygdp_df <- scen_allforcings_allES %>% 
        filter(forcing == "onlygdp")

        # Step 2: Join the two dataframes
        combined_df <- left_join(both_df, onlygdp_df, 
                                by = c("gridcell_id", "year", "type"), 
                                suffix = c("", "_onlygdp"))

        # Step 3: Subtract benefit_change and retain other values from both_df
        result_df <- combined_df %>% 
        mutate(dif_benefit_change = benefit_change -  benefit_change_onlygdp) %>%
        select(-ends_with("_onlygdp"))

        scen_allforcings_allES_dif <- result_df
    glimpse(scen_allforcings_allES_dif)
    unique(scen_allforcings_allES_dif$countrycode)
    write.csv(scen_allforcings_allES_dif,file="Data/projections/scen_allforcings_allES_dif_ssp570_Sept2024.csv")
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp570_Sept2024.csv")

    scen_allforcings_allES_country <- scen_allforcings_allES %>% group_by(countrycode,year,forcing,type) %>%
                    summarise(
                        benefit_change = sum(benefit_change,na.rm=TRUE),
                        benefit_future = sum(benefits_future,na.rm=TRUE),
                        R5 = first(R5),
                        POP_SSP5 = sum(POP_SSP5,na.rm=TRUE),
                        GDP_SSP5 = sum(GDP_SSP5,na.rm=TRUE),
                        GDP_Country_ssp5 = first(GDP_Country_ssp5),
                        Pop_Country_ssp5 = first(Pop_Country_ssp5),
                        mangrove_area_future_loss = sum(mangrove_area_future_loss,na.rm=TRUE),
                        mangrove_area = sum(mangrove_area,na.rm=TRUE)
                    ) %>%
                    ungroup()


    diff_df_country_type <- scen_allforcings_allES_country %>%
    filter(forcing %in% c("both", "onlygdp")) %>%
    group_by(year, countrycode,type) %>%
    summarise(
        diff_benefit_change = benefit_change[forcing == "both"] - benefit_change[forcing == "onlygdp"],
        benefit_future =  benefit_future[forcing == "onlygdp"],
        diff_mangrove_area_future_loss =mangrove_area_future_loss[forcing == "both"] - mangrove_area_future_loss[forcing == "onlygdp"],
        benefit_change_both = benefit_change[forcing == "both"],
        benefit_change_onlygdp = benefit_change[forcing == "onlygdp"],
        mangrove_area_future_loss_both = mangrove_area_future_loss[forcing == "both"],
        mangrove_area_future_loss_onlygdp = mangrove_area_future_loss[forcing == "onlygdp"],
        mangrove_area_future = mangrove_area[forcing == "both"],
        POP_SSP5 = POP_SSP5[forcing == "both"],
        GDP_SSP5 = GDP_SSP5[forcing == "both"],
        GDP_Country_ssp5 = GDP_Country_ssp5[forcing == "both"],
        Pop_Country_ssp5 = Pop_Country_ssp5[forcing == "both"],
        R5 = R5[forcing == "both"]
    ) %>%
    ungroup()

    library(ggrepel)
    `%notin%` = Negate(`%in%`)
    
    ggplot(diff_df_country_type)+
    geom_point(aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/benefit_future, color=R5, shape=type, alpha=exp((year-2020)/100)))+ 
    #geom_point(aes(x=diff_mangrove_area_future_loss,y=diff_benefit_change, color=R5, shape=type, size= mangrove_area_future_loss_both+mangrove_area_future, alpha=(year-2020)/100)) + 
    geom_line(aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/benefit_future, color=R5,group=interaction(countrycode,type)),alpha=0.3) + 
    #geom_point(data=diff_df_country_type %>% filter(year %in% c(2030,2050,2080,2100)),aes(x=diff_mangrove_area_future_loss,y=diff_benefit_change, color=R5, shape=type)) + 
    scale_color_scico_d(palette="batlow",end=0.7)+theme_bw()+
    scale_alpha_continuous(range = c(0, 0.8))+
    geom_text_repel(data=diff_df_country_type%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year ==2100),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/benefit_future, color=R5, label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Region",shape="ES Type")+xlab("Mangrove Area Change (ha)")
    
    
    BenefitChange_2100 <- ggplot(diff_df_country_type %>% filter(countrycode %notin% c("IND","IDN")))+
    geom_point(aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, shape=type, alpha=exp((year-2020)/100)))+ 
    #geom_point(aes(x=diff_mangrove_area_future_loss,y=diff_benefit_change, color=R5, shape=type, size= mangrove_area_future_loss_both+mangrove_area_future, alpha=(year-2020)/100)) + 
    geom_line(aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5,group=interaction(countrycode,type)),alpha=0.3) + 
    #geom_point(data=diff_df_country_type %>% filter(year %in% c(2030,2050,2080,2100)),aes(x=diff_mangrove_area_future_loss,y=diff_benefit_change, color=R5, shape=type)) + 
    scale_color_scico_d(palette="batlow",end=0.7)+theme_bw()+
    scale_alpha_continuous(range = c(0, 0.8))+
    geom_text_repel(data=diff_df_country_type%>% filter(countrycode %notin% c("IND","IDN")) %>% filter(year ==2100),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Region",shape="ES Type")+xlab("Mangrove Area Change (ha)")

    BenefitChange_2100
    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type_ssp570_brander_corrected_countries_Sept2024.csv")
    long_df<-diff_df_country_type
    write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp570_corrected_countries_Sept2024.csv")
    unique(diff_df_country_type$countrycode)
    #ggsave("Figures/Draft2/BenefitChange_2100.png",dpi=600)    

    glimpse(diff_df_country_type)
    diff_df_country <- diff_df_country_type %>% group_by(year,countrycode) %>%
                        summarise(benefit_change = diff_benefit_change[type=="provision"]  + diff_benefit_change[type=="regulation"] + diff_benefit_change[type=="cultural"], 
                                    R5 = R5[type=="provision"],
                                    POP_SSP5 = POP_SSP5[type=="provision"],
                                    GDP_SSP5 = GDP_SSP5[type=="provision"],
                                    GDP_Country_ssp5 = GDP_Country_ssp5[type=="provision"],
                                    Pop_Country_ssp5 = Pop_Country_ssp5[type=="provision"],
                                    diff_mangrove_area_future_loss =diff_mangrove_area_future_loss[type=="provision"]) %>% ungroup()
    glimpse(diff_df_country)
    diff_country_total <- diff_df_country
    write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp570_brander_corrected_countries_Sept2024.csv")
    
    

######
######

### SSP1

    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP170_Sept2024.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Onlygdp_SSP170_Sept2024.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP170_Sept2024.csv")
    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)
    

    grid_with_countries_ALL <- read.csv("Data/grid_with_specific_countries_FINAL.csv")
    scen_allforcings <- scen_allforcings %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_allforcings$countrycode_old <-scen_allforcings$countrycode
    scen_allforcings$countrycode <- scen_allforcings$iso_a3

    types <- c("provision", "cultural","regulation")
    b0_values <- list(b0_prov, b0_cult, b0_regu)
    b_ln.mangrove.ha_values <- list(b_ln.mangrove.ha_prov, b_ln.mangrove.ha_cult, b_ln.mangrove.ha_regu)
    b_ln.gdp.pcap_values <- list(b_ln.gdp.pcap_prov, b_ln.gdp.pcap_cult, b_ln.gdp.pcap_regu)
    b_ln.pop_values <- list(b_ln.pop_prov, b_ln.pop_cult, b_ln.pop_regu)
    list_of_dfs <- list()
    for (i in seq_along(types)) {
        temp_df <- scen_allforcings
        #temp_df$dummy_ES <- values[[i]]
        temp_df$b0 <- as.double(b0_values[[i]][[1]])
        temp_df$b_ln.mangrove.ha <- as.double(b_ln.mangrove.ha_values[[i]][[1]])
        temp_df$b_ln.gdp.pcap <- as.double(b_ln.gdp.pcap_values[[i]][[1]])
        temp_df$b_ln.pop <- as.double(b_ln.pop_values[[i]][[1]])
        temp_df$type <- types[i]
        list_of_dfs[[i]] <- temp_df
    }

    scen_allforcings_allES <- do.call(rbind, list_of_dfs)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 +
                                                b_ln.gdp.pcap*log(gdppc1) + 
                                                b_ln.pop * log(POP_SSP1_50km) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100) 
    
    
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp370_Sept2024.csv")
    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc1 > 0)
    
    benefit_perha_val <- scen_allforcings_allES %>% filter(!is.na(benefits_perha) & is.finite(benefits_perha) & year==2026 & forcing=="both") %>% select(benefits_perha) %>% unlist()
    
    scen_allforcings_allES %>% group_by(type) %>% filter(!is.na(benefits_perha) & 
                            is.finite(benefits_perha) & year==2026 & forcing=="both") %>% 
                            #select(benefits_perha) %>% unlist()
                            summarise(mean_benefit_perha = mean(benefits_perha, na.rm=TRUE),
                                        median_benefit_perha = median(benefits_perha, na.rm=TRUE))
    

    


        both_df <- scen_allforcings_allES %>% 
        filter(forcing == "both")

        onlygdp_df <- scen_allforcings_allES %>% 
        filter(forcing == "onlygdp")

        # Step 2: Join the two dataframes
        combined_df <- left_join(both_df, onlygdp_df, 
                                by = c("gridcell_id", "year", "type"), 
                                suffix = c("", "_onlygdp"))

        # Step 3: Subtract benefit_change and retain other values from both_df
        result_df <- combined_df %>% 
        mutate(dif_benefit_change = benefit_change -  benefit_change_onlygdp) %>%
        select(-ends_with("_onlygdp"))

        scen_allforcings_allES_dif <- result_df
    
    write.csv(scen_allforcings_allES_dif,file="Data/projections/scen_allforcings_allES_dif_ssp170_Sept2024.csv")
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp170_Sept2024.csv")

    scen_allforcings_allES_country <- scen_allforcings_allES %>% group_by(countrycode,year,forcing,type) %>%
                    summarise(
                        benefit_change = sum(benefit_change,na.rm=TRUE),
                        benefit_future = sum(benefits_future,na.rm=TRUE),
                        R5 = first(R5),
                        POP_SSP1 = sum(POP_SSP1,na.rm=TRUE),
                        GDP_SSP1 = sum(GDP_SSP1,na.rm=TRUE),
                        GDP_Country_ssp1 = first(GDP_Country_ssp1),
                        Pop_Country_ssp1 = first(Pop_Country_ssp1),
                        mangrove_area_future_loss = sum(mangrove_area_future_loss,na.rm=TRUE),
                        mangrove_area = sum(mangrove_area,na.rm=TRUE)
                    ) %>%
                    ungroup()


    diff_df_country_type <- scen_allforcings_allES_country %>%
    filter(forcing %in% c("both", "onlygdp")) %>%
    group_by(year, countrycode,type) %>%
    summarise(
        diff_benefit_change = benefit_change[forcing == "both"] - benefit_change[forcing == "onlygdp"],
        benefit_future =  benefit_future[forcing == "onlygdp"],
        diff_mangrove_area_future_loss =mangrove_area_future_loss[forcing == "both"] - mangrove_area_future_loss[forcing == "onlygdp"],
        benefit_change_both = benefit_change[forcing == "both"],
        benefit_change_onlygdp = benefit_change[forcing == "onlygdp"],
        mangrove_area_future_loss_both = mangrove_area_future_loss[forcing == "both"],
        mangrove_area_future_loss_onlygdp = mangrove_area_future_loss[forcing == "onlygdp"],
        mangrove_area_future = mangrove_area[forcing == "both"],
        POP_SSP1 = POP_SSP1[forcing == "both"],
        GDP_SSP1 = GDP_SSP1[forcing == "both"],
        GDP_Country_ssp1 = GDP_Country_ssp1[forcing == "both"],
        Pop_Country_ssp1 = Pop_Country_ssp1[forcing == "both"],
        R5 = R5[forcing == "both"]
    ) %>%
    ungroup()

    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type_ssp170_brander_corrected_countries_Sept2024.csv")
    long_df<-diff_df_country_type
    write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp170_corrected_countries_Sept2024.csv")

    #ggsave("Figures/Draft2/BenefitChange_2100.png",dpi=600)    

    glimpse(diff_df_country_type)
    diff_df_country <- diff_df_country_type %>% group_by(year,countrycode) %>%
                        summarise(benefit_change = diff_benefit_change[type=="provision"]  + diff_benefit_change[type=="regulation"] + diff_benefit_change[type=="cultural"], 
                                    R5 = R5[type=="provision"],
                                    POP_SSP1 = POP_SSP1[type=="provision"],
                                    GDP_SSP1 = GDP_SSP1[type=="provision"],
                                    GDP_Country_ssp1 = GDP_Country_ssp1[type=="provision"],
                                    Pop_Country_ssp1 = Pop_Country_ssp1[type=="provision"],
                                    diff_mangrove_area_future_loss =diff_mangrove_area_future_loss[type=="provision"]) %>% ungroup()
    diff_country_total <- diff_df_country
    write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp170_brander_corrected_countries_Sept2024.csv")
    
    

######
######

### SSP2

    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP270_Sept2024.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Onlygdp_SSP270_Sept2024.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP270_Sept2024.csv")
    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)
    

    grid_with_countries_ALL <- read.csv("Data/grid_with_specific_countries_FINAL.csv")
    scen_allforcings <- scen_allforcings %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_allforcings$countrycode_old <-scen_allforcings$countrycode
    scen_allforcings$countrycode <- scen_allforcings$iso_a3


    types <- c("provision", "cultural","regulation")
    b0_values <- list(b0_prov, b0_cult, b0_regu)
    b_ln.mangrove.ha_values <- list(b_ln.mangrove.ha_prov, b_ln.mangrove.ha_cult, b_ln.mangrove.ha_regu)
    b_ln.gdp.pcap_values <- list(b_ln.gdp.pcap_prov, b_ln.gdp.pcap_cult, b_ln.gdp.pcap_regu)
    b_ln.pop_values <- list(b_ln.pop_prov, b_ln.pop_cult, b_ln.pop_regu)
    list_of_dfs <- list()
    for (i in seq_along(types)) {
        temp_df <- scen_allforcings
        #temp_df$dummy_ES <- values[[i]]
        temp_df$b0 <- as.double(b0_values[[i]][[1]])
        temp_df$b_ln.mangrove.ha <- as.double(b_ln.mangrove.ha_values[[i]][[1]])
        temp_df$b_ln.gdp.pcap <- as.double(b_ln.gdp.pcap_values[[i]][[1]])
        temp_df$b_ln.pop <- as.double(b_ln.pop_values[[i]][[1]])
        temp_df$type <- types[i]
        list_of_dfs[[i]] <- temp_df
    }

    scen_allforcings_allES <- do.call(rbind, list_of_dfs)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 +
                                                b_ln.gdp.pcap*log(gdppc2) + 
                                                b_ln.pop * log(POP_SSP2_50km) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100) 
    
    
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp270_Sept2024.csv")
    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc2 > 0)
    
    benefit_perha_val <- scen_allforcings_allES %>% filter(!is.na(benefits_perha) & is.finite(benefits_perha) & year==2026 & forcing=="both") %>% select(benefits_perha) %>% unlist()
    
    scen_allforcings_allES %>% group_by(type) %>% filter(!is.na(benefits_perha) & 
                            is.finite(benefits_perha) & year==2026 & forcing=="both") %>% 
                            #select(benefits_perha) %>% unlist()
                            summarise(mean_benefit_perha = mean(benefits_perha, na.rm=TRUE),
                                        median_benefit_perha = median(benefits_perha, na.rm=TRUE))
    

    


        both_df <- scen_allforcings_allES %>% 
        filter(forcing == "both")

        onlygdp_df <- scen_allforcings_allES %>% 
        filter(forcing == "onlygdp")

        # Step 2: Join the two dataframes
        combined_df <- left_join(both_df, onlygdp_df, 
                                by = c("gridcell_id", "year", "type"), 
                                suffix = c("", "_onlygdp"))

        # Step 3: Subtract benefit_change and retain other values from both_df
        result_df <- combined_df %>% 
        mutate(dif_benefit_change = benefit_change -  benefit_change_onlygdp) %>%
        select(-ends_with("_onlygdp"))

        scen_allforcings_allES_dif <- result_df
    
    write.csv(scen_allforcings_allES_dif,file="Data/projections/scen_allforcings_allES_dif_ssp270_Sept2024.csv")
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp270_Sept2024.csv")

    scen_allforcings_allES_country <- scen_allforcings_allES %>% group_by(countrycode,year,forcing,type) %>%
                    summarise(
                        benefit_change = sum(benefit_change,na.rm=TRUE),
                        benefit_future = sum(benefits_future,na.rm=TRUE),
                        R5 = first(R5),
                        POP_SSP2 = sum(POP_SSP2,na.rm=TRUE),
                        GDP_SSP2 = sum(GDP_SSP2,na.rm=TRUE),
                        GDP_Country_ssp2 = first(GDP_Country_ssp2),
                        Pop_Country_ssp2 = first(Pop_Country_ssp2),
                        mangrove_area_future_loss = sum(mangrove_area_future_loss,na.rm=TRUE),
                        mangrove_area = sum(mangrove_area,na.rm=TRUE)
                    ) %>%
                    ungroup()


    diff_df_country_type <- scen_allforcings_allES_country %>%
    filter(forcing %in% c("both", "onlygdp")) %>%
    group_by(year, countrycode,type) %>%
    summarise(
        diff_benefit_change = benefit_change[forcing == "both"] - benefit_change[forcing == "onlygdp"],
        benefit_future =  benefit_future[forcing == "onlygdp"],
        diff_mangrove_area_future_loss =mangrove_area_future_loss[forcing == "both"] - mangrove_area_future_loss[forcing == "onlygdp"],
        benefit_change_both = benefit_change[forcing == "both"],
        benefit_change_onlygdp = benefit_change[forcing == "onlygdp"],
        mangrove_area_future_loss_both = mangrove_area_future_loss[forcing == "both"],
        mangrove_area_future_loss_onlygdp = mangrove_area_future_loss[forcing == "onlygdp"],
        mangrove_area_future = mangrove_area[forcing == "both"],
        POP_SSP2 = POP_SSP2[forcing == "both"],
        GDP_SSP2 = GDP_SSP2[forcing == "both"],
        GDP_Country_ssp2 = GDP_Country_ssp2[forcing == "both"],
        Pop_Country_ssp2 = Pop_Country_ssp2[forcing == "both"],
        R5 = R5[forcing == "both"]
    ) %>%
    ungroup()

    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type_ssp270_brander_corrected_countries_Sept2024.csv")
    long_df<-diff_df_country_type
    write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp270_corrected_countries_Sept2024.csv")

    #ggsave("Figures/Draft2/BenefitChange_2100.png",dpi=600)    

    glimpse(diff_df_country_type)
    diff_df_country <- diff_df_country_type %>% group_by(year,countrycode) %>%
                        summarise(benefit_change = diff_benefit_change[type=="provision"]  + diff_benefit_change[type=="regulation"] + diff_benefit_change[type=="cultural"], 
                                    R5 = R5[type=="provision"],
                                    POP_SSP2 = POP_SSP2[type=="provision"],
                                    GDP_SSP2 = GDP_SSP2[type=="provision"],
                                    GDP_Country_ssp2 = GDP_Country_ssp2[type=="provision"],
                                    Pop_Country_ssp2 = Pop_Country_ssp2[type=="provision"],
                                    diff_mangrove_area_future_loss =diff_mangrove_area_future_loss[type=="provision"]) %>% ungroup()
    diff_country_total <- diff_df_country
    write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp270_brander_corrected_countries_Sept2024.csv")
    
    

######
######

### SSP3

    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP370_Sept2024.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Onlygdp_SSP370_Sept2024.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP370_Sept2024.csv")
    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)
    
    grid_with_countries_ALL <- read.csv("Data/grid_with_specific_countries_FINAL.csv")
    scen_allforcings <- scen_allforcings %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_allforcings$countrycode_old <-scen_allforcings$countrycode
    scen_allforcings$countrycode <- scen_allforcings$iso_a3


    types <- c("provision", "cultural","regulation")
    b0_values <- list(b0_prov, b0_cult, b0_regu)
    b_ln.mangrove.ha_values <- list(b_ln.mangrove.ha_prov, b_ln.mangrove.ha_cult, b_ln.mangrove.ha_regu)
    b_ln.gdp.pcap_values <- list(b_ln.gdp.pcap_prov, b_ln.gdp.pcap_cult, b_ln.gdp.pcap_regu)
    b_ln.pop_values <- list(b_ln.pop_prov, b_ln.pop_cult, b_ln.pop_regu)
    list_of_dfs <- list()
    for (i in seq_along(types)) {
        temp_df <- scen_allforcings
        #temp_df$dummy_ES <- values[[i]]
        temp_df$b0 <- as.double(b0_values[[i]][[1]])
        temp_df$b_ln.mangrove.ha <- as.double(b_ln.mangrove.ha_values[[i]][[1]])
        temp_df$b_ln.gdp.pcap <- as.double(b_ln.gdp.pcap_values[[i]][[1]])
        temp_df$b_ln.pop <- as.double(b_ln.pop_values[[i]][[1]])
        temp_df$type <- types[i]
        list_of_dfs[[i]] <- temp_df
    }

    scen_allforcings_allES <- do.call(rbind, list_of_dfs)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 +
                                                b_ln.gdp.pcap*log(gdppc3) + 
                                                b_ln.pop * log(POP_SSP3_50km) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100) 
    
    
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp370_Sept2024.csv")
    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc3 > 0)
    
    benefit_perha_val <- scen_allforcings_allES %>% filter(!is.na(benefits_perha) & is.finite(benefits_perha) & year==2026 & forcing=="both") %>% select(benefits_perha) %>% unlist()
    
    scen_allforcings_allES %>% group_by(type) %>% filter(!is.na(benefits_perha) & 
                            is.finite(benefits_perha) & year==2026 & forcing=="both") %>% 
                            #select(benefits_perha) %>% unlist()
                            summarise(mean_benefit_perha = mean(benefits_perha, na.rm=TRUE),
                                        median_benefit_perha = median(benefits_perha, na.rm=TRUE))
    

    


        both_df <- scen_allforcings_allES %>% 
        filter(forcing == "both")

        onlygdp_df <- scen_allforcings_allES %>% 
        filter(forcing == "onlygdp")

        # Step 2: Join the two dataframes
        combined_df <- left_join(both_df, onlygdp_df, 
                                by = c("gridcell_id", "year", "type"), 
                                suffix = c("", "_onlygdp"))

        # Step 3: Subtract benefit_change and retain other values from both_df
        result_df <- combined_df %>% 
        mutate(dif_benefit_change = benefit_change -  benefit_change_onlygdp) %>%
        select(-ends_with("_onlygdp"))

        scen_allforcings_allES_dif <- result_df
    
    write.csv(scen_allforcings_allES_dif,file="Data/projections/scen_allforcings_allES_dif_ssp370_Sept2024.csv")
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp370_Sept2024.csv")

    scen_allforcings_allES_country <- scen_allforcings_allES %>% group_by(countrycode,year,forcing,type) %>%
                    summarise(
                        benefit_change = sum(benefit_change,na.rm=TRUE),
                        benefit_future = sum(benefits_future,na.rm=TRUE),
                        R5 = first(R5),
                        POP_SSP3 = sum(POP_SSP3,na.rm=TRUE),
                        GDP_SSP3 = sum(GDP_SSP3,na.rm=TRUE),
                        GDP_Country_ssp3 = first(GDP_Country_ssp3),
                        Pop_Country_ssp3 = first(Pop_Country_ssp3),
                        mangrove_area_future_loss = sum(mangrove_area_future_loss,na.rm=TRUE),
                        mangrove_area = sum(mangrove_area,na.rm=TRUE)
                    ) %>%
                    ungroup()


    diff_df_country_type <- scen_allforcings_allES_country %>%
    filter(forcing %in% c("both", "onlygdp")) %>%
    group_by(year, countrycode,type) %>%
    summarise(
        diff_benefit_change = benefit_change[forcing == "both"] - benefit_change[forcing == "onlygdp"],
        benefit_future =  benefit_future[forcing == "onlygdp"],
        diff_mangrove_area_future_loss =mangrove_area_future_loss[forcing == "both"] - mangrove_area_future_loss[forcing == "onlygdp"],
        benefit_change_both = benefit_change[forcing == "both"],
        benefit_change_onlygdp = benefit_change[forcing == "onlygdp"],
        mangrove_area_future_loss_both = mangrove_area_future_loss[forcing == "both"],
        mangrove_area_future_loss_onlygdp = mangrove_area_future_loss[forcing == "onlygdp"],
        mangrove_area_future = mangrove_area[forcing == "both"],
        POP_SSP3 = POP_SSP3[forcing == "both"],
        GDP_SSP3 = GDP_SSP3[forcing == "both"],
        GDP_Country_ssp3 = GDP_Country_ssp3[forcing == "both"],
        Pop_Country_ssp3 = Pop_Country_ssp3[forcing == "both"],
        R5 = R5[forcing == "both"]
    ) %>%
    ungroup()

    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type_ssp370_brander_corrected_countries_Sept2024.csv")
    long_df<-diff_df_country_type
    write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp370_corrected_countries_Sept2024.csv")

    #ggsave("Figures/Draft2/BenefitChange_2100.png",dpi=600)    

    glimpse(diff_df_country_type)
    diff_df_country <- diff_df_country_type %>% group_by(year,countrycode) %>%
                        summarise(benefit_change = diff_benefit_change[type=="provision"]  + diff_benefit_change[type=="regulation"] + diff_benefit_change[type=="cultural"], 
                                    R5 = R5[type=="provision"],
                                    POP_SSP3 = POP_SSP3[type=="provision"],
                                    GDP_SSP3 = GDP_SSP3[type=="provision"],
                                    GDP_Country_ssp3 = GDP_Country_ssp3[type=="provision"],
                                    Pop_Country_ssp3 = Pop_Country_ssp3[type=="provision"],
                                    diff_mangrove_area_future_loss =diff_mangrove_area_future_loss[type=="provision"]) %>% ungroup()
    diff_country_total <- diff_df_country
    write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp370_brander_corrected_countries_Sept2024.csv")
    
    

######
######


### SSP4

    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP470_Sept2024.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Onlygdp_SSP470_Sept2024.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP470_Sept2024.csv")
    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)
    
    grid_with_countries_ALL <- read.csv("Data/grid_with_specific_countries_FINAL.csv")
    scen_allforcings <- scen_allforcings %>% left_join(grid_with_countries_ALL %>% mutate(gridcell_id=id),by="gridcell_id")
    scen_allforcings$countrycode_old <-scen_allforcings$countrycode
    scen_allforcings$countrycode <- scen_allforcings$iso_a3


    types <- c("provision", "cultural","regulation")
    b0_values <- list(b0_prov, b0_cult, b0_regu)
    b_ln.mangrove.ha_values <- list(b_ln.mangrove.ha_prov, b_ln.mangrove.ha_cult, b_ln.mangrove.ha_regu)
    b_ln.gdp.pcap_values <- list(b_ln.gdp.pcap_prov, b_ln.gdp.pcap_cult, b_ln.gdp.pcap_regu)
    b_ln.pop_values <- list(b_ln.pop_prov, b_ln.pop_cult, b_ln.pop_regu)
    list_of_dfs <- list()
    for (i in seq_along(types)) {
        temp_df <- scen_allforcings
        #temp_df$dummy_ES <- values[[i]]
        temp_df$b0 <- as.double(b0_values[[i]][[1]])
        temp_df$b_ln.mangrove.ha <- as.double(b_ln.mangrove.ha_values[[i]][[1]])
        temp_df$b_ln.gdp.pcap <- as.double(b_ln.gdp.pcap_values[[i]][[1]])
        temp_df$b_ln.pop <- as.double(b_ln.pop_values[[i]][[1]])
        temp_df$type <- types[i]
        list_of_dfs[[i]] <- temp_df
    }

    scen_allforcings_allES <- do.call(rbind, list_of_dfs)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 +
                                                b_ln.gdp.pcap*log(gdppc4) + 
                                                b_ln.pop * log(POP_SSP4_50km) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100) 
    
    
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp470_Sept2024.csv")
    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc4 > 0)
    
    benefit_perha_val <- scen_allforcings_allES %>% filter(!is.na(benefits_perha) & is.finite(benefits_perha) & year==2026 & forcing=="both") %>% select(benefits_perha) %>% unlist()
    
    scen_allforcings_allES %>% group_by(type) %>% filter(!is.na(benefits_perha) & 
                            is.finite(benefits_perha) & year==2026 & forcing=="both") %>% 
                            #select(benefits_perha) %>% unlist()
                            summarise(mean_benefit_perha = mean(benefits_perha, na.rm=TRUE),
                                        median_benefit_perha = median(benefits_perha, na.rm=TRUE))
    

    


        both_df <- scen_allforcings_allES %>% 
        filter(forcing == "both")

        onlygdp_df <- scen_allforcings_allES %>% 
        filter(forcing == "onlygdp")

        # Step 2: Join the two dataframes
        combined_df <- left_join(both_df, onlygdp_df, 
                                by = c("gridcell_id", "year", "type"), 
                                suffix = c("", "_onlygdp"))

        # Step 3: Subtract benefit_change and retain other values from both_df
        result_df <- combined_df %>% 
        mutate(dif_benefit_change = benefit_change -  benefit_change_onlygdp) %>%
        select(-ends_with("_onlygdp"))

        scen_allforcings_allES_dif <- result_df
    
    write.csv(scen_allforcings_allES_dif,file="Data/projections/scen_allforcings_allES_dif_ssp470_Sept2024.csv")
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp470_Sept2024.csv")

    scen_allforcings_allES_country <- scen_allforcings_allES %>% group_by(countrycode,year,forcing,type) %>%
                    summarise(
                        benefit_change = sum(benefit_change,na.rm=TRUE),
                        benefit_future = sum(benefits_future,na.rm=TRUE),
                        R5 = first(R5),
                        POP_SSP4 = sum(POP_SSP4,na.rm=TRUE),
                        GDP_SSP4 = sum(GDP_SSP4,na.rm=TRUE),
                        GDP_Country_ssp4 = first(GDP_Country_ssp4),
                        Pop_Country_ssp4 = first(Pop_Country_ssp4),
                        mangrove_area_future_loss = sum(mangrove_area_future_loss,na.rm=TRUE),
                        mangrove_area = sum(mangrove_area,na.rm=TRUE)
                    ) %>%
                    ungroup()


    diff_df_country_type <- scen_allforcings_allES_country %>%
    filter(forcing %in% c("both", "onlygdp")) %>%
    group_by(year, countrycode,type) %>%
    summarise(
        diff_benefit_change = benefit_change[forcing == "both"] - benefit_change[forcing == "onlygdp"],
        benefit_future =  benefit_future[forcing == "onlygdp"],
        diff_mangrove_area_future_loss =mangrove_area_future_loss[forcing == "both"] - mangrove_area_future_loss[forcing == "onlygdp"],
        benefit_change_both = benefit_change[forcing == "both"],
        benefit_change_onlygdp = benefit_change[forcing == "onlygdp"],
        mangrove_area_future_loss_both = mangrove_area_future_loss[forcing == "both"],
        mangrove_area_future_loss_onlygdp = mangrove_area_future_loss[forcing == "onlygdp"],
        mangrove_area_future = mangrove_area[forcing == "both"],
        POP_SSP4 = POP_SSP4[forcing == "both"],
        GDP_SSP4 = GDP_SSP4[forcing == "both"],
        GDP_Country_ssp4 = GDP_Country_ssp4[forcing == "both"],
        Pop_Country_ssp4 = Pop_Country_ssp4[forcing == "both"],
        R5 = R5[forcing == "both"]
    ) %>%
    ungroup()

    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type_ssp470_brander_corrected_countries_Sept2024.csv")
    long_df<-diff_df_country_type
    write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp470_corrected_countries_Sept2024.csv")

    #ggsave("Figures/Draft2/BenefitChange_2100.png",dpi=600)    

    glimpse(diff_df_country_type)
    diff_df_country <- diff_df_country_type %>% group_by(year,countrycode) %>%
                        summarise(benefit_change = diff_benefit_change[type=="provision"]  + diff_benefit_change[type=="regulation"] + diff_benefit_change[type=="cultural"], 
                                    R5 = R5[type=="provision"],
                                    POP_SSP4 = POP_SSP4[type=="provision"],
                                    GDP_SSP4 = GDP_SSP4[type=="provision"],
                                    GDP_Country_ssp4 = GDP_Country_ssp4[type=="provision"],
                                    Pop_Country_ssp4 = Pop_Country_ssp4[type=="provision"],
                                    diff_mangrove_area_future_loss =diff_mangrove_area_future_loss[type=="provision"]) %>% ungroup()
    diff_country_total <- diff_df_country
    write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp470_brander_corrected_countries_Sept2024.csv")
    
    

######
######