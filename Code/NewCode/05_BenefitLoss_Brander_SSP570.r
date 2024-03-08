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


    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\Proj_Area_Perc_OnlyCC_SSP570.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\Proj_Area_Perc_Onlygdp_SSP570.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\Proj_Area_Perc_both_SSP570.csv")
    glimpse(scen_arealoss_perc_both)
    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)
    glimpse(scen_allforcings)



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
    glimpse(scen_allforcings_allES)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 +
                                                b_ln.gdp.pcap*log(gdppc5) + 
                                                b_ln.pop * log(POP_SSP5_50km) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100) 
    glimpse(scen_allforcings_allES)
    
    ggplot(scen_allforcings_allES %>% filter(countrycode=="IDN",type=="cultural"))+
    geom_point(aes(x=year,y=benefit_change,color=forcing,group=interaction(forcing)))
    
    #write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp370.csv")
    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc3 > 0)
    
    benefit_perha_val <- scen_allforcings_allES %>% filter(!is.na(benefits_perha) & is.finite(benefits_perha) & year==2026 & forcing=="both") %>% select(benefits_perha) %>% unlist()
    mean(benefit_perha_val) # 24 456 $/ha
    median(benefit_perha_val) #4 898 $/ha
    #Wetlands in Fund:  280,000 $/km2 == 2 800 $/ha
    #De Groot, coastal wetlands: 19 384 400 $/km == 193 844 $/ha

    scen_allforcings_allES %>% group_by(type) %>% filter(!is.na(benefits_perha) & 
                            is.finite(benefits_perha) & year==2026 & forcing=="both") %>% 
                            #select(benefits_perha) %>% unlist()
                            summarise(mean_benefit_perha = mean(benefits_perha, na.rm=TRUE),
                                        median_benefit_perha = median(benefits_perha, na.rm=TRUE))
    # Wood/Here: 769 -- Raw Materias DeGroot: 358
    # Water/Here: 12 641 -- Water/DeGroot: 1 217
    # Fisheries/Here: 5 381 -- Food/DeGroot: 1 111
    # Coastal/Here: 9 766 -- Disturbance/DeGroot: 5 351
    ## Total/Here: 28 577 -- Pre-Total/DeGroot: 8 037 -- Total/DeGroot: 193 845 $/ha
    ## FUND Model:  2 800

    #De Groot 2012, Provisioning services: 2 998 $/ha
    #Here: wood + fisheries: 6 150 $/ha
    #De Groot 2012, Regulating services: 171 515 $/ha
    #Here - coastal: 171 515 $/ha

    glimpse(scen_allforcings_allES)
    
    ggplot(scen_allforcings_allES %>% filter(year %in% c(2020+seq(1:16)*5),countrycode=="IND",type=="cultural")) + 
    geom_point(aes(x=mangrove_area_future_loss*100,y=benefit_change/10^6,shape=forcing,color=log(GDP_SSP5/POP_SSP5))) + theme_bw()+
    ylab("Benefit Change (million USD)") + 
    labs(color="Log GDP",shape="Forcing")+xlab("Mangrove Area Change (ha)")+
    scale_color_scico()

    ggplot() + 
    geom_point(data=scen_allforcings_allES %>% filter(year %in% c(2100,2080,2050),countrycode=="MEX",type=="cultural"),aes(x=mangrove_area_future_loss*100,
                    #color=log(benefit_change/10^6),
                    color=forcing,shape=forcing,y=log(GDP_SSP5/POP_SSP5))) + theme_bw()+
    geom_line(data=scen_allforcings_allES%>%
                    arrange(gridcell_id, year) %>% filter(year %in% c(2020+seq(1:16)*10),countrycode=="MEX",type=="cultural"),
                    aes(x=mangrove_area_future_loss*100,
                    color=forcing,y=log(GDP_SSP5/POP_SSP5),group=interaction(gridcell_id,forcing)),alpha=0.5)+
    #ylab("Benefit Change (million USD)") + 
    #labs(color="Forcing",shape="Forcing")+xlab("Mangrove Area Change (ha)")+
    scale_color_scico_d()

    


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
        glimpse(result_df)

        scen_allforcings_allES_dif <- result_df
        ggplot() + 
        geom_point(data=scen_allforcings_allES_dif %>% filter(year %in% c(2100),countrycode=="NZL"),aes(x=dif_benefit_change/10^6,
                        #color=log(benefit_change/10^6),
                        color=type,shape=forcing,y=log(GDP_SSP5/POP_SSP5))) + theme_bw()+
       
        scale_color_scico_d()
    
    write.csv(scen_allforcings_allES_dif,file="Data/projections/scen_allforcings_allES_dif_ssp570.csv")
    write.csv(scen_allforcings_allES,file="Data/projections/scen_allforcings_allES_ssp570.csv")

    glimpse(scen_allforcings_allES)
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

    glimpse(scen_allforcings_allES_country)
    ggplot(scen_allforcings_allES_country %>% filter(countrycode=="IND",type=="cultural")) + 
    geom_point(aes(x=year,y=benefit_change/benefit_future,color=forcing)) 

    ggplot(scen_allforcings_allES_country %>% filter(forcing=="onlyCC",type=="cultural")) + 
    geom_line(aes(x=mangrove_area_future_loss,y=benefit_change,group=countrycode)) + xlim(c(-10,10)) + ylim(c(-25000,25000))

     ggplot(scen_allforcings_allES_country %>% filter(countrycode=="IND",type=="cultural")) + 
    geom_point(aes(x=mangrove_area_future_loss,y=benefit_change,color=forcing)) 

    #scen_allforcings_allES_country %>% filter(type=="water",year==2100 & forcing=="both" & GDP_SSP5>0 & benefit_future<1) %>%as.data.frame()

    
    #ggsave("Figures/Draft2/Benefits_2100.png",dpi=600)    

    glimpse(scen_allforcings_allES_country)
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

    ggplot(diff_df_country_type %>% filter(countrycode=="IND",type=="cultural")) + 
    geom_point(aes(x=year,y=diff_benefit_change/benefit_future)) 

    ggplot(diff_df_country_type %>% filter(countrycode=="IND",type=="cultural")) + 
    geom_point(aes(x=year,y=diff_benefit_change)) 

    glimpse(diff_df_country_type)
    diff_df_country_type %>% min(diff_df_country_type$diff_benefit_change)
    which(diff_df_country_type$diff_benefit_change ==  min(diff_df_country_type$diff_benefit_change))
    diff_df_country_type$countrycode[13765]
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
    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type_ssp570.csv")
    long_df<-diff_df_country_type
    write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp570.csv")

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
    write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp570.csv")
    
    # long_df <- diff_df_country %>%
    #     pivot_longer(cols = c(benefit_change_provisioning, benefit_change_reg),
    #                 names_to = "benefit_type",
    #                 values_to = "benefit_change") %>%
    #     mutate(type = recode(benefit_type,
    #                                 benefit_change_provisioning = "provisioning",
    #                                 benefit_change_reg = "regulating"))
    # glimpse(long_df)              

    # extrarow <- long_df[1,]
    # extrarow$type <- "All"
    # extrarow$year <- 2030
    # long_df <- rbind(long_df,extrarow)
    # long_df$type <- factor(long_df$type)
    # unique(long_df$type)

    # write.csv(long_df,file="Data/projections/benefit_loss_2types_ssp370.csv")


    # diff_country_total <- long_df %>% group_by(year,countrycode) %>%
    #                     summarise(benefit_change = benefit_change[type=="provisioning"] + benefit_change[type=="regulating"], 
    #                                 R5 = R5[type=="provisioning"],
    #                                 POP_SSP5 = POP_SSP5[type=="provisioning"],
    #                                 GDP_SSP5 = GDP_SSP5[type=="provisioning"],
    #                                 diff_mangrove_area_future_loss = diff_mangrove_area_future_loss[type=="regulating"]) %>% ungroup()


    # library("scales")
  

    # write.csv(diff_country_total,file="Data/projections/diff_country_total_ssp370.csv")


    # glimpse(long_df)

######
######

