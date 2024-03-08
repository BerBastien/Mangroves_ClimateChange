
    scen_arealoss_perc_onlyCC <- read.csv(file="Results\\Area\\scen_arealoss_perc_onlyCC.csv")
    scen_arealoss_perc_onlygdp <- read.csv(file="Results\\Area\\scen_arealoss_perc_onlygdp.csv")
    scen_arealoss_perc_both <- read.csv(file="Results\\Area\\scen_arealoss_perc_both.csv")

    scen_arealoss_perc_onlyCC$forcing <- "onlyCC"
    scen_arealoss_perc_onlygdp$forcing <- "onlygdp"
    scen_arealoss_perc_both$forcing <- "both"
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_both))]
    scen_arealoss_perc_onlygdp <- scen_arealoss_perc_onlygdp[,which(names(scen_arealoss_perc_onlygdp) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlyCC))]
    scen_arealoss_perc_onlyCC <- scen_arealoss_perc_onlyCC[,which(names(scen_arealoss_perc_onlyCC) %in% names(scen_arealoss_perc_onlygdp))]
    scen_arealoss_perc_both <- scen_arealoss_perc_both[,which(names(scen_arealoss_perc_both) %in% names(scen_arealoss_perc_onlygdp))]
    scen_allforcings <- rbind(scen_arealoss_perc_onlyCC,scen_arealoss_perc_onlygdp,scen_arealoss_perc_both)
    glimpse(scen_allforcings)



    #Reference: https://www.sciencedirect.com/science/article/pii/S0308597X19302386?casa_token=wgOsSd-b5PkAAAAA:GvO0M3GBG3h9NoSBWxDgdEG5i0anOyoNWbd_hiwbbSgm4wCQnjChYbuU-2wPOgTlTKX-XAxh#appsec3
    #N=111
    #Dependent: USD/ha/year; 2007; ln
    b0 <- -0.590
    b_coastalES <- 1.456
    b_waterES <- 1.714
    b_fisheries <- 0.860
    b_wood <- -1.085
    b_ln.gdp.pcap <- 0.785
    b_ln.pop <- 0.284
    b_ln.mangrove.ha <- -0.343
    b_ln.mangrove.km <- 0.248

    #Or Using DeGroot (2012) mean estimates (N=139)
    Prov_2007IntD_perkm_peryr <- 2998* 100
    Reg_2007IntD_perkm_peryr <- 171515* 100
    Hab_2007IntD_perkm_peryr <- 17138* 100
    Cult_2007IntD_perkm_peryr <- 2193* 100
    Total_ES <- Cult_2007IntD_perkm_peryr + Prov_2007IntD_perkm_peryr +   Reg_2007IntD_perkm_peryr + Hab_2007IntD_perkm_peryr
    Total_ES_perha <- Total_ES/100
    # scen_allforcings_coastalES <- scen_allforcings
    # scen_allforcings_coastalES$dummy_ES <- b_coastalES
    # scen_allforcings_coastalES$type= "coastal"
    # scen_allforcings_waterES <- scen_allforcings
    # scen_allforcings_waterES$dummy_ES <- b_waterES
    # scen_allforcings_waterES$type= "water"
    # scen_allforcings_fisheries <- scen_allforcings
    # scen_allforcings_fisheries$dummy_ES <- b_fisheries
    # scen_allforcings_fisheries$type= "fisheries"
    # scen_allforcings_wood <- scen_allforcings
    # scen_allforcings_wood$dummy_ES <- b_wood
    # scen_allforcings_wood$type= "wood"
    # scen_allforcings_allES <- rbind(scen_allforcings_coastalES,scen_allforcings_waterES,scen_allforcings_fisheries,scen_allforcings_wood)
    

    types <- c("coastal", "water", "fisheries", "wood")
    values <- list(b_coastalES, b_waterES, b_fisheries, b_wood)
    list_of_dfs <- list()
    for (i in seq_along(types)) {
        temp_df <- scen_allforcings
        temp_df$dummy_ES <- values[[i]]
        temp_df$type <- types[i]
        list_of_dfs[[i]] <- temp_df
    }

    scen_allforcings_allES <- do.call(rbind, list_of_dfs)
    glimpse(scen_allforcings_allES)

    scen_allforcings_allES <- scen_allforcings_allES %>% 
                    mutate( benefits_perha = exp(b0 + dummy_ES + 
                                                b_ln.gdp.pcap*log(gdppc5) + 
                                                b_ln.pop * log(pop_ssp5) + 
                                                b_ln.mangrove.ha * log(mangrove_area*100)+ 
                                                b_ln.mangrove.km * log(mangrove_area))) %>%
                    mutate(benefit_change = mangrove_area_future_loss * benefits_perha * 100) %>%
                    mutate(benefits_future = (mangrove_area+mangrove_area_future_loss) * benefits_perha * 100,
                            benefits_change_percGDP = benefit_change / GDP_Country_ssp5 ) 

    s <- scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc2 > 0)
    dim(s)
    ggplot(scen_allforcings_allES %>% filter(year==2100 & forcing=="both" & gdppc2 > 0))+
        geom_point(aes(x=log(pop_ssp2),y=(benefits_future/10^6),color=R5,shape=type,size=gdppc2),alpha=0.5) + scale_color_scico_d(palette="batlow",end=0.8)+theme_bw()+
        scale_y_continuous(trans="log") + ylab("Benefits (million USD)")

    glimpse(scen_allforcings_allES)
    scen_allforcings_allES_country <- scen_allforcings_allES %>% group_by(countrycode,year,forcing,type) %>%
                    summarise(
                        benefit_change = sum(benefit_change,na.rm=TRUE),
                        benefit_future = sum(benefits_future,na.rm=TRUE),
                        R5 = first(R5),
                        gdp_ssp2 = sum(gdp_ssp2,na.rm=TRUE),
                        pop_ssp2 = sum(pop_ssp2,na.rm=TRUE),
                        #GDP_Country_ssp2 = first(GDP_Country_ssp2),
                        mangrove_area_future_loss = sum(mangrove_area_future_loss,na.rm=TRUE),
                        mangrove_area = sum(mangrove_area,na.rm=TRUE)
                    ) %>%
                    ungroup()

    scen_allforcings_allES_country %>% filter(type=="water",year==2100 & forcing=="both" & gdp_ssp2>0 & benefit_future<1) %>%as.data.frame()

    library(ggrepel)
    ggplot(scen_allforcings_allES_country %>% filter(year==2100 & forcing=="both" & gdp_ssp2>0& benefit_future>1))+
        geom_point(aes(x=(pop_ssp2/10^6),y=(benefit_future/10^6),color=R5,shape=type,size=(mangrove_area_future_loss+mangrove_area)),alpha=0.3) + scale_color_scico_d(palette="batlow",end=0.8)+theme_bw()+
        geom_text_repel(data=scen_allforcings_allES_country %>% filter(type=="water",year==2100 & forcing=="both" & gdp_ssp2>0& benefit_future>1),
                        aes(x=(pop_ssp2/10^6),y=(benefit_future/10^6),color=R5,label=countrycode),alpha=0.9) + 
        scale_y_continuous(trans="log2") +   
        scale_x_continuous(trans="log2") +
        ylab("Benefits in 2100 (million USD)") +
        xlab("Population Near Mangroves (millions)")+
        labs(size=expression("Mangrove Area (km"^{2}*")"),color="Region",shape="ES Type")
    #ggsave("Figures/Draft2/Benefits_2100.png",dpi=600)    

    glimpse(scen_allforcings_allES_country)
    diff_df_country_type <- scen_allforcings_allES_country %>%
    filter(forcing %in% c("both", "onlygdp")) %>%
    group_by(year, countrycode,type) %>%
    summarise(
        diff_benefit_change = benefit_change[forcing == "both"] - benefit_change[forcing == "onlygdp"],
        diff_mangrove_area_future_loss =mangrove_area_future_loss[forcing == "both"] - mangrove_area_future_loss[forcing == "onlygdp"],
        benefit_change_both = benefit_change[forcing == "both"],
        benefit_change_onlygdp = benefit_change[forcing == "onlygdp"],
        mangrove_area_future_loss_both = mangrove_area_future_loss[forcing == "both"],
        mangrove_area_future_loss_onlygdp = mangrove_area_future_loss[forcing == "onlygdp"],
        mangrove_area_future = mangrove_area[forcing == "both"],
        pop_ssp2 = pop_ssp2[forcing == "both"],
        R5 = R5[forcing == "both"]
    ) %>%
    ungroup()

    BenefitChange_2100 <- ggplot(diff_df_country_type)+
    geom_point(data=diff_df_country_type,aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, shape=type, alpha=exp((year-2020)/100)))+ 
    #geom_point(aes(x=diff_mangrove_area_future_loss,y=diff_benefit_change, color=R5, shape=type, size= mangrove_area_future_loss_both+mangrove_area_future, alpha=(year-2020)/100)) + 
    geom_line(aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5,group=interaction(countrycode,type)),alpha=0.3) + 
    #geom_point(data=diff_df_country_type %>% filter(year %in% c(2030,2050,2080,2100)),aes(x=diff_mangrove_area_future_loss,y=diff_benefit_change, color=R5, shape=type)) + 
    scale_color_scico_d(palette="batlow",end=0.7)+theme_bw()+
    scale_alpha_continuous(range = c(0, 0.8))+
    geom_text_repel(data=diff_df_country_type %>% filter(year ==2100),aes(x=diff_mangrove_area_future_loss*100,y=diff_benefit_change/10^6, color=R5, label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Region",shape="ES Type")+xlab("Mangrove Area Change (ha)")

    BenefitChange_2100
    write.csv(diff_df_country_type,file="Data/projections/diff_df_country_type.csv")

    #ggsave("Figures/Draft2/BenefitChange_2100.png",dpi=600)    

    glimpse(diff_df_country_type)
    diff_df_country <- diff_df_country_type %>% group_by(year,countrycode) %>%
                        summarise(benefit_change_provisioning = diff_benefit_change[type=="wood"] + diff_benefit_change[type=="fisheries"], 
                                    benefit_change_reg = diff_benefit_change[type=="water"] + diff_benefit_change[type=="coastal"] , 
                                    R5 = R5[type=="water"],
                                    pop_ssp2 = pop_ssp2[type=="water"],
                                    diff_mangrove_area_future_loss =diff_mangrove_area_future_loss[type=="water"]) %>% ungroup()
    long_df <- diff_df_country %>%
        pivot_longer(cols = c(benefit_change_provisioning, benefit_change_reg),
                    names_to = "benefit_type",
                    values_to = "benefit_change") %>%
        mutate(type = recode(benefit_type,
                                    benefit_change_provisioning = "provisioning",
                                    benefit_change_reg = "regulating"))
    glimpse(long_df)              

    extrarow <- long_df[1,]
    extrarow$type <- "All"
    extrarow$year <- 2030
    long_df <- rbind(long_df,extrarow)
    long_df$type <- factor(long_df$type)
    unique(long_df$type)

    write.csv(long_df,file="Data/projections/benefit_loss_2types.csv")

    BenefitChange_2100_2types <- ggplot(long_df)+
    geom_point(data=long_df%>% filter(year %in% c(2020+seq(1:16)*5)),aes(x=diff_mangrove_area_future_loss*100,y=benefit_change/10^6, color=log(pop_ssp2), 
                     #alpha=((year-2020)/(80)),
                     shape=type))+ 
    geom_line(data=long_df%>% filter(year %in% c(2040+seq(1:16)*5)),aes(x=diff_mangrove_area_future_loss*100,y=benefit_change/10^6,group=interaction(countrycode,type)),alpha=0.2)+ 
    scale_color_scico(palette="lajolla",end=0.9)+theme_bw()+
    scale_alpha_continuous(range = c(0.2, 0.8))+
    #geom_text_repel(data=long_df %>% filter(year ==2100),aes(x=diff_mangrove_area_future_loss*100,y=benefit_change/10^6,  label=countrycode)) +
    ylab("Benefit Change (million USD)") + 
    labs(color="Population",shape="ES Type")+xlab("Mangrove Area Change due to Climate Change (ha)")

    BenefitChange_2100_2types                     

    diff_country_total <- long_df %>% group_by(year,countrycode) %>%
                        summarise(benefit_change = benefit_change[type=="provisioning"] + benefit_change[type=="regulating"], 
                                    R5 = R5[type=="provisioning"],
                                    pop_ssp2 = pop_ssp2[type=="provisioning"],
                                    diff_mangrove_area_future_loss = diff_mangrove_area_future_loss[type=="regulating"]) %>% ungroup()
    glimpse(diff_country_total)          

    diff_country_total %>% filter(year==2100) %>% ungroup() %>% as.data.frame()

    library("scales")
    losses_2100 <- ggplot(data = diff_country_total %>% filter(year == 2100),
        aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=R5)) +
        geom_point(alpha=0.5)+
        geom_text_repel(aes(label=countrycode))+
        scale_y_continuous(trans="log10", labels = comma) +   
        scale_x_continuous(trans="log10", labels = comma) +
        scale_color_scico_d(palette="batlow",end=0.8)+
        xlab("Mangrove Area Loss due to Climate Change (ha)")+
        ylab("Benefit Losses (million USD)") + ggtitle("Losses in 2100") + theme_bw()

    gains_2100 <-ggplot(data = diff_country_total %>% filter(year == 2100),
        aes(x = diff_mangrove_area_future_loss*100, y = benefit_change/10^6, color=R5)) +
        geom_point(alpha=0.5)+
        geom_text_repel(aes(label=countrycode))+
        scale_y_continuous(trans="log10", labels = comma) +   
        scale_x_continuous(trans="log10", labels = comma) +
        scale_color_scico_d(palette="batlow",end=0.8)+
        xlab("Mangrove Area Gain due to Climate Change (ha)")+
        ylab("Benefit Gains (million USD)") + ggtitle("Gains in 2100") + theme_bw()

    write.csv(diff_country_total,file="Data/projections/diff_country_total.csv")
    ggarrange(BenefitChange_2100_2types,ggarrange(losses_2100,gains_2100,legend=FALSE,ncol=2),ncol=1)