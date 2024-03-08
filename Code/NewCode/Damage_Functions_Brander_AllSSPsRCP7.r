
    diff_country_total <- read.csv(file="Data/projections/diff_country_total.csv")
    diff_country_total1 <- read.csv(file="Data/projections/diff_country_total_ssp170_brander.csv")
    diff_country_total2 <- read.csv(file="Data/projections/diff_country_total_ssp270_brander.csv")
    diff_country_total3 <- read.csv(file="Data/projections/diff_country_total_ssp370_brander.csv") 
    diff_country_total4 <- read.csv(file="Data/projections/diff_country_total_ssp470_brander.csv")
    diff_country_total5 <- read.csv(file="Data/projections/diff_country_total_ssp570_brander.csv")

    glimpse(diff_country_total3)
    glimpse(diff_country_total2)

    ssp_gdp <- read.csv(file='C:\\Users\\basti\\Box\\Data\\SSPs\\ssp_gdp.csv')
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP585_magicc_202310021547.csv")
    ssp_temp <- read.csv(file="C:\\Users\\basti\\Box\\Data\\SSPs\\CO2Pulse\\SSP370_magicc_202311031621.csv")
    glimpse(ssp_temp)
    glimpse(ssp_gdp)
    unique(ssp_temp$variable)
    
    ssp_temp %>% filter(variable=="Surface Temperature")
    ssp_gdp$countrycode <- ssp_gdp$ISO3
    glimpse(diff_country_total)

    library(tidyverse)

    ssp_temp_long <- ssp_temp %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "year",
        values_to = "value"
    ) %>%
    # Remove the "X" prefix from the year column and convert to numeric
    mutate(year = as.numeric(str_remove(year, "X"))) %>% filter(variable=="Surface Temperature")

    ssp_temp_long$temp2025 <- ssp_temp_long %>% filter(year==2025) %>% select(value) %>% unlist()
    glimpse(ssp_temp_long)
    ssp_temp_long$temp <- ssp_temp_long$value - ssp_temp_long$temp2025
    ssp_temp_long$countrycode <- ssp_temp_long$region - ssp_temp_long$temp2025

    glimpse(ssp_gdp)

    glimpse(diff_country_total)
    diff_country_total <- rbind(diff_country_total1 %>% rename("POP"="POP_SSP1","GDP"="GDP_SSP1","POP_Country"="Pop_Country_ssp1","GDP_Country"="GDP_Country_ssp1") %>% mutate(scenario="SSP1"),
                                diff_country_total2 %>% rename("POP"="POP_SSP2","GDP"="GDP_SSP2","POP_Country"="Pop_Country_ssp2","GDP_Country"="GDP_Country_ssp2") %>% mutate(scenario="SSP2"),
                                diff_country_total3 %>% rename("POP"="POP_SSP3","GDP"="GDP_SSP3","POP_Country"="Pop_Country_ssp3","GDP_Country"="GDP_Country_ssp3") %>% mutate(scenario="SSP3"),
                                diff_country_total4 %>% rename("POP"="POP_SSP4","GDP"="GDP_SSP4","POP_Country"="Pop_Country_ssp4","GDP_Country"="GDP_Country_ssp4") %>% mutate(scenario="SSP4"),
                                diff_country_total5 %>% rename("POP"="POP_SSP5","GDP"="GDP_SSP5","POP_Country"="Pop_Country_ssp5","GDP_Country"="GDP_Country_ssp5") %>% mutate(scenario="SSP5"))
    glimpse(diff_country_total)
    glimpse(diff_country_total5)
    benefit_ssp <- merge(ssp_gdp,diff_country_total,by=c("countrycode","year","scenario"),all=T)
    benefit_ssp <- merge(benefit_ssp ,ssp_temp_long,by=c("year"),all=F) %>% filter(year>2025)
    benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)
    glimpse(benefit_ssp)

    # ggplot(benefit_ssp %>% filter(countrycode=="MEX")) + 
    # geom_point(aes(x=temp,y=benefit_change_perGDP,col=log(GDP/POP),shape=scenario.x))+
    # scale_color_scico(begin=0.3) + theme_bw()

    # ggplot(benefit_ssp %>% filter(countrycode=="MEX")) + 
    # geom_point(aes(col=temp,y=benefit_change_perGDP,x=log(GDP/POP),shape=scenario.x))+
    # scale_color_scico(begin=0.3) + theme_bw()

    write.csv(benefit_ssp ,file = "Results/Benefits/benefit_ssp_brander.csv")
    # ggplot(benefit_ssp %>% filter(countrycode=="MEX") %>%filter(year %in% c(2020+seq(1:30)*5))) + 
    # geom_point(aes(x=temp,y=benefit_change_perGDP,col=log(GDP/POP),size=POP/10^6,shape=scenario.x),alpha=0.9)+
    # scale_color_scico(begin=0.3) + theme_bw() + 
    # labs(title="Climate Impacts on Benefits Provided by Mangroves in Mexico",x = "Temperature Change (C)",size="Population \n(million)", y = "Benefit Change (%GDP)",shape="Scenario",color="Log GDPpc")

    

    coefficients_by_country <- benefit_ssp %>% filter(!is.na(benefit_change_perGDP)) %>%
        group_by(countrycode) %>%
        do({
            model_USD <- lm(benefit_change ~ 0 + temp:factor(scenario.x), data = .)
            model_USD_allssps <- lm(benefit_change ~ 0 + temp, data = .)
            model_percGDP <- lm(benefit_change_perGDP ~ 0 + temp:factor(scenario.x), data = .)
            model_percGDP_allssps <- lm(benefit_change_perGDP ~ 0 + temp, data = .)
            data.frame(coefficient_temp_USD_allSSPs_temp = coef(model_USD_allssps)["temp"],
            coefficient_temp_USD_allSSPs_se = summary(model_USD_allssps)$coefficients[2],
            coefficient_temp_USD_SSP1 = coef(model_USD)["temp:factor(scenario.x)SSP1"],
            coefficient_temp_USD_SSP1_se = summary(model_USD)$coefficients[6],
            coefficient_temp_USD_SSP2 = coef(model_USD)["temp:factor(scenario.x)SSP2"],
            coefficient_temp_USD_SSP2_se = summary(model_USD)$coefficients[7],
            coefficient_temp_USD_SSP3 = coef(model_USD)["temp:factor(scenario.x)SSP3"],
            coefficient_temp_USD_SSP3_se = summary(model_USD)$coefficients[8],
            coefficient_temp_USD_SSP4 = coef(model_USD)["temp:factor(scenario.x)SSP4"],
            coefficient_temp_USD_SSP4_se = summary(model_USD)$coefficients[9],
            coefficient_temp_USD_SSP5 = coef(model_USD)["temp:factor(scenario.x)SSP5"],
            coefficient_temp_USD_SSP5_se = summary(model_USD)$coefficients[10],
            coefficient_temp_percGDP_allSSPs_temp = coef(model_percGDP_allssps)["temp"],
            coefficient_temp_percGDP_allSSPs_se = summary(model_percGDP_allssps)$coefficients[2],
            coefficient_temp_percGDP_SSP1 = coef(model_percGDP)["temp:factor(scenario.x)SSP1"],
            coefficient_temp_percGDP_SSP1_se = summary(model_percGDP)$coefficients[6],
            coefficient_temp_percGDP_SSP2 = coef(model_percGDP)["temp:factor(scenario.x)SSP2"],
            coefficient_temp_percGDP_SSP2_se = summary(model_percGDP)$coefficients[7],
            coefficient_temp_percGDP_SSP3 = coef(model_percGDP)["temp:factor(scenario.x)SSP3"],
            coefficient_temp_percGDP_SSP3_se = summary(model_percGDP)$coefficients[8],
            coefficient_temp_percGDP_SSP4 = coef(model_percGDP)["temp:factor(scenario.x)SSP4"],
            coefficient_temp_percGDP_SSP4_se = summary(model_percGDP)$coefficients[9],
            coefficient_temp_percGDP_SSP5 = coef(model_percGDP)["temp:factor(scenario.x)SSP5"],
            coefficient_temp_percGDP_SSP5_se = summary(model_percGDP)$coefficients[10]
            )
        })

    glimpse(coefficients_by_country)
    
    mean((coefficients_by_country$coefficient_temp_USD_allSSPs_temp/10^6))
    max((coefficients_by_country$coefficient_temp_percGDP_allSSPs_temp))

library(rnaturalearth)
world_ne <- ne_countries(scale = "medium", returnclass = "sf")

world_ne_with_coeffs <-  merge(world_ne,coefficients_by_country,by.x="iso_a3",by.y="countrycode",all=T)
glimpse(world_ne_with_coeffs)

# # 1. Transform the coefficient_temp column
# world_ne_with_coeffs <- world_ne_with_coeffs %>%
#   mutate(log_coeff_temp = log10(-coefficient_temp))

  
# world_ne_with_coeffs_perc <- world_ne_with_coeffs_perc %>%
#   mutate(log_coeff_temp = log10(-coefficient_temp))

# # 2. Compute the breaks for the transformed data

# log_breaks <- pretty(log10(-world_ne_with_coeffs$coefficient_temp/10^6))
#     actual_breaks <- 10^(log_breaks)

# log_breaks_perc <- pretty(log(-world_ne_with_coeffs_perc$coefficient_temp))
#     actual_breaks_perc <- exp(log_breaks_perc)


save(world_ne_with_coeffs,file="Data/projections/world_ne_with_coeffs_sspAll_RCP70.Rds")
save(world_ne_with_coeffs_perc,file="Data/projections/world_ne_with_coeffs_perc_ssp370.Rds")
# 3. Plot the transformed data using those breaks
library("scales")
gg <- ggplot(data = world_ne_with_coeffs) +
  geom_sf(aes(fill = log10(-coefficient_temp/10^6))) +
  scale_fill_scico(palette = "lajolla", breaks = log_breaks, 
    labels = comma(actual_breaks),na.value="transparent",
    begin=0.1,end=0.9) +
  labs(fill = "Losses \n(Million USD/yr/C)") +
  theme_minimal()+
        coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))

gg

gg_perc <- ggplot(data = world_ne_with_coeffs_perc) +
  geom_sf(aes(fill = log(-coefficient_temp))) +
  scale_fill_scico(palette = "lajolla", breaks = log_breaks_perc, 
    labels = comma(actual_breaks_perc),na.value="transparent",
    begin=0.1,end=0.9) +
  labs(fill = "Losses \n(%GDP/C)") +
  theme_minimal()+
        coord_sf(crs = "+proj=robin", ylim = c(-39*10^5, 30*10^5))

gg_perc



log_breaks <- pretty(log10(-world_ne_with_coeffs$coefficient_temp/10^6))
    actual_breaks <- 10^(log_breaks)

    losses_2100_pop <- ggplot(data = diff_country_total %>% filter(year == 2100& benefit_change<0),
                          aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log10(pop_ssp2/10^6))) +
                          #aes(x = -diff_mangrove_area_future_loss*100, y = -benefit_change/10^6, color=log10(pop_ssp5/10^6))) +
        geom_point() +
        geom_text_repel(aes(label=countrycode)) +
        scale_y_continuous(trans="log10", labels = comma) +   
        scale_x_continuous(trans="log10", labels = comma) +
        
        #scale_color_scico(palette="lajolla",begin=0.2,end=0.9, breaks = log_breaks, labels = comma(actual_breaks)) +
        
        scale_color_scico(palette="devon",begin=0.1,end=0.8, breaks = log_breaks, labels = comma(actual_breaks),direction=-1,
                    guide = guide_colourbar(title.position = "top", title.hjust = 0.1, barwidth = 11)) +
        xlab("Area Loss (ha)") +
        ylab("Benefit Losses (million USD)") + 
        ggtitle("Mangrove Losses in 2100") + 
        theme_bw() + labs(color="Population near Mangroves (millions)")
losses_2100_pop


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ####
    ####
    ####
    ####
    ####  Temperature
    
    
    glimpse(diff_country_total3)
    benefit_ssp3 <- benefit_ssp %>% filter(scenario.x=="SSP3")
    glimpse(benefit_ssp3)

    ggplot(benefit_ssp3 %>% filter(countrycode=="MEX"))+
    geom_line(aes(x=year,y=diff_mangrove_area_future_loss))

  coefficients_by_country <- benefit_ssp3 %>% filter(!is.na(diff_mangrove_area_future_loss)) %>%
        mutate(mangrove_loss_ha = diff_mangrove_area_future_loss*100) %>%
        group_by(countrycode) %>%
        do({
            model_area <- lm(mangrove_loss_ha ~ 0 + temp, data = .)
            data.frame(coefficient_temp = coef(model_area)["temp"],
            coefficient_temp_se = summary(model_area)$coefficients[2]
            )
        })

    glimpse(coefficients_by_country)
    glimpse(mcn)
    
    write.csv(coefficients_by_country,file="C:\\Users\\basti\\Documents\\GitHub\\BlueDICE\\Data\\intermediate_output\\mangrove_area_coefficients.csv")
 