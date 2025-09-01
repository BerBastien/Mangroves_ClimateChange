
    diff_country_total <- read.csv(file="Data/projections/diff_country_total.csv")
    diff_country_total <- read.csv(file="Data/projections/diff_country_total_ssp370.csv")
    unique(diff_country_total$countrycode)
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

    benefit_ssp <- merge(ssp_gdp %>% filter(scenario=="SSP2"),diff_country_total,by=c("countrycode","year"),all=T)
    benefit_ssp <- merge(ssp_gdp %>% filter(scenario=="SSP3"),diff_country_total,by=c("countrycode","year"),all=T)
    benefit_ssp <- merge(benefit_ssp ,ssp_temp_long,by=c("year"),all=F) %>% filter(year>2025)
    benefit_ssp$benefit_change_perGDP <- 100*benefit_ssp$benefit_change / (benefit_ssp$GDP.billion2005USDperYear * 10^9)
    glimpse(benefit_ssp)


    coefficients_by_country <- benefit_ssp %>% filter(!is.na(benefit_change_perGDP)) %>%
        group_by(countrycode) %>%
        do({
            model <- lm(benefit_change ~ 0 + temp, data = .)
            data.frame(coefficient_temp = coef(model)["temp"])
        })

    glimpse(coefficients_by_country )
    
    mean((coefficients_by_country$coefficient_temp/10^6))

    coefficients_by_country_percGDP <- benefit_ssp %>% filter(!is.na(benefit_change_perGDP)) %>%
        group_by(countrycode) %>%
        do({
            model <- lm(benefit_change_perGDP ~ 0 + temp, data = .)
            data.frame(coefficient_temp = coef(model)["temp"])
        })

    glimpse(coefficients_by_country_percGDP )
    
    mean(coefficients_by_country_percGDP$coefficient_temp,na.rm=T)
    max(coefficients_by_country_percGDP$coefficient_temp,na.rm=T)
    min(coefficients_by_country_percGDP$coefficient_temp,na.rm=T)
    # Get the world map data
world_ne <- ne_countries(scale = "medium", returnclass = "sf")
glimpse(world_ne)
glimpse(coefficients_by_country )


world_ne_with_coeffs <-  merge(world_ne,coefficients_by_country,by.x="iso_a3",by.y="countrycode",all=T)
glimpse(world_ne_with_coeffs)

world_ne_with_coeffs_perc <-  merge(world_ne,coefficients_by_country_percGDP,by.x="iso_a3",by.y="countrycode",all=T)
glimpse(world_ne_with_coeffs_perc)

# 1. Transform the coefficient_temp column
world_ne_with_coeffs <- world_ne_with_coeffs %>%
  mutate(log_coeff_temp = log10(-coefficient_temp))

  
world_ne_with_coeffs_perc <- world_ne_with_coeffs_perc %>%
  mutate(log_coeff_temp = log10(-coefficient_temp))

# 2. Compute the breaks for the transformed data

log_breaks <- pretty(log10(-world_ne_with_coeffs$coefficient_temp/10^6))
    actual_breaks <- 10^(log_breaks)

log_breaks_perc <- pretty(log(-world_ne_with_coeffs_perc$coefficient_temp))
    actual_breaks_perc <- exp(log_breaks_perc)


save(world_ne_with_coeffs,file="Data/projections/world_ne_with_coeffs_ssp370.Rds")
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
