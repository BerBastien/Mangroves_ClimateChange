library(SPEI)
thornthwaite(25, 30)


mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
data <- mcn
glimpse(data)
# Calculate Thornthwaite's heat index (I) and alpha
data$mean_temp_C <- data$temp # Convert temperature from K to C if necessary
data$I <- (data$mean_temp_C / 5) ^ 1.514
data$alpha <- 0.49239 + 0.01792 * data$I + 0.0006445 * data$I ^ 2

# Calculate potential evapotranspiration (PET)
data$PET <- 16 * ((10 * data$mean_temp_C / data$I) ^ data$alpha)

# Calculate water balance (P - PET)
data$water_balance <- data$Mean_Precipitation*12 - data$PET

# Calculate z-score to approximate SPEI
data$SPEI_approx <- scale(data$water_balance)

ggplot(data,aes(x=spei,y=SPEI_approx))+geom_point()
ggplot(data,aes(x=spei,y=Mean_Salinity))+geom_point()

ggplot(data,aes(x=spei,y=water_balance))+geom_point()
