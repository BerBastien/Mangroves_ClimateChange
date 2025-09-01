libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe")

lapply(libraries, library, character.only = TRUE)

setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_sept2023.csv")
mcn$GDPpc_country <- (mcn$GDP_country/mcn$Population_country)
subset_mcn <- mcn[,c("b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", "b36_37C")]
mcn$hot_bin <- rowSums(subset_mcn, na.rm = TRUE)
subset_mcn <- mcn[,c("b8_9C", "b9_10C", "b10_11C", "b11_12C", "b12_13C", "b13_14C","b14_15C","b15_16C","b16_17C","b17_18C")]
mcn$cold_bin <- rowSums(subset_mcn, na.rm = TRUE)
mcn$logGDPpc_country <- log(mcn$GDPpc_country)
mcn$abs_lat <- abs(mcn$Latitude)
mcn_2020 <- mcn %>% filter(year == 2020)

df_f <- mcn %>% filter(
                               
                                mangrove_area>1 & 
                                #!is.na(logGDPpc) &
                                #!is.na(lag_gap_density) &
                                #lag_holes>0 & 
                                #np>0 &
                                #lag_gap_density>0 & 
                                #gap_density>0 &
                                #year > 2014 & 
                                #is.finite(logGDPpc_country) &
                                #is.finite(pafrac) &
                                is.finite(lag_gap_density_avg) &
                                #gap_density <50 &
                                #holes_size>0 &
                                #ntl > 0 &
                                is.finite(logGDPpc)# & 
                                #is.finite(mcw_int)
                                #abs(Latitude)>20
                                )
        df_f$lag_gap_density <- df_f$lag_gap_density_avg
                                unique(mcn$year)
                                unique(df_f$year)         

        mcn_2020 <- df_f %>% filter(year == 2020)


        
        glimpse(df_f)
        model_area_ssthot <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I(lag_gap_density^2) + 
                                        factor(year):countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))

                      
        #save(model_area_ssthot,file="Models/Round2/pref_area_model.RData")
        #load("Models/Round1/pref_area_model.RData") 
        summary(model_area_ssthot)

        
        df_f_noAus <- df_f %>% filter(countrycode!="AUS")
        model_area_ssthot_noAus <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I(lag_gap_density^2) + 
                                        factor(year):countrycode
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f_noAus,
                                        weights=log(df_f_noAus$mangrove_area+1))

                      
        #save(model_area_ssthot,file="Models/Round2/pref_area_model.RData")
        #load("Models/Round1/pref_area_model.RData") 
        summary(model_area_ssthot_noAus)



        df_f <- df_f %>% mutate(cy = paste0(countrycode,year))
        model_area_ssthot_cy <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I(lag_gap_density^2) 
                                        |gridcell_id  +  year + cy |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        summary(model_area_ssthot_cy)


        df_f_noAus <- df_f_noAus %>% mutate(cy = paste0(countrycode,year))
        model_area_ssthot_cy_noAus <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I(lag_gap_density^2) 
                                        |gridcell_id  +  year + cy |0|gridcell_id,data=df_f_noAus,
                                        weights=log(df_f_noAus$mangrove_area+1))
        summary(model_area_ssthot_cy_noAus)
        library(stargazer)
        stargazer(model_area_ssthot_cy,model_area_ssthot_cy_noAus,type="html",out="Tables/noaus.html")

        glimpse(df_f)
        glimpse(df_f_noAus)
        11168/9737
        
        ggplot(df_f)+
        geom_point(aes(x=log(mangrove_area),y=sst_hottest),col="red")+
        geom_point(data=df_f_noAus,aes(x=log(mangrove_area),y=sst_hottest),col="black")

        model_area_Onlyssthot <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)
                                        |gridcell_id  +  year |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        #save(model_area_ssthot,file="Models/Round2/pref_area_model.RData")
        #load("Models/Round1/pref_area_model.RData") 
        summary(model_area_Onlyssthot)

        summary(model)
        # Load necessary libraries
library(plm)
library(ggplot2)
library(caret)
library(lmtest)
# Load necessary libraries
library(dplyr)
library(plm)
library(caret)
library(ggplot2)
library(gridExtra)

# Create new variables in the dataset
df_f2 <- df_f %>%
  mutate(
    log_mangrove_area = log(mangrove_area),
    sst_hottest_sq = sst_hottest^2,
    preci_sq = preci^2,
    logGDPpc_sq = logGDPpc^2,
    lag_gap_density_sq = lag_gap_density^2
  )

# Select relevant columns and remove rows with any NA values
df_f_clean <- df_f2 %>%
  select(gridcell_id, year, log_mangrove_area, sst_hottest, sst_hottest_sq, 
         preci, preci_sq, logGDPpc, logGDPpc_sq, lag_gap_density, lag_gap_density_sq, 
         countrycode) %>%
  na.omit()

# Convert the cleaned data to a pdata.frame
df_f_clean <- pdata.frame(df_f_clean, index = c("gridcell_id", "year"))

# Set up k-fold cross-validation (k = 10)
k <- 10
set.seed(12)
folds <- createFolds(df_f_clean$log_mangrove_area, k = k, list = TRUE)

# Initialize vectors to store results
rmse_values <- c()
mae_values <- c()
all_predictions <- data.frame()
all_residuals <- data.frame()

for(i in 1:k) {
  # Split data into training and validation sets
  train_indices <- folds[[i]]
  train_data <- df_f_clean[train_indices, ]
  validation_data <- df_f_clean[-train_indices, ]

  # Fit the model on the training set
  model <- plm(log_mangrove_area ~ sst_hottest + sst_hottest_sq + 
               preci + preci_sq + logGDPpc + logGDPpc_sq + 
               lag_gap_density + lag_gap_density_sq, 
               data = train_data, model = "within")

  # Predict on the validation set
  validation_data$predicted <- predict(model, newdata = validation_data)

  # Calculate performance metrics
  rmse <- sqrt(mean((validation_data$predicted - validation_data$log_mangrove_area)^2,na.rm=TRUE))
  mae <- mean(abs(validation_data$predicted - validation_data$log_mangrove_area),na.rm=TRUE)

  # Store the results
  rmse_values <- c(rmse_values, rmse)
  mae_values <- c(mae_values, mae)

  # Store predictions and residuals for plotting
  validation_data$fold <- i
  all_predictions <- rbind(all_predictions, validation_data)
  all_residuals <- rbind(all_residuals, data.frame(
    log_mangrove_area = validation_data$log_mangrove_area,
    residual = validation_data$log_mangrove_area - validation_data$predicted,
    predicted = validation_data$predicted,
    fold = i
  ))
}

# Print average performance metrics
performance <- data.frame(
  RMSE = mean(rmse_values,na.rm=T),
  MAE = mean(mae_values,na.rm=T)
)
print(performance)

# Split data into training (70%) and test set (30%) for final model
set.seed(123)
train_index <- createDataPartition(df_f_clean$log_mangrove_area, p = 0.7, list = FALSE)
train_data <- df_f_clean[train_index, ]
test_data <- df_f_clean[-train_index, ]

# Fit the model on the training set
final_model <- plm(log_mangrove_area ~ sst_hottest + sst_hottest_sq + 
                   preci + preci_sq + logGDPpc + logGDPpc_sq + 
                   lag_gap_density + lag_gap_density_sq, 
                   data = train_data, model = "within")

# Predict on the test set
test_data$predicted <- predict(final_model, newdata = test_data)

# Plot mosaic of training and validation data across folds
glimpse(all_predictions)
train_val_plot <- ggplot(all_predictions, aes(x = as.integer(as.character(year)), y = as.double(gridcell_id), fill = as.factor(fold))) +
  facet_wrap(~fold,ncol=5)+
  scale_fill_scico_d(palette="glasgow","Folds")+
  geom_tile() +
  labs(title = "A. Training and Validation Data Across Folds",
       x = "Year",
       y = "Gridcell ID",
       fill = "Fold") +
  theme_minimal() + 
  xlim(c(2014,2020))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange/Figures/Response2Rev/Folds.png",dpi=300)
train_val_plot

# Individual predictions vs actuals for each fold
individual_pred_plot <- ggplot(all_predictions, aes(x = log_mangrove_area, y = predicted, color = as.factor(fold))) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_color_scico_d(palette="glasgow","Folds")+
  #facet_wrap(~ fold) +
  labs(title = "C. Actual vs Predicted Values by Fold",
       x = "Actual log(Mangrove Area)",
       y = "Predicted log(Mangrove Area)") +
  theme_minimal()
individual_pred_plot

# Aggregated RMSE results from cross-validation
rmse_plot <- ggplot(data.frame(fold = 1:k, rmse = rmse_values), aes(x = factor(fold), y = rmse)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "B. RMSE Across Folds",
       x = "Fold",
       y = "RMSE") +
  theme_minimal()
rmse_plot
# Distribution of residuals

glimpse(all_residuals)
residual_plot <- ggplot(all_residuals, aes(y = residual, x= predicted, color = as.factor(fold))) +
  geom_point(alpha = 0.7) +
  labs(title = "Distribution of Residuals Across Folds",
       x = "Residuals",
       y = "Count") +
  theme_minimal()
residual_plot

ggarrange(train_val_plot,ggarrange(rmse_plot,individual_pred_plot,ncol=2),ncol=1)

#ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange/Figures/Response2Rev/Folds_Hindcast.png",dpi=300)




#### Within




# Log-transform the mangrove area
model_area_ssthot_cy <- felm(log(mangrove_area) ~ 
                                        sst_hottest+I(sst_hottest^2)+
                                        preci+I(preci^2)+                                   
                                        logGDPpc + I(logGDPpc^2)+
                                        lag_gap_density + I(lag_gap_density^2) 
                                        |gridcell_id  +  year + cy |0|gridcell_id,data=df_f,
                                        weights=log(df_f$mangrove_area+1))
        summary(model_area_ssthot_cy)

#plot(model_area_Onlyssthot$fit,model_area_Onlyssthot$residuals)

# Extract the fixed effects using getfe
fixed_effects <- getfe(model_area_ssthot_cy)
df_cy_factor <- df_f %>%
  left_join(fixed_effects %>% filter(fe=="gridcell_id") %>% 
    mutate(gridcell_id=as.integer(as.character(idx))), by = "gridcell_id") %>%
  left_join(fixed_effects %>% filter(fe=="year") %>% 
    mutate(year=as.integer(as.character(idx))), by = "year") %>% 
  left_join(fixed_effects %>% filter(fe=="cy") %>% 
    mutate(cy=(as.character(idx))), by = "cy")

# Adjust the dependent variable by subtracting the fixed effects
df_cy_factor$adjusted_log_mangrove_area <- log(df_cy_factor$mangrove_area) - df_cy_factor$effect.x - df_cy_factor$effect.y - df_cy_factor$effect
# Create the plot
plot_noFE_sst_factor <- ggplot(df_cy_factor, 
  aes(x= sst_hottest, y = adjusted_log_mangrove_area))+
  geom_point(aes(group=gridcell_id),alpha=0.3) +
  geom_smooth(method="lm",formula="y~poly(x,2)")+
  theme_minimal() +
  scale_color_scico(direction=-1)

plot_noFE_logGDPpc_factor <- ggplot(df_cy_factor, 
  aes(x= (logGDPpc), y = adjusted_log_mangrove_area))+
  geom_point(aes(group=gridcell_id),alpha=0.3) +
  geom_smooth(method="lm",formula="y~poly(x,2)")+
  theme_minimal() +
  scale_color_scico(direction=-1)+
  xlab("Gridcell-level Log GDP per capita")

plot_noFE_factor <- ggarrange(plot_noFE_sst_factor, plot_noFE_logGDPpc_factor)
plot_noFE_factor
# ggplot(df_cy_factor, 
#   aes(x= logGDPpc/logGDPpc_country, y = adjusted_log_mangrove_area))+
#   geom_line(aes(group=gridcell_id)) +
#   geom_smooth(method="lm",formula="y~poly(x,2)")
#   theme_minimal() +
#   scale_color_scico(direction=-1)
  
# ggplot(df_f2, 
#   aes(x= logGDPpc_country, y = adjusted_log_mangrove_area))+
#   geom_line(aes(group=gridcell_id,alpha = log(mangrove_area))) +
#   geom_smooth(method="lm",formula="y~poly(x,2)")
#   theme_minimal() +
#   scale_color_scico(direction=-1)

# ggplot( df_f2 %>% group_by(gridcell_id) %>% 
#   summarise(sst_hottest = mean(sst_hottest,na.rm=TRUE), 
#   adjusted_log_mangrove_area = mean(adjusted_log_mangrove_area,na.rm=TRUE),
#    log_mangrove_area = mean(log_mangrove_area,na.rm=T)), aes(x= sst_hottest, y = adjusted_log_mangrove_area))+
#   geom_point(aes(col=log_mangrove_area),alpha = 0.5) +
#   theme_minimal() +
#   scale_color_scico(direction=-1)

#   outliers_gridcells  <- df_cy_factor  %>% filter(adjusted_log_mangrove_area < quantile(adjusted_log_mangrove_area,0.01,na.rm=TRUE) | 
#   adjusted_log_mangrove_area > quantile(adjusted_log_mangrove_area,0.99,na.rm=TRUE)) %>% 
#     select(gridcell_id,countrycode,) %>% unique()

#   glimpse(outliers_gridcells)

# '%notin%' <- Negate('%in%')
# ggplot(df_f2 %>% filter(gridcell_id  %notin% df_f3$gridcell_id), aes(x= sst_hottest, y = adjusted_log_mangrove_area))+
#   geom_point(aes(group=gridcell_id,col=log_mangrove_area),alpha = 0.5) +
#   theme_minimal() +
#   scale_color_scico(direction=-1)

# ggplot(df_f2 %>% filter(gridcell_id  %in% df_f3$gridcell_id), aes(x= sst_hottest, y = adjusted_log_mangrove_area))+
#   geom_point(aes(group=gridcell_id,col=log_mangrove_area),alpha = 0.5) +
#   theme_minimal() +
#   scale_color_scico(direction=-1)

# ggplot(df_f2,# %>% filter(gridcell_id  %notin% df_f3$gridcell_id), 
#   aes(x= logGDPpc, y = adjusted_log_mangrove_area))+
#   geom_point(aes(group=gridcell_id,col=log_mangrove_area),alpha = 0.5) +
#   theme_minimal() +
#   scale_color_scico(direction=-1)

# ggplot(df_f2 %>% filter(gridcell_id  %in% df_f3$gridcell_id), aes(x= logGDPpc, y = adjusted_log_mangrove_area))+
#   geom_point(aes(group=gridcell_id,col=log_mangrove_area),alpha = 0.5) +
#   theme_minimal() +
#   scale_color_scico(direction=-1)

# glimpse(df_f2)  
  
#   mangrove_sst_plot <- ggplot(df_f2 %>% filter(gridcell_id  %notin% df_f3$gridcell_id), aes(x = sst_hottest, y = adjusted_log_mangrove_area)) +
#   geom_point(alpha = 0.3) +
#   #geom_line(aes(group=gridcell_id),alpha = 0.5) +
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = T, color = "blue") +
#   labs(title = "De-meaned values",
#        x = "SST Hottest Month (C)",
#        y = "Log(Mangrove Area) No F.E.") +
#   theme_minimal()
# mangrove_sst_plot












# df_f2 <- df_f2 %>% arrange(gridcell_id,year) %>% 
#   mutate(lag_mangrove_area=lag(mangrove_area,1))
# Convert the cleaned data to a pdata.frame
df_f_clean <- pdata.frame(df_cy_factor, index = c("gridcell_id", "year"))

# Set up k-fold cross-validation (k = 10)
k <- 10
set.seed(12)
folds <- createFolds(df_f_clean$adjusted_log_mangrove_area, k = k, list = TRUE)

# Initialize vectors to store results
rmse_values <- c()
mae_values <- c()
all_predictions <- data.frame()
all_residuals <- data.frame()
all_train <- data.frame()

for(i in 1:k) {
  # Split data into training and validation sets
  print(i)
  train_indices <- folds[[i]]
  train_data <- as.data.frame(df_f_clean[train_indices, ])
  train_data$fold <- i
  validation_data <- as.data.frame(df_f_clean[-train_indices, ])


  validation_data  <- validation_data %>% filter(countrycode %in% unique(train_data$countrycode))
  validation_data  <- validation_data %>% filter(gridcell_id %in% unique(train_data$gridcell_id))
  validation_data  <- validation_data %>% filter(cy %in% unique(train_data$cy))
  
  # Fit the model on the training set
  # Fit the model on the training set
  model <- lm(adjusted_log_mangrove_area ~ sst_hottest + I(sst_hottest^2) + 
              preci + I(preci^2) + logGDPpc + I(logGDPpc^2) + 
              lag_gap_density + I(lag_gap_density^2) , 
            data = train_data, weights = log(mangrove_area + 1))
               summary(model)

  # Predict on the validation set
  validation_data$predicted <- predict(model, newdata = validation_data)

  # Calculate performance metrics
  rmse <- sqrt(mean((validation_data$predicted - validation_data$adjusted_log_mangrove_area)^2,na.rm=TRUE))
  mae <- mean(abs(validation_data$predicted - validation_data$adjusted_log_mangrove_area),na.rm=TRUE)

  # Store the results
  rmse_values <- c(rmse_values, rmse)
  mae_values <- c(mae_values, mae)

  # Store predictions and residuals for plotting
  validation_data$fold <- i
  all_predictions <- rbind(all_predictions, validation_data)
  all_train <- rbind(all_train, train_data)
  all_residuals <- rbind(all_residuals, data.frame(
    log_mangrove_area = validation_data$log_mangrove_area,
    residual = validation_data$log_mangrove_area - validation_data$predicted,
    predicted = validation_data$predicted,
    fold = i
  ))
}


glimpse(all_predictions)
glimpse(train_data)
train_val <- rbind(all_predictions %>% select(fold,gridcell_id,year)%>% mutate(Data="Validating"), 
 all_train %>% select(gridcell_id,fold,year) %>% mutate(Data="Training"))

train_val_plot <- ggplot(train_val, aes(x = as.integer(as.character(year)), y = as.double(gridcell_id), fill = as.factor(Data))) +
  facet_wrap(~fold,ncol=5)+
  scale_fill_scico_d(palette="lajolla","Data",direction=1,begin=0.2,end=0.8)+
  geom_tile()+
  #geom_tile(data = all_predictions, aes(x = as.integer(as.character(year)), y = as.double(gridcell_id), fill = as.factor(fold))) +
  #geom_tile(data=train_data,aes(x = as.integer(as.character(year)), y = as.double(gridcell_id)), fill = "black")+
  labs(title = "A. Training and Validation Data Across Folds",
       x = "Year",
       y = "Gridcell ID",
       fill = "Data") +
  theme_minimal() + 
  xlim(c(2014,2020))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange/Figures/Response2Rev/Folds.png",dpi=300)
train_val_plot

#save(all_residuals, file = "Data/output/all_residuals.RData")
#load( file = "Data/output/all_residuals.RData")
# Display the results
rmse_values
mae_values
kfolds_predicted <- ggplot(all_predictions, aes(x = adjusted_log_mangrove_area, y = predicted, col=factor(fold))) +
  #geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_point(aes(group=gridcell_id),alpha=0.3) + 
  labs(title = "K-fold predictions",
       x = "Observed (De-meaned Log Mangrove Area)",
       y = "Predicted", col="Fold") +
  theme_minimal()+
  scale_color_scico_d()
kfolds_predicted
# # Plotting residuals
# ggplot(all_predictions, aes(x = adjusted_log_mangrove_area, y = predicted - adjusted_log_mangrove_area)) +
#   #geom_abline(slope = 1, intercept = 0, color = "red") +
#   geom_point(aes(col=factor(year)),alpha=0.5) +
#   labs(title = "Residuals across folds",
#        x = "Observed",
#        y = "Predicted")







# Convert pseries columns to numeric
all_residuals <- all_residuals %>%
  mutate(across(where(is.pseries), ~ as.numeric(as.character(.))))

# Convert fold to factor
all_residuals$fold <- as.factor(all_residuals$fold)

# Ensure proper data types
all_prediction <- as.data.frame(all_predictions) %>%
  mutate(
    adjusted_log_mangrove_area = as.numeric(as.character(adjusted_log_mangrove_area)),
    predicted = as.numeric(as.character(predicted)),
    #residual = as.numeric(as.character(residual)),
    fold = as.factor(fold)
  )



heatmap_adj_pred <- ggplot(all_predictions, aes(x = adjusted_log_mangrove_area, y = predicted)) +
  geom_smooth(method="lm")+
  geom_bin2d(bins = 100) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Prediction",
       x = "Observed (De-meaned Log Mangrove Area)",
       y = "Predicted") +
  theme_minimal()#+
  #coord_cartesian(xlim = c(0.5,1.5),ylim=c(0.5,1.5)) 
heatmap_adj_pred

ggarrange(train_val_plot,ggarrange(kfolds_predicted, heatmap_adj_pred,ncol=2),ncol=1)
ggsave("Figures/Response2Rev/K_folds_NoFE.png",dpi=300)
exp(0.15)
getwd()


glimpse(mcn)





  scen_arealoss_perc_both <- read.csv("Results\\Area\\Proj_Area_Perc_both_SSP370.csv")
 
  glimpse(scen_arealoss_perc_both)
  max(scen_arealoss_perc_both$sst_hot_70,na.rm=T)
  max(scen_arealoss_perc_both %>% filter(is.finite(sst_hottest)) %>% select(sst_hottest),na.rm=T)
  
  ggplot(scen_arealoss_perc_both, aes(x = sst_hot_70,y=sst_hot_70_capped))+
  geom_point(aes(color=R5))

  
  ggplot(scen_arealoss_perc_both, aes(x = log(gdppc5),y=log(gdppc5_capped)))+
  geom_point(aes(color=R5))

scen <- merge(scen,mcn %>% filter(year==2020) %>% dplyr::select("gridcell_id","sst_hottest","logGDPpc"),by="gridcell_id",all=TRUE)
scen$sst_hot_70_capped <- matrixStats::rowMins(cbind(scen$sst_hot_70,rep(max(exp(scen$sst_hottest),na.rm=TRUE),times=length(scen$sst_hot_70))))
scen$gdppc1_capped <- matrixStats::rowMins(cbind(scen$gdppc1,rep(max(exp(scen$logGDPpc),na.rm=TRUE),times=length(scen$gdppc1))))

ggplot(scen_arealoss_perc_both, aes(x = log(gdppc5),y=log(gdppc5_capped)))+
  geom_point(aes(color=R5))