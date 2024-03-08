
libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe")

lapply(libraries, library, character.only = TRUE)

mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_aug2023.csv")
mcn_2020 <- mcn %>% filter(year == 2020)
            
sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(start1:end1),c(start1:end1)]
            beta.hat <- coef(model)[c(start1:end1)]
            # Calculate the sum of the coefficients
            sum_coef = sum(beta.hat)
            
            # Compute the standard error of the sum
            sum_se = sqrt(sum(sigma))
            
            # Calculate the t-statistic
            t_stat = sum_coef / sum_se
            
            # Compute the p-value
            p_value = 2 * (1 - pt(abs(t_stat), df = df.residual(model)))


            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(1, 2*x)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            significant <- rep("Not significant (p > 0.05)",length(x))
            significant[which(p_value<0.05)] <- "Significant (p < 0.05)"
            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x, p_value=p_value,significant=significant))
        }


        ## Mediation (start)
                model_holes_ssthot_med <- felm(log(gap_density)~
                                log(Mean_Salinity)  + 
                                #logGDPpc+I(logGDPpc^2)+#I(logGDPpc^3)+
                                #logPop+I(logPop^2)+
                                year:countrycode + 
                                R5:year + 
                                income:year
                                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
                                data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],
                                weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$holes>0 &  is.finite(mcn$logGDPpc))]+1))
                summary( model_holes_ssthot_med)

                model_holes_ssthot_ef1 <- felm(log(holes/mangrove_area)~
                                #log(Mean_Salinity)  + 
                                sst + I(sst^2)+
                                I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP_country/Population_country))+
                                logGDPpc+I(logGDPpc^2)+#I(logGDPpc^3)+
                                logPop+I(logPop^2)+
                                year:countrycode + 
                                R5:year + 
                                income:year
                                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
                                data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],
                                weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc))]+1))
                summary( model_holes_ssthot_ef1)

                model_holes_ssthot_ef2 <- felm(log(holes/mangrove_area)~
                                log(Mean_Salinity)  + 
                                temp + I(temp^2)+
                                sst + I(sst^2)+
                                Mean_Precipitation + I(Mean_Precipitation)+
                                #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                                logGDPpc+I(logGDPpc^2)+#I(logGDPpc^3)+
                                logPop+I(logPop^2)+
                                year:countrycode + 
                                R5:year + 
                                income:year
                                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
                                data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],
                                weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))]+1))
                summary( model_holes_ssthot_ef2)
                
                0.9363539*0.8866070

            ## Mediation (end)
            #model_holes_ssthot <- felm(log(holes)~
            
            ## Gaps Salinity (start)
                mcn$logSal <- log(mcn$Mean_Salinity)
                subset_mcn0 <- mcn[which(mcn$mangrove_area>1 & 
                                                        #mcn$ntl>0 &
                                                        mcn$gap_density>0  & 
                                                        is.finite(mcn$logGDPpc) & 
                                                        is.finite(mcn$logSal) & 
                                                        is.finite(mcn$Mean_Precipitation) & 
                                                        is.finite(mcn$sst)  & 
                                                        is.finite(mcn$Population_Count_50km) 
                                                        #mcn$year>2012
                                                        ),]
                
                #model_holes_ssthot <- felm(log(holes_size/holes)~
                model_holes_ssthot <- felm(log(gap_density)~
                    #log(holes_size/holes)+
                    #log(holes/mangrove_area)+
                    #lag(holes)+
                    #log(mangrove_area)+
                    #temp + I(temp^2)+
                    sst + I(sst^2)+
                    #sst_hottest + I(sst_hottest^2) +
                    Mean_Precipitation + #I(Mean_Precipitation^2) +
                    #Mean_Salinity + I(Mean_Salinity^2)+
                    #Mean_Precipitation*logGDPpc+
                    #Mean_Precipitation*logPop+
                    logSal + #I(logSal^2)  + 
                    #log(Sum_GDP_50km/Population_Count_50km)+
                    #I(log(Sum_GDP_50km)/log(GDP))+
                    #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                    #log(Population_Count_50km)+ 
                    #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                    #log(GDP/Population)sst_hottest + I(sst_hottest^2)+
                    #I(Mean_Precipitation^2) + Mean_Precipitation +
                    #log(Mean_Salinity)+
                    #factor(rich)*log(Sum_GDP_50km/Population_Count_50km)+ #
                    #I(log(GDP/Population))+ 
                    #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                    log(Population_Count_50km)+ I(log(Population_Count_50km)^2)+ 
                    logGDPpc + #I(logGDPpc^2)
                    #logPop + #factor(rich)*logGDPpc+factor(rich)*I(logGDPpc^2)+#I(logGDPpc^3)+
                    #factor(rich)*logPop+factor(rich)*I(logPop^2)+#I(logPop^3)
                    #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                    #income:log(ntl) + income:I(log(ntl)^2)
                    #log(ntl) + I(log(ntl)^2) + #log(GDP_country) + 
                    #I(logGDPpc/logGDPpc_country)+
                    logGDPpc_country# + I(logGDPpc_country)
                    #year:countrycode+
                    #R5:year + 
                    #income:year
                    |gridcell_id |0|gridcell_id,
                    data=subset_mcn0,
                    weights=(subset_mcn$mangrove_area+1))
                summary( model_holes_ssthot)
                
                #save(model_holes_ssthot,file="Models/Round1/pref_holes_model.RData") 

                coefs_pref_holes <- plot_coefs(model_holes_ssthot, ci_level = 0.95,
                    coefs = c("Precipitation"="Mean_Precipitation","SST"="sst","SST2"="I(sst^2)","Log Salinity" = "logSal","Log GDPpc"="logGDPpc","Log Population"="logPop"))
                coefs_pref_holes



                sq_estimate_sst_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"temp","Gaps")
                sq_estimate_preci_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Precipitation","Gaps")
                sq_estimate_pop_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"logPop","Gaps")
                sq_estimate_gdp_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"logGDPpc","Gaps")
                sq_estimate_sal_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"logSal","Gaps")

                #sq_estimate_sal_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Salinity","Gaps")
                ggplot(sq_estimate_pop_holes)+geom_point(aes(x=temp,y=gestimated))
                ggplot(sq_estimate_gdp_holes)+geom_point(aes(x=temp,y=gestimated))

                sq_estimate_pop_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_pop_holes$significant[which(sq_estimate_pop_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_gdp_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_gdp_holes$significant[which(sq_estimate_gdp_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_sst_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_sst_holes$significant[which(sq_estimate_sst_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_preci_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_preci_holes$significant[which(sq_estimate_preci_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_sal_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_sal_holes$significant[which(sq_estimate_sal_holes$p_value<0.05)] <- "Significant (p < 0.05)"

                save(sq_estimate_sst_holes,file="Models/Round1/sq_estimate_sst_holes.RData") 
                save(sq_estimate_preci_holes,file="Models/Round1/sq_estimate_sst_holes.RData") 
                save(sq_estimate_gdp_holes,file="Models/Round1/sq_estimate_sst_holes.RData") 
                save(sq_estimate_pop_holes,file="Models/Round1/sq_estimate_sst_holes.RData") 
                save(sq_estimate_sal_holes,file="Models/Round1/sq_estimate_sst_holes.RData") 

                



                # Assuming you've already loaded necessary libraries and data
# For plotting, we'll make use of ggplot2
library(ggplot2)

# 1. Residuals vs Fitted
residuals <- resid(model_holes_ssthot)
fitted <- fitted.values(model_holes_ssthot)
glimpse(fitted)

df_resid <- data.frame(Residuals = residuals, Fitted = fitted)
names(df_resid) <- c("Residuals" , "Fitted")
glimpse(df_resid)

ggplot(df_resid, aes(x = Fitted, y = Residuals)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "loess") + 
  theme_minimal() +
  ggtitle("Residuals vs Fitted")

# 2. Check for Normality of Residuals
ggplot(df_resid, aes(sample = Residuals)) +
  stat_qq() +
  geom_qq_line() +
  theme_minimal() +
  ggtitle("QQ Plot of Residuals")


# 3. Residuals over Time (assuming 'year' is your time variable)
df_resid$Year <- subset_mcn0$year


ggplot(df_resid, aes(x = Year, y = Residuals)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "loess") + 
  theme_minimal() +
  ggtitle("Residuals over Time")

# 4. Actual vs Predicted
df_resid$Actual <- log(subset_mcn0$gap_density)
df_resid$Predicted <- fitted

ggplot(df_resid, aes(x = Actual, y = Predicted)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red") + 
  theme_minimal() +
  ggtitle("Actual vs Predicted")



# 1. Extract Fixed Effects
fe_gridcell <- getfe(model_holes_ssthot) %>% filter(fe=="gridcell_id")
fe_year <- getfe(model_holes_ssthot) %>% filter(fe=="year")
glimpse(fe_gridcell)
glimpse(fe_year)
hist(fe_gridcell$effect)
subset_mcn2 <- merge(subset_mcn0,fe_gridcell,by.x="gridcell_id",by.y="idx")
subset_mcn2 <- merge(subset_mcn2,fe_year,by.x="year",by.y="idx")
glimpse(subset_mcn2)

ggplot(subset_mcn2)+geom_point(aes(x=logGDPpc_country,y=effect.x,col=temp))+ylab("Gridcell_id Fixed Effect")+theme_bw()+scale_color_scico()+geom_smooth(aes(x=logGDPpc_country,y=effect.x))

# 2. Compute Adjusted Values
subset_mcn2$adjusted_actual <- log(subset_mcn2$gap_density) - subset_mcn2$effect.x - subset_mcn2$effect.y

subset_mcn2$predicted <- as.numeric(fitted(model_holes_ssthot))
glimpse(subset_mcn2)
names(subset_mcn2)[length(subset_mcn2)] <-"predicted"
subset_mcn2$adjusted_predicted <- subset_mcn2$predicted - subset_mcn2$effect.x - subset_mcn2$effect.y

# Create a dataframe for plotting
#df_adjusted <- data.frame(AdjustedActual = adjusted_actual, AdjustedPredicted = adjusted_predicted)

# 3. Plot Adjusted Actual vs Adjusted Predicted
#library(ggplot2)

ggplot(subset_mcn2, aes(x = adjusted_predicted, y = adjusted_actual)) + 
  geom_point(aes(color=gridcell_id)) + 
  geom_smooth(method = "lm", color = "red") + 
  theme_minimal() +
  ggtitle("Adjusted Actual vs Adjusted Predicted")



  # Refit the model with a quadratic term for the predictor that seems most likely to have a non-linear relationship
    model_holes_ssthot_v2 <- felm(log(gap_density) ~ 
                              sst + I(sst^2) + 
                              # ... other predictors ...
                              | gridcell_id + year | 0 | gridcell_id,
                              data=subset_mcn,
                              weights=(subset_mcn$mangrove_area + 1))

    summary(model_holes_ssthot_v2)




                

                
                Models_ssthot_plot_sal_holes <- ggplot(sq_estimate_sal_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#06215b") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#06215b",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log Sea Surface Salinity (pss)**")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_sal_holes

                
                mcn_2020 <- mcn %>% filter(year == 2020)
                histogram_plot_sal_holes <- ggplot(mcn_2020, aes(x = logSal)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#06215b") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_sal_holes <- ggarrange(Models_ssthot_plot_sal_holes, histogram_plot_sal_holes,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_sal_holes
                
                Models_ssthot_plot_gdp_holes <- ggplot(sq_estimate_gdp_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log GDP per capita")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_gdp_holes

                histogram_plot_gdp_holes <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_gdp_holes <- ggarrange(Models_ssthot_plot_gdp_holes, histogram_plot_gdp,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_gdp_holes

                Models_ssthot_plot_pop_holes <- ggplot(sq_estimate_pop_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log Population")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_pop_holes
                
            
                histogram_plot <- ggplot(mcn_2020, aes(x = logPop)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_pop_holes <- ggarrange(Models_ssthot_plot_pop_holes, histogram_plot,legend="none",
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_ssthot_plot_pop_holes

                Models_ssthot_plot_holes <- ggplot(sq_estimate_sst_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1°C Increase")+xlab("Annual Air Temperature (°C)")#+
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_holes

                histogram_plot_ssthot_holes <- ggplot(mcn_2020, aes(x = temp)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_ssthot_plot_holes<-ggarrange(Models_ssthot_plot_holes, histogram_plot_ssthot_holes,legend="none", 
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_ssthot_plot_holes

                Models_preci_hot_plot_holes <- ggplot(sq_estimate_preci_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1 mm Increase")+xlab("Monthly Mean Precipitation (mm)")
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                    Models_preci_hot_plot_holes

                histogram_plot_preci_holes <- ggplot(mcn_2020, aes(x = Mean_Precipitation)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_preci_hot_plot_holes<-ggarrange(Models_preci_hot_plot_holes, histogram_plot_preci_holes,legend="none", 
                            ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_preci_hot_plot_holes


                df_dummy <- data.frame(value = c(rnorm(100), rnorm(100), rnorm(100)), Variable = rep(c("Climatic", "Socioeconomic", "Interdependent"), each = 100))
                df_dummy$Variable <- factor(df_dummy$Variable,levels=c("Climatic", "Socioeconomic", "Interdependent"))
                dummy_plot <- ggplot(df_dummy, aes(value, fill = Variable)) +
                geom_histogram(color = "black") +
                scale_fill_manual(values = c("Climatic" = "#e9995c", "Socioeconomic" = "#25625f", "Interdependent" = "#06215b")) +
                theme(legend.position="bottom")  # Remove all non-data ink

                # Extract the legend
                combined_legend <- get_legend(dummy_plot)


                arr_area_loss_plot <- ggarrange(ggarrange(ggarrange(Models_ssthot_plot_holes,
                                    Models_preci_hot_plot_holes,
                                    Models_ssthot_plot_gdp_holes,
                                    Models_ssthot_plot_pop_holes,nrow=2,ncol=2),Models_ssthot_plot_sal_holes,widths=c(3,2),ncol=2),
                                    combined_legend,ncol=1,heights=c(11,1))
                
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Gaps Model", face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n **: p<0.05; ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        )

                

                # Printing the annotated figure
                print(annotated_figure)
                ggsave("Figures/Draft/Model_Gaps_V2.png",dpi=300)

                
                arr_area_loss_plot <- ggarrange(ggarrange(Models_ssthot_plot_holes,
                                    Models_preci_hot_plot_holes,
                                    Models_ssthot_plot_gdp_holes,
                                    Models_ssthot_plot_pop_holes,ncol=4),
                                    combined_legend,ncol=1,heights=c(11,1))
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Gaps Model", face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n **: p<0.05; ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        )

                # Printing the annotated figure
                print(annotated_figure)
                ggsave("Figures/Draft/Gaps_Modelv2.png",dpi=600)
            
            ## Gaps Salinity (end)

            ## Gaps Salinity (start)
                mcn$logSal <- log(mcn$Mean_Salinity)

                 
                model_holes_ssthot <- felm(log(holes/mangrove_area)~
                    #log(mangrove_area)+
                    #Mean_Precipitation*temp + I(temp^2)+
                    sst + I(sst^2)+
                    #sst_hottest + I(sst_hottest^2) +
                    #Mean_Precipitation+I(Mean_Precipitation^2) +
                    #temp + I(temp^2)+
                    #Mean_Salinity + I(Mean_Salinity^2)+
                    #logSal  +
                    Mean_Precipitation+
                    spei+
                    I(Mean_Precipitation)*logGDPpc +
                    I(Mean_Precipitation)*logPop +
                    #log(Sum_GDP_50km/Population_Count_50km)+
                    #I(log(Sum_GDP_50km)/log(GDP))+
                    #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                    #log(Population_Count_50km)+ 
                    #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                    #log(GDP/Population)sst_hottest + I(sst_hottest^2)+
                    #I(Mean_Precipitation^2) + Mean_Precipitation +
                    #log(Mean_Salinity)+
                    #factor(rich)*log(Sum_GDP_50km/Population_Count_50km)+ #
                    #I(log(GDP/Population))+ 
                    #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                    #log(Population_Count_50km)+ 
                    #log(logGDPpc)+ 
                    #Mean_Precipitation*logGDPpc+Mean_Precipitation*I(logGDPpc^2)+#I(logGDPpc^3)+
                    #Mean_Precipitation*logPop+Mean_Precipitation*I(logPop^2)+#I(logPop^3)
                    #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                    #logGDPpc+I(logGDPpc^2)+
                    #logPop+I(logPop^2)+
                    year:countrycode+
                    R5:year + 
                    income:year
                    |gridcell_id + year+ countrycode+R5|0|gridcell_id,
                    data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],
                    weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))]+1))
                summary( model_holes_ssthot)
                
                mcn$logSal <- log(mcn$Mean_Salinity)
                model_holes_ssthot <- felm(log(holes/mangrove_area)~
                    #log(mangrove_area)+
                    #Mean_Precipitation*temp + I(temp^2)+
                    sst + I(sst^2)+
                    #sst_hottest + I(sst_hottest^2) +
                    Mean_Precipitation+I(Mean_Precipitation^2) +
                    #temp + I(temp^2)+
                    #Mean_Salinity + I(Mean_Salinity^2)+
                    logSal + I(logSal^2)  +
                    #log(Sum_GDP_50km/Population_Count_50km)+
                    #I(log(Sum_GDP_50km)/log(GDP))+
                    #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                    #log(Population_Count_50km)+ 
                    #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                    #log(GDP/Population)sst_hottest + I(sst_hottest^2)+
                    #I(Mean_Precipitation^2) + Mean_Precipitation +
                    #log(Mean_Salinity)+
                    #factor(rich)*log(Sum_GDP_50km/Population_Count_50km)+ #
                    #I(log(GDP/Population))+ 
                    #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                    #log(Population_Count_50km)+ 
                    #log(logGDPpc)+ 
                    #Mean_Precipitation*logGDPpc+Mean_Precipitation*I(logGDPpc^2)+#I(logGDPpc^3)+
                    #Mean_Precipitation*logPop+Mean_Precipitation*I(logPop^2)+#I(logPop^3)
                    #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                    logGDPpc+I(logGDPpc^2)+
                    logPop+I(logPop^2)+
                    year:countrycode+
                    R5:year + 
                    income:year
                    |gridcell_id + year+ countrycode+R5|0|gridcell_id,
                    data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],
                    weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))]+1))
                summary( model_holes_ssthot)


                sq_estimate_sst_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"temp","Gaps")
                sq_estimate_preci_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Precipitation","Gaps")
                sq_estimate_pop_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"logPop","Gaps")
                sq_estimate_gdp_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"logGDPpc","Gaps")
                sq_estimate_sal_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"logSal","Gaps")
                #sq_estimate_sal_holes <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Salinity","Gaps")
                ggplot(sq_estimate_pop_holes)+geom_point(aes(x=temp,y=gestimated))
                ggplot(sq_estimate_gdp_holes)+geom_point(aes(x=temp,y=gestimated))

                sq_estimate_pop_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_pop_holes$significant[which(sq_estimate_pop_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_gdp_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_gdp_holes$significant[which(sq_estimate_gdp_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_sst_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_sst_holes$significant[which(sq_estimate_sst_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_preci_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_preci_holes$significant[which(sq_estimate_preci_holes$p_value<0.05)] <- "Significant (p < 0.05)"
                sq_estimate_sal_holes$significant <- "Not significant (p > 0.05)"
                sq_estimate_sal_holes$significant[which(sq_estimate_sal_holes$p_value<0.05)] <- "Significant (p < 0.05)"

                
                Models_ssthot_plot_sal_holes <- ggplot(sq_estimate_sal_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#06215b") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#06215b",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log Sea Surface Salinity (pss)**")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_sal_holes

                
                mcn_2020 <- mcn %>% filter(year == 2020)
                histogram_plot_sal_holes <- ggplot(mcn_2020, aes(x = logSal)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#06215b") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_sal_holes <- ggarrange(Models_ssthot_plot_sal_holes, histogram_plot_sal_holes,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_sal_holes
                
                Models_ssthot_plot_gdp_holes <- ggplot(sq_estimate_gdp_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log GDP per capita")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_gdp_holes

                histogram_plot_gdp_holes <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_gdp_holes <- ggarrange(Models_ssthot_plot_gdp_holes, histogram_plot_gdp,
                legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
                Models_ssthot_plot_gdp_holes

                Models_ssthot_plot_pop_holes <- ggplot(sq_estimate_pop_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                        theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                        geom_hline(aes(yintercept=0),linetype="dashed")+
                        ylab("Effect of 1% Increase")+xlab("Log Population")
                        #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                        #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                        #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_pop_holes
                
            
                histogram_plot <- ggplot(mcn_2020, aes(x = logPop)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                #library(gridExtra)
                Models_ssthot_plot_pop_holes <- ggarrange(Models_ssthot_plot_pop_holes, histogram_plot,legend="none",
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_ssthot_plot_pop_holes

                Models_ssthot_plot_holes <- ggplot(sq_estimate_sst_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1°C Increase")+xlab("Annual Air Temperature (°C)")#+
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_holes

                histogram_plot_ssthot_holes <- ggplot(mcn_2020, aes(x = temp)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_ssthot_plot_holes<-ggarrange(Models_ssthot_plot_holes, histogram_plot_ssthot_holes,legend="none", 
                ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_ssthot_plot_holes

                Models_preci_hot_plot_holes <- ggplot(sq_estimate_preci_holes)+
                    geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1 mm Increase")+xlab("Monthly Mean Precipitation (mm)")
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                    Models_preci_hot_plot_holes

                histogram_plot_preci_holes <- ggplot(mcn_2020, aes(x = Mean_Precipitation)) +
                        geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                        #geom_density(alpha = .2, fill = "#FF6666") +
                        theme_bw() +
                        xlab("Log Pop") +
                        ylab("Density") + theme_void()
                
                Models_preci_hot_plot_holes<-ggarrange(Models_preci_hot_plot_holes, histogram_plot_preci_holes,legend="none", 
                            ncol = 1,heights=c(3,1),align="hv", hjust=0)

                Models_preci_hot_plot_holes


                df_dummy <- data.frame(value = c(rnorm(100), rnorm(100), rnorm(100)), Variable = rep(c("Climatic", "Socioeconomic", "Interdependent"), each = 100))
                df_dummy$Variable <- factor(df_dummy$Variable,levels=c("Climatic", "Socioeconomic", "Interdependent"))
                dummy_plot <- ggplot(df_dummy, aes(value, fill = Variable)) +
                geom_histogram(color = "black") +
                scale_fill_manual(values = c("Climatic" = "#e9995c", "Socioeconomic" = "#25625f", "Interdependent" = "#06215b")) +
                theme(legend.position="bottom")  # Remove all non-data ink

                # Extract the legend
                combined_legend <- get_legend(dummy_plot)


                arr_area_loss_plot <- ggarrange(ggarrange(ggarrange(Models_ssthot_plot_holes,
                                    Models_preci_hot_plot_holes,
                                    Models_ssthot_plot_gdp_holes,
                                    Models_ssthot_plot_pop_holes,nrow=2,ncol=2),Models_ssthot_plot_sal_holes,widths=c(3,2),ncol=2),
                                    combined_legend,ncol=1,heights=c(11,1))
                
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Gaps Model", face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n **: p<0.05; ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        )

                

                # Printing the annotated figure
                print(annotated_figure)
                ggsave("Figures/Draft/Model_Gaps_V2.png",dpi=300)

                
                arr_area_loss_plot <- ggarrange(ggarrange(Models_ssthot_plot_holes,
                                    Models_preci_hot_plot_holes,
                                    Models_ssthot_plot_gdp_holes,
                                    Models_ssthot_plot_pop_holes,ncol=4),
                                    combined_legend,ncol=1,heights=c(11,1))
                annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                        top = text_grob("Gaps Model", face = "bold", size = 14),
                                        bottom = text_grob("Sum of coefficients significane\n **: p<0.05; ***: p<0.01", face = "italic", size = 10),
                                        #left = text_grob("Left annotation", rot = 90, size = 10),
                                        #right = text_grob("Right annotation", rot = -90, size = 10)
                                        )

                # Printing the annotated figure
                print(annotated_figure)
                ggsave("Figures/Draft/Gaps_Modelv2.png",dpi=600)
            
            ## Gaps Salinity (end)





































        ## Project Salinity (Annual Changes)
                    
                    mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
                    scen <-    read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen.csv")
                    glimpse(scen)
                    glimpse(mcn)

                    scen$sst <- scen$sst_85 
                    scen$sst_hottest <- scen$sst_hot_85
                    scen$temp <- scen$temp_85
                    scen$logGDPpc <- log(scen$gdp_ssp5/scen$pop_ssp5)
                    scen$logPop <- log(scen$pop_ssp5)
                    scen$Mean_Precipitation <- scen$preci_85 
                    scen$Longitude <- scen$Longitude
                    scen$Latitude <- scen$Latitude

                    var_annual_change <- c("sst","sst_hottest","temp","logGDPpc","logPop","Mean_Precipitation","Longitude","Latitude") 

                    scen <- scen %>%
                    group_by(gridcell_id) %>%
                    arrange(year) %>%
                    mutate(across(all_of(var_annual_change), ~ . - lag(.), .names = 'change_{.col}'))
                    
                    
                    var_annual_change <- c("year","change_sst","change_sst_hottest","change_temp","change_logGDPpc","change_logPop","change_Mean_Precipitation","gridcell_id","Longitude","Latitude") 


                    pred_data <- ungroup(scen[which(names(scen)%in%c(var_annual_change))])
                    pred_data <- pred_data[which(is.finite(pred_data$change_logGDPpc)),]
                    glimpse(pred_data)


                    load("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\change_sss_70p_nooutliers01.RData") 
                    pred_data$Mean_Salinity_change <- predict(change_sss.rf, newdata = pred_data)
                    pred_data <- pred_data[,which(names(pred_data) %in% c("Mean_Salinity_change","gridcell_id","year"))]
                    scen2 <- merge(scen,pred_data,by=c("gridcell_id","year"))
                    scen_2100 <- scen2 %>% filter(year==2100)
                    mcn_2020 <- mcn %>% filter(year==2020)
                    mcn_2020$Mean_Salinity2020 <- mcn_2020$Mean_Salinity
                    
                    scen2$Mean_Salinity_change <- ifelse(is.na(scen2$Mean_Salinity_change), 0, scen2$Mean_Salinity_change)

                    scen3 <- scen2 %>%
                        arrange(gridcell_id, year) %>%
                        group_by(gridcell_id) %>%
                        mutate(Mean_Salinity_Change_cumulative = cumsum(Mean_Salinity_change)) %>%
                        ungroup()


                    scen3_2100<- ungroup(scen3 %>% filter(year==2100))
                    gneg <- scen3_2100$gridcell_id[which(scen3_2100$Mean_Salinity_Change_cumulative<0)]
                    
                    scen3$pos <- 1
                    scen3$pos[which(scen3$gridcell_id %in% gneg)] <- 0
                    years_i <- c(2030,2040,2050,2060,2070,2080,2090,2100)
                    glimpse(scen3)
                    
                    mcn_2020 <- mcn %>% filter(year==2020)
                    mcn_2020$Mean_Salinity2020 <- mcn_2020$Mean_Salinity
                    mcn_2020 <- mcn_2020[,which(names(mcn_2020) %in% c("Mean_Salinity2020","gridcell_id","countrycode","R5"))]
                    scen3 <- merge(scen3,mcn_2020,by="gridcell_id",all=T)
                    
                    
                    scen3$Salinity_perc <- 100*scen3$Mean_Salinity_Change_cumulative/scen3$Mean_Salinity2020
                    scen_sal_85 <- scen3[,which(names(scen3) %in% c("gridcell_id","year","Salinity_perc","Mean_Salinity_Change_cumulative","Mean_Salinity_change"))]
                    write.csv(scen_sal_85,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen_sal_85.csv")
                    write.csv(scen3,"C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\projections\\scen3.csv")
                    
                     
                    
                ## Project Salinity (Annual Changes)   
                   