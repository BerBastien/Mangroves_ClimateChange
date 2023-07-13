 
 ## Libraries
library(viridis)
library("ggridges")
library("lfe")
library("ggridges")
library("stargazer")
library("modelsummary")
library("AICcmodavg")
#install.packages("AICcmodavg")
library("dplyr")
library("randomForest")
library("ggplot2")
library("rpart.plot")
library("plotmo")
library(jtools)
setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
library("scico")
library(ggpubr)



## Function
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
## Function
pal_lapaz <- scico(15, palette = 'batlow')
        pal_lapaz <- pal_lapaz[c(2,7,12)]
        pal_roma <- scico(15, palette = 'roma')

mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        
    # Bins one-by-one (start)   
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        
        model_area_bin <- felm(log(mangrove_area)~
            b8_9C  +b9_10C +b10_11C   + b11_12C + b12_13C + b13_14C + b14_15C+
            b15_16C + b16_17C +b17_18C+ b18_19C+b19_20C + b20_21C +
            b21_22C +b22_23C+ b23_24C  +
            b24_25C   + b25_26C  +b26_27C   + 
            b27_28C + b28_29C + b29_30C  + b30_31C + b31_32C+
            b32_33C + b33_34C + b34_35C+ I(b35_36C + b36_37C + b37_38C )+ 
            I(log(GDP/Population))+
            logGDPpc*rich
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)) ,])
        summary(model_area_bin)

        mcn$threshold <- #mcn$b28_29C + 
            #mcn$b29_30C  + 
            #mcn$b30_31C + 
            #mcn$b31_32C+
            mcn$b32_33C + 
            mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C 
            
        model_area_bin <- felm(log(mangrove_area)~
            threshold + #I(threshold^2)+
            #sst_hottest + I(sst_hottest^2) + 
            #sst + I(sst^2) + 
            Mean_Precipitation+ I(Mean_Precipitation^2)+
            I(log(GDP/Population))+
            logGDPpc*rich
        |gridcell_id + year + countrycode |0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)) ,])
        summary(model_area_bin)


        plot_coefs(model_area_bin,ci=0.9,omit.coefs = c("b9_10C","logGDPpc","logGDPpc:rich","I(log(GDP/Population))","log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"))

        plot_coefs(model_area_bin)

        model_holes_bin <- felm(log(holes)~
            b8_9C  +b9_10C +b10_11C  + b11_12C  + b12_13C + b13_14C + b14_15C+
            b15_16C + b16_17C +b17_18C+ b18_19C+b19_20C + b20_21C +
            b21_22C +b22_23C+ b23_24C  +
            b24_25C   + b25_26C  +b26_27C   + 
            b27_28C + b28_29C + b29_30C  + b30_31C + b31_32C+
            b32_33C + b33_34C + b34_35C+ b35_36C + b36_37C + b37_38C + 
            I(log(GDP/Population))+
            logGDPpc*rich
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)) ,])
        summary(model_holes_bin)
        plot_coefs(model_holes_bin,omit.coefs = c("logGDPpc*rich","I(log(GDP/Population))","log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"))


        model_np_bin <- felm(log(np)~
            b8_9C  +b9_10C +b10_11C  + b11_12C  + b12_13C + b13_14C + b14_15C+
            b15_16C + b16_17C +b17_18C+ b18_19C+b19_20C + b20_21C +
            b21_22C +b22_23C+ b23_24C  +
            b24_25C   + b25_26C  +b26_27C   + 
            b27_28C + b28_29C + b29_30C  + b30_31C + b31_32C+
            b32_33C + b33_34C + b34_35C+ b35_36C + b36_37C + b37_38C + 
            I(log(GDP/Population))+
            logGDPpc*rich
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)) ,])
        summary(model_np_bin)


        plot_coefs(list(model_area_bin,model_np_bin,model_holes_bin), ci_level = 0.90,
            omit.coefs = c("logGDPpc*rich","I(log(GDP/Population))","log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"),
            model.names=c("Area","Patches","Gaps"))


        model_area_bin_anom <- felm(log(mangrove_area)~
            anom_bn10_n9C  + anom_bn9_n8C + anom_bn8_n7C +
            anom_bn7_n6C + anom_bn6_n5C +
            anom_bn5_n4C + anom_bn4_n3C + anom_bn3_n2C + 
            anom_bn2_n1C+
            anom_bn1_0C+
            anom_b0_1C +
            anom_b1_2C + 
            anom_b2_3C+ anom_b3_4C + anom_b4_5C + 
            anom_b5_6C + anom_b6_7C + anom_b7_8C +
            logGDPpc*rich  
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])

        model_holes_bin_anom <- felm(log(holes)~
            anom_bn10_n9C  + anom_bn9_n8C + anom_bn8_n7C +
            anom_bn7_n6C + anom_bn6_n5C +
            anom_bn5_n4C + anom_bn4_n3C + anom_bn3_n2C + 
            anom_bn2_n1C+
            anom_bn1_0C+
            anom_b0_1C +
            anom_b1_2C + 
            anom_b2_3C+ anom_b3_4C + anom_b4_5C + 
            anom_b5_6C + anom_b6_7C + anom_b7_8C +
            logGDPpc*rich  
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),])


        model_np_bin_anom <- felm(log(np)~
            anom_bn10_n9C  + anom_bn9_n8C + anom_bn8_n7C +
            anom_bn7_n6C + anom_bn6_n5C +
            anom_bn5_n4C + anom_bn4_n3C + anom_bn3_n2C + 
            anom_bn2_n1C+
            anom_bn1_0C+
            anom_b0_1C +
            anom_b1_2C + 
            anom_b2_3C+ anom_b3_4C + anom_b4_5C + 
            anom_b5_6C + anom_b6_7C + anom_b7_8C +
            logGDPpc*rich  
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),])

        plot_coefs(model_area_bin_anom,model_np_bin_anom,model_holes_bin_anom, ci_level = 0.90,
            omit.coefs = c("logGDPpc*rich","I(log(GDP/Population))","log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"),
            model.names=c("Area","Patches","Gaps"))
    # Bins one-by-one (end)


        ggplot(mcn[which(mcn$mangrove_area>0),])+
        geom_point(aes(x=(pafrac),col=log(holes/mangrove_area),size=(mangrove_area),y=log(np/mangrove_area),group=gridcell_id),alpha=0.5)+
        scale_colour_scico(palette="romaO")+theme_bw()+
        xlab("Fractal Index")+ylab("Log Patches per Area")+
        labs(col="Log Gaps \nper Area",size="Area")
        #ggsave("Figures/Draft/Supp/Relation_Fragmentation.png",dpi=600)

        quantile(mcn$pafrac[which(mcn$mangrove_area>0)],na.rm=T,0.99)
        mcn2 <- mcn[which(mcn$pafrac<quantile(mcn$pafrac[which(mcn$mangrove_area>0)],na.rm=T,0.99)),]
        mcn2 <- mcn2[which(mcn2$pafrac>quantile(mcn$pafrac[which(mcn$mangrove_area>0)],na.rm=T,0.01)),]
        
        ggplot(mcn2[which(mcn2$mangrove_area>0),])+
        geom_point(aes(col=pafrac,x=log(holes/mangrove_area),size=(mangrove_area),y=log(np/mangrove_area),group=gridcell_id),alpha=0.5)+
        scale_colour_scico(palette="romaO")+theme_bw()+
        xlab("Fractal Index")+ylab("Log Patches per Area")+
        labs(col="Log Gaps \nper Area",size="Area")
        #ggsave("Figures/Draft/Supp/Relation_Fragmentation.png",dpi=600)

        ggplot(mcn2[which(mcn2$mangrove_area>0),])+
        geom_point(aes(x=(pafrac),col=log(holes/mangrove_area),size=(mangrove_area),y=log(np/mangrove_area),group=gridcell_id),alpha=0.5)+
        scale_colour_scico(palette="romaO")+theme_bw()+
        xlab("Fractal Index")+ylab("Log Patches per Area")+
        labs(col="Log Gaps \nper Area",size="Area")
    
    #Models SST Hottest (start)
        
        
        mcn$poor <- 1
        mcn$poor[which(mcn$income!="low")]<-0
        
        ggplot(mcn[which(mcn$mangrove_area>0),])+
        geom_point(aes(x=log(GDP/Population),y=log(GDP),col=factor(rich)))
        glimpse(mcn)
        quantile(mcn$logGDPpc_country[which(mcn$mangrove_area>0 & mcn$year==2018)],0.5,na.rm=T)
        mcn$rich <- 0
        mcn$rich[which(mcn$logGDPpc_country>quantile(mcn$logGDPpc_country[which(mcn$mangrove_area>0 & mcn$year==2018)],0.5,na.rm=T))] <- 1
        mcn$logPop <- log(mcn$Population_Count)
        
        model_area_ssthot <- felm(I(-log(mangrove_area))~
            sst_hottest + I(sst_hottest^2)+
            #I(Mean_Precipitation^2) + Mean_Precipitation +
            #log(Mean_Salinity)+
            #factor(rich)*log(Sum_GDP_50km/Population_Count_50km)+ #
            #I(log(GDP/Population))+ 
            #I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                #factor(rich)*log(Population_Count_50km)+ 
                logGDPpc+I(logGDPpc^2)+#I(logGDPpc^3)+
                logPop+I(logPop^2)+#I(logPop^3)
                #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                
                year:countrycode + 
                R5:year + 
                income:year
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc))]+1))
        summary(model_area_ssthot)

        # mcn <- mcn %>% 
        #     arrange(gridcell_id, year) %>%  # Ensure data is in correct order
        #     group_by(gridcell_id) %>%       # Define groups
        #     mutate(time_trend = row_number()) 
        # glimpse(mcn)
        
        ##Plots SST Hot
            sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area Loss")
            sq_estimate_preci_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"Mean_Precipitation","Area Loss")
            sq_estimate_pop_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"logPop","Area Loss")
            sq_estimate_gdp_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"logGDPpc","Area Loss")
            ggplot(sq_estimate_pop_area)+geom_point(aes(x=temp,y=gestimated))
            ggplot(sq_estimate_gdp_area)+geom_point(aes(x=temp,y=gestimated))

            sq_estimate_pop_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_pop_area$significant[which(sq_estimate_pop_area$p_value<0.05)] <- "Significant (p < 0.05)"
            sq_estimate_gdp_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_gdp_area$significant[which(sq_estimate_gdp_area$p_value<0.05)] <- "Significant (p < 0.05)"
            sq_estimate_sst_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_sst_area$significant[which(sq_estimate_sst_area$p_value<0.05)] <- "Significant (p < 0.05)"
            sq_estimate_preci_area$significant <- "Not significant (p > 0.05)"
            sq_estimate_preci_area$significant[which(sq_estimate_preci_area$p_value<0.05)] <- "Significant (p < 0.05)"

            Models_ssthot_plot_gdp <- ggplot(sq_estimate_gdp_area)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log GDP per capita***")
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
            Models_ssthot_plot_gdp

            histogram_plot_gdp <- ggplot(mcn_2020, aes(x = logGDPpc)) +
                    geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                    #geom_density(alpha = .2, fill = "#FF6666") +
                    theme_bw() +
                    xlab("Log Pop") +
                    ylab("Density") + theme_void()
            #library(gridExtra)
            Models_ssthot_plot_gdp <- ggarrange(Models_ssthot_plot_gdp, histogram_plot_gdp,
            legend="none", ncol = 1,heights=c(3,1),align="hv", hjust=0)
            Models_ssthot_plot_gdp

            Models_ssthot_plot_pop <- ggplot(sq_estimate_pop_area)+
                geom_line(aes(x=temp,y=gestimated),color="#25625f") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#25625f",alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=4)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1% Increase")+xlab("Log Population**")
                    #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                    #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                    #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                    #theme(strip.background =element_rect(fill=c(pal_roma[9])))
            Models_ssthot_plot_pop
            
            library(ggExtra)
            # Select the population data for 2020
            mcn_2020 <- mcn %>% filter(year == 2020)
            mcn_2020$logPop
            histogram_plot <- ggplot(mcn_2020, aes(x = logPop)) +
                    geom_histogram(aes(y = ..density..), colour = "black", fill = "#25625f") +
                    #geom_density(alpha = .2, fill = "#FF6666") +
                    theme_bw() +
                    xlab("Log Pop") +
                    ylab("Density") + theme_void()
            #library(gridExtra)
            Models_ssthot_plot_pop <- ggarrange(Models_ssthot_plot_pop, histogram_plot,legend="none",
             ncol = 1,heights=c(3,1),align="hv", hjust=0)


            Models_ssthot_plot_area <- ggplot(sq_estimate_sst_area)+
                geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1°C Increase")+xlab("Mean SST in the Hottest Month (°C)***")#+
                #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
            Models_ssthot_plot_area

            histogram_plot_ssthot <- ggplot(mcn_2020, aes(x = sst_hottest)) +
                    geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                    #geom_density(alpha = .2, fill = "#FF6666") +
                    theme_bw() +
                    xlab("Log Pop") +
                    ylab("Density") + theme_void()
            
            Models_ssthot_plot_area<-ggarrange(Models_ssthot_plot_area, histogram_plot_ssthot,legend="none", 
            ncol = 1,heights=c(3,1),align="hv", hjust=0)

            Models_ssthot_plot_area

            Models_preci_hot_plot_area <- ggplot(sq_estimate_preci_area)+
                geom_line(aes(x=temp,y=gestimated),color="#e9995c") +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),fill="#e9995c",alpha=0.2)+
                theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1 mm Rain Increase")+xlab("Monthly Mean Precipitation (mm)***")
                #guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                #scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                #scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_preci_hot_plot_area

            histogram_plot_preci <- ggplot(mcn_2020, aes(x = Mean_Precipitation)) +
                    geom_histogram(aes(y = ..density..), colour = "black", fill = "#e9995c") +
                    #geom_density(alpha = .2, fill = "#FF6666") +
                    theme_bw() +
                    xlab("Log Pop") +
                    ylab("Density") + theme_void()
            
            Models_preci_hot_plot_area<-ggarrange(Models_preci_hot_plot_area, histogram_plot_preci,legend="none", 
                        ncol = 1,heights=c(3,1),align="hv", hjust=0)

            Models_preci_hot_plot_area

            ggarrange(Models_ssthot_plot_area,Models_preci_hot_plot_area,Models_ssthot_plot_gdp,Models_ssthot_plot_pop,ncol=4)

            df_dummy <- data.frame(value = c(rnorm(100), rnorm(100)), Variable = rep(c("Climatic", "Socioeconomic"), each = 100))
            dummy_plot <- ggplot(df_dummy, aes(value, fill = Variable)) +
            geom_histogram(color = "black") +
            scale_fill_manual(values = c("Climatic" = "#e9995c", "Socioeconomic" = "#25625f")) +
            theme(legend.position="bottom")  # Remove all non-data ink

            # Extract the legend
            combined_legend <- get_legend(dummy_plot)

            arr_area_loss_plot <- ggarrange(ggarrange(Models_ssthot_plot_area,
                                Models_preci_hot_plot_area,
                                Models_ssthot_plot_gdp,
                                Models_ssthot_plot_pop,ncol=4),
                                combined_legend,ncol=1,heights=c(11,1))
            annotated_figure <- annotate_figure(arr_area_loss_plot, 
                                    top = text_grob("Area Loss Model", face = "bold", size = 14),
                                    bottom = text_grob("Sum of coefficients significane\n **: p<0.05; ***: p<0.01", face = "italic", size = 10),
                                    #left = text_grob("Left annotation", rot = 90, size = 10),
                                    #right = text_grob("Right annotation", rot = -90, size = 10)
                                    )

            # Printing the annotated figure
            print(annotated_figure)
    

            gdp_coefs_ssthot_area <- plot_coefs(model_area_ssthot, ci_level = 0.90,
                coefs = c("Log GDPpc"="log(Sum_GDP_50km/Population_Count_50km)",
                            "Log GDPpc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
                            "Log country GDPpc"="I(log(GDP/Population))",
                            "Log Pop" = "log(Population_Count_50km)",
                            "Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
                            "Log Salinity"="log(Mean_Salinity)"))
            gdp_coefs_ssthot_area                
        ##Plots SST Hot
        
        ## Model Gaps (start)
            model_holes_ssthot <- felm(log(holes)~
            #model_holes_ssthot <- felm(log(holes/mangrove_area)~
                log(mangrove_area)+
                #sst + I(sst^2)+
                sst_hottest + I(sst_hottest^2) +
                Mean_Precipitation+I(Mean_Precipitation^2) +
                log(Mean_Salinity)  + 
                log(Sum_GDP_50km/Population_Count_50km)+
                #I(log(Sum_GDP_50km)/log(GDP))+
                I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                log(Population_Count_50km)+ 
                #year:factor(gridcell_id) + I(year^2):factor(gridcell_id) + R5:year + 
                year:countrycode + R5:year + 
                income:year #+ 
                #log(GDP/Population)
                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
            data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],
                weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))])
            summary( model_holes_ssthot)

            
                    
            gdp_coefs_ssthot_gaps <- plot_coefs(model_holes_ssthot, ci_level = 0.90,
            coefs = c("Log GDP pc"="log(Sum_GDP_50km/Population_Count_50km)","Log GDP pc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
            "Log Pop" = "log(Population_Count_50km)","Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
            #"Ratio GDP local/country"="log(Sum_GDP_50km/Population_Count_50km)",
            #"Ratio GDPpc local/country"="I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))"
            "Log Salinity" = "log(Mean_Salinity)"))
            gdp_coefs_ssthot_gaps
            #ggsave("Figures/Draft/newmodel/linear_coef_ssthot_gaps.png",dpi=600)
            
            #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst","Gaps")
            sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")
            sq_estimate_preci_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Precipitation","Gaps")

            plot_gaps_ssthot <-    ggplot(sq_estimate_sst_holes)+
                    geom_line(aes(x=temp,y=gestimated)) +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1°C increase on \n Gaps (% Change)")+xlab("Monthly Average SST in the Hottest Month (C)")
            plot_gaps_ssthot_preci <-    ggplot(sq_estimate_preci_holes)+
                    geom_line(aes(x=temp,y=gestimated)) +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1mm rain increase on \n Gaps (% Change)")+xlab("Monthly Average Precipitation (mm)")
            
            ggarrange(plot_gaps_ssthot,plot_gaps_ssthot_preci,gdp_coefs_ssthot_gaps,ncol=3)
            ggplot(mcn[which(mcn$year>2006),],aes(x=log(Sum_GDP_50km)/log(GDP),y=log(mangrove_area),color=R5))+geom_point(alpha=0.2)+theme_bw()
            ggplot(mcn[which(mcn$year>2006),],aes(x=Mean_Salinity,y=sst_hot,color=R5))+geom_point(alpha=0.2)+theme_bw()
            
            install.packages("car")
            library(car)
            cor((mcn[, which(names(mcn) %in% c("Mean_Salinity", "sst_hottest", "Mean_Precipitation","sst","temp"))]),use="complete.obs")

            preci_ssthot <- felm(Mean_Salinity~
                sst_hottest + I(sst_hottest^2)
                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
            data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),])
            car::vif(preci_ssthot)
            summary(preci_ssthot)
        
        ## Model Gaps (end)

        ## Model Patches (start)
            glimpse(mcn)
            ggplot(mcn[which(mcn$annual_np_change!=0 & mcn$Population_Count_50km>0 &mcn$mangrove_area>0 & mcn$np>0& mcn$ntl>0 & is.finite(mcn$logGDPpc)),])+
            geom_point(aes(x=log(Population_Count),y=log(np)))
            #model_np_ssthot <- felm(log(np/mangrove_area)~
            model_np_ssthot <- felm(log(np)~    #sst + I(sst^2)+
                log(mangrove_area)+
                #sst+I(sst^2)+
                sst_hottest + I(sst_hottest^2) + 
                I(Mean_Precipitation^2)+#:log(Population_Count_50km) +
                Mean_Precipitation +
                log(Mean_Salinity)  + #I(Mean_Salinity^2)+
                log(Sum_GDP_50km/Population_Count_50km)+ 
                #I(log(GDP/Population))
                #factor(rich)*log(Sum_GDP_50km/Population_Count_50km) +# I(log(GDP/Population)) #+ 
                #log(Population_Count_50km)+ 
                #log(Population_Count_50km)+
                #year:countrycode
                I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                log(Population_Count_50km)+ 
                year:countrycode + R5:year + 
                income:year #+ 
                #log(GDP/Population)
                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
            data=mcn[which(mcn$annual_np_change!=0 & mcn$Population_Count_50km>0 &mcn$mangrove_area>0 & mcn$np>0& mcn$ntl>0 & is.finite(mcn$logGDPpc)),],
                weights=mcn$mangrove_area[which(mcn$annual_np_change!=0 & mcn$Population_Count_50km>0 & mcn$mangrove_area>0 & mcn$np>0& mcn$ntl>0 & is.finite(mcn$logGDPpc))])
            summary(model_np_ssthot)

            gdp_coefs_ssthot_np <- plot_coefs(model_np_ssthot, ci_level = 0.90,
            coefs = c("Log GDP pc"="log(Sum_GDP_50km/Population_Count_50km)","Log GDP pc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
            "Log Pop" = "log(Population_Count_50km)","Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
            #"Ratio GDP local/country"="log(Sum_GDP_50km/Population_Count_50km)",
            #"Ratio GDPpc local/country"="I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))"
            "Log Salinity" = "log(Mean_Salinity)"))
            gdp_coefs_ssthot_np
            
            sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
            sq_estimate_preci_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"Mean_Precipitation","Patches")

            plot_np_ssthot <-    ggplot(sq_estimate_sst_np)+
                    geom_line(aes(x=temp,y=gestimated)) +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1°C increase on \n Patches (% Change)")+xlab("Monthly Average SST in the Hottest Month (C)")
            plot_np_ssthot_preci <-    ggplot(sq_estimate_preci_np)+
                    geom_line(aes(x=temp,y=gestimated)) +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1mm rain increase on \n Patches (% Change)")+xlab("Monthly Average Precipitation (mm)")

            ggarrange(plot_np_ssthot,plot_np_ssthot_preci,gdp_coefs_ssthot_np,ncol=3)
        ## Model Patches (end)

        
        ## Model Fractal (start)
            glimpse(mcn)
            model_pafrac_ssthot <- felm(log(pafrac)~
            #model_np_ssthot <- felm(log(np)~    #sst + I(sst^2)+
                #log(mangrove_area)+
                sst_hottest + I(sst_hottest^2) +#:log(Population_Count_50km) +
                #Mean_Precipitation+ 
                #I(Mean_Precipitation^2)+
                log(Mean_Salinity)  + log(Sum_GDP_50km/Population_Count_50km)+ 
                #I(log(GDP/Population))
                #factor(rich)*log(Sum_GDP_50km/Population_Count_50km) +# I(log(GDP/Population)) #+ 
                #log(Population_Count_50km)+ 
                #log(Population_Count_50km)+
                #year:countrycode
                I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
                log(Population_Count_50km)+ 
                year:countrycode + R5:year + 
                income:year #+ 
                |gridcell_id + year+ countrycode+R5|0|gridcell_id,
            data=mcn[which(mcn$mangrove_area>0 & mcn$pafrac>0 & is.finite(mcn$logGDPpc)),],
                weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$pafrac>0 & is.finite(mcn$logGDPpc))])
            summary(model_pafrac_ssthot)

            gdp_coefs_ssthot_pafrac <- plot_coefs(model_pafrac_ssthot, ci_level = 0.90,
            coefs = c("Log GDP pc"="log(Sum_GDP_50km/Population_Count_50km)","Log GDP pc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
            "Log Pop" = "log(Population_Count_50km)","Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
            #"Ratio GDP local/country"="log(Sum_GDP_50km/Population_Count_50km)",
            #"Ratio GDPpc local/country"="I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))"
            "Log Salinity" = "log(Mean_Salinity)"))
            gdp_coefs_ssthot_pafrac
            
            sq_estimate_sst_pafrac <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$pafrac>0 & is.finite(mcn$logGDPpc)),],model_pafrac_ssthot,"sst_hottest","Fractal Index")
            sq_estimate_preci_pafrac <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$pafrac>0 & is.finite(mcn$logGDPpc)),],model_pafrac_ssthot,"Mean_Precipitation","Fractal Index")

            plot_pafrac_ssthot <-    ggplot(sq_estimate_sst_pafrac)+
                    geom_line(aes(x=temp,y=gestimated)) +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1°C increase on \n Fractal Index (% Change)")+xlab("Monthly Average SST in the Hottest Month (C)")
            plot_pafrac_ssthot_preci <-    ggplot(sq_estimate_preci_pafrac)+
                    geom_line(aes(x=temp,y=gestimated)) +
                    geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
                    theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                    ylab("Effect of 1mm rain increase on \n Fractal Index (% Change)")+xlab("Monthly Average Precipitation (mm)")

            ggarrange(plot_pafrac_ssthot,plot_pafrac_ssthot_preci,gdp_coefs_ssthot_pafrac,ncol=3)
        ## Model Patches (start)

        
        ## Plots SST Hot (start)
            
            ## Prepare Data (start)
            
                Models_sst_hot <- rbind(sq_estimate_sst_area,sq_estimate_sst_holes,sq_estimate_sst_np,sq_estimate_sst_pafrac)
                Models_preci_hot <- rbind(sq_estimate_preci_area,sq_estimate_preci_holes,sq_estimate_preci_np,sq_estimate_preci_pafrac)
                
                pal_lapaz <- scico(15, palette = 'batlow')
                pal_lapaz <- pal_lapaz[c(2,7,12)]
                Models_sst_hot$significant <- "Not significant (p > 0.05)"
                Models_sst_hot$significant[which(Models_sst_hot$p_value<0.05)] <- "Significant (p < 0.05)"

                Models_preci_hot$significant <- "Not significant (p > 0.05)"
                Models_preci_hot$significant[which(Models_preci_hot$p_value<0.05)] <- "Significant (p < 0.05)"

            ## Prepare Data

            ## Plot Maringal Effects (start)
                Models_ssthot_plot <- ggplot(Models_sst_hot)+
                geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
                theme_bw()  + facet_wrap(~exp,ncol=4)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1C increase")+xlab("Monthly Average SST in the Hottest Month (C)")+
                guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot

                Models_ssthot_plot_area <- ggplot(Models_sst_hot[which(Models_sst_hot$exp=="Area"),])+
                geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
                theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1°C increase on \n Area Loss (% Change)")+xlab("Monthly Average SST in the Hottest Month (C)")+
                guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_area


                Models_ssthot_plot_gaps <- ggplot(Models_sst_hot[which(Models_sst_hot$exp=="Gaps"),])+
                geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
                theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1°C increase on \n Fragmentation (% Change)")+xlab("Monthly Average SST in the Hottest Month (C)")+
                guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_ssthot_plot_gaps


            

                Models_preci_hot_plot <- ggplot(Models_preci_hot)+
                geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
                theme_bw()  + facet_wrap(~exp,ncol=4)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1mm increase")+xlab("Mean Monthly Precipitation (mm)")+
                guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_preci_hot_plot

                Models_preci_hot_plot_area <- ggplot(Models_preci_hot[which(Models_preci_hot$exp=="Area"),])+
                geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
                theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1mm rain increase on \n Mangrove Area (% Change)")+xlab("Mean Monthly Precipitation (mm)")+
                guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_preci_hot_plot_area

                Models_preci_hot_plot_gaps <- ggplot(Models_preci_hot[which(Models_preci_hot$exp=="Gaps"),])+
                geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
                geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
                theme_bw()  + #facet_wrap(~exp,ncol=3)+ 
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Effect of 1mm rain increase on \n Fragmentation (% Change)")+xlab("Mean Monthly Precipitation (mm)")+
                guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
                scale_color_manual(values =c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
                scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred"))#+
                #theme(strip.background =element_rect(fill=c(pal_roma[9])))
                Models_preci_hot_plot_gaps
            ## Plot Maringal Effects (end)


            ## Plot Linear Effects (start)


                plot_coefs(model_area_ssthot,model_np_ssthot,model_holes_ssthot,model_pafrac_ssthot)
                
                summary(model_area_ssthot)
                gdp_coefs_ssthot <- plot_coefs(model_area_ssthot,model_np_ssthot,model_holes_ssthot,model_pafrac_ssthot, ci_level = 0.90,
                coefs = c("Log lGDPpc"="log(Sum_GDP_50km/Population_Count_50km)",
                            "Log GDPpc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
                            "Log country GDPpc"="I(log(GDP/Population))",
                            "Log Pop" = "log(Population_Count_50km)",
                            "Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
                            "Log Salinity"="log(Mean_Salinity)"),
                    model.names=c("Area Loss","Patches","Gaps","Fractal Index"))
                gdp_coefs_ssthot

                ssthot_all_models <- ggarrange(ggarrange(Models_ssthot_plot,
                    Models_preci_hot_plot,common.legend=TRUE,legend="bottom",ncol=1,nrow=2),
                    gdp_coefs_ssthot,legend="bottom",ncol=2,widths=c(3,2))
                ssthot_all_models
                #ggsave("Figures/Draft/newmodel/linear_coef_ssthot_area.png",dpi=600)
        ## Plots SST Hot (start)
    
    #Models SST Hottest (end)

    #Models SSTs (start)
        model_area_sst <- felm(I(-log(mangrove_area))~
            sst + I(sst^2) + 
            #rich*sst + rich*I(sst^2) + 
            #rich:sst_hottest + rich:I(sst_hottest^2) + 
            #sst:sst_hottest + sst:I(sst_hottest^2) + 
            #sst_hottest + I(sst_hottest^2) + 
            #sst:hot_location_alldata + I(sst^2):hot_location_alldata + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            #factor(rich)*logGDPpc+
            #I(log(GDP/Population))+ factor(rich)*log(Population_Count_50km)
            #year:countrycode
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Population_Count_50km)+ 
            year:countrycode + R5:year + 
            income:year #+ 
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_sst)


        sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_sst,"sst","Area Loss")
        sq_estimate_preci_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_sst,"Mean_Precipitation","Area Loss")
        #sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area")


        ggplot(mcn,aes(x=log(holes/mangrove_area),y=sst))+geom_point()
        ggplot(mcn,aes(x=log(holes/mangrove_area),y=log(GDP/Population)))+geom_point()
        glimpse(mcn)
        
        
        #model_holes_ssthot <- felm(log(holes)~
        model_holes_sst <- felm(log(holes/mangrove_area)~
            sst + I(sst^2)+
            #log(mangrove_area)+ 
            #sst_hottest + I(sst_hottest^2) + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            #temp + I(temp^2)+
            log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Sum_GDP_50km/Population_Count_50km)+
            log(Population_Count_50km)+
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc) ),],
            weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))])
        summary( model_holes_sst)
        
        sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_sst,"sst","Gaps")
        sq_estimate_preci_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_sst,"Mean_Precipitation","Gaps")
        #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")

        
        ggplot(mcn)+geom_point(aes(x=log(np/mangrove_area),y=(Mean_Salinity)))
        
        model_np_sst <- felm(log(np/mangrove_area)~
        #model_np_ssthot <- felm(log(np)~
            #log(mangrove_area)+
            sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Population_Count_50km)+
            log(Sum_GDP_50km/Population_Count_50km)+
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode|0|gridcell_id,
            data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),],
            weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))])
        
        summary(model_np_sst)
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_sst,"sst","Patches")
        sq_estimate_preci_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_sst,"Mean_Precipitation","Patches")


        model_pafrac_sst <- felm(log(pafrac)~
        #model_np_ssthot <- felm(log(np)~
            log(mangrove_area)+
            sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Sum_GDP_50km/Population_Count_50km)+
            log(Population_Count_50km)+
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode|0|gridcell_id,
            data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),],
            weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))])
        
        summary(model_pafrac_sst)
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_sst_pafrac <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_pafrac_sst,"sst","Fractal")
        sq_estimate_preci_pafrac <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_pafrac_sst,"Mean_Precipitation","Fractal")
        

        Models_sst <- rbind(sq_estimate_sst_area,sq_estimate_sst_holes,sq_estimate_sst_np,sq_estimate_sst_pafrac)
        Models_preci_sst <- rbind(sq_estimate_preci_area,sq_estimate_preci_holes,sq_estimate_preci_np,sq_estimate_preci_pafrac)
        glimpse(Models_sst)

        Models_sst_plot <- ggplot(Models_sst)+
        geom_line(aes(x=temp,y=gestimated,color=exp)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=exp),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase")+xlab("Annual Mean SST (C)")+
        guides(fill = guide_legend(title = "Dimension"),color = guide_legend(title = "Dimension"))
        Models_sst_plot
        
        Models_preci_sst_plot <- ggplot(Models_preci_sst)+
        geom_line(aes(x=temp,y=gestimated,color=exp)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=exp),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1mm increase")+xlab("Mean Monthly Precipitation (mm)")+
        guides(fill = guide_legend(title = "Dimension"),color = guide_legend(title = "Dimension"))
        Models_preci_sst_plot

        

        gdp_coefs_sst <- plot_coefs(model_area_sst,model_np_sst,model_holes_sst,model_pafrac_sst, ci_level = 0.90,
                coefs = c("Log GDPpc"="log(Sum_GDP_50km/Population_Count_50km)",
                            "Log GDPpc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
                            "Log country GDPpc"="I(log(GDP/Population))",
                            "Log Pop" = "log(Population_Count_50km)",
                            "Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
                            "Log Salinity"="log(Mean_Salinity)"),
                    model.names=c("Area Loss","Patches","Gaps","Fractal Index"))
                gdp_coefs_sst
        
    #Models SSTs (start)    

    #Plot Dimensions (SST and SST Hottest)
        
    
        Models_preci_sst$significant <- "Not significant (p > 0.05)"
        Models_preci_sst$significant[which(Models_preci_sst$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_preci_sst_plot <- ggplot(Models_preci_sst)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1mm increase")+xlab("Mean Monthly Precipitation (mm)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) 
        #theme(strip.background =element_rect(fill=c(pal_roma[8])))
        #theme(strip.text = element_text(colour = 'white'))
        Models_preci_sst_plot
        
        Models_sst$significant <- "Not significant (p > 0.05)"
        Models_sst$significant[which(Models_sst$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_sst_plot <- ggplot(Models_sst)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase")+xlab("Mean Annual SST (C)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) 
        #theme(strip.background =element_rect(fill=c(pal_roma[8])))
        #theme(strip.text = element_text(colour = 'white'))
        Models_sst_plot

        library("ggpubr")
        ggarrange(Models_sst_plot,Models_ssthot_plot,common.legend=T,nrow=2,legend="bottom")
        #ggsave("Figures/Draft/sst_and_sstHOT.png",dpi=600)

        ggarrange(Models_preci_sst_plot,Models_preci_hot_plot,common.legend=T,nrow=2,legend="bottom")

        sst_all_models <- ggarrange(ggarrange(Models_sst_plot,Models_preci_sst_plot,common.legend=T,nrow=2,legend="none",ncol=1),
        gdp_coefs_sst,legend="none",widths=c(3,2))
        sst_all_models
    #Plot Dimensions (SST and SST Hottest)

    #Models Temp (start)
        model_area_t <- felm(I(-log(mangrove_area))~
            #sst + I(sst^2) + 
            temp + I(temp^2)+
            #rich*sst + rich*I(sst^2) + 
            #rich:sst_hottest + rich:I(sst_hottest^2) + 
            #sst:sst_hottest + sst:I(sst_hottest^2) + 
            #sst_hottest + I(sst_hottest^2) + 
            #sst:hot_location_alldata + I(sst^2):hot_location_alldata + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            log(Mean_Salinity)+
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Sum_GDP_50km/Population_Count_50km)+
            log(Population_Count_50km)+
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_t)


        sq_estimate_t_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_t,"temp","Area Loss")
        sq_estimate_preci_area_t <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_t,"Mean_Precipitation","Area Loss")
        #sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area")


        
        
        #model_holes_t <- felm(log(holes)~
        model_holes_t <- felm(log(holes/mangrove_area)~
            #sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            temp + I(temp^2)+
            I(Mean_Precipitation^2) +
            #log(mangrove_area)+ 
            Mean_Precipitation +
            log(Mean_Salinity)+
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            rich*log(Sum_GDP_50km/Population_Count_50km)+
            log(Population_Count_50km)+
            year:countrycode
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc) ),],
        weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc))])
        summary( model_holes_t)
        
        sq_estimate_t_holes <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_t,"temp","Gaps")
        sq_estimate_preci_holes_t <- sqest(mcn[which(mcn$mangrove_area>0 &mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_t,"Mean_Precipitation","Gaps")
        #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")

        ggplot(mcn,aes(x=year,y=(np/mangrove_area)))+geom_point()+geom_smooth()+ylim(c(0,10000))

            #model_np_t <- felm(log(np)~
        mcn$hot_bin <- mcn$b30_31C+mcn$b31_32C+mcn$b32_33C+mcn$b33_34C
        model_np_t <- felm(log(np/mangrove_area)~
            #model_np_t <- felm(log(np)~
            temp+I(temp^2)+
            #hot_bin+
            #log(mangrove_area)+ 
            #mhw_int:mhw_freq+
            #sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            Mean_Salinity+
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            rich*log(Sum_GDP_50km/Population_Count_50km)+
            rich*log(Population_Count_50km)+
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),],
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))]+1))
        summary(model_np_t)
        
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_t_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_t,"temp","Patches")
        sq_estimate_preci_np_t <- sqest(mcn[which(mcn$mangrove_area>0 &mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_np_t,"Mean_Precipitation","Patches")


        model_pafrac_t <- felm(log(pafrac)~
            #model_np_t <- felm(log(np)~
            temp+I(temp^2)+
            #hot_bin+
            #log(mangrove_area)+ 
            #mhw_int:mhw_freq+
            #sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            Mean_Salinity+
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            rich*log(Sum_GDP_50km/Population_Count_50km)+
            log(Population_Count_50km)+
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),],
        weights=log(mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))]+1))
        summary(model_pafrac_t)
        
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_t_pafrac <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_pafrac_t,"temp","Fractal Index")
        sq_estimate_preci_pafrac_t <- sqest(mcn[which(mcn$mangrove_area>0 &mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_pafrac_t,"Mean_Precipitation","Fractal Index")

        gdp_coefs_t <- plot_coefs(model_area_t,model_np_t,model_holes_t,model_pafrac_t, ci_level = 0.90,
                coefs = c("Log GDPpc"="log(Sum_GDP_50km/Population_Count_50km)",
                            "Log GDPpc (rich)"="factor(rich)1:log(Sum_GDP_50km/Population_Count_50km)",
                            "Log country GDPpc"="I(log(GDP/Population))",
                            "Log Pop" = "log(Population_Count_50km)",
                            "Log Pop (rich)" = "factor(rich)1:log(Population_Count_50km)",
                            "Log Pop (rich)"="rich:log(Sum_GDP_50km/Population_Count_50km)",
                            "Log Salinity"="log(Mean_Salinity)"),
                    model.names=c("Area","Patches","Gaps","Fractal Index"))
                gdp_coefs_t
        

       
        Models_t <- rbind(sq_estimate_t_area,sq_estimate_t_holes,sq_estimate_t_np,sq_estimate_t_pafrac)
        Models_preci_t <- rbind(sq_estimate_preci_area_t,sq_estimate_preci_holes_t,sq_estimate_preci_np_t,sq_estimate_preci_pafrac_t)
        #glimpse(Models_sst)

        Models_t_plot <- ggplot(Models_t)+
        geom_line(aes(x=temp,y=gestimated,color=exp)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=exp),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1mm increase")+xlab("Mean Monthly Precipitation (mm)")+
        guides(fill = guide_legend(title = "Dimension"),color = guide_legend(title = "Dimension"))
        Models_t_plot
        
        Models_preci_t$significant <- "Not significant (p > 0.05)"
        Models_preci_t$significant[which(Models_preci_t$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_preci_t_plot <- ggplot(Models_preci_t)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1mm increase")+xlab("Monthly Mean Precipitation (mm)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) 


        Models_preci_t_plot_gaps <- ggplot(Models_preci_t[which(Models_preci_t$exp=="Gaps"),])+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + #facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1mm rain increase on \n Fragmentation (% Change)")+xlab("Monthly Mean Precipitation (mm)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) 
        
        Models_t$significant <- "Not significant (p > 0.05)"
        Models_t$significant[which(Models_t$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_t_plot <- ggplot(Models_t)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=4)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase")+xlab("Mean Annual Air Temperature (C)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        theme(legend.position="bottom")
        #theme(strip.background =element_rect(fill=c(pal_roma[8])))
        #theme(strip.text = element_text(colour = 'white'))
        Models_t_plot

        leg <- get_legend(Models_t_plot)
        leg_plot <- as_ggplot(leg)

        Models_t_plot_gaps <- ggplot(Models_t[which(Models_t$exp=="Gaps"),])+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + #facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1°C increase on \n Fragmentation (% Change)")+xlab("Mean Annual Air Temperature (C)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) +
        scale_fill_manual(values = c("Not significant (p > 0.05)"=pal_lapaz[1],"Significant (p < 0.05)"="indianred")) 
        Models_t_plot_gaps

        t_all_models <- ggarrange(ggarrange(Models_t_plot,Models_preci_t_plot,common.legend=T,nrow=2,legend="none",ncol=1),
        gdp_coefs_t,legend="none",widths=c(3,2))
        t_all_models

        all_mods <- ggarrange(t_all_models,sst_all_models,ssthot_all_models,legend="bottom",ncol=1)
        all_mods
    #Models Temp (start)


    #Models Combined (start)
        model_area_sst <- felm(log(mangrove_area)~
            #sst + I(sst^2) + 
            #rich*sst + rich*I(sst^2) + 
            #rich:sst_hottest + rich:I(sst_hottest^2) + 
            #sst:sst_hottest + sst:I(sst_hottest^2) + 
            sst_hottest + I(sst_hottest^2) + 
            #sst:hot_location_alldata + I(sst^2):hot_location_alldata + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            #log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            #factor(rich)*logGDPpc+
            I(log(GDP/Population))+ #factor(rich)*log(Population_Count_50km)
            #year:countrycode
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Population_Count_50km)+ 
            year:countrycode + R5:year + 
            income:year #+ 
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_sst)


        sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_sst,"sst","Area")
        sq_estimate_preci_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_sst,"Mean_Precipitation","Area")
        #sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area")


        ggplot(mcn,aes(x=log(holes/mangrove_area),y=sst))+geom_point()
        ggplot(mcn,aes(x=log(holes/mangrove_area),y=log(GDP/Population)))+geom_point()
        glimpse(mcn)
        
        
        #model_holes_ssthot <- felm(log(holes)~
        model_holes_sst <- felm(log(holes/mangrove_area)~
            sst + I(sst^2)+
            #log(mangrove_area)+ 
            #sst_hottest + I(sst_hottest^2) + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            #temp + I(temp^2)+
            log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            I(log(Sum_GDP_50km/Population_Count_50km)/log(GDP/Population))+
            log(Population_Count_50km)+ 
            year:countrycode + R5:year + 
            income:year #+
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc) ),],
            weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))])
        summary( model_holes_sst)
        
        sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_sst,"sst","Gaps")
        sq_estimate_preci_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_sst,"Mean_Precipitation","Gaps")
        #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")

        
        ggplot(mcn)+geom_point(aes(x=log(np/mangrove_area),y=(Mean_Salinity)))
        
        model_np_sst <- felm(log(np/mangrove_area)~
        #model_np_ssthot <- felm(log(np)~
            #log(mangrove_area)+
            sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            log(Mean_Salinity)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+ 
            factor(rich)*log(Population_Count_50km)+
            year:countrycode
            |gridcell_id + year+ countrycode|0|gridcell_id,
            data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),],
            weights=mcn$mangrove_area[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc))])
        
        summary(model_np_sst)
        
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_sst,"sst","Patches")
        sq_estimate_preci_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_sst,"Mean_Precipitation","Patches")

        Models_sst <- rbind(sq_estimate_sst_area,sq_estimate_sst_holes,sq_estimate_sst_np)
        Models_preci_sst <- rbind(sq_estimate_preci_area,sq_estimate_preci_holes,sq_estimate_preci_np)
        glimpse(Models_sst)

        Models_preci_sst_plot <- ggplot(Models_preci_sst)+
        geom_line(aes(x=temp,y=gestimated,color=exp)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=exp),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase \non mangrove characteristics")+xlab("Mean Monthly Precipitation (mm)")+
        guides(fill = guide_legend(title = "Dimension"),color = guide_legend(title = "Dimension"))
        Models_preci_sst_plot

        gdp_coefs_sst <- plot_coefs(model_area_ssthot,model_np_ssthot,model_holes_ssthot, ci_level = 0.90,
        coefs = c("Log local GDPpc"="logGDPpc","Log local GDPpc (rich)"="factor(rich)1:logGDPpc","Log country GDPpc"="I(log(GDP/Population))"),
            model.names=c("Area","Patches","Gaps"))

    #Models SSTs (start)     

        
        #ggsave("Figures/Draft/sst_and_sstHOT.png",dpi=600)

        
        all_preci_plots <- ggarrange(Models_preci_sst_plot,Models_preci_hot_plot,Models_preci_t_plot,common.legend=T,nrow=3,legend="bottom")
        all_temp_plots <- ggarrange(Models_sst_plot,Models_ssthot_plot,Models_t_plot,common.legend=T,nrow=3,legend="bottom")
        all_all_plots <- ggarrange(all_temp_plots,all_preci_plots,common.legend=T,ncol=2)
        all_all_plots
        ggsave("Figures/Draft/Supp/All_models_divided_by_mangrove_area.png")

        ggarrange(gdp_coefs_sst+
            ggtitle("Annual SST"),
        gdp_coefs_ssthot+
            theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),plot.title = element_text(hjust = 0.5))+
            ggtitle("SST Hottest Month"),
        gdp_coefs_t+
            theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),plot.title = element_text(hjust = 0.5))+
            ggtitle("Air Temperature"),
        nrow=1,ncol=3,common.legend=T,legend="bottom")
        ggsave("Figures/Draft/Supp/All_models_GDP_coefs.png")

        
        area_pref <- ggarrange(ggarrange(Models_ssthot_plot_area,Models_preci_hot_plot_area,common.legend=T,legend="none"),
                gdp_coefs_ssthot_area+xlab("Estimated Effect on Area \n(% Change)"),ncol=2,widths=c(4,2))
        #ggsave("Figures/Draft/Model_pref_area.png")

        gaps_pref <- ggarrange(ggarrange(Models_ssthot_plot_gaps,Models_preci_hot_plot_gaps,legend="none"),
                gdp_coefs_ssthot_gaps+xlab("Estimated Effect on Fragmentation \n(% Change)"),ncol=2,widths=c(4,2))

        blank_plot <- ggplot() + theme_void()

        ggarrange(annotate_figure(area_pref, top = text_grob("Area Model", 
                color = "black", face = "bold", size = 14),fig.lab.pos ="top.left"),
                annotate_figure(gaps_pref, top = text_grob("Fragmentation Model", 
                color = "black", face = "bold", size = 14)),
                    ggarrange(leg_plot,blank_plot,widths=c(4,2),ncol=2),
                    ncol=1,nrow=3,heights=c(10,10,2),common.legend=T,legend="bottom")
        ggsave("Figures/Draft/Model_pref_area_frag.png")

        annotate_figure(plot_holes, top = text_grob("", 
                color = "black", face = "bold", size = 14))

        ggarrange(ggarrange(Models_ssthot_plot_area,Models_preci_hot_plot_area,common.legend=T,legend="none"),
                gdp_coefs_ssthot_area+xlab("Estimated effect on mangrove area \n (percent change)"),ncol=2,widths=c(4,2),
                ggarrange(ggarrange(Models_t_plot_gaps,Models_preci_t_plot_gaps,common.legend=T,legend="none"),
                gdp_coefs_t_gaps+xlab("Estimated effect on mangrove area \n (percent change)"),ncol=2,widths=c(4,2)),ncol=1,nrow=2)

    ## Save coefficients
        model <- model_area_ssthot
        
        for (i in 1:dim(summary(model)$coef)){
            var <- rownames(summary(model)$coef)[i]
            coef <- summary(model)$coef[i,1]
            se <- summary(model)$coef[i,2]
            if(i==1){
                all_
            }
        }
        model_holes_t

## Models Salt
    #model_holes_ssthot <- felm(log(holes)~
        variables_of_interest <- c("Mean_Salinity","sst","sst_hottest","temp","logGDPpc","rich","year","gridcell_id","Mean_Precipitation","Population_Count_50km") 
        
        sub_mcn <- mcn[,which(names(mcn) %in% variables_of_interest)]
        sub_mcn <- sub_mcn[complete.cases(sub_mcn), ] 
        glimpse(sub_mcn)
        sub_mcn$p2 <- sub_mcn$Mean_Precipitation^2
        sub_mcn$temp2 <- sub_mcn$temp^2
        
        grid_means <- aggregate(. ~ gridcell_id, data=sub_mcn, FUN=mean)

        # subtract grid cell-level means from each observation
        for (var in names(sub_mcn)[-which(names(sub_mcn)=="gridcell_id" | names(sub_mcn)=="year")]) {
        sub_mcn[[paste0(var, "_demean")]] <- sub_mcn[[var]] - grid_means[[var]][match(sub_mcn$gridcell_id, grid_means$gridcell_id)]
        }

        glimpse(sub_mcn)

        cor(sub_mcn)

        
        ggplot(sub_mcn[which(abs(sub_mcn$Mean_Salinity_demean)<0.5),])+
        geom_point(aes(y=temp_demean,col=Mean_Salinity_demean,x=Mean_Precipitation_demean),alpha=0.2)+
        scale_colour_scico(palette="oleron")+theme_bw()

        ggplot(sub_mcn[which(abs(sub_mcn$Mean_Salinity_demean)<0.5),])+
        geom_point(aes(y=temp2_demean,col=Mean_Salinity_demean,x=p2_demean),alpha=0.2)+
        scale_colour_scico(palette="oleron")+theme_bw()

        year_means <- aggregate(. ~ year, data=sub_mcn, FUN=mean,na.rm=TRUE)
        for (var in names(sub_mcn)[-which( names(sub_mcn)=="year")]) {
        sub_mcn[[paste0(var, "_demean")]] <- sub_mcn[[var]] - year_means[[var]][match(sub_mcn$year, year_means$year)]
        }

        glimpse(sub_mcn)
        
        

        ggplot(sub_mcn)+geom_point(aes(y=temp_demean_demean,col=Mean_Salinity_demean_demean,x=Mean_Precipitation_demean_demean),alpha=0.5)
        
        ggplot(mcn)+geom_point(aes(y=Mean_Salinity,x=temp))
        ggplot(mcn)+geom_point(aes(y=log(Sum_GDP_50km/Population_Count_50km),x=logGDPpc))
        ggplot(mcn)+geom_point(aes(y=Mean_Salinity,x=Mean_Precipitation))
        
        ggplot(mcn[which(mcn$Mean_Salinity>0 ),])+
            geom_point(aes(col=Mean_Precipitation*temp^2,x=Mean_Precipitation,y=Mean_Salinity))+
            scale_colour_scico()+theme_bw()+geom_smooth(aes(x=Mean_Precipitation,y=Mean_Salinity))
        
        ggplot(mcn)+geom_point(aes(y=Mean_Salinity,x=Mean_Precipitation*temp,col=spei),alpha=0.3)
        
        ggplot(mcn[which(mcn$Mean_Salinity>0 & mcn$year==2020 & !is.na(mcn$rich)),])+geom_point(aes(y=Mean_Salinity,x=logGDPpc))+
        geom_smooth(aes(y=Mean_Salinity,x=logGDPpc))+
        theme_bw()

        sal_mod <- felm(Mean_Salinity~
            #sst + I(sst^2) + 
            year:factor(countrycode)+I(Mean_Precipitation^2)+I(temp)+Mean_Precipitation*I(temp^2)+#year + I(year^2)+
            temp+ log(logGDPpc) + I(logGDPpc^2)+Latitude+I(Latitude^2)|gridcell_id|0|0,
        data=mcn[which(is.finite(mcn$logGDPpc)),])
        summary(sal_mod)

        sal_mod <- felm(log(Mean_Salinity)~
            #sst + I(sst^2) + 
            #year:factor(countrycode)+
            temp*Mean_Precipitation+#I(temp^2)+#year + I(year^2)+
            +I(temp^2)+#I(Mean_Precipitation^2)+#temp+ 
            log(logGDPpc)*factor(rich)|gridcell_id|0|0,
        data=mcn[which(is.finite(mcn$logGDPpc)),])
        summary(sal_mod)

        sal_mod <- felm(log(Mean_Salinity)~
            #sst + I(sst^2) + 
            year:factor(countrycode)+I(temp^2)+Mean_Precipitation+#I(temp^2)+#year + I(year^2)+
            #temp+ 
            log(logGDPpc) + I(logGDPpc^2)+Latitude+I(Latitude^2)|gridcell_id|0|0,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),])
        summary(sal_mod)

        
        sal_mod <- felm(Mean_Salinity~
            sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) +
            #log(mangrove_area)+ 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            I(temp^2)*Mean_Precipitation+I(Mean_Precipitation^2) +
             
            #log(Mean_Salinity)  + #I(Mean_Salinity^2)+
            #factor(rich)*logGDPpc+ I(log(GDP/Population))
            factor(rich)*log(Sum_GDP_50km/Population_Count_50km) +# I(log(GDP/Population)) #+ 
            #factor(rich)*log(Population_Count_50km)+ 
            year:countrycode
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),])

        library(modelsummary)

##Models Salt
# Dynamic Model Bins (start)
        
                
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        
        # helper function to create hot and cold groups
        create_groups <- function(mcn, hot_bins, cold_bins) {
        mcn$hot <- rowSums(mcn[, which(names(mcn) %in% hot_bins)],na.rm=T)
        mcn$cold <- rowSums(mcn[, which(names(mcn) %in% cold_bins)],na.rm=T)
        return(mcn)
        }

        # helper function to create hot and cold groups
        create_groups_h <- function(mcn, hot_bins) {
        mcn$hot <- rowSums(mcn[, which(names(mcn) %in% hot_bins)],na.rm=T)
        #mcn$cold <- rowSums(mcn[, which(names(mcn) %in% cold_bins)],na.rm=T)
        return(mcn)
        }
        # helper function to create hot and cold groups
        create_groups_c <- function(mcn, hot_bins) {
        #mcn$hot <- rowSums(mcn[, which(names(mcn) %in% hot_bins)],na.rm=T)
        mcn$cold <- rowSums(mcn[, which(names(mcn) %in% cold_bins)],na.rm=T)
        return(mcn)
        }

        # helper function to run regression
        run_regression <- function(mcn, ind_vars) {
        formula <- as.formula(paste0("log(mangrove_area) ~ cold + ", paste(ind_vars, collapse = " + "), " + hot + sst_hottest_mean0020 | gridcell_id + year + countrycode + R5| 0 | gridcell_id"))
        model <- felm(formula, data = mcn[which(mcn$mangrove_area > 0), ])
        return(model)
        }

        run_regression_h <- function(mcn, ind_vars) {
        formula <- as.formula(paste0("log(mangrove_area) ~  hot + Mean_Precipitation + I(Mean_Precipitation^2) + I(log(GDP/Population))+
            logGDPpc*rich| gridcell_id + year + countrycode| 0 | gridcell_id"))
        model <- felm(formula, data = mcn[which(mcn$mangrove_area > 0 & is.finite(mcn$logGDPpc)), ])
        return(model)
        }

        run_regression_c <- function(mcn, ind_vars) {
        formula <- as.formula(paste0("log(mangrove_area) ~  cold + Mean_Precipitation + I(Mean_Precipitation^2) + I(log(GDP/Population))+
            logGDPpc*rich| gridcell_id + year + countrycode| 0 | gridcell_id"))
        model <- felm(formula, data = mcn[which(mcn$mangrove_area > 0 & is.finite(mcn$logGDPpc)), ])
        return(model)
        }

        # helper function to extract coefficients and p-values
        extract_results <- function(model, hot_bins, cold_bins) {
        summary_model <- summary(model)
        results <- data.frame(
            Variable = rownames(summary_model$coefficients),
            Coefficient = summary_model$coef[, 1],
            P_value = summary_model$coef[, 4],
            SE = summary_model$coef[, 2],
            Hot_bins_n = length(hot_bins),
            Cold_bins_n = length(cold_bins),
            Hot_bins = paste(hot_bins, collapse = ", "),
            Cold_bins = paste(cold_bins, collapse = ", ")
        )
        return(results)
        }

        extract_results_h <- function(model, hot_bins) {
        summary_model <- summary(model)
        results <- data.frame(
            Variable = rownames(summary_model$coefficients),
            Coefficient = summary_model$coef[, 1],
            P_value = summary_model$coef[, 4],
            SE = summary_model$coef[, 2],
            Hot_bins_n = length(hot_bins),
            Hot_bins = paste(hot_bins, collapse = ", "),
            Threshold = as.double(substr(hot_bins[1],start=2,stop=3))
        )
        return(results)
        }

        extract_results_c <- function(model, hot_bins) {
        summary_model <- summary(model)
        results <- data.frame(
            Variable = rownames(summary_model$coefficients),
            Coefficient = summary_model$coef[, 1],
            P_value = summary_model$coef[, 4],
            SE = summary_model$coef[, 2],
            Hot_bins_n = length(cold_bins),
            Hot_bins = paste(cold_bins, collapse = ", "),
            Threshold = as.double(substr(cold_bins[length(cold_bins)],start=5,stop=6))
        )
        return(results)
        }

        # list of all possible bins
        all_bins <- paste("b", seq(8, 38, by = 1), "_", seq(9, 39, by = 1), "C", sep = "")

        # data frame to store results
        results <- data.frame()

        # loop over different groupings
        n <- 1
        for (j in 10:29) {
            # create hot and cold groups
            hot_bins <- all_bins[j:31]
            mcn2 <- create_groups_h(mcn, hot_bins)
            
            # run regression
            model <- run_regression_h(mcn2)
            
            # extract and store results
            result <- extract_results_h(model, hot_bins)
            results <- rbind(results, result)
            print(paste0("i-",i,n))
            n <- n+1
        }


        library(scico)
        
        
        pal_lapaz <- scico(15, palette = 'vikO')

        results$significant <- 0
        results$significant[which(results$P_value<0.01)] <- 1
        results$stars <- ""
        results$stars[which(results$P_value<0.1)]<-"*"
        results$stars[which(results$P_value<0.05)]<-"**"
        results$stars[which(results$P_value<0.01)]<-"***"
        results$co <- paste0(substr(as.character(results$Coefficient),start=1,stop=7),results$stars)
        
        threshold_hot_plot <- ggplot(results[which(results$Variable=="hot"),],aes(x=Threshold,y=Coefficient),col=pal_lapaz[13]) + 
        geom_point() + theme_bw()+
        geom_text(data=results[which(results$Variable=="hot" & results$P_value<0.1),],
            aes(x=Threshold-0.5,y=(Coefficient-SE*1.64)-0.0035,label=co),angle=90)+
        geom_text(data=results[which(results$Variable=="hot" & results$P_value<0.1 & results$Threshold==34),],
            aes(x=Threshold-0.5,y=(Coefficient),label=co),angle=90)+
        geom_errorbar(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[13])+
        #geom_ribbon(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64),alpha=0.2))+
        #xlim(c(17,35)) + ylim(c(-0.015,0.001)) +
        #coord_cartesian(xlim = c(17,35),ylim = c(-0.015,0.001))
        coord_cartesian(xlim = c(17,35),ylim = c(-0.015,0.001))+
        geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
            xlab("SST-bin threshold")+
            ylab("Effect of one additional day \nabove the threshold")

            results_c <- data.frame()

            n <- 1
        for (i in 3:15) {
            # create hot and cold groups
            cold_bins <- all_bins[1:i]
            mcn2 <- create_groups_c(mcn, cold_bins)
            
            # run regression
            model <- run_regression_c(mcn2)
            
            # extract and store results
            result <- extract_results_c(model, cold_bins)
            results_c <- rbind(results_c, result)
            print(paste0("i-",i,n))
            n <- n+1
        }


        results_c$significant <- 0
        results_c$significant[which(results_c$P_value<0.01)] <- 1
        results_c$stars <- ""
        results_c$stars[which(results_c$P_value<0.1)]<-"*"
        results_c$stars[which(results_c$P_value<0.05)]<-"**"
        results_c$stars[which(results_c$P_value<0.01)]<-"***"
        results_c$co <- paste0(substr(as.character(results_c$Coefficient),start=1,stop=7),results_c$stars)
        
        threshold_cold_plot <- ggplot(results_c[which(results_c$Variable=="cold"),],aes(x=Threshold,y=Coefficient),col=pal_lapaz[13]) + 
        geom_point() + theme_bw()+
        geom_text(data=results_c[which(results_c$Variable=="cold" & results_c$P_value<0.1),],
            aes(x=Threshold-0.3,y=(Coefficient-SE*1.64)-0.003,label=co),angle=90)+
        geom_text(data=results[which(results_c$Variable=="cold" & results_c$P_value<0.1 & results_c$Threshold==34),],
            aes(x=Threshold-0.3,y=(Coefficient),label=co),angle=90)+
        geom_errorbar(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[4])+
        #geom_ribbon(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64),alpha=0.2))+
        #xlim(c(17,35)) + ylim(c(-0.015,0.001)) +
        #coord_cartesian(xlim = c(17,35),ylim = c(-0.015,0.001))
        #coord_cartesian(xlim = c(17,35),ylim = c(-0.015,0.001))+
        geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
            xlab("SST-bin threshold")+
            ylab("Effect of one additional day \nbelow the threshold")


        ggarrange(threshold_hot_plot,threshold_cold_plot,ncol=1)
        ggsave("Figures/Draft/Thresholds.png",dpi=600)
        getwd()
        
        ggplot(results_c[which(results_c$Variable=="cold"),],aes(x=Threshold,y=Coefficient),col=pal_lapaz[4]) + 
        geom_point() + theme_bw()+
        geom_errorbar(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[4])
        #coord_cartesian(
        #    xlim = c(17,35),
        #    ylim = c(-0.015,0.001))
        

        
        library(ggbreak) 
        
        ggplot() + 
        geom_point(data=results_c[which(results_c$Variable=="cold"),],aes(x=Threshold+0.2,y=Coefficient),col=pal_lapaz[4]) + theme_bw()+
        geom_errorbar(data=results_c[which(results_c$Variable=="cold"),],aes(x=Threshold+0.2,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[4])+
        geom_point(data=results[which(results$Variable=="hot"),],aes(x=Threshold,y=Coefficient),col=pal_lapaz[13]) + 
        theme_bw()+
        geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
        geom_errorbar(data=results[which(results$Variable=="hot"),],aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[13])+
            xlab("SST-bin threshold")+
            ylab("Effect of one additional day \nabove or below the threshold")+
            scale_y_break(c(-0.0025, -0.28))+
        #coord_cartesian(xlim = c(13,35),
            #ylim = c(-0.015,0.0025))
            #ylim = c(-0.0025,0.0025))+
            #ylim = c(-0.3,0.0025))
        ylim(c(-0.281,0.01))
        #ylim(c(-0.281,0.0025))
            
            



        for (i in 2:10) {
        for (j in 17:29) {
            # create hot and cold groups
            hot_bins <- all_bins[j:31]
            cold_bins <- all_bins[1:i]
            mcn2 <- create_groups(mcn, hot_bins, cold_bins)
            
            # list of independent bins not included in hot or cold
            ind_bins <- all_bins[!(all_bins %in% c(hot_bins, cold_bins))]
            
            # run regression
            model <- run_regression(mcn2, ind_bins)
            
            # extract and store results
            result <- extract_results(model, hot_bins, cold_bins)
            results <- rbind(results, result)
            print(paste0("i-",i,n))
            n <- n+1
        }
        }

        # plot results
        glimpse(results)

        levels(factor(results$Variable))

        
        # reorder factor levels so "cold" is first
        results$Variable <- relevel(factor(results$Variable), ref = "cold")

        name_map <- c(
        "b10_11C" = "Bin: 10 to 11C",
        "b11_12C" = "Bin: 11 to 12C",
        "b12_13C"= "Bin: 12 to 13C",
         "b13_14C"= "Bin: 13 to 14C",
         "b14_15C"= "Bin: 14 to 15C",
         "b15_16C"= "Bin: 15 to 16C",
         "b16_17C"= "Bin: 16 to 17C",
         "b17_18C" = "Bin: 17 to 18C",
        "b18_19C"= "Bin: 18 to 19C",
         "b19_20C"= "Bin: 19 to 20C",
         "b20_21C" = "Bin: 20 to 21C",
        "b21_22C"= "Bin: 21 to 22C",
         "b22_23C"= "Bin: 22 to 23C",
          "b23_24C"= "Bin: 23 to 24C",
         "b24_25C" = "Bin: 24 to 25C",
        "b25_26C"= "Bin: 25 to 26C",
         "b26_27C" = "Bin: 26 to 27C",
        "b27_28C"= "Bin: 27 to 28C",
         "b28_29C" = "Bin: 28 to 29C",
        "b29_30C"= "Bin: 29 to 30C",
        "b30_31C"= "Bin: 30 to 31C",
         "b31_32C"= "Bin: 31 to 32C",
         "b32_33C"= "Bin: 32 to 33C",
         "b33_34C" = "Bin: 33 to 34C",
        "b34_35C"= "Bin: 34 to 35C",
         "b35_36C"= "Bin: 35 to 36C",
        "cold" = "Cold bin",
        "hot" = "Hot bin"
        )
        # assign new names to factor levels
        levels(results$Variable) <- name_map[levels(results$Variable)]
        glimpse(results)
        results$R5 <- ir
        if(ir=="ASIA"){
            results_all <- results
        }else{
            results_all <- rbind(results_all,results)
        }
        
        }
        glimpse(results_all)
        boxplot_binsr <- ggplot(results_all, aes(y = Variable, x = Coefficient,color=R5)) +
        geom_boxplot()+
        geom_vline(aes(xintercept=0),linetype="dashed")+
        xlab("Coefficient") +
        ylab("") +
        ggtitle("Effect of different hot and cold bin groupings\n on regression coefficients")+
        theme_bw()+xlim(c(-0.1,0.05))
        boxplot_binsr   
        
        results <-  results[-which(results$Variable %in% c("Bin: 35 to 36C","Bin: 34 to 35C","Bin: 33 to 34C","Bin: 10 to 11C","Bin: 11 to 12C")),]
        
        boxplot_bins <- ggplot(results, aes(y = Variable, x = Coefficient)) +
        geom_boxplot()+
        geom_vline(aes(xintercept=0),linetype="dashed")+
        xlab("Coefficient") +
        ylab("") +
        ggtitle("Effect of different hot and cold bin groupings\n on regression coefficients")+
        theme_bw()
        boxplot_bins        
        
        ggplot(data = results[which(results$P_value<0.01),], aes(x = Variable, y = Coefficient, color = Hot_bins_n)) +
        geom_point(size = 4, position=position_jitter(width=0.1))
    
        Significant_binsplot <- ggplot(results[which(results$P_value<0.05),], aes(x = Variable, y = Coefficient, color = factor(Hot_bins_n))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Only coefficients with \nP-value<0.01")+
        theme_bw() + coord_flip() + guides(color="none")

        library("ggpubr")
        ggarrange(boxplot_bins,Significant_binsplot)

        

        plots_hot <- ggplot(results[which(results$P_value<0.05 & results$Variable=="Hot bin"),], aes(x = Hot_bins_n, y = Coefficient, color = factor(Cold_bins_n))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
        xlab("Hot_bins_n") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Hot Bin")+
        theme_bw() + coord_flip()

        plots_hot
        
        results$unique <- paste0(results$Hot_bins,results$Cold_bins)
        significanthot <- results[which((results$unique) %in% (results$unique[which(results$P_value<0.3 
           # & results$Variable=="Hot bin")])),]
           )])),]
        
        plots_significant_all <- ggplot(significanthot, aes(x = Variable, y = Coefficient, color = factor(Hot_bins))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Only regressions which at least one \ncoefficient has P-value<0.01")+
        theme_bw() + coord_flip()  + guides(color="none")

        plots_significant_all


        ggplot(significanthot, aes(x = Variable, y = Coefficient, color = factor(Hot_bins))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Only P-values<0.01")+
        theme_bw() + coord_flip()
        
        
        ggarrange(boxplot_bins,Significant_binsplot,plots_hot,ncol=3)
        ggarrange(boxplot_bins,Significant_binsplot,plots_significant_all,ncol=3,widths=c(3,2,2))
        ggsave("Figures/bins/Effect_of_Bins.png",dpi=600)

        results[which(results$P_value<0.01 & results$Variable=="Hot bin"),]
        
        mcn$hot <- mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C+ mcn$b32_33C+ mcn$b31_32C +mcn$b30_31C +mcn$b29_30C + mcn$b28_29C #+  mcn$b27_28C + mcn$b26_27C
        mcn$cold <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C 
        model_area_bin <- felm(log(mangrove_area)~
            cold +
            b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C  +b27_28C + b26_27C+
            hot
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0) ,])
        summary(model_area_bin)
        plot_coefs(model_area_bin)
        model_area_bin_prefered <- model_area_bin
        modelsummary(model_area_bin,stars=TRUE,output="Tables/bins/reg_table_bin.docx")

        model_area_bin <- felm(log(mangrove_area)~
            cold +
            b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C  +b27_28C + b26_27C+
            hot
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0) ,])
        summary(model_area_bin)
        plot_coefs(model_area_bin,ci_level = 0.95)
        ggsave("Figures/bins/model_bin_area.png",dpi=300)
        

        
        #library("jtools") 
        plot_coefs(model_area_bin,ci_level = 0.95,omit.coefs = c("log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"))
        
        plot_coefs(model_area_bin,ci_level = 0.95,
            coefs = c(
                "Colder than 15"="cold",
                "Bin: 15 to 16C"="b15_16C" ,
                "Bin: 16 to 17C"= "b16_17C",
                "Bin: 17 to 18C" = "b17_18C",
                "Bin: 18 to 19C"= "b18_19C",
                "Bin: 19 to 20C"= "b19_20C" ,
                "Bin: 20 to 21C"= "b20_21C"  ,
                "Bin: 21 to 22C" = "b21_22C",
                "Bin: 22 to 23C"=  "b22_23C",
                "Bin: 23 to 24C" =  "b23_24C",
                "Bin: 24 to 25C"  ="b24_25C" ,
                "Bin: 25 to 26C"="b25_26C" ,
                "Bin: 26 to 27C" =  "b26_27C",
                "Hotter than 27"="hot"))

        ggsave("Figures/bins/model_bin_area.png",dpi=300)

                     
        #geom_boxplot()+
        geom_point(position=position_jitter(height=0.2))+
        geom_vline(aes(xintercept=0),linetype="dashed")+
        geom_errorbar(aes(xmin = Coefficient - 1.64 * SE, xmax = Coefficient + 1.64 * SE),
        position=position_jitter(height=0.2)) + 
        #facet_grid(Hot_bins ~ Cold_bins) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Coefficient") +
        ylab("Variable") +
        ggtitle("Effect of different hot and cold bin groupings on regression coefficients")+
        theme_bw()
        
        ggplot(results[which(results$P_value<0.01),], aes(y = Variable, x = Coefficient, color=factor(Hot_bins_n))) +
        geom_point() +
        geom_vline(aes(xintercept=0),linetype="dashed")+
        geom_errorbar(aes(xmin = Coefficient - 1.64 * SE, xmax = Coefficient + 1.64 * SE)) + geom_jitter()+
        #facet_grid(Hot_bins ~ Cold_bins) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Coefficient") +
        ylab("Variable") +
        ggtitle("Effect of different hot and cold bin groupings on regression coefficients")+
        theme_bw()
    

    # Dymanic Model Bins (end)
