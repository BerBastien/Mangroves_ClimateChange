
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
glimpse(mcn)
setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")

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

## Function
        sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(1:end1),c(1:end1)]
            beta.hat <- coef(model)[c(1:end1)]
            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(1, 2*x)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x))
        }
## Function

### Sensor VIIRS (post 2012)
    #Filter (start)
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn$temp <- mcn$mean_temp + mcn$temp_anom
        mcn$log_ntl <- log(mcn$ntl)
        mcn2013 <- mcn[which(mcn$year>2012),]
        hist(mcn2013$annual_area_change)
        mcn2013 <- mcn2013 %>%
        group_by(gridcell_id) %>%
        filter(sum(which(abs(annual_area_change)>1))<1)

    #Filter (end)

    # Plot
        ggplot(mcn) +
        geom_point(aes(y = annual_area_change, x = year), color = "cyan4", alpha = 0.1) +
        #geom_point(aes(y = mcw_dur, x = year, color = "Coldwave"), alpha = 0.1) +
        #geom_smooth(aes(y = mcw_dur, x = year), method="lm", color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year), method="lm",color = "brown2",se=TRUE,level=0.99) +
        geom_smooth(aes(y = annual_area_change, x = year), method="lm",color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year),color = "brown2",se=TRUE,level=0.99) +
        #scale_color_manual(values = c("Heatwave" = "brown2", "Coldwave" = "cyan4"))+
        scale_y_continuous(trans = "log") +
        labs(y = "Duration (days)", x = "Year", title = "Marine hot and cold waves duration by year", color = "Type") +
        theme_bw()

        ggplot(mcn) +
        geom_point(aes(y = mangrove_area, x = year), color = "cyan4", alpha = 0.1) +
        #geom_point(aes(y = mcw_dur, x = year, color = "Coldwave"), alpha = 0.1) +
        #geom_smooth(aes(y = mcw_dur, x = year), method="lm", color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year), method="lm",color = "brown2",se=TRUE,level=0.99) +
        geom_smooth(aes(y = mangrove_area, x = year), method="lm",
            #k=3,
            color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year),color = "brown2",se=TRUE,level=0.99) +
        #scale_color_manual(values = c("Heatwave" = "brown2", "Coldwave" = "cyan4"))+
        scale_y_continuous(trans = "log") +
        labs(y = "Duration (days)", x = "Year", title = "Marine hot and cold waves duration by year", color = "Type") +
        theme_bw()
        
        summary(felm(mangrove_area~year|gridcell_id|0|0,data=mcn2013))
        summary(felm(mangrove_area~year|gridcell_id|0|0,data=mcn))
        summary(felm(np~year|gridcell_id|0|0,data=mcn2013))
        summary(felm(np~year|gridcell_id|0|0,data=mcn))
        summary(felm(log(mangrove_area)~year|gridcell_id|0|0,data=mcn2013[which(mcn2013$mangrove_area>0),]))
        summary(felm(log(mangrove_area)~year|gridcell_id|0|0,data=mcn[which(mcn$mangrove_area>0),]))

        ggplot(mcn) +
        geom_point(aes(y = np, x = year), color = "cyan4", alpha = 0.1) +
        #geom_point(aes(y = mcw_dur, x = year, color = "Coldwave"), alpha = 0.1) +
        #geom_smooth(aes(y = mcw_dur, x = year), method="lm", color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year), method="lm",color = "brown2",se=TRUE,level=0.99) +
        geom_smooth(aes(y = np, x = year), method="lm",
              #formula = y ~ splines::bs(x,  knots = 4),
            #k=3,
            color = "cyan4",se=TRUE,level=0.99) +
        #geom_smooth(aes(y = mhw_dur, x = year),color = "brown2",se=TRUE,level=0.99) +
        #scale_color_manual(values = c("Heatwave" = "brown2", "Coldwave" = "cyan4"))+
        scale_y_continuous(trans = "log") +
        labs(y = "Duration (days)", x = "Year", title = "Marine hot and cold waves duration by year", color = "Type") +
        theme_bw()
    # Plot

    # Random Forest
        library("randomForest")
        install.packages("rpart.plot")
        install.packages("plotmo")
        library("rpart.plot")
        library("plotmo")
        #randomForest(x=mcn$annual_area_change,y=mcn[,which(names(mcn)!="annual_area_change")],ntree=300,mtry=4)
        randomForest(annual_area_change ~ sst + ntl, mcn2013, ntree=300,mtry=4)

        variables_of_interest <- c("annual_area_change","ntl","sst","Mean_Precipitation","Mean_Salinity","b8_9C","b9_10C","b10_11C","b11_12C","b12_13C","b13_14C","b14_15C","b15_16C",
        "b16_17C","b17_18C","b18_19C","b19_20C","b20_21C","b21_22C","b22_23C","b23_24C","b24_25C","b25_26C","b26_27C","b27_28C",
        "b28_29C","b29_30C","b30_31C","b31_32C","b32_33C","b33_34C","b34_35C","b35_36C","b36_37C","b37_38C","gridcell_id")
        sub_mcn2013 <- mcn2013[,which(names(mcn2013) %in% variables_of_interest)]
        sub_mcn2013 <- sub_mcn2013[complete.cases(sub_mcn2013), ] 
        glimpse(sub_mcn2013)
        
        area.rf <- randomForest(annual_area_change ~ ., data=sub_mcn2013, importance=TRUE,
                        proximity=TRUE)
        varImpPlot(area.rf)
        plotmo(area.rf, persp.ticktype="detailed")

    # Random Forest
    
    # Model bins
        mcn2013$year<-as.double(mcn2013$year)
        #model_area_bin <- felm(annual_area_change~
        table(factor(mcn$sensor[which(mcn$mangrove_area>0 & (mcn$ntl>0))]))
        model_area_bin <- felm(log(mangrove_area)~
            b8_9C+b9_10C  + b10_11C   + b11_12C + b12_13C + b13_14C + b14_15C + b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C  +b26_27C   +b27_28C + I(b28_29C +b29_30C  +b30_31C +b31_32C+ b32_33C+
            b33_34C + b34_35C+b35_36C + b36_37C + b37_38C) + #Latitude + I(Latitude^2)+
            log(mangrove_area) + 
            log(ntl) +I(log(ntl)^2) + # sst + I(sst^2) #+# I(salinity_change^2)  + salinity_change + 
            #Mean_Precipitation #+ I(Mean_Precipitation^2) + 
            Mean_Salinity + I(Mean_Salinity^2) #+ year + I(year^2)

        |gridcell_id+year|0|countrycode,
        #data=mcn2013[which(mcn2013$mangrove_area>0 & (mcn2013$ntl>0)),])
        data=mcn[which(mcn$mangrove_area>0 & (mcn$ntl>0)),])
        summary(model_area_bin)
        
        #library("jtools")
        plot_coefs(model_area_bin)
        plot_coefs(model_area_bin,omit.coefs = c("log(mangrove_area)","year","I(year^2)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))
    # Model bins (end)

    # Model bins anom
        model_area_bin_anom <- felm(log(mangrove_area)~
        #model_area_bin_anom <- felm(annual_area_change~
            I(anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + 
                anom_bn7_n6C +anom_bn6_n5C +
                anom_bn5_n4C +anom_bn4_n3C)+ 
            I(anom_bn3_n2C) +(anom_bn2_n1C)+
            I(anom_bn1_0C)+(anom_b0_1C) +
            I(anom_b1_2C) + I(anom_b2_3C +
            anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C) +
            #log(mangrove_area) + 
            log(ntl) + I(log(ntl)^2) + 
            #Sum_GDP+ 
            Mean_Salinity + I(Mean_Salinity^2)#+
            #sst + I(sst^2) + 
            #I(Mean_Precipitation^2) + Mean_Precipitation
        |gridcell_id+year|0|countrycode,
        #data=mcn2013[which(mcn2013$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        #(mcn2013$ntl>0)),])
        data=mcn[which(mcn$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        (mcn$ntl>0)),])
        summary(model_area_bin_anom)
        
        #library("jtools")
        plot_coefs(model_area_bin_anom,omit.coefs =  c("log(mangrove_area)","year","I(year^2)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))
    # Model bins anom (end)

    # Model mhw 
        mcn2013$year<-as.double(mcn2013$year)
        #model_area_mw <- felm(annual_area_change~
        model_area_mw <- felm(log(mangrove_area)~
            mhw_int + I(mhw_int^2) +
            mcw_int + I(mcw_int^2) +
            #log(ntl) + I(log(ntl)^2) +
            sensor:log(ntl) + sensor:I(log(ntl)^2) +
            mhw_freq + mcw_freq+
            #sst + I(sst^2) + 
            I(Mean_Precipitation^2) + Mean_Precipitation
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        (mcn$ntl>0)),])
        #data=mcn2013[which(mcn2013$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        #(mcn2013$ntl>0)),])
        #data=mcn[which(mcn$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        #(mcn$ntl>0)),])
        summary(model_area_mw)
        
        #library("jtools")
        plot_coefs(model_area_mw,omit.coefs = c("log(mangrove_area)","sst","I(sst^2)"))
    # Model mhw (end)

    # Model ntl 
        glimpse(mcn)
        mcn$logSumGDP <- log(mcn$Sum_GDP_50km)
        mcn$logPop <- log(mcn$Population_Count_50km)
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        ggplot(mcn,aes(x=log(mangrove_area),y=logGDPpc))+geom_point()
        ggplot(mcn,aes(x=log(mangrove_area),y=log(GDP)))+geom_point()
        model_area_human <- felm(log(mangrove_area)~
            #log(ntl):anom_b2_3C + I(log(ntl)^2)*anom_b2_3C #+
            #sensor:log(ntl)*rich+log(GDP)*rich #sensor:I(log(ntl)^2)*rich +
            #logGDPpc + I(logGDPpc^2) + 
            #log(GDP)
            logGDPpc*rich+
            #logSumGDP*rich #+
            #logPop#+#*hot_location_alldata + I(logPop^2)*hot_location_alldata#*rich #+#sensor:log(ntl) + sensor:I(log(ntl)^2) +
            #I(logPop^2)#sst  + 
            sst #+ I(sst^2)+
            #Mean_Precipitation#I(Mean_Precipitation^2) + 
            #I(anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + 
             #   anom_bn7_n6C +anom_bn6_n5C +
              #  anom_bn5_n4C +anom_bn4_n3C)+ 
            #I(anom_bn3_n2C) +(anom_bn2_n1C)+
            #I(anom_bn1_0C)+(anom_b0_1C) +
            #I(anom_b1_2C) #+ 
            #temp+I(temp^2)
            #I(anom_b2_3C +anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C)
        |gridcell_id+year|0|countrycode,
        #data=mcn2013[which(mcn2013$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        #(mcn2013$ntl>0)),])
        data=mcn[which(mcn$mangrove_area>0 & 
        #mcn2013$mhw==1 & 
        #(mcn$ntl>0)),])
        is.finite(mcn$logGDPpc)),])
        #(mcn$Sum_GDP_50km>0) &
        #(mcn$Sum_GDP_50km>0)),])
        #(mcn$Population_Count_50km>0)),])
        summary(model_area_human)

        summary(felm(logSumGDP~log(ntl):sensor|gridcell_id+year|0|countrycode,data=mcn[which(mcn$mangrove_area>0 & 
        (mcn$Sum_GDP_50km>0) &
        (mcn$ntl>0)),]))

        summary(felm(temp~sst|gridcell_id+year|0|countrycode,data=mcn))
        
        summary(felm(logGDPpc~sst|gridcell_id+year|0|countrycode,data=mcn[which(
        is.finite(mcn$logGDPpc)),]))
        
        #library("jtools")
        plot_coefs(model_area_human,omit.coefs = c("log(mangrove_area)","sst","I(sst^2)"))
    # Model ntl (end)

### Sensor VIIRS (post 2012)

### All data DMSP + VIIRS (start)

    # Random Forest
        
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn$temp <- mcn$mean_temp + mcn$temp_anom
        mcn$log_area <- log(mcn$mangrove_area)
        variables_of_interest <- c("log_area","b8_9C","b9_10C","b10_11C","b11_12C","b12_13C","b13_14C","b14_15C","b15_16C",
        "b16_17C","b17_18C","b18_19C","b19_20C","b20_21C","b21_22C","b22_23C","b23_24C","b24_25C","b25_26C","b26_27C","b27_28C",
        "b28_29C","b29_30C","b30_31C","b31_32C","b32_33C","b33_34C","b34_35C","b35_36C","b36_37C","b37_38C","gridcell_id",
        "anom_bn10_n9C","anom_bn9_n8C","anom_bn8_n7C","anom_bn7_n6C","anom_bn6_n5C","anom_bn5_n4C","anom_bn4_n3C",
        "anom_bn3_n2C","anom_bn2_n1C","anom_bn1_0C","anom_b0_1C","anom_b1_2C","anom_b2_3C","anom_b3_4C",
        "anom_b4_5C","anom_b5_6C","anom_b6_7C","anom_b7_8C","year") 
        
        sub_mcn <- mcn[,which(names(mcn) %in% variables_of_interest)]
        #sub_mcn <- sub_mcn[complete.cases(sub_mcn), ] 
        glimpse(sub_mcn)

        # calculate grid cell-level means
        grid_means <- aggregate(. ~ gridcell_id, data=sub_mcn, FUN=mean)

        # subtract grid cell-level means from each observation
        for (var in names(sub_mcn)[-which(names(sub_mcn)=="gridcell_id" | names(sub_mcn)=="year")]) {
        sub_mcn[[paste0(var, "_demean")]] <- sub_mcn[[var]] - grid_means[[var]][match(sub_mcn$gridcell_id, grid_means$gridcell_id)]
        }
        glimpse(sub_mcn)    

        sub_mcn <- sub_mcn[is.finite(sub_mcn$log_area_demean),]
        # calculate grid cell-level means
        year_means <- aggregate(. ~ year, data=sub_mcn, FUN=mean,na.rm=TRUE)
        for (var in names(sub_mcn)[-which(names(sub_mcn)=="gridcell_id" | names(sub_mcn)=="year")]) {
        sub_mcn[[paste0(var, "_demean")]] <- sub_mcn[[var]] - year_means[[var]][match(sub_mcn$year, year_means$year)]
        }

        
        glimpse(sub_mcn)   
        names(sub_mcn) 

        sub_mcn <- sub_mcn[,c(101:149)]    
        glimpse(sub_mcn)
        
        names(sub_mcn) <-  sub("_demean_demean*", "", names(sub_mcn))


        #sub_mcn <- sub_mcn[is.finite(sub_mcn$log_area),]
        trf <- tuneRF(sub_mcn[,1:48], sub_mcn[,49])
        mt <- trf[which.min(trf[,2]), 1]
        
        area.rf <- randomForest(log_area ~ ., data=sub_mcn[is.finite(sub_mcn$log_area),], importance=TRUE,
                        #proximity=TRUE,
                        mtry =mt)
        varImpPlot(area.rf)
        #ggsave("Figures/RandomForest/Importance_area_bins.png",dpi=600)
        
        library(randomForestExplainer)
        importance_frame <- measure_importance(area.rf)
        
        vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees"))
        interactions_frame <- min_depth_interactions(area.rf, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
        #interactions_frame <- interactions_frame[-which(interactions_frame$interaction %in% c("damage:damage")),]
        plot_min_depth_interactions(interactions_frame)        
        library(ggplot2)
        #ggsave("Figures/RandomForest/Interactions_area_bins.png",dpi=600)

        
        plotmo(area.rf, persp.ticktype="detailed")
        #ggsave("Figures/RandomForest/Perspective_area_bins.png",dpi=600)

        
        library(randomForestExplainer)
        min_depth_frame <- min_depth_distribution(area.rf)
        md1 <- plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)
        plot(md1)    
        #ggsave("Figures/RandomForest/MinDepth_area_bins.png",dpi=600)
    
    # Random Forest

    ## Lasso Bins
        library(glmnet)
        #library(dplyr)

        # Filter data
        mcn$log_area <- log(mcn$mangrove_area)
        data_filtered <- mcn[which(mcn$mangrove_area > 0 ), ]
        glimpse(mcn)

        # Calculate means for fixed effects and subtract from the variables
        # Here, I am first demeaning by gridcell_id and then by year
        data_filtered <- data_filtered %>%
        group_by(gridcell_id) %>%
        mutate_at(vars(b8_9C,b9_10C,b10_11C,b11_12C,b12_13C,b13_14C,b14_15C,b15_16C,
        b16_17C,b17_18C,b18_19C,b19_20C,b20_21C,b21_22C,b22_23C,b23_24C,b24_25C,b25_26C,b26_27C,b27_28C,
        b28_29C,b29_30C,b30_31C,b31_32C,b32_33C,b33_34C,b34_35C,b35_36C,b36_37C,b37_38C,gridcell_id,
        anom_bn10_n9C,anom_bn9_n8C,anom_bn8_n7C,anom_bn7_n6C,anom_bn6_n5C,anom_bn5_n4C,anom_bn4_n3C,
        anom_bn3_n2C,anom_bn2_n1C,anom_bn1_0C,anom_b0_1C,anom_b1_2C,anom_b2_3C,anom_b3_4C,
        anom_b4_5C,anom_b5_6C,anom_b6_7C,anom_b7_8C, sst,log_area), funs(demean = . - mean(.,na.rm=TRUE))) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate_at(vars(b8_9C,b9_10C,b10_11C,b11_12C,b12_13C,b13_14C,b14_15C,b15_16C,
        b16_17C,b17_18C,b18_19C,b19_20C,b20_21C,b21_22C,b22_23C,b23_24C,b24_25C,b25_26C,b26_27C,b27_28C,
        b28_29C,b29_30C,b30_31C,b31_32C,b32_33C,b33_34C,b34_35C,b35_36C,b36_37C,b37_38C,gridcell_id,
        anom_bn10_n9C,anom_bn9_n8C,anom_bn8_n7C,anom_bn7_n6C,anom_bn6_n5C,anom_bn5_n4C,anom_bn4_n3C,
        anom_bn3_n2C,anom_bn2_n1C,anom_bn1_0C,anom_b0_1C,anom_b1_2C,anom_b2_3C,anom_b3_4C,
        anom_b4_5C,anom_b5_6C,anom_b6_7C,anom_b7_8C, sst,log_area), funs(demean = . - mean(.,na.rm=TRUE))) %>%
        ungroup()

        # Prepare matrix of predictors and response variable
        glimpse(data_filtered)
        
        X <- model.matrix(log_area_demean~ b8_9C_demean+b9_10C_demean+b10_11C_demean+b11_12C_demean+b12_13C_demean+b13_14C_demean+b14_15C_demean+b15_16C_demean+
        b16_17C_demean+b17_18C_demean+b18_19C_demean+b19_20C_demean+b20_21C_demean+b21_22C_demean+b22_23C_demean+b23_24C_demean+b24_25C_demean+b25_26C_demean+b26_27C_demean+b27_28C_demean+
        b28_29C_demean+b29_30C_demean+b30_31C_demean+b31_32C_demean+b32_33C_demean+b33_34C_demean+b34_35C_demean+b35_36C_demean+b36_37C_demean+b37_38C_demean+gridcell_id+
        anom_bn10_n9C_demean +anom_bn9_n8C_demean+anom_bn8_n7C_demean+anom_bn7_n6C_demean+anom_bn6_n5C_demean+anom_bn5_n4C_demean+anom_bn4_n3C_demean+
        anom_bn3_n2C_demean+anom_bn2_n1C_demean+anom_bn1_0C_demean+anom_b0_1C_demean+anom_b1_2C_demean+anom_b2_3C_demean+anom_b3_4C_demean+
        anom_b4_5C_demean+anom_b5_6C_demean+anom_b6_7C_demean+anom_b7_8C_demean+sst, data = data_filtered)

        y <- data_filtered$log_area_demean[which(!is.na(data_filtered$b22_23C))]

        # Apply lasso regression
        lasso_model <- glmnet(X, y, alpha = 1)

        # Choose lambda using cross-validation
        cv.lasso <- cv.glmnet(X, y, alpha = 1)
        best_lambda <- cv.lasso$lambda.min

        # Fit the final model on the dataset
        final_lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)

        # Display the coefficients
        coef(final_lasso_model)

        plot_glmnet(final_lasso_model, xvar = "lambda", label = TRUE)
        plot(cv.lasso)
        plot_glmnet(final_lasso_model, xvar = "lambda", label = TRUE, type.coef = "2norm")
        hist(coef(final_lasso_model)[-1], main="Distribution of Lasso Coefficients", xlab="Coefficients", border="blue")
        # extract non-zero coefficients
        non_zero_coef <- coef(final_lasso_model)
        non_zero_coef <-(as.data.frame(as.matrix(non_zero_coef)))
        glimpse(non_zero_coef)
        non_zero_coef$variable <- rownames(non_zero_coef)
        non_zero_coef <- non_zero_coef[which(non_zero_coef$s0 != 0),]
        non_zero_coef <- non_zero_coef[which(non_zero_coef$variable != "X.Intercept."),]
        
        ggplot(non_zero_coef,aes(y=variable,x=s0))+geom_point()+
        theme_bw()+geom_vline(aes(xintercept=0),linetype="dashed")

        var_reg <- sub("_demean","",non_zero_coef$variable)
        formula <- as.formula(paste0("log_area ~", paste(var_reg, collapse = " + "), " | gridcell_id + year + countrycode| 0 | gridcell_id"))
        model_lasso <- felm(formula, data = mcn[which(mcn$mangrove_area > 0), ])
        summary(model_lasso)
        plot_coefs(model_lasso,ci=0.9)


        mcn$logSumGDP <- log(mcn$Sum_GDP_50km)
        formula2 <- as.formula(paste0("log_area ~", paste(var_reg, collapse = " + "), "+logSumGDP*rich | gridcell_id + year | 0 | countrycode"))
        model_lasso_withGDP <- felm(formula2, data = mcn[which(mcn$mangrove_area > 0 & is.finite(mcn$logSumGDP)), ])
        summary(model_lasso_withGDP)
        plot_coefs(model_lasso_withGDP,ci=0.9)
        
        var_reg_anom <- var_reg[grep("anom",var_reg)]
        formula_anom <- as.formula(paste0("log_area ~", paste(var_reg_anom, collapse = " + "), " | gridcell_id + year | 0 | countrycode"))
        model_lasso_withGDP_anom <- felm(formula_anom, data = mcn[which(mcn$mangrove_area > 0 & is.finite(mcn$logSumGDP)), ])
        summary(model_lasso_withGDP_anom)
        plot_coefs(model_lasso_withGDP_anom,ci=0.9)

        var_reg_bin <- var_reg[-grep("anom",var_reg)]
        formula_bin <- as.formula(paste0("log_area ~", paste(var_reg_bin, collapse = " + "), "| gridcell_id + year | 0 | countrycode"))
        model_lasso_withGDP_bin <- felm(formula_bin, data = mcn[which(mcn$mangrove_area > 0 & is.finite(mcn$logSumGDP)), ])
        summary(model_lasso_withGDP_bin)
        plot_coefs(model_lasso_withGDP_bin,ci=0.9)
        
        # make a bar plot
        barplot(non_zero_coef, main="Variable Importance", horiz=TRUE, las=1, cex.names=0.7)





    ## Lasso Bins
    
    
    # Model bins
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        glimpse(mcn)
        ggplot(mcn, aes(y=log(mangrove_area),x=b23_24C))+geom_point()
        unique(mcn$year[which(mcn$mangrove_area>0)])
        unique(mcn$year[which(mcn$b21_22C>0)])
        hist(mcn$Latitude)
        class(mcn$Latitude)
        mcn$Latitude  <- as.double(mcn$Latitude)
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        #mcn$hot <- sum(mcn$b33_34C , mcn$b34_35C, mcn$b35_36C , mcn$b36_37C , mcn$b37_38C, mcn$b32_33C, mcn$b31_32C ,mcn$b30_31C ,mcn$b29_30C , mcn$b28_29C ,  mcn$b27_28C,na.rm=T)
        mcn$hot <- rowSums(data.frame(mcn$b33_34C , mcn$b34_35C, mcn$b35_36C , mcn$b36_37C , mcn$b37_38C, mcn$b32_33C, mcn$b31_32C),na.rm=T) #,mcn$b30_31C ,mcn$b29_30C),na.rm=T)
        mcn$cold <- rowSums(data.frame(mcn$b8_9C , mcn$b9_10C,mcn$b10_11C , mcn$b11_12C , mcn$b12_13C , mcn$b13_14C , mcn$b14_15C ,mcn$b15_16C , mcn$b16_17C ,mcn$b17_18C, mcn$b18_19C,mcn$b19_20C,mcn$b20_21C), na.rm=T)
        mcn$b21_24 <- rowSums(data.frame(mcn$b21_22C,mcn$b22_23C, mcn$b23_24C), na.rm=T)
        mcn$b24_27 <- rowSums(data.frame(mcn$b24_25C,mcn$b25_26C, mcn$b26_27C), na.rm=T)
        mcn$b27_29 <- rowSums(data.frame(mcn$b27_28C,mcn$b28_29C), na.rm=T)
        model_area_bin <- felm(log(mangrove_area)~
            #cold +
            b8_9C  +b9_10C +b10_11C  + b11_12C  + b12_13C + b13_14C + b14_15C+
            b15_16C + b16_17C +b17_18C+ b18_19C+b19_20C + b20_21C +
            b21_22C +b22_23C+ b23_24C  +
            b24_25C   + b25_26C  +b26_27C   + 
            b27_28C + b28_29C + b29_30C  + b30_31C + b31_32C+
            b32_33C + b33_34C + b34_35C+ b35_36C + b36_37C + b37_38C + 
            I(log(GDP/Population))
            #Latitude + #I(Latitude^2) +
            #b21_24 + b24_27 + b27_29 +
            #hot# + 
            #Mean_Precipitation + + I(Mean_Precipitation^2)  
            #year:countrycode + I(year^2):countrycode#logGDPpc*rich
        |gridcell_id + year + countrycode + R5 |0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0) ,])
        #data=mcn[which(mcn$mangrove_area>0 & (mcn$Sum_GDP_50km>0)) ,]) 
        #data=mcn[which(mcn$mangrove_area>0 & (mcn$hot_month=="hot")) ,]) 
        #data=mcn[which(mcn$mangrove_area>0) ,]) 
        #data=mcn[which(mcn$mangrove_area>0 & #mcn$Latitude<10 & mcn$Latitude>(-10) & 
        #mcn$R5=="ASIA") ,]) 
        #mcn$R5=="OECD") ,]) 
        #mcn$R5=="MAF") ,]) 
        #data=mcn[which(mcn$mangrove_area>0 & (mcn$rich==0) & (mcn$hot_location_2020==1)) ,]) 
        summary(model_area_bin)
        unique(mcn$R5)
        
        #library("jtools")
        plot_coefs(model_area_bin,ci_level = 0.90,omit.coefs = c("log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"))
    # Model bins (end)

    # Dynamic Model Bins (start)
        
                
        mcn1 <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        
        for(ir in unique(mcn1$R5)[-c(1)]){
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn <- mcn[which(mcn$R5==ir),]
        glimpse(mcn)
        unique(mcn$R5)
        # helper function to create hot and cold groups
        create_groups <- function(mcn, hot_bins, cold_bins) {
        mcn$hot <- rowSums(mcn[, which(names(mcn) %in% hot_bins)],na.rm=T)
        mcn$cold <- rowSums(mcn[, which(names(mcn) %in% cold_bins)],na.rm=T)
        return(mcn)
        }

        # helper function to run regression
        run_regression <- function(mcn, ind_vars) {
        formula <- as.formula(paste0("log(mangrove_area) ~ cold + ", paste(ind_vars, collapse = " + "), " + hot + sst_hottest_mean0020 | gridcell_id + year + countrycode| 0 | gridcell_id"))
        model <- felm(formula, data = mcn[which(mcn$mangrove_area > 0), ])
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

        # list of all possible bins
        all_bins <- paste("b", seq(8, 38, by = 1), "_", seq(9, 39, by = 1), "C", sep = "")

        # data frame to store results
        results <- data.frame()

        # loop over different groupings
        n <- 1
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

    # Model bins + hot dummy
        
        hot_50 <- quantile(mcn$sst, probs=c(0.5),na.rm=TRUE)
        
        mean_sst_id <- aggregate(sst ~ gridcell_id, FUN="mean", data=mcn)
        mean_sst_id$hot_location <- 1
        mean_sst_id$hot_location[which(mean_sst_id$sst<hot_50)] <- 0
        glimpse(mean_sst_id)
        mcn <- merge(mcn,mean_sst_id[,c(1,3)],by="gridcell_id",all=TRUE)
        mcn$hot <- mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C+ mcn$b32_33C+ mcn$b31_32C +mcn$b30_31C +mcn$b29_30C + mcn$b28_29C #+  mcn$b27_28C
        mcn$cold <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C + mcn$b14_15C
        glimpse(mcn)
        
        model_area_bin_hot_location <- felm(log(mangrove_area)~
             hot_location_alldata*cold +
            hot_location_alldata*b15_16C + hot_location_alldata*b16_17C +hot_location_alldata*b17_18C+
            hot_location_alldata*b18_19C+hot_location_alldata*b19_20C+ hot_location_alldata*b20_21C +hot_location_alldata*b21_22C +hot_location_alldata*b22_23C+ hot_location_alldata*b23_24C  +hot_location_alldata*b24_25C   + 
            hot_location_alldata*b25_26C  +hot_location_alldata*b26_27C   + hot_location_alldata*b27_28C + #hot_location_alldata*b28_29C + hot_location_alldata*b29_30C  + hot_location_alldata*b30_31C +
            hot_location_alldata*hot
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0) ,])
        summary(model_area_bin_hot_location)
        
        #library("jtools")
        plot_coefs(model_area_bin_hot_location,ci_level = 0.95,omit.coefs = c("log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)"))
    
            mcn$hot <-  mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C#mcn$b33_34C ++ mcn$b32_33C+ mcn$b31_32C #+mcn$b30_31C #+mcn$b29_30C #+ mcn$b28_29C #+  mcn$b27_28C
            mcn$cold <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C + mcn$b14_15C +mcn$b15_16C + mcn$b16_17C +mcn$b17_18C+
            mcn$b18_19C+mcn$b19_20C+ mcn$b20_21C +mcn$b21_22C +mcn$b22_23C
        
        model_area_bin_hotloc <- felm(log(mangrove_area)~
            cold +
           b24_25C   + 
            b25_26C  +b26_27C   + b27_28C + b28_29C +b29_30C  +b30_31C+ b31_32C +  b32_33C +  b33_34C  + b34_35C+ b35_36C + b36_37C + b37_38C #+ hot
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$hot_location_alldata==1) ,])
        summary(model_area_bin_hotloc)

        mcn$hot <- mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C+ mcn$b32_33C+ mcn$b31_32C +mcn$b30_31C +mcn$b29_30C + mcn$b28_29C #+  mcn$b27_28C
        mcn$cold <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C + mcn$b14_15C
        
         model_area_bin_coldloc <- felm(log(mangrove_area)~
            cold +
           b15_16C + b16_17C +b17_18C+
           b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C  +b26_27C   + b27_28C +# b28_29C + +b29_30C  +b30_31C
            hot
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$hot_location_alldata==0) ,])
        summary(model_area_bin_coldloc)
        plot_coefs(model_area_bin_coldloc)

        plot_coefs(list(model_area_bin_coldloc,model_area_bin_hotloc),ci_level = 0.95,
            model.names = c("Cold locations","Hot Locations"),
            coefs = c(
                "Bin: 15 to 16C"="b15_16C" ,
                "Bin: 16 to 17C"= "b16_17C",
                "Bin: 17 to 18C" = "b17_18C",
                "Bin: 18 to 19C"= "b18_19C",
                "Bin: 19 to 20C"= "b19_20C" ,
                "Bin: 20 to 21C"= "b20_21C"  ,
                "Bin: 21 to 22C" = "b21_22C",
                "Bin: 22 to 23C"=  "b22_23C",
                "Bin: 23 to 24C" =  "b23_24C",
                "Colder than 24"="cold",
                "Bin: 24 to 25C"  ="b24_25C" ,
                "Bin: 25 to 26C"="b25_26C" ,
                "Bin: 26 to 27C" =  "b26_27C",
                "Bin: 27 to 28C" =  "b27_28C",
                "Hotter than 28"="hot",
                "Bin: 28 to 29C" =  "b28_29C",
                "Bin: 29 to 30C" =  "b29_30C",
                "Bin: 30 to 31C" =  "b30_31C",
                "Bin: 31 to 32C" =  "b31_32C",
                "Bin: 32 to 33C" =  "b32_33C"))
                ggsave("Figures/bins/Difference_models.png",dpi=600)

            
            
            mcn$hot1 <- mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C #+ mcn$b32_33C+ mcn$b31_32C #+mcn$b30_31C +mcn$b29_30C + mcn$b28_29C #+  mcn$b27_28C + mcn$b26_27C
            mcn$cold1 <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C 
            model_area_bin <- felm(log(mangrove_area)~
                cold1 +
                b15_16C + b16_17C +b17_18C+
                b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
                b25_26C  +b27_28C + b26_27C+b30_31C +b29_30C + b28_29C+
                b32_33C+ b31_32C +
                hot1
            |gridcell_id+year|0|countrycode,
            data=mcn[which(mcn$mangrove_area>0) ,])
            summary(model_area_bin)
            plot_coefs(model_area_bin)
        
        plot_coefs(list(model_area_bin_coldloc,model_area_bin_hotloc,model_area_bin),ci_level = 0.95, 
            model.names = c("Cold locations","Hot Locations","All"),
            coefs = c(
                
                "Colder than 14"="cold1",
                "Bin: 15 to 16C"="b15_16C" ,
                "Bin: 16 to 17C"= "b16_17C",
                "Bin: 17 to 18C" = "b17_18C",
                "Bin: 18 to 19C"= "b18_19C",
                "Bin: 19 to 20C"= "b19_20C" ,
                "Bin: 20 to 21C"= "b20_21C"  ,
                "Bin: 21 to 22C" = "b21_22C",
                "Bin: 22 to 23C"=  "b22_23C",
                "Bin: 23 to 24C" =  "b23_24C",
                "Colder than 24"="cold",
                "Bin: 24 to 25C"  ="b24_25C" ,
                "Bin: 25 to 26C"="b25_26C" ,
                "Bin: 26 to 27C" =  "b26_27C",
                "Bin: 27 to 28C" =  "b27_28C",
                "Hotter than 28"="hot",
                "Hotter than 28"="hot1",
                "Bin: 28 to 29C" =  "b28_29C",
                "Bin: 29 to 30C" =  "b29_30C",
                "Bin: 30 to 31C" =  "b30_31C",
                "Bin: 31 to 32C" =  "b31_32C",
                "Bin: 32 to 33C" =  "b32_33C"))

                ggsave("Figures/bins/Difference_models_bins_All.png",dpi=600)
    
    # Model bins + hot dummy (end)

    # Model sst

        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        glimpse(mcn)
        qhot <- quantile(mcn$sst_hottest_mean0020,na.rm=T)
        hist(mcn$sst_hottest_mean0020,na.rm=T)
        mcn$hot_month <- "medium"
        mcn$hot_month[which(mcn$sst_hottest_mean0020<qhot[1])] <- "cold"
        mcn$hot_month[which(mcn$sst_hottest_mean0020>qhot[3])] <- "hot"
        
        model_area_sst <- felm(log(mangrove_area)~ 
            #log(mangrove_area) + 
            #sensor:log(ntl) + sensor:I(log(ntl)^2) + 
            #log(ntl) + I(log(ntl)^2) + 
            #logSumGDP+
            #income*logSumGDP + #hot_location_alldata*I(logSumGDP^2) + 
            #sst+I(sst^2)+lag(sst)+#
            #sst* hot_month + I(sst^2)*hot_month+# lag(sst)+#rich*I(sst^2) +# +  I(sst^3) 
            rich * logGDPpc+
            sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020+# lag(sst)+#rich*I(sst^2) +# +  I(sst^3) 
            #Mean_Salinity +# I(Mean_Salinity) #+
            #spei
            #ntl_change +
            #hot_location_alldata*Population_Count_50km+
            Mean_Precipitation + I(Mean_Precipitation^2) 
        |gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & (mcn$ntl>0)),])
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logSumGDP)),])
        summary(model_area_sst)

        ### Get marginal effect in colder 
            model <- model_area_sst
            data <- mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),]
            dataset <- mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),]
            Sigma <- vcov(model)
            coefT <- c("sst","I(sst^2)","sst_hottest_mean0020","sst:sst_hottest_mean0020","sst_hottest_mean0020:I(sst^2)","sst_hottest_mean0020:I(sst^2)","sst:sst_hottest_mean0020")
            start1 <- which(names(coef(model)) %in% coefT)
            
            
            sigma = Sigma[c(start1),c(start1)]
            beta.hat <- coef(model)[c(start1)]
            namevar <- coefT
            x <- seq(from=min(dataset[which(dataset$sst_hottest_mean0020<25),which(names(dataset)%in%c("sst"))],na.rm=TRUE),to=max(dataset[which(dataset$sst_hottest_mean0020<25),which(names(dataset)%in%c("sst"))],na.rm=TRUE), length=100)
            xmat <- cbind(1*25, 2*x*25) #25 hottest
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            model_sst_area <- data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp="Cold Summer",temp=x)

            x <- seq(from=min(dataset[which(dataset$sst_hottest_mean0020>30),which(names(dataset)%in%c("sst"))],na.rm=TRUE),to=max(dataset[which(dataset$sst_hottest_mean0020>30),which(names(dataset)%in%c("sst"))],na.rm=TRUE), length=100)
            xmat <- cbind(1*30, 2*x*30) #25 hottest
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            model_sst_area <- rbind(model_sst_area,data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp="Hot Summer",temp=x))

            ggplot(model_sst_area,aes(x=temp,y=gestimated,color=exp))+geom_line()+theme_bw()
        

        ### Get marginal effect in colder



        model_area_sst <- felm(log(mangrove_area)~ 
            #log(mangrove_area) + 
            #sensor:log(ntl) + sensor:I(log(ntl)^2) + 
            #log(ntl) + I(log(ntl)^2) + 
            #logSumGDP+
            #income*logSumGDP + #hot_location_alldata*I(logSumGDP^2) + 
            #sst+I(sst^2)+lag(sst)+#
            #sst* hot_month + I(sst^2)*hot_month+# lag(sst)+#rich*I(sst^2) +# +  I(sst^3) 
            sst_hottest + I(sst_hottest^2)+
            rich * logGDPpc+sst+I(sst^2)+
            
            #sst:sst_hottest_mean0020 + sst:I(sst_hottest_mean0020^2)+# lag(sst)+#rich*I(sst^2) +# +  I(sst^3) 
            #Mean_Salinity +# I(Mean_Salinity) #+
            #spei
            #ntl_change +
            #hot_location_alldata*Population_Count_50km+
            Mean_Precipitation + I(Mean_Precipitation^2) 
        |gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & (mcn$ntl>0)),])
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logSumGDP)),])
        summary(model_area_sst)

        df_model_area_sst <- data.frame(sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_sst,"sst_hottest","cold"))
        ggplot(df_model_area_sst,aes(x=temp,y=gestimated))+geom_line()+
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.4,fill="brown4")+
        theme_bw()+
        geom_hline(aes(yintercept=0),linetype="dashed")+
        xlab("Mean SST of the hottest month (C)")+
        ylab("Effect of one degree warming in the \nhottest month on mangrove area (pp)")

        unique(mcn$countrycode[which(mcn$rich==1)])
        unique(mcn$countrycode[which(mcn$income=="high")])
        unique(mcn$countrycode[which(mcn$income=="med")])
        unique(mcn$countrycode[which(mcn$income=="low")])
        

        glimpse(mcn)        
        #library("jtools")
        plot_coefs(model_area_sst,ci_level = 0.90,omit.coefs = c("log(mangrove_area)"))
    # Model sst (end)

    # Model bins anom
        
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn$anom_n3 <- rowSums(data.frame(mcn$anom_bn10_n9C ,mcn$anom_bn9_n8C ,mcn$anom_bn8_n7C , 
                mcn$anom_bn7_n6C ,mcn$anom_bn6_n5C ,
                mcn$anom_bn5_n4C ,mcn$anom_bn4_n3C,anom_bn3_n2C),na.rm=TRUE)
        mcn$anom_3more <- rowSums(data.frame( #mcn$anom_b2_3C ,
            mcn$anom_b3_4C,mcn$anom_b4_5C,mcn$anom_b5_6C,mcn$anom_b6_7C,mcn$anom_b7_8C),na.rm=TRUE)
            
            ggplot(mcn,aes(x=sst,y=anom_3more))+geom_point()
            ggplot(mcn,aes(x=Latitude,y=anom_3more))+geom_point()
            ggplot(mcn,aes(x=Latitude,y=log(anom_3more)))+geom_point()
            ggplot(mcn,aes(x=Latitude,y=log(mangrove_area)))+geom_point()
        
        model_area_bin_anom <- felm(log(mangrove_area)~
            anom_bn10_n9C  + anom_bn9_n8C + anom_bn8_n7C +
            anom_bn7_n6C + anom_bn6_n5C +
            anom_bn5_n4C + anom_bn4_n3C + anom_bn3_n2C + 
            #anom_n3+ #anom_bn3_n2C +
            anom_bn2_n1C+
            anom_bn1_0C+
            anom_b0_1C +
            anom_b1_2C + 
            anom_b2_3C+ anom_b3_4C + anom_b4_5C + 
            anom_b5_6C + anom_b6_7C + anom_b7_8C +#I(log(GDP/Population)) + 
            logGDPpc  
            
            #Mean_Precipitation+ 
            #I(Mean_Precipitation^2)
        |gridcell_id+year+R5|0|gridcell_id,
        #data=mcn[which(mcn$mangrove_area>0),])
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc) & mcn$year>2012),])
        
        summary(model_area_bin_anom)
        plot_coefs(model_area_bin_anom,ci_level = 0.90,
            #coefs = c("Anom bin: -3 to -2C" = "anom_bn3_n2C", "Anom bin: -2 to -1C" = "anom_bn2_n1C",
            #         "Anom bin: -1 to 0C" = "anom_bn1_0C","Anom bin: 0 to 1C" = "anom_b0_1C","Anom bin: 1 to 2C" = "anom_b1_2C", "Anom bin: 2 to 3C" = "anom_b2_3C",
            #         "Anom bin: 3C+" = "anom_3more"),
            omit.coefs =  c("I(Mean_Precipitation^2)","Mean_Precipitation","log(mangrove_area)","year","I(year^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))
        
        
        summary(model_area_bin_anom)
         
         model_area_bin_anom <- felm(log(mangrove_area)~
            #sst + 
            sst_hottest + I(sst_hottest^2) + 
            #temp+I(temp^2)+
            #anom_bn2_n1C+
            #anom_bn1_0C+
            #anom_b0_1C +
            #anom_b1_2C + 
            #anom_b2_3C+ 
            #anom_3more + 
            #log(mangrove_area) 
            #sensor:factor(rich)*log(ntl) + sensor:factor(rich)*I(log(ntl)^2) +
            #Sum_GDP+ 
            #Mean_Salinity #+ I(Mean_Salinity^2)#+
            #sst + I(sst^2) + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc
            #I(Latitude^2) + Latitude + factor(year)
        |gridcell_id + year|0|gridcell_id,
        #|gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)& (mcn$ntl)>0),])
        #data=mcn[which(mcn$mangrove_area>0),])

        summary(model_area_bin_anom)
        
        
        #library("jtools")
        plot_coefs(model_area_bin_anom,ci_level = 0.90,
            coefs = c("Anom bin: -3 to -2C" = "anom_bn3_n2C", "Anom bin: -2 to -1C" = "anom_bn2_n1C",
                     "Anom bin: -1 to 0C" = "anom_bn1_0C","Anom bin: 0 to 1C" = "anom_b0_1C","Anom bin: 1 to 2C" = "anom_b1_2C", "Anom bin: 2 to 3C" = "anom_b2_3C",
                     "Anom bin: 3C+" = "anom_3more"),
            omit.coefs =  c("I(Mean_Precipitation^2)","Mean_Precipitation","log(mangrove_area)","year","I(year^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))
        #ggsave("Figures/bins/anom_bin_coefs.png",dpi=600)

        glimpse(mcn)
        mcn$log_ntl <- log(mcn$ntl)
        model_area_bin_anom <- felm(log(mangrove_area)~
            anom_n3+ 
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + anom_3more #+
            #logSumGDP*hot_location_alldata + I(logSumGDP^2)*hot_location_alldata
            #sensor:log_ntl*rich + sensor:I(log_ntl^2)*rich
            #gdp_change*rich + gdp_change*rich
        |gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logSumGDP)),])
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$log_ntl)),])
        #data=mcn[which(mcn$mangrove_area>0),])
        summary(model_area_bin_anom)
        plot_coefs(model_area_bin_anom,ci_level = 0.95)
        ggsave("Figures/bins/anom_bin_coefs_area.png",dpi=600)

    
    # Model bins anom (end)

    # Model bins anom + hot dummy
        mcn$anom_n3 <- mcn$anom_bn10_n9C +mcn$anom_bn9_n8C +mcn$anom_bn8_n7C + 
                mcn$anom_bn7_n6C +mcn$anom_bn6_n5C +
                mcn$anom_bn5_n4C +mcn$anom_bn4_n3C
        mcn$anom_3more <- #mcn$anom_b2_3C +
            mcn$anom_b3_4C+mcn$anom_b4_5C+mcn$anom_b5_6C+mcn$anom_b6_7C+mcn$anom_b7_8C
        
        model_area_bin_anom_hotloc_dummy1 <- felm(log(mangrove_area)~
            anom_n3+ 
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + anom_3more 
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0),])
        summary(model_area_bin_anom_hotloc_dummy1)

        
        mcn$anom_n3 <- mcn$anom_bn10_n9C +mcn$anom_bn9_n8C +mcn$anom_bn8_n7C + 
                mcn$anom_bn7_n6C +mcn$anom_bn6_n5C +
                mcn$anom_bn5_n4C +mcn$anom_bn4_n3C
        mcn$anom_2more <- mcn$anom_b2_3C +
            mcn$anom_b3_4C+mcn$anom_b4_5C+mcn$anom_b5_6C+mcn$anom_b6_7C+mcn$anom_b7_8C
            
        model_area_bin_anom_hotloc_dummy1_c <- felm(log(mangrove_area)~
            anom_n3+ 
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + anom_3more 
            #anom_2more
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$hot_location_alldata==0),])
        summary(model_area_bin_anom_hotloc_dummy1_c)


        mcn$anom_n3 <- mcn$anom_bn10_n9C +mcn$anom_bn9_n8C +mcn$anom_bn8_n7C + 
                mcn$anom_bn7_n6C +mcn$anom_bn6_n5C + #mcn$anom_bn3_n2C+
                mcn$anom_bn5_n4C +mcn$anom_bn4_n3C
        mcn$anom_2more <- mcn$anom_b2_3C +
            mcn$anom_b3_4C+mcn$anom_b4_5C+mcn$anom_b5_6C+mcn$anom_b6_7C+mcn$anom_b7_8C
            
        model_area_bin_anom_hotloc_dummy1_h <- felm(log(mangrove_area)~
            anom_n3+ 
            anom_bn3_n2C +
            anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + anom_3more 
            #anom_2more
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$hot_location_alldata==1),])
        summary(model_area_bin_anom_hotloc_dummy1_h)
        
        model_list <- list(model_area_bin_anom_hotloc_dummy1_c,model_area_bin_anom_hotloc_dummy1_h,model_area_bin_anom)
        plot_coefs(model_list,
            coefs = c( "Anom bin: Colder than -3C" = "anom_n3","Anom bin: -3 to -2C" = "anom_bn3_n2C", "Anom bin: -2 to -1C" = "anom_bn2_n1C",
                     "Anom bin: -1 to 0C" = "anom_bn1_0C","Anom bin: 0 to 1C" = "anom_b0_1C","Anom bin: 1 to 2C" = "anom_b1_2C", "Anom bin: 2 to 3C" = "anom_b2_3C",
                     "Anom bin: Hotter than 3C" = "anom_3more"),
                     model.names=c("Cold Locations","Hot Locations","All"))
        ggsave("Figures/bins/comp_area_anombins_hotloc.png",dpi=600)

        model_area_bin_anom_hotloc_dummy <- felm(log(mangrove_area)~
            anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + 
            anom_bn7_n6C +anom_bn6_n5C +
            anom_bn5_n4C +anom_bn4_n3C+
            #anom_n3+ 
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + 
            #anom_3more +
            anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0),])
        summary(model_area_bin_anom_hotloc_dummy)


        model_area_bin_anom_hotloc <- felm(log(mangrove_area)~
            anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + 
            anom_bn7_n6C +anom_bn6_n5C +
           anom_bn5_n4C +anom_bn4_n3C+
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + 
            anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C#+sst

        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$hot_location_alldata==1),])
        summary(model_area_bin_anom_hotloc)

        model_area_bin_anom_coldloc <- felm(log(mangrove_area)~
            anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + 
            anom_bn7_n6C +anom_bn6_n5C +
           anom_bn5_n4C +anom_bn4_n3C+
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + 
            anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C#+sst

        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$hot_location_alldata==0),])
        summary(model_area_bin_anom_coldloc)
        
        mod_list <- list(model_area_bin_anom_coldloc,model_area_bin_anom_hotloc,model_area_bin_anom)
        plot_coefs(mod_list)


        model_area_bin_anom_allloc <- felm(log(mangrove_area)~
            anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + 
            anom_bn7_n6C +anom_bn6_n5C +
           anom_bn5_n4C +anom_bn4_n3C+
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + 
            anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C#+sst

        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0),])
        summary(model_area_bin_anom_allloc)
        
        mod_list <- list(model_area_bin_anom_coldloc,model_area_bin_anom_hotloc,model_area_bin_anom)
        plot_coefs(mod_list)



        #library("jtools")
        plot_coefs(model_area_bin_anom,ci_level = 0.90,
            coefs = c("Anom bin: -3 to -2C" = "anom_bn3_n2C", "Anom bin: -2 to -1C" = "anom_bn2_n1C",
                     "Anom bin: -1 to 0C" = "anom_bn1_0C","Anom bin: 0 to 1C" = "anom_b0_1C","Anom bin: 1 to 2C" = "anom_b1_2C", "Anom bin: 2 to 3C" = "anom_b2_3C",
                     "Anom bin: 3C+" = "anom_3more"),
            omit.coefs =  c("I(Mean_Precipitation^2)","Mean_Precipitation","log(mangrove_area)","year","I(year^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))
        ggsave("Figures/bins/anom_bin_coefs.png",dpi=600)


        model_area_bin_anom <- felm(log(mangrove_area)~
            anom_n3+ 
            anom_bn3_n2C +anom_bn2_n1C+
            anom_bn1_0C+anom_b0_1C +
            anom_b1_2C + anom_b2_3C + anom_3more
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0),])
        summary(model_area_bin_anom)
        plot_coefs(model_area_bin_anom,ci_level = 0.95)
        ggsave("Figures/bins/anom_bin_coefs_area.png",dpi=600)

    
    # Model bins anom + hot dummy (end)


    # Dynamic Model Anom Bins (start)
        # helper function to create hot and cold groups
        create_groups <- function(mcn, hot_bins, cold_bins) {
        mcn$hot <- rowSums(mcn[, which(names(mcn) %in% hot_bins)])
        mcn$cold <- rowSums(mcn[, which(names(mcn) %in% cold_bins)])
        return(mcn)
        }

        # helper function to run regression
        glimpse(mcn)
        run_regression <- function(mcn, ind_vars) {
        formula <- as.formula(paste0("log(mangrove_area) ~ cold + ", paste(ind_vars, collapse = " + "), " + hot | gridcell_id + year | 0 | countrycode"))
        model <- felm(formula, data = mcn[which(mcn$mangrove_area > 0), ])
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

        # list of all possible bins
        all_bins <- paste("anom_b", seq(-10, 7, by = 1), "_", seq(-9, 8, by = 1), "C", sep = "")
        #library("stringr")
        all_bins <- str_replace_all(all_bins, "-", "n")


        # data frame to store results
        results <- data.frame()

        # loop over different groupings
        n <- 1
        #max=18
        for (i in 2:9) {
        for (j in 12:17) {
            # create hot and cold groups
            
            cold_bins <- all_bins[1:i]
            hot_bins <- all_bins[j:18]
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

        name_map <- c("anom_bn8_n7C" = "Anom bin: -8 to -7C","anom_bn7_n6C"  = "Anom bin: -7 to -6C",
                    "anom_bn6_n5C" = "Anom bin: -6 to -5C","anom_bn5_n4C" =  "Anom bin: -5 to -4C","anom_bn4_n3C"=  "Anom bin: -4 to -3C" ,
                     "anom_bn3_n2C"  =   "Anom bin: -3 to -2C","anom_bn2_n1C" =  "Anom bin: -2 to -1C",
                    "anom_bn1_0C" =  "Anom bin: -1 to 0C","anom_b0_1C" = "Anom bin: 0 to 1C",
                    "anom_b1_2C" =  "Anom bin: 1 to 2C", "anom_b2_3C" = "Anom bin: 2 to 3C",
                      "anom_b3_4C" ="Anom bin: 3 to 4C", "anom_b4_5C" = "Anom bin: 4 to 5C",
                     "anom_b5_6C"="Anom bin: 5 to 6C"  ,"anom_b6_7C"="Anom bin: 6 to 7C"  ,"cold" = "Cold bin",
        "hot" = "Hot bin"
        )
        # assign new names to factor levels
        levels(results$Variable) <- name_map[levels(results$Variable)]
        levels(results$Variable) <- c(levels(results$Variable)[-which(levels(results$Variable) == "Hot bin")],"Hot bin")

        new_levels <- c("Cold bin", "Anom bin: -8 to -7C", "Anom bin: -7 to -6C", "Anom bin: -6 to -5C", "Anom bin: -5 to -4C", 
                "Anom bin: -4 to -3C", "Anom bin: -3 to -2C", "Anom bin: -2 to -1C","Anom bin: -1 to 0C", "Anom bin: 0 to 1C", "Anom bin: 1 to 2C", 
                "Anom bin: 2 to 3C", "Anom bin: 3 to 4C","Anom bin: 4 to 5C", "Anom bin: 5 to 6C", "Hot bin")

        results$Variable <- factor(results$Variable, levels = new_levels)
        levels(results$Variable) 




        
        results <-  results[-which(results$Variable %in% c("Anom bin: -8 to -7C")),]
        
        boxplot_bins <- ggplot(results, aes(y = Variable, x = Coefficient)) +
        geom_boxplot()+
        geom_vline(aes(xintercept=0),linetype="dashed")+
        xlab("Coefficient") +
        ylab("") +
        ggtitle("Effect of different hot and cold bin groupings\n on regression coefficients")+
        theme_bw()
        boxplot_bins        
        
        
        Significant_binsplot <- ggplot(results[which(results$P_value<0.05),], aes(x = Variable, y = Coefficient, color = factor(Hot_bins_n))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Only coefficients with \nP-value<0.05")+
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

        results$unique <- paste0(results$Hot_bins,results$Cold_bins)
        significanthot <- results[which((results$unique) %in% (results$unique[which(results$P_value<0.2)])),]
        
        plots_significant_all <- ggplot(significanthot, aes(x = Variable, y = Coefficient, color = factor(Hot_bins))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Only regressions which at least one \ncoefficient has P-value<0.05")+
        theme_bw() + coord_flip()  + guides(color="none")
        
        
        ggarrange(boxplot_bins,Significant_binsplot,plots_hot,ncol=3)
        ggarrange(boxplot_bins,Significant_binsplot,plots_significant_all,ncol=3,widths=c(2.5,2,2))
        ggsave("Figures/bins/Effect_of_AnomBins_AREA.png",dpi=600)

        
    # Dymanic Model Anom Bins (end)

    # Dynamic Model Anom Bins MedRes (start)
        # helper function to create hot and cold groups
        create_groups <- function(mcn, hot_bins, cold_bins) {
        mcn$hot <- rowSums(mcn[, which(names(mcn) %in% hot_bins)])
        mcn$cold <- rowSums(mcn[, which(names(mcn) %in% cold_bins)])
        return(mcn)
        }

        # helper function to run regression
        glimpse(mcn)
        run_regression <- function(mcn, ind_vars) {
        formula <- as.formula(paste0("log(mangrove_area) ~ cold + ", paste(ind_vars, collapse = " + "), " + hot | gridcell_id + year | 0 | countrycode"))
        model <- felm(formula, data = mcn[which(mcn$mangrove_area > 0), ])
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

        # list of all possible bins
        all_bins <- paste("anom_b", seq(-10, 7.5, by = 0.5), "_", seq(-9.5, 8, by = 0.5), "C", sep = "")
        #library("stringr")
        all_bins <- str_replace_all(all_bins, "-", "n")


        # data frame to store results
        results <- data.frame()

        # loop over different groupings
        n <- 1
        #max=18
        for (i in 2:19) {
        for (j in 22:34) {
            # create hot and cold groups
            
            cold_bins <- all_bins[1:i]
            hot_bins <- all_bins[j:36]
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
        results0 <- results
        results <- results0
        levels(factor(results$Variable))

        
        # reorder factor levels so "cold" is first
        results$Variable <- relevel(factor(results$Variable), ref = "cold")

        name_map <- c("anom_bn9_n8.5C" = "Anom bin: -9 to -8.5C","anom_bn8.5_n8C" = "Anom bin: -8.5 to -8C","anom_bn8_n7.5C" = "Anom bin: -8 to -7.5C","anom_bn7.5_n7C"  = "Anom bin: -7.5 to -7C",
        "anom_bn7_n6.5C" = "Anom bin: -7 to -6.5C","anom_bn6.5_n6C"  = "Anom bin: -6.5 to -6C",
                    "anom_bn6_n5.5C" = "Anom bin: -6 to -5.5C","anom_bn5.5_n5C" =  "Anom bin: -5.5 to -5C","anom_bn5_n4.5C"=  "Anom bin: -5 to -4.5C" ,
                    "anom_bn4.5_n4C" = "Anom bin: -4.5 to -4C", "anom_bn4_n3.5C" =  "Anom bin: -4 to -3.5C",
                     "anom_bn3.5_n3C"  =   "Anom bin: -3.5 to -3C","anom_bn3_n2.5C" =  "Anom bin: -3 to -2.5C",
                     "anom_bn2.5_n2C"  =   "Anom bin: -2.5 to -2C","anom_bn2_n1.5C" =  "Anom bin: -2 to -1.5C",
                    "anom_bn1.5_n1C" =  "Anom bin: -1.5 to -1C","anom_bn1_n0.5C" =  "Anom bin: -1 to -0.5C","anom_bn0.5_0C" =  "Anom bin: -0.5 to 0C",
                    "anom_b0_0.5C" = "Anom bin: 0 to 0.5C",
                    "anom_b0.5_1C" = "Anom bin: 0.5 to 1C",
                    "anom_b1_1.5C" = "Anom bin: 1 to 1.5C",
                    "anom_b1.5_2C" =  "Anom bin: 1.5 to 2C", "anom_b2_2.5C" = "Anom bin: 2 to 2.5C",
                    "anom_b2.5_3C" = "Anom bin: 2.5 to 3C",
                      "anom_b3_3.5C" ="Anom bin: 3 to 3.5C", 
                      "anom_b3.5_4C" ="Anom bin: 3.5 to 4C", 
                      "anom_b4_4.5C" = "Anom bin: 4 to 4.5C",
                      "anom_b4.5_5C" = "Anom bin: 4.5 to 5C",
                      "anom_b5_5.5C" = "Anom bin: 5 to 5.5C",
                     "anom_b5.5_6C"="Anom bin: 5.5 to 6C"  ,
                     "anom_b6_6.5C"="Anom bin: 6 to 6.5C"  ,
                     "anom_b6.5_7C"="Anom bin: 6.5 to 7C"  ,
                     "anom_b7_7.5C"="Anom bin: 7 to 7.5C"  ,
                     "cold" = "Cold bin",
        "hot" = "Hot bin"
        )


        # assign new names to factor levels
        levels(results$Variable) <- name_map[levels(results$Variable)]
        #levels(results$Variable) <- c(levels(results$Variable)[-which(levels(results$Variable) == "Hot bin")],"Hot bin")

        new_levels <- c("Anom bin: -9 to -8.5C",
        "Anom bin: -8.5 to -8C", 
        "Anom bin: -8 to -7.5C",
        "Anom bin: -7.5 to -7C", 
        "Anom bin: -7 to -6.5C",
        "Anom bin: -6.5 to -6C",
         "Anom bin: -6 to -5.5C", 
         "Anom bin: -5.5 to -5C",
         "Anom bin: -5 to -4.5C" ,
         "Anom bin: -4.5 to -4C",
          "Anom bin: -4 to -3.5C", 
           "Anom bin: -3.5 to -3C", 
           "Anom bin: -3 to -2.5C",
           "Anom bin: -2.5 to -2C",  
           "Anom bin: -2 to -1.5C", 
           "Anom bin: -1.5 to -1C", 
           "Anom bin: -1 to -0.5C",
           "Anom bin: -0.5 to 0C",
           "Anom bin: 0 to 0.5C", 
           "Anom bin: 0.5 to 1C",
           "Anom bin: 1 to 1.5C",
           "Anom bin: 1.5 to 2C", 
           "Anom bin: 2 to 2.5C", 
           "Anom bin: 2.5 to 3C",
           "Anom bin: 3 to 3.5C",
           "Anom bin: 3.5 to 4C",
            "Anom bin: 4 to 4.5C", 
            "Anom bin: 4.5 to 5C",
             "Anom bin: 5 to 5.5C",
             "Anom bin: 5.5 to 6C"  ,
             "Anom bin: 6 to 6.5C"  ,
             "Anom bin: 6.5 to 7C"  ,
             "Anom bin: 7 to 7.5C"  ,
             "Cold bin", "Hot bin"
        )

        results$Variable <- factor(results$Variable, levels = new_levels)
        levels(results$Variable) <- c("Cold bin",levels(results$Variable)[-which(levels(results$Variable) == "Cold bin")])
        levels(results$Variable) 




        
        #results <-  results[-which(results$Variable %in% c("Anom bin: -8 to -7C")),]
        
        boxplot_bins <- ggplot(results, aes(y = Variable, x = Coefficient)) +
        geom_boxplot()+
        geom_vline(aes(xintercept=0),linetype="dashed")+
        xlab("Coefficient") +
        ylab("") +
        ggtitle("Effect of different hot and cold bin groupings\n on regression coefficients")+
        theme_bw()
        boxplot_bins        
        
        
        Significant_binsplot <- ggplot(results[which(results$P_value<0.01),], aes(x = Variable, y = Coefficient, color = factor(Hot_bins_n))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") + #ylim(c(-0.01,0.01))+
        ggtitle("Only coefficients with \nP-value<0.05")+
        theme_bw() + coord_flip() + guides(color="none")

        Significant_binsplot

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

        results$unique <- paste0(results$Hot_bins,results$Cold_bins)
        significanthot <- results[which((results$unique) %in% (results$unique[which(results$P_value<0.001)])),]
        
        plots_significant_all <- ggplot(significanthot, aes(x = Variable, y = Coefficient, color = factor(Hot_bins))) +
        geom_pointrange(aes(ymin = (Coefficient - 1.96 * SE), ymax = (Coefficient + 1.96 * SE)), 
                    position=position_jitter(width=0.2),linetype="dotted") +
                    xlab("") +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Coefficient") +
        ggtitle("Only regressions which at least one \ncoefficient has P-value<0.05")+
        theme_bw() + coord_flip()  + guides(color="none")
        plots_significant_all 
        
        ggarrange(boxplot_bins,Significant_binsplot,plots_hot,ncol=3)
        ggarrange(boxplot_bins,Significant_binsplot,plots_significant_all,ncol=3,widths=c(2.5,2,2))
        ggsave("Figures/bins/Effect_of_AnomBins_AREA.png",dpi=600)

        
    # Dymanic Model Anom Bins MedRes (end)

    # Prefered Anom Bins
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        glimpse(mcn)
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        mcn$anom_n3 <- rowSums(data.frame(mcn$anom_bn10_n9C +mcn$anom_bn9_n8C +mcn$anom_bn8_n7C + 
                mcn$anom_bn7_n6C +mcn$anom_bn6_n5C +
                mcn$anom_bn5_n4C +mcn$anom_bn4_n3C 
                #mcn$anom_bn3_n2C
                ),na.rm=TRUE)
        mcn$anom_3more <- #mcn$anom_b2_3C +
            mcn$anom_b3_4C+mcn$anom_b4_5C+mcn$anom_b5_6C+mcn$anom_b6_7C+mcn$anom_b7_8C
        model_area_bin_anom <- felm(log(mangrove_area)~
        #model_area_bin_anom <- felm(log(mangrove_area)~
            anom_n3+ 
            anom_bn3_n2C +
            anom_bn2_n1C+
            anom_bn1_0C+ 
            #anom_bn1_n0.5C+ 
            #anom_bn0.5_0C+ 
            #I(anom_bn0.5_n0.4C+
            #anom_bn0.4_n0.3C+
            #anom_bn0.3_n0.2C+
            #anom_bn0.2_n0.1C+
            #anom_bn0.1_n0C)+ 
            anom_b0_1C +
            #anom_b0_0.5C + anom_b0.5_1C+
            anom_b1_2C + anom_b2_3C +
            anom_3more# +
            #logGDPpc*rich+#*mhw_dur*rich
            #logGDPpc:mhw_dur
            #log(mangrove_area) 
            #log(ntl) + I(log(ntl)^2) +
            #Sum_GDP+ 
            #Mean_Salinity+ #+ I(Mean_Salinity^2)#+
            #sst + I(sst^2) + 
            #I(Mean_Precipitation^2) +
            #Mean_Precipitation
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0),])
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_bin_anom)
        #library(jtools)
        plot_coefs(model_area_bin_anom,omit.coefs = c("logGDPpc:rich","logGDPpc","Mean_Precipitation","I(Mean_Precipitation^2)","log(mangrove_area)","year","I(year^2)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))

    # Prefered Anom Bins

    #Random Forest for Human Footprint
        mcn$log_ntl <- log(mcn$ntl)
        mcn$log_area <- log(mcn$mangrove_area)
        variables_of_interest <- c("log_area","log_ntl","gdp_change","GDP","Population_Count_50km","ntl_change","Sum_GDP_50km","gridcell_id")
        
        sub_mcn <- mcn[,which(names(mcn) %in% variables_of_interest)]
        sub_mcn <- sub_mcn[complete.cases(sub_mcn), ] 
        glimpse(sub_mcn)

        # calculate grid cell-level means
        grid_means <- aggregate(. ~ gridcell_id, data=sub_mcn, FUN=mean)

        # subtract grid cell-level means from each observation
        for (var in names(sub_mcn)[-which(names(sub_mcn)=="gridcell_id")]) {
        sub_mcn[[paste0(var, "_demean")]] <- sub_mcn[[var]] - grid_means[[var]][match(sub_mcn$gridcell_id, grid_means$gridcell_id)]
        }
        glimpse(sub_mcn)    

        sub_mcn <- sub_mcn[,c(9:15)]    
        glimpse(sub_mcn)
        sub_mcn <- sub_mcn[is.finite(sub_mcn$log_area_demean),]
        sub_mcn <- sub_mcn[is.finite(sub_mcn$log_ntl_demean),]
        sub_mcn <- sub_mcn[is.finite(sub_mcn$ntl_change_demean),]
        sub_mcn <- sub_mcn[!is.na(sub_mcn$ntl_change_demean),]
        sub_mcn <- na.omit(sub_mcn)
        sub_mcn <- sub_mcn[complete.cases(sub_mcn), ]
        #trf <- tuneRF(sub_mcn[,c(1:6)], sub_mcn[,7])
        #mt <- trf[which.min(trf[,2]), 1]
        
        area.rf <- randomForest(log_area_demean ~ GDP_demean + Sum_GDP_50km_demean + 
                                        Population_Count_50km_demean +
                                        ntl_change_demean +
                                        gdp_change_demean +
                                        log_ntl_demean 
        , data=sub_mcn, importance=TRUE,
                        #proximity=TRUE,
                        ntree=100,mtry =2)
        varImpPlot(area.rf)


        importance_frame <- measure_importance(area.rf)
        
        vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees"))
        interactions_frame <- min_depth_interactions(area.rf, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
        #interactions_frame <- interactions_frame[-which(interactions_frame$interaction %in% c("damage:damage")),]
        plot_min_depth_interactions(interactions_frame)        
        library(ggplot2)
        ggsave("Figures/RandomForest/Interactions_area_human.png",dpi=600)

        
        plotmo(area.rf, persp.ticktype="detailed")
        plotmo(area.rf)
        ggsave("Figures/RandomForest/Plotmo_area_human.png",dpi=600)
        
        
        library(randomForestExplainer)
        min_depth_frame <- min_depth_distribution(area.rf)
        md1 <- plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)
        plot(md1)    
        ggsave("Figures/RandomForest/MinDepth_area_human.png",dpi=600)
    #Random Forest for Human Footprint

    # Relation between Pop and NTL
        ggplot(mcn, aes(x=log_ntl,y=log(Population_Count_50km),color=countrycode))+
        geom_point()

    # Relation between Pop and NTL
    
    ## Bins Prefered + Human FootPrint 
        
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        mcn$hot <- mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C+ mcn$b32_33C+ mcn$b31_32C +mcn$b30_31C +mcn$b29_30C + mcn$b28_29C #+  mcn$b27_28C + mcn$b26_27C
        mcn$cold <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C 
        model_area_bin_ntl <- felm(log(mangrove_area)~
            #cold +
            #b15_16C + b16_17C +b17_18C+
            #b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            #b25_26C  +b27_28C + b26_27C+
            #hot+ 
            sensor:log(ntl)*rich + sensor:I(log(ntl)^2)*rich
            #sensor:(ntl_change) + sensor:I(ntl_change^2)*rich
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0) ,])
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$ntl_change)) ,])
        summary(model_area_bin_ntl)
        plot_coefs(model_area_bin_ntl)

        glimpse(mcn)
        ggplot(mcn,aes(x=log(mangrove_area),y=log(Sum_GDP_50km),color=countrycode))+geom_point()

        ggplot(mcn,aes(x=sst,y=log(Sum_GDP_50km),color=factor(rich)))+geom_point()
        summary(felm(log(Sum_GDP_50km)~sst|gridcell_id+year|0|countrycode,data=mcn[which(mcn$Sum_GDP_50km>0 ) ,]))

        model_area_bin_gdp <- felm(log(mangrove_area)~
            cold +
            b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +
            b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C +
            b27_28C + b26_27C+
            hot+logGDPpc*hot_location_alldata
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$Sum_GDP_50km>0 & mcn$rich==0) ,])
        summary(model_area_bin_gdp)
        
        model_area_bin_gdp <- felm(log(mangrove_area)~
            cold +
            b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +
            b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C +
            b27_28C + b26_27C+
            hot+
            #logGDPpc*anom_b2_3C
            logGDPpc:mhw
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$Sum_GDP_50km>0) ,])
        summary(model_area_bin_gdp)
        plot_coefs(model_area_bin_gdp)

        install.packages("glmnet")

        model_area_bin_pop <- felm(log(mangrove_area)~
            #cold +
            #b15_16C + b16_17C +b17_18C+
            #b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            #b25_26C  +b27_28C + b26_27C+
            #hot+ 
            log(Population_Count_50km)*rich + I((Population_Count_50km)^2)*rich # + I((Population_Count_50km)^3)
            #log(Population_Count) + I(log(Population_Count)^2)  + I(log(Population_Count)^3)
            #log(Sum_GDP_50km) + I(log(Sum_GDP_50km)^2) + I(log(Sum_GDP_50km)^3)
            #log(Sum_GDP_50km) + I(log(Sum_GDP_50km)^2) + I(log(Sum_GDP_50km)^3)
        |gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & mcn$Sum_GDP>0) ,])
        data=mcn[which(mcn$mangrove_area>0 & mcn$Population_Count>0) ,])
        summary(model_area_bin_pop)
        plot_coefs(model_area_bin_pop)


        model_area_bin_gdppc <- felm(log(mangrove_area)~
            #cold +
            #b15_16C + b16_17C +b17_18C+
            #b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            #b25_26C  +b27_28C + b26_27C+
            #hot+ 
            I(Sum_GDP_50km/Population_Count_50km) + I((Sum_GDP_50km/Population_Count_50km^2))#+ I((Population_Count_50km)^2)  + I((Population_Count_50km)^3)
            #log(Population_Count) + I(log(Population_Count)^2)  + I(log(Population_Count)^3)
            #log(Sum_GDP_50km) + I(log(Sum_GDP_50km)^2) + I(log(Sum_GDP_50km)^3)
            #log(Sum_GDP_50km) + I(log(Sum_GDP_50km)^2) + I(log(Sum_GDP_50km)^3)
        |gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & mcn$Sum_GDP>0) ,])
        data=mcn[which(mcn$mangrove_area>0 & mcn$Population_Count>0) ,])
        summary(model_area_bin_gdppc)
        plot_coefs(model_area_bin_gdppc)

    ## Bins Prefered + Human Footprint

    ## Lasso for human foorprint (start)
            # Load required library
        library(glmnet)
        #library(dplyr)

        # Filter data
        mcn$logSumGDP <- log(mcn$Sum_GDP_50km)
        mcn$logSumGDP <- log(mcn$Sum_GDP_50km)
        mcn$log_area <- log(mcn$mangrove_area)
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        mcn$hot <- mcn$b33_34C + mcn$b34_35C+ mcn$b35_36C + mcn$b36_37C + mcn$b37_38C+ mcn$b32_33C+ mcn$b31_32C +mcn$b30_31C +mcn$b29_30C + mcn$b28_29C +  mcn$b27_28C
        mcn$cold <- mcn$b8_9C + mcn$b9_10C+ mcn$b10_11C + mcn$b11_12C + mcn$b12_13C + mcn$b13_14C + mcn$b14_15C
        data_filtered <- mcn[which(mcn$mangrove_area > 0 & is.finite(mcn$logGDPpc)), ]
        
        # Calculate means for fixed effects and subtract from the variables
        # Here, I am first demeaning by gridcell_id and then by year
        data_filtered <- data_filtered %>%
        group_by(gridcell_id) %>%
        mutate_at(vars(cold, b15_16C:b26_27C, hot, logGDPpc, log_area), funs(demean = . - mean(.,na.rm=TRUE))) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate_at(vars(cold, b15_16C:b26_27C, hot, logGDPpc, log_area), funs(demean = . - mean(.,na.rm=TRUE))) %>%
        ungroup()

        # Prepare matrix of predictors and response variable
        glimpse(data_filtered)

        data_filtered <- data_filtered[,which(names(data_filtered) %in% c("log_area_demean" ,"cold_demean","b15_16C_demean",
        "b16_17C_demean","b17_18C_demean","b18_19C_demean","b19_20C_demean","b20_21C_demean","b21_22C_demean",
        "b22_23C_demean","b23_24C_demean","b24_25C_demean","b25_26C_demean","b26_27C_demean","hot_demean","logGDPpc_demean","rich"))]

        data_filtered <- data_filtered[complete.cases(data_filtered),]        
        
        X <- model.matrix(log_area_demean~ cold_demean + b15_16C_demean + b16_17C_demean + b17_18C_demean + b18_19C_demean + b19_20C_demean + b20_21C_demean +
                                        b21_22C_demean + b22_23C_demean + b23_24C_demean + b24_25C_demean + b25_26C_demean  + 
                                        b26_27C_demean + hot_demean + rich*logGDPpc_demean, data = data_filtered)
        y <- data_filtered$log_area_demean

        # Apply lasso regression
        lasso_model <- glmnet(X, y, alpha = 1)

        # Choose lambda using cross-validation
        cv.lasso <- cv.glmnet(X, y, alpha = 1)
        best_lambda <- cv.lasso$lambda.min

        # Fit the final model on the dataset
        final_lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)

        # Display the coefficients
        coef(final_lasso_model)


        model_area_bin_ntl <- felm(log(mangrove_area)~
            b17_18C+b18_19C+b21_22C+b22_23C+b24_25C+b25_26C+b26_27C+
            logGDPpc*rich
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$ntl_change)) ,])
        summary(model_area_bin_ntl)
        plot_coefs(model_area_bin_ntl)

        ## Bootstrap
                        # Setup
            set.seed(123) # for reproducibility
            B <- 10000 # number of bootstrap samples
            n <- nrow(data_filtered) # number of rows in the dataset
            lasso_coefs <- matrix(NA, nrow = B, ncol = ncol(X)) # empty matrix to store coefficients
            glimpse(X)
            # Bootstrapping
            for(i in 1:B){
            bootstrap_sample <- sample(1:n, n, replace = TRUE) # generate a bootstrap sample
            Xb <- X[bootstrap_sample, ] # get the predictors for the bootstrap sample
            yb <- y[bootstrap_sample] # get the response for the bootstrap sample
            cv.lasso_b <- cv.glmnet(Xb, yb, alpha = 1) # perform cross-validation
            best_lambda_b <- cv.lasso_b$lambda.min # choose lambda
            lasso_b <- glmnet(Xb, yb, alpha = 1, lambda = best_lambda_b) # fit the model
            lasso_coefs[i, ] <- coef(lasso_b)[-1] # save the coefficients (exclude intercept)
            }

            # Calculate p-values
            p_values <- colSums(lasso_coefs == 0) / B

            # Print p-values
            print(p_values)

            # Plot coefficients
            coefficients <- coef(final_lasso_model)[-1] # exclude intercept
            as.data.frane(coefficients)
            plot(coefficients, type = 'h', main = 'Lasso Coefficients with p-values', xlab = 'Index', ylab = 'Coefficient')
            text(x = 1:length(coefficients), y = coefficients, labels = paste0('p = ', round(p_values, 3)), pos = ifelse(coefficients >= 0, 3, 1))



            ## Ridges
                # Prepare matrix of predictors and response variable
                X <- model.matrix(log_area_demean ~ cold_demean + b15_16C_demean + b16_17C_demean + b17_18C_demean + b18_19C_demean + 
                                b19_20C_demean + b20_21C_demean + b21_22C_demean + b22_23C_demean + b23_24C_demean + 
                                b24_25C_demean + b25_26C_demean + b26_27C_demean + hot_demean+logGDPpc_demean, data = data_filtered)
                y <- data_filtered$log_area_demean

                # Apply ridge regression
                ridge_model <- glmnet(X, y, alpha = 0)

                # Choose lambda using cross-validation
                cv.ridge <- cv.glmnet(X, y, alpha = 0)
                best_lambda <- cv.ridge$lambda.min

                # Fit the final model on the dataset
                final_ridge_model <- glmnet(X, y, alpha = 0, lambda = best_lambda)

                # Display the coefficients
                coef(final_ridge_model)

                # Plot coefficients
                plot_glmnet(final_ridge_model, xvar = "lambda", label = TRUE)
        ## Bootstrap

    ## Lasso for human footprint (end)

    # Human footprint + temp
        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        #mcn <- mcn4
        glimpse(mcn)
        mcn$logGDPpc <- log(mcn$Sum_GDP_50km/mcn$Population_Count_50km)
        mcn$anom_n3 <- rowSums(data.frame(mcn$anom_bn10_n9C +mcn$anom_bn9_n8C +mcn$anom_bn8_n7C + 
                mcn$anom_bn7_n6C +mcn$anom_bn6_n5C +
                mcn$anom_bn5_n4C +mcn$anom_bn4_n3C 
                #mcn$anom_bn3_n2C
                ),na.rm=TRUE)
        mcn$anom_3more <- #mcn$anom_b2_3C +
            mcn$anom_b3_4C+mcn$anom_b4_5C+mcn$anom_b5_6C+mcn$anom_b6_7C+mcn$anom_b7_8C
        
        ggplot(mcn,aes(x=log(GDP/Population),y=((logGDPpc)/log(GDP/Population)),group=gridcell_id,color=countrycode))+
        geom_line()
        
        mcn$GDPpc_frac <- ((mcn$logGDPpc)/log(mcn$GDP/mcn$Population))
        mcn$temp <- mcn$temp_anom + mcn$mean_temp

        unique(mcn$gridcell_id[which(mcn$mangrove_area>0)])
        unique(mcn$gridcell_id[which(!is.na(mcn$sst))])
        unique(mcn$gridcell_id[which(!is.na(mcn$b22_23C))])
        unique(mcn$gridcell_id[which(!is.na(mcn$anom_b0_1C))])
        unique(mcn$gridcell_id[which(!is.na(mcn$anom_b0_1C))])
        unique(mcn$gridcell_id[which(!is.na(mcn$logGDPpc))])
        unique(mcn$gridcell_id[which(!is.na(mcn$temp))])
        
        model_area_bin_anom <- felm(log(mangrove_area)~
            #mhw*factor(income):logGDPpc#+
            #factor(mhw)+
            #factor(year)+#log(Sum_GDP_50km)
            #log(GDP/Population)+
            #GDPpc_frac+
            #logGDPpc*rich
            #ntl_change#*income+


            #ntl_change:sensor
            rich*logGDPpc+
            temp:income + lag(temp):income#+ I(temp^2):income
            #factor(income):Population_Count_50km+
            #sst*rich+#sst*hot_location_alldata+
            #logGDPpc*rich#*mhw_dur*rich
            #log(GDP)*rich
            #log(np)+#*sst+
            #sst+lag(sst)#+#*rich+I(sst^2)*rich+
            #logGDPpc*rich+
            #sst + lag(sst)
            #+log(ntl):sensor
        |gridcell_id+year|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0),])
        #data=mcn[which(mcn$mangrove_area>0 & mcn$GDPpc_frac>0 ),])
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),]) 
        #data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$ntl_change)),]) 
        #data=mcn[which(mcn$mangrove_area>0 & mcn$sensor=="DMSP" & is.finite(mcn$ntl_change)),]) 
        #data=mcn[which(mcn$mangrove_area>0 & mcn$Sum_GDP_50km>0),])
        summary(model_area_bin_anom)
        plot_coefs(model_area_bin_anom)
        #library(jtools)
        plot_coefs(model_area_bin_anom,omit.coefs = c("logGDPpc:rich","logGDPpc","Mean_Precipitation","I(Mean_Precipitation^2)","log(mangrove_area)","year","I(year^2)","salinity_change","I(salinity_change^2)","sst","I(sst^2)","Mean_Salinity"))
        
        levels(factor(mcn$countrycode[which(mcn$rich==1)]))
        levels(factor(mcn$countrycode[which(mcn$rich==0)]))
        levels(factor(mcn$countrycode[which(mcn$income=="low")]))
        table(factor(mcn$income))

    # Human footprint + temp
    
    #Extra
        stargazer(model_area_bin,model_area_bin_anom,type="text")
        modelsummary(list(model_area_bin,model_area_bin_anom),stars=TRUE,output="Tables/bins/reg_table_bin_anom_comp.docx") 
        modelsummary(list(model_area_bin_anom),stars=TRUE,output="Tables/bins/reg_table_bin_anom.docx") 
        
        AIC(model_area_bin,model_area_bin_anom)
        
        
        AIC(model_area_sst,model_area_bin,model_area_bin_anom)
        model_list<- list(model_area_sst,model_area_bin,model_area_bin_anom)
        modelsummary(model_list,stars=TRUE,output="reg_table.docx")
        mod.names <- c('SST', 'Bins', 'Anomaly Bins')   
        aictab(cand.set = model_list, modnames = mod.names)
    #Extra




### All data DMSP + VIIRS (end)

#### Model mangrove_area ~ ntl + mhw  (start)


        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn <- mcn %>%
        group_by(gridcell_id) %>%
        filter(sum(which(abs(annual_area_change)>1))<1)

        mcn <- mcn %>%
        group_by(gridcell_id) %>%
        filter(sum(which(abs(ntl_change)>2))<1)
       
        table(mcn$mhw)
        glimpse(mcn)
        mcn$temp <- mcn$mean_temp + mcn$temp_anom
        mcn$log_ntl <- log(mcn$ntl)

        ggplot(data=mcn,aes(y=annual_area_change,x=temp))+
        geom_point()

        model_area <- felm(annual_area_change~
            b8_9C+b9_10C  + b10_11C   + b11_12C + b12_13C + b13_14C + b14_15C  + b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C  +b26_27C   +b27_28C + b28_29C +b29_30C  +b30_31C +b31_32C+ b32_33C+
            b33_34C + b34_35C+b35_36C + b36_37C + b37_38C + 
            log(mangrove_area)
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & !is.na(mcn$ntl_change)),])
        
        #data=mcn[which(mcn$ntl>0),])

        summary(model_area)
        glimpse(mcn)

        model_area <- felm(annual_area_change~ log_ntl:sensor + sensor:I(log_ntl^2) +
            b8_9C+b9_10C  + b10_11C   + b11_12C + b12_13C + b13_14C + b14_15C  + b15_16C + b16_17C +b17_18C+
            b18_19C+b19_20C+ b20_21C +b21_22C +b22_23C+ b23_24C  +b24_25C   + 
            b25_26C  +b26_27C   +b27_28C + b28_29C +b29_30C  +b30_31C +b31_32C+ b32_33C+
            b33_34C + b34_35C+b35_36C + b36_37C + b37_38C + Mean_Precipitation + I(Mean_Precipitation^2)+
            log(mangrove_area)
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & (mcn$ntl>0)),])
        
        #data=mcn[which(mcn$ntl>0),])

        summary(model_area)

        model_area <- felm(annual_area_change~ sensor:I(log_ntl^2) + sensor:log_ntl +
           # anom_bn10_n9C +anom_bn9_n8C +anom_bn8_n7C + anom_bn7_n6C +anom_bn6_n5C +
        #anom_bn5_n4C +anom_bn4_n3C+anom_bn3_n2C +anom_bn2_n1C+
        #anom_bn1_0C+anom_b0_1C +anom_b1_2C +anom_b2_3C +anom_b3_4C+anom_b4_5C+anom_b5_6C+anom_b6_7C+anom_b7_8C +
            Mean_Precipitation + I(Mean_Precipitation^2) +
            mcw_int + I(mcw_int^2) +mhw_int + I(mhw_int^2) + mcw_dur + mhw_dur + #+ sst + I(sst^2)
            log(mangrove_area)
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & (abs(mcn$ntl_change)>0)),])

           summary(model_area)
           hist(mcn$ntl_change)

        

        glimpse(mcn)
        stargazer(model_area,type="html",out="area_reg.html")
        hist(mcn$spei)

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"temp","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"sst","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"log_ntl","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1% increase in NTL\non change in patch size")+xlab("NTL intensity")
        #ggsave("Figures/NTL_intensity_area.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"mhw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#c7457c")+
        #scale_fill_manual(values=c("#c7457c","#2b9089")) #"#c7457c","#2b9089"
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MHW\non change in patch size")+xlab("MHW intensity")
        #ggsave("Figures/MHW_intensity_area.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"mcw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#2b9089")+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MCW\non change in patch size")+xlab("MCW intensity")
        #ggsave("Figures/MCW_intensity_area.png")

#### Model mangrove_area ~ ntl + mhw  (end)

#### Model mangrove_area ~ gdp + mhw  (start)
        ggplot(mcn)+
        geom_point(aes(x=annual_area_change,y=log(Population_Count_50km)))
        mcn$logGDP50km <- log(mcn$Mean_GDP_50km)
        mcn$logPop50km <- log(mcn$Population_Count_50km)
        mcn$Gdppc <- mcn$Mean_GDP_50km/mcn$Population_Count_50km

        model_ntl_gdppop <- felm(log(ntl)~ logGDP50km+logPop50km
                        #logPop50km
                        |year+countrycode|0|countrycode,#data=mcn)
        data=mcn[which(mcn$ntl>0 & mcn$Mean_GDP_50km>0),])
        
        summary(model_ntl_gdppop)

        model_area2 <- felm(annual_area_change~
                        #Gdppc + I(Gdppc^2)+
                        logGDP50km + I(logGDP50km^2)+
                        #logPop50km + I(logPop50km^2)+
                        #Mean_GDP_50km + I(Mean_GDP_50km^2)+
                        #Population_Count_50km + I(Population_Count_50km^2)+
                        #spei+ 
                        #temp+I(temp^2)+
                        Mean_Precipitation+I(Mean_Precipitation^2)+
                        mcw_int+I(mcw_int^2)+mhw_int+I(mhw_int^2)+
                        mhw_dur+
                        mcw_dur+
                        #sst+I(sst^2)+
                        #np+
                        log(mangrove_area)
                        #annual_area_change+
                        
                        #spei
                        #I(spei^2)+
                        #ntl_change+
        |year+gridcell_id|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & mcn$Mean_GDP_50km>0),])
        data=mcn[which(mcn$mangrove_area>0 & mcn$Mean_GDP_50km>0),])
        #data=mcn[which(mcn$ntl>0),])
        summary(model_area2)

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$Mean_GDP_50km>0),],model_area2,"Mean_GDP_50km","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1% increase in NTL\non change in patch size")+xlab("NTL intensity")
        #ggsave("Figures/NTL_intensity_area.png")

####  Model mangrove_area ~ gdp + mhw (end)

#### Model mangrove_area ~ ntl + mhw  (start)


        model_area_spei <- felm(annual_area_change~
                                log_ntl + I(log_ntl^2)+
                                log(mangrove_area)+
                                spei + lag(spei)
                                
                |gridcell_id+year|0|countrycode,
                data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
                summary(model_area_spei)

                mcn$mhw_INT <- mcn$mhw_int
                mcn$mhw_INT[is.na(mcn$mhw_int)] <- 0
                mcn$mcw_INT <- mcn$mcw_int
                mcn$mcw_INT[is.na(mcn$mcw_int)] <- 0
                class(mcn$mhw)
                mcn$mhw <- as.factor(mcn$mhw)
                mcn$mcw <- as.factor(mcn$mcw)
                model_area_temp <- felm(annual_area_change~
                                log_ntl + I(log_ntl^2)+
                                temp+I(temp^2)+ 
                                #sst+I(sst^2)+ 
                                log(mangrove_area)+
                                Mean_Precipitation+#mhw+mcw+
                                I(Mean_Precipitation^2)#+
                                #mhw:mhw_INT+mhw:I(mhw_INT^2)+mcw:mcw_INT+mcw:I(mcw_INT^2)
                                
                |gridcell_id+year|0|countrycode,
                data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
                #data=mcn[which(mcn$mangrove_area>0),])
                summary(model_area_temp)
                
                max(mcn$year,na.rm=TRUE)
                min(mcn$year[which(!is.na(mcn$mhw_int_anom))],na.rm=TRUE)
                
                model_area_anom <- felm(annual_area_change~
                                log_ntl + I(log_ntl^2)+
                                log(mangrove_area)+
                                mhw_int_anom+
                                mcw_int_anom
                                #Mean_Precipitation+I(Mean_Precipitation^2)
                |gridcell_id+year|0|countrycode,
                data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
                summary(model_area_anom)

                
        stargazer(model_area,model_area_anom,model_area_spei,type="text")

        stargazer(model_area,model_area_anom,model_area_temp,model_area_spei,type="html",out="area_robustness.html")
                
        install.packages("modelsummary")
        library("modelsummary")
        model_list <- list(model_area,model_area_anom,model_area_temp,model_area_spei)
        modelsummary(model_list,stars=TRUE,output="reg_table.docx")
        getwd()        
        
        stargazer(model_area,model_area_anom,model_area_temp,model_area_spei,type="html",out="area_robustness.html")
        hist(mcn$spei)

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"temp","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"sst","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"log_ntl","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1% increase in NTL\non change in patch size")+xlab("NTL intensity")
        #ggsave("Figures/NTL_intensity_area.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"mhw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#c7457c")+
        #scale_fill_manual(values=c("#c7457c","#2b9089")) #"#c7457c","#2b9089"
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MHW\non change in patch size")+xlab("MHW intensity")
        #ggsave("Figures/MHW_intensity_area.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_area,"mcw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#2b9089")+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MCW\non change in patch size")+xlab("MCW intensity")
        #ggsave("Figures/MCW_intensity_area.png")

#### Model mangrove_area ~ ntl + mhw  (end)

#### Model number patch start
        model_np <- felm(annual_np_change~
                        #spei+ 
                        #temp+I(temp^2)+
                        Mean_Precipitation+I(Mean_Precipitation^2)+
                        sst+I(sst^2)+
                        #np+
                        #log(mangrove_area)+
                        annual_area_change+
                        #mhw:mhw_int+mhw:I(mhw_int^2)+
                        #mcw:mcw_int+mcw:I(mcw_int^2)+
                        #mhw:mhw_freq+
                        #mcw:mcw_freq+
                        #mhw:mhw_int_anom+
                        #mcw:mcw_int_anom+
                        #spei+
                        #I(spei^2)+
                        #ntl_change+
                        log(ntl) + I(log(ntl)^2)
        |gridcell_id+year|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0 & mcn$annual_np_change>0),])
        #data=mcn[which(mcn$ntl>0),])

        summary(model_np)
        stargazer(model_np,type="text")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0 & mcn$annual_np_change>0),],model_np,"Mean_Precipitation","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw()+
        ylab("Effect of 1mm per month wetter\non number of patches")+xlab("Mean monthly precipitation")
        ggsave("Figures/Effect_Rain_Npatches.png",dpi=300)
#### Model number patches end

#### Model area start
        ggplot(data=mcn, aes(x=temp,y=annual_area_change))+
        geom_point()

        model_area <- felm(annual_area_change~
                        #log(mangrove_area)+
                        #log_ntl+I(log_ntl^2)+
                        #sst + I(sst^2)+
                        #spei+I(spei^2)+
                        #Mean_Precipitation+I(Mean_Precipitation^2)+
                        temp#+I(temp^2)#+mhw:mhw_dur+mcw:mcw_dur+mhw:mhw_int_anom+mcw:mcw_int_anom
        #|gridcell_id+year|0|countrycode,
        |0|0|0,
        #data=mcn[which(mcn$log_ntl>0 &!is.na(mcn$temp) & !is.na(mcn$annual_area_change)),])
        data=mcn)
        #data=mcn[which(mcn$countrycode==cs[i]),])

        summary(model_area)


        ggplot(mcn,aes(y=(annual_area_change),x=temp,color=factor(year)))+
        geom_point()
        
        ggplot(mcn[which(mcn$countrycode==cs[i]),],aes(y=(annual_area_change),x=temp,color=factor(year)))+
        geom_point()

        cs <- unique(mcn$countrycode)
        ggplot(mcn[which(mcn$countrycode==cs[i]),],aes(color=factor(annual_area_change),y=temp,x=factor(year)))+
        geom_point()

        ggplot(mcn[which(mcn$countrycode==cs[i]),],aes(color=factor(annual_area_change),y=Mean_Precipitation,x=factor(year)))+
        geom_point()

        as.data.frame(mcn$spei[which(mcn$countrycode==cs[i])])

        model_np <- felm(annual_area_change~
                        #spei+ 
                        #temp+I(temp^2)+
                        #Mean_Precipitation+I(Mean_Precipitation^2)
                        sst+I(sst^2)
                        #np+
                        #log(mangrove_area)+
                        #annual_area_change+
                        #mhw:mhw_int+mhw:I(mhw_int^2)+
                        #mcw:mcw_int+mcw:I(mcw_int^2)+
                        #mhw:mhw_freq+
                        #mcw:mcw_freq+
                        #mhw:mhw_int_anom+
                        #mcw:mcw_int_anom+
                        #spei+
                        #I(spei^2)+
                        #ntl_change+
                        #log(ntl) + I(log(ntl)^2)
        |gridcell_id|0|countrycode,
        #data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0 & mcn$annual_np_change>0),])
        data=mcn[which( mcn$temp >0 & mcn$annual_area_change>0),])

        summary(model_np)
        stargazer(model_np,type="text")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0 & mcn$annual_np_change>0),],model_np,"Mean_Precipitation","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_hline(aes(yintercept=0),linetype="dashed")+
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw()+
        ylab("Effect of 1mm per month wetter\non number of patches")+xlab("Mean monthly precipitation")
        ggsave("Figures/Effect_Rain_Npatches.png",dpi=300)
#### Model area end

glimpse(mcn)
mcn$country <- mcn$sovereignt
mcn <- mcn[,which(names(mcn) %in% c("gridcell_id","country","year","ntl","sst","mhw_int","mhw_int_anom","mhw_dur","mhw_freq","mcw_int","mcw_int_anom","mcw_dur","mcw_freq","mangrove_area","sum_pixels_mangrove","sum_all_pixels","area","pafrac","annual_area_change","annual_pafrac_change","sovereignt","continent","economy","income_grp","subregion","ntl_change"))]


ggplot(mcn[which(mcn$year>2014),],aes(x=annual_area_change,y=annual_pafrac_change))+
geom_point(aes(color=factor(year)))+
coord_cartesian(xlim=c(-1,1))

ggplot(mcn[which(mcn$year>2014),],aes(x=log(mangrove_area),y=pafrac))+
geom_point(aes(color=factor(year)))

coord_cartesian(xlim=c(-1,1))


#Test stationarity
    mcn$unique_id <- factor(paste0(mcn$year,"-",mcn$gridcell_id))
    table(mcn$unique_id)[table(mcn$unique_id)!=1]

    mcn_pdata <- pdata.frame(mcn[which(names(mcn)%in%c("mangrove_area","sst","gridcell_id","year"))],index=c("gridcell_id","year"))
    View(mcn_pdata)

    # Check for NA values
    print(sum(is.na(mcn_pdata$mangrove_area)))

    # Check for infinite values
    print(sum(is.infinite(mcn_pdata$mangrove_area)))

    # Checking for NA values
    sum(is.na(mcn_pdata$mangrove_area))

    # If there are NA values, consider removing or filling them
    mcn_pdata <- na.omit(mcn_pdata)

    plmFit1 <- plm(log(mangrove_area) ~ sst,
            data = mcn_pdata, index = c("gridcell_id","year"))
    summary(plmFit1)

    print(sum(is.infinite(mcn_pdata$mangrove_area)))


    # If the minimum count is less than 2, you might want to filter out those panels
    mcn_pdata <- mcn_pdata %>% group_by(gridcell_id) %>% filter(n() > 2)

    # Convert the filtered data back to a pdata.frame
    mcn_pdata <- pdata.frame(mcn_pdata, index = c("gridcell_id", "year"))

    # Count the number of observations in each panel
    counts <- mcn_pdata %>% group_by(gridcell_id) %>% summarise(count = n())
    # Print the minimum count
    print(min(counts$count))

    purtest_pafrac <- purtest(mcn_pdata$pafrac, test = "ips",lags=1)

    # Count the number of unique values in each panel
    unique_counts <- mcn_pdata %>% 
    group_by(gridcell_id) %>% 
    summarise(unique_count = n_distinct(mangrove_area))

    # Print the minimum unique_count
    print(min(unique_counts$unique_count))

    # If the minimum unique_count is 1, you might want to filter out those panels
    mcn_pdata <- mcn_pdata %>% group_by(gridcell_id) %>% filter(n_distinct(mangrove_area) > 2)

    # Convert the filtered data back to a pdata.frame
    mcn_pdata <- pdata.frame(mcn_pdata, index = c("gridcell_id", "year"))
    purtest_mangrove <- purtest(mcn_pdata$mangrove_area, test = "Pm", lags = 1)
    summary(purtest_mangrove)
    purtest_mangrove[[1]]

    mcn$unique_id <- factor(paste0(mcn$year,"-",mcn$gridcell_id))
    table(mcn$unique_id)[table(mcn$unique_id)!=1]

    mcn_pdata <- pdata.frame(mcn[which(names(mcn)%in%c("pafrac","sst","gridcell_id","year"))],index=c("gridcell_id","year"))
    View(mcn_pdata)

    # Check for NA values
    print(sum(is.na(mcn_pdata$mangrove_area)))

    # Check for infinite values
    print(sum(is.infinite(mcn_pdata$mangrove_area)))

    # Checking for NA values
    sum(is.na(mcn_pdata$mangrove_area))

    # If there are NA values, consider removing or filling them
    mcn_pdata <- na.omit(mcn_pdata)

    plmFit1 <- plm(log(pafrac) ~ sst,
            data = mcn_pdata, index = c("gridcell_id","year"))
    summary(plmFit1)



    # If the minimum count is less than 2, you might want to filter out those panels
    mcn_pdata <- mcn_pdata %>% group_by(gridcell_id) %>% filter(n() > 2)

    # Convert the filtered data back to a pdata.frame
    mcn_pdata <- pdata.frame(mcn_pdata, index = c("gridcell_id", "year"))

    # Count the number of observations in each panel
    counts <- mcn_pdata %>% group_by(gridcell_id) %>% summarise(count = n())
    # Print the minimum count
    print(min(counts$count))

    purtest_pafrac <- purtest(mcn_pdata$pafrac, test = "ips",lags=1)

    # Count the number of unique values in each panel
    unique_counts <- mcn_pdata %>% 
    group_by(gridcell_id) %>% 
    summarise(unique_count = n_distinct(pafrac))

    # Print the minimum unique_count
    print(min(unique_counts$unique_count))

    # If the minimum unique_count is 1, you might want to filter out those panels
    mcn_pdata <- mcn_pdata %>% group_by(gridcell_id) %>% filter(n_distinct(pafrac) > 2)

    # Convert the filtered data back to a pdata.frame
    mcn_pdata <- pdata.frame(mcn_pdata, index = c("gridcell_id", "year"))
    purtest_mangrove <- purtest(mcn_pdata$pafrac, test = "Pm", lags = 1)
    summary(purtest_mangrove)
    purtest_mangrove[[1]]

#Test instationarity


## Model 1, area change
hist((mcn$mhw_int))
hist(log(-mcn$mcw_int_anom))
hist((mcn$mcw_dur))

sum(is.na(mcn$mhw_int))
sum(is.na(mcn$sst))
sum(is.na(mcn$year))
sum(!is.na(mcn$mhw))
table(mcn$mhw)
hist(mcn$ntl_change)

m1_area <- felm(annual_area_change~sst+ntl_change+pafrac+log(mangrove_area)
                    |year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0 & abs(mcn$annual_area_change)<1),])
summary(m1_area)

residuals_m1_area <- resid(m1_area)
plot(residuals_m1_area, ylab = "Residuals", main = "Residual Plot")
abline(h = 0, lty = 2)

m1_pafrac <- felm(annual_pafrac_change~sst+ntl_change#+mhw_dur+mcw_dur+mhw_int_anom+mcw_int_anom
                    |year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0 & abs(mcn$annual_area_change)<1),])
summary(m1_pafrac)

stargazer(m1_area,m1_pafrac,type="text")

glimpse(mcn)
table(mcn$mhw)

mcn$mhw <- mcn$mhw_int
mcn$mhw[which(is.na(mcn$mhw))] <- 0
mcn$mhw[which((mcn$mhw)>0)] <- 1
mcn$mhw_dur[mcn$mhw==0] <- 0
mcn$mhw_int_anom[mcn$mhw==0] <- 0
mcn$mhw_int[mcn$mhw==0] <- 0

mcn$mcw <- mcn$mcw_int
mcn$mcw[which(is.na(mcn$mcw))] <- 0
mcn$mcw[which((mcn$mcw)>0)] <- 1
mcn$mcw_int[mcn$mcw==0] <- 0
mcn$mcw_int_anom[mcn$mcw==0] <- 0
mcn$mcw_dur[mcn$mcw==0] <- 0

m2_area <- felm(annual_area_change~sst+ntl_change+pafrac+log(mangrove_area)+factor(mhw):mcw_dur+factor(mcw):mhw_dur+factor(mhw)
                    |year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0 & abs(mcn$annual_area_change)<1),])
summary(m2_area)


m1_area <- felm(annual_area_change~sst+I(sst^2)+
                    log(mhw_int_anom)+
                    #log(mcw_dur)+log(mhw_dur)+
                    log(ntl)+I(log(ntl)^2)
                    #ntl_change+I(ntl_change^2)
                    #pafrac
                    |year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0 & abs(mcn$annual_area_change)<1),])
summary(m1_area)
sq_estimate_sst <- sqest(mcn[which(mcn$ntl>0 & mcn$mangrove_area>0),],m1_area,"sst","m1")

ggplot(sq_estimate_sst)+
geom_line(aes(x=temp,y=gestimated))

m1_area2 <- felm(log(mangrove_area)~ntl_change+I(ntl_change^2)+sst+I(sst^2)+
                    #mhw_int_anom+mcw_int_anom+mcw_dur+mhw_dur+
                    #log(ntl)+I(log(ntl)^2)+
                    
                    pafrac
                    |year+country|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0),])
summary(m1_area2)

sq_estimate_sst <- sqest(mcn[which(mcn$ntl>0 & mcn$mangrove_area>0),],m1_area2,"ntl_change","m2")
ggplot(sq_estimate_sst)+
geom_line(aes(x=temp,y=gestimated))


m1_area <- felm(log(mangrove_area)~sst+I(sst^2)+
                    #mhw_int_anom+mcw_int_anom+mcw_dur+mhw_dur+
                    log(ntl)+I(log(ntl)^2)+
                    pafrac
                    |year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0),])
summary(m1_area)

m2_area <- felm(annual_area_change~ sst+I(sst^2)+
                                    mhw_int_anom+
                                    mcw_int_anom+
                                    mcw_dur+mhw_dur+
                                    ntl_change+
                                    #log(ntl)+
                                    log(mangrove_area)+lag(annual_area_change)+pafrac|year+gridcell_id|0|0,data=mcn)
summary(m2_area)

m1_pafrac <- felm(annual_pafrac_change~sst+I(sst^2)+
                                    mhw_int+
                                    #I(mhw_int^2)+
                                    mcw_int_anom+
                                    mcw_dur+mhw_dur+
                                    #log(ntl)+#I(log(ntl)^2)+
                                    ntl_change+#I(ntl_change^2)+
                                    log(mangrove_area)
                                    |year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0),])
summary(m1_pafrac)


m2_area <- felm(annual_area_change~ mcw_dur+mhw_dur|year+gridcell_id|0|0,data=mcn)
summary(m2_area)

m1_pafrac <- felm(log(pafrac)~#sst+I(sst^2)+
                                mhw_int_anom+mcw_int_anom+
                                mcw_dur+mhw_dur+
                                log(ntl)+I(log(ntl)^2)+
                                log(mangrove_area)|year+gridcell_id|0|0,data=mcn[which(mcn$ntl>0 & mcn$mangrove_area>0),])
summary(m1_pafrac)
