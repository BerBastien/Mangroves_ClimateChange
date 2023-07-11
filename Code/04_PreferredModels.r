 
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
            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x, p_value=p_value))
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


        
    #Models SST Hottest (start)
        model_area_ssthot <- felm(log(mangrove_area)~
            #sst + I(sst^2) + 
            #rich*sst + rich*I(sst^2) + 
            #rich:sst_hottest + rich:I(sst_hottest^2) + 
            #sst:sst_hottest + sst:I(sst_hottest^2) + 
            sst_hottest + I(sst_hottest^2) + 
            #sst:hot_location_alldata + I(sst^2):hot_location_alldata + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+I(log(GDP/Population))
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_ssthot)
        
        #sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst","Area")
        sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area")
        sq_estimate_preci_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"Mean_Precipitation","Area")


        model_holes_ssthot <- felm(log(holes/mangrove_area)~
            #sst + I(sst^2)+
            sst_hottest + I(sst_hottest^2) + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+ I(log(GDP/Population))
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),])
        summary( model_holes_ssthot)
        
        #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst","Gaps")
        sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")
        sq_estimate_preci_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Precipitation","Gaps")

        model_np_ssthot <- felm(log(np/mangrove_area)~
            #sst + I(sst^2)+
            sst_hottest + I(sst_hottest^2) + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc + I(log(GDP/Population)) 
            |gridcell_id + year+ countrycode|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),])
        summary(model_np_ssthot)
        
        sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_preci_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"Mean_Precipitation","Patches")

        Models_sst_hot <- rbind(sq_estimate_sst_area,sq_estimate_sst_holes,sq_estimate_sst_np)
        Models_preci_hot <- rbind(sq_estimate_preci_area,sq_estimate_preci_holes,sq_estimate_preci_np)
        glimpse(Models_sst_hot)
        glimpse(Models_preci_hot)

        
        pal_lapaz <- scico(15, palette = 'batlow')
        pal_lapaz <- pal_lapaz[c(2,7,12)]
        Models_sst_hot$significant <- "Not significant (p > 0.05)"
        Models_sst_hot$significant[which(Models_sst_hot$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_ssthot_plot <- ggplot(Models_sst_hot)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase on\n mangrove characteristics")+xlab("Monthly Average SST in the Hottest Month (C)")+
        guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
        scale_color_manual(values =c(pal_lapaz[1],"indianred")) +
        scale_fill_manual(values = c(pal_lapaz[1],"indianred"))#+
        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
        Models_ssthot_plot

        Models_preci_hot$significant <- "Not significant (p > 0.05)"
        Models_preci_hot$significant[which(Models_preci_hot$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_preci_hot_plot <- ggplot(Models_preci_hot)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase on\n mangrove characteristics")+xlab("Monthly Average SST in the Hottest Month (C)")+
        guides(fill = guide_legend(title = "Sum of Coefficients"),color = guide_legend(title = "Sum of Coefficients"))+
        scale_color_manual(values =c(pal_lapaz[1],"indianred")) +
        scale_fill_manual(values = c(pal_lapaz[1],"indianred"))#+
        #theme(strip.background =element_rect(fill=c(pal_roma[9])))
        Models_preci_hot_plot


        
        gdp_coefs_ssthot <- plot_coefs(model_area_ssthot,model_np_ssthot,model_holes_ssthot, ci_level = 0.90,
            omit.coefs = c("logGDPpc*rich",
                #"I(log(GDP/Population))",
                "log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)",
            "sst_hottest" , "I(sst_hottest^2)", "temp","I(temp^2)",
            "I(Mean_Precipitation^2)","Mean_Precipitation"),
            model.names=c("Area","Patches","Gaps"))
    
    #Models SST Hottest (end)

    #Models SSTs (start)
        model_area_ssthot <- felm(log(mangrove_area)~
            sst + I(sst^2) + 
            #rich*sst + rich*I(sst^2) + 
            #rich:sst_hottest + rich:I(sst_hottest^2) + 
            #sst:sst_hottest + sst:I(sst_hottest^2) + 
            #sst_hottest + I(sst_hottest^2) + 
            #sst:hot_location_alldata + I(sst^2):hot_location_alldata + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+I(log(GDP/Population))
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_ssthot)


        sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst","Area")
        sq_estimate_preci_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"Mean_Precipitation","Area")
        #sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area")


        ggplot(mcn,aes(x=log(holes/mangrove_area),y=sst))+geom_point()
        ggplot(mcn,aes(x=log(holes/mangrove_area),y=log(GDP/Population)))+geom_point()
        glimpse(mcn)
        
        model_holes_ssthot <- felm(log(holes/mangrove_area)~
            sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            #temp + I(temp^2)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+ I(log(GDP/Population))
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc) ),])
        summary( model_holes_ssthot)
        
        sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst","Gaps")
        sq_estimate_preci_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"Mean_Precipitation","Gaps")
        #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")

        model_np_ssthot <- felm(log(np/mangrove_area)~
            sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+ I(log(GDP/Population))
            |gridcell_id + year+ countrycode|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),])
        summary(model_np_ssthot)
        
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst","Patches")
        sq_estimate_preci_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"Mean_Precipitation","Patches")

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
            omit.coefs = c("logGDPpc*rich",
                #"I(log(GDP/Population))",
                "log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)",
            "sst_hottest" , "I(sst_hottest^2)", "temp","I(temp^2)",
            "I(Mean_Precipitation^2)","Mean_Precipitation"),
            model.names=c("Area","Patches","Gaps"))

    #Models SSTs (start)    

    #Plot Dimensions (SST and SST Hottest)
        
    
        Models_preci_sst$significant <- "Not significant (p > 0.05)"
        Models_preci_sst$significant[which(Models_preci_sst$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_preci_sst_plot <- ggplot(Models_preci_sst)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase on \nmangrove characteristics")+xlab("Mean Annual SST (C)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c(pal_lapaz[1],"indianred")) +
        scale_fill_manual(values = c(pal_lapaz[1],"indianred")) 
        #theme(strip.background =element_rect(fill=c(pal_roma[8])))
        #theme(strip.text = element_text(colour = 'white'))
        Models_preci_sst_plot
        
        Models_sst$significant <- "Not significant (p > 0.05)"
        Models_sst$significant[which(Models_sst$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_sst_plot <- ggplot(Models_sst)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase on \nmangrove characteristics")+xlab("Mean Annual SST (C)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c(pal_lapaz[1],"indianred")) +
        scale_fill_manual(values = c(pal_lapaz[1],"indianred")) 
        #theme(strip.background =element_rect(fill=c(pal_roma[8])))
        #theme(strip.text = element_text(colour = 'white'))
        Models_sst_plot

        library("ggpubr")
        ggarrange(Models_sst_plot,Models_ssthot_plot,common.legend=T,nrow=2,legend="bottom")
        #ggsave("Figures/Draft/sst_and_sstHOT.png",dpi=600)

        ggarrange(Models_preci_sst_plot,Models_preci_hot_plot,common.legend=T,nrow=2,legend="bottom")
    #Plot Dimensions (SST and SST Hottest)

    #Models Temp (start)
        model_area_t <- felm(log(mangrove_area)~
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
            factor(rich)*logGDPpc+I(log(GDP/Population))
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),])
        summary(model_area_t)


        sq_estimate_t_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_t,"temp","Area")
        sq_estimate_preci_area_t <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_t,"Mean_Precipitation","Area")
        #sq_estimate_sst_area <- sqest(mcn[which(mcn$mangrove_area>0 & is.finite(mcn$logGDPpc)),],model_area_ssthot,"sst_hottest","Area")


        
        
        model_holes_t <- felm(log(holes/mangrove_area)~
            #sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            #sst:sst_hottest_mean0020 + I(sst^2):sst_hottest_mean0020 + 
            temp + I(temp^2)+
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc+ I(log(GDP/Population))
            |gridcell_id + year+ countrycode+R5|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc) ),])
        summary( model_holes_t)
        
        sq_estimate_t_holes <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_t,"temp","Gaps")
        sq_estimate_preci_holes_t <- sqest(mcn[which(mcn$mangrove_area>0 &mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_t,"Mean_Precipitation","Gaps")
        #sq_estimate_sst_holes <- sqest(mcn[which(mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_holes_ssthot,"sst_hottest","Gaps")

        ggplot(mcn,aes(x=year,y=(np/mangrove_area)))+geom_point()+geom_smooth()+ylim(c(0,10000))

            model_np_t <- felm(log(np/mangrove_area)~
            temp+I(temp^2)+
            #sst + I(sst^2)+
            #sst_hottest + I(sst_hottest^2) + 
            I(Mean_Precipitation^2) +
            Mean_Precipitation +
            factor(rich)*logGDPpc
            |gridcell_id + year+ countrycode|0|gridcell_id,
        data=mcn[which(mcn$mangrove_area>0 & mcn$np>0 & is.finite(mcn$logGDPpc)),])
        summary(model_np_t)
        
        #sq_estimate_sst_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_ssthot,"sst_hottest","Patches")
        sq_estimate_t_np <- sqest(mcn[which(mcn$np>0 & is.finite(mcn$logGDPpc)),],model_np_t,"temp","Patches")
        sq_estimate_preci_np_t <- sqest(mcn[which(mcn$mangrove_area>0 &mcn$holes>0 & is.finite(mcn$logGDPpc)),],model_np_t,"Mean_Precipitation","Patches")

        gdp_coefs_t <- plot_coefs(model_area_t,model_np_t,model_holes_t, ci_level = 0.90,
            omit.coefs = c("logGDPpc*rich",
                #"I(log(GDP/Population))",
                "log(mangrove_area)","sst","I(sst^2)","Mean_Salinity","I(Mean_Salinity^2)","I(log(ntl)^2)","log(ntl)",
            "sst_hottest" , "I(sst_hottest^2)", "temp","I(temp^2)",
            "I(Mean_Precipitation^2)","Mean_Precipitation"),
            model.names=c("Area","Patches","Gaps"))
        
        Models_t <- rbind(sq_estimate_t_area,sq_estimate_t_holes,sq_estimate_t_np)
        Models_preci_t <- rbind(sq_estimate_preci_area_t,sq_estimate_preci_holes_t,sq_estimate_preci_np_t)
        #glimpse(Models_sst)

        Models_t_plot <- ggplot(Models_t)+
        geom_line(aes(x=temp,y=gestimated,color=exp)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=exp),alpha=0.2)+
        theme_bw()  + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase \non mangrove characteristics")+xlab("Mean Monthly Precipitation (mm)")+
        guides(fill = guide_legend(title = "Dimension"),color = guide_legend(title = "Dimension"))
        Models_t_plot

    #Models Temp (start)    

        Models_preci_t$significant <- "Not significant (p > 0.05)"
        Models_preci_t$significant[which(Models_preci_t$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_preci_t_plot <- ggplot(Models_preci_t)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1mm precipitation increase on \nmangrove characteristics")+xlab("Monthly Mean Precipitation (mm)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c(pal_lapaz[1],"indianred")) +
        scale_fill_manual(values = c(pal_lapaz[1],"indianred")) 
        
        Models_t$significant <- "Not significant (p > 0.05)"
        Models_t$significant[which(Models_t$p_value<0.05)] <- "Significant (p < 0.05)"

        Models_t_plot <- ggplot(Models_t)+
        geom_line(aes(x=temp,y=gestimated,color=factor(significant))) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2,fill=factor(significant)),alpha=0.2)+
        theme_bw() + facet_wrap(~exp,ncol=3)+ 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1C increase on \nmangrove characteristics")+xlab("Mean Annual Air Temperature (C)") +
        guides(fill = guide_legend(title = "Sum of coefficients"),color = guide_legend(title = "Sum of coefficients")) +
        guides(fill = guide_legend(title = "Sum of coefficients"), color = guide_legend(title = "Sum of coefficients")) +
        scale_color_manual(values = c(pal_lapaz[1],"indianred")) +
        scale_fill_manual(values = c(pal_lapaz[1],"indianred")) 
        #theme(strip.background =element_rect(fill=c(pal_roma[8])))
        #theme(strip.text = element_text(colour = 'white'))

        library("ggpubr")
        ggarrange(Models_sst_plot,Models_ssthot_plot,Models_t_plot,common.legend=T,nrow=3,legend="bottom")
        #ggsave("Figures/Draft/sst_and_sstHOT.png",dpi=600)

        
        all_preci_plots <- ggarrange(Models_preci_sst_plot,Models_preci_hot_plot,Models_preci_t_plot,common.legend=T,nrow=3,legend="bottom")
        all_temp_plots <- ggarrange(Models_sst_plot,Models_ssthot_plot,Models_t_plot,common.legend=T,nrow=3,legend="bottom")
        all_all_plots <- ggarrange(all_temp_plots,all_preci_plots,common.legend=T,ncol=2)
        all_all_plots

        ggarrange(gdp_coefs_sst,gdp_coefs_ssthot,gdp_coefs_t,nrow=1,ncol=3,common.legend=T)
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
