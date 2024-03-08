
mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata_15sept2023_buffers.csv")
mcn <- mcn %>% filter(mean_mangrove_area>1 & 
                                is.finite(lag_gap_density_avg) &
                                is.finite(logGDPpc)# & 
                                )
mcn$lag_gap_density <- mcn$lag_gap_density_avg
        
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
            formula <- as.formula(paste0("log(mangrove_area) ~ cold + hot+ preci+I(preci^2)+                                   
                                            logGDPpc + I(logGDPpc^2)+
                                            lag_gap_density + I((lag_gap_density)^2) + 
                                            year:countrycode|gridcell_id  +  year |0|gridcell_id"))
            # formula <- as.formula(paste0("log(mangrove_area) ~ cold + ", paste(ind_vars, collapse = " + "), "+ hot+ preci+I(preci^2)+                                   
            #                                 logGDPpc + I(logGDPpc^2)+
            #                                 lag_gap_density + I((lag_gap_density)^2) + 
            #                                 year:countrycode|gridcell_id  +  year |0|gridcell_id"))
            model <- felm(formula, data = mcn,weights=log(mcn$mangrove_area+1))
            return(model)
        }

        

        run_regression_h <- function(mcn, ind_vars) {
            formula <- as.formula(paste0("log(mangrove_area) ~  hot + preci+I(preci^2)+                                   
                                            logGDPpc + I(logGDPpc^2)+
                                            lag_gap_density + I((lag_gap_density)^2) + 
                                            year:countrycode|gridcell_id  +  year |0|gridcell_id"))
            model <- felm(formula, data = mcn,weights=log(mcn$mangrove_area+1))
            return(model)
        }

        run_regression_c <- function(mcn, ind_vars) {
            #formula <- as.formula(paste0("log(mangrove_area) ~  cold + preci+I(preci^2)+                                   
            #                                logGDPpc + I(logGDPpc^2)+
            #                                lag_gap_density + I((lag_gap_density)^2) + 
            #                                year:countrycode|gridcell_id  +  year |0|gridcell_id"))
            formula <- as.formula(paste0("log(mangrove_area) ~  cold |gridcell_id  +  year |0|gridcell_id"))
            model <- felm(formula, data = mcn,weights=log(mcn$mangrove_area+1))
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
            print(paste0("i-",j,n))
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
        
        #results$co <- results$Coefficient
        
        glimpse(result)
        threshold_hot_plot <- ggplot(results[which(results$Variable=="hot"),],aes(x=Threshold,y=Coefficient),col=pal_lapaz[13]) + 
        geom_point() + theme_bw()+
        geom_text(data=results[which(results$Variable=="hot" & results$P_value<0.1),],
            aes(x=Threshold-0.5,y=(Coefficient-SE*1.64)-0.0005,label=co),angle=90)+
        geom_text(data=results[which(results$Variable=="hot" & results$P_value<0.1 & results$Threshold==34),],
            aes(x=Threshold-0.5,y=(Coefficient),label=co),angle=90)+
        geom_errorbar(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[13])+
        #geom_ribbon(aes(x=Threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64),alpha=0.2))+
        #xlim(c(17,35)) + ylim(c(-0.015,0.001)) +
        #coord_cartesian(xlim = c(17,35),ylim = c(-0.015,0.001))
        coord_cartesian(xlim = c(17,35),ylim = c(-0.005,0.001))+
        geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
            xlab("SST-bin threshold")+
            ylab("Effect of one additional day \nabove the threshold")
        threshold_hot_plot 


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
            aes(x=Threshold-0.3,y=(Coefficient-SE*1.64)-0.0003,label=co),angle=90)+
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
        threshold_cold_plot


        ggarrange(threshold_hot_plot,threshold_cold_plot,ncol=1)


        results <- data.frame()
        n <- 1
        for (i in 2:15) {
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

       

        
        results_f <- results %>% filter( Variable %in% c("cold","hot"))
        

        library(stringr)
        library(purrr)

        results_f$hot_threshold <- results_f$Hot_bins %>%
        str_extract_all(pattern = "b\\d+_") %>%
        map_chr(~ .x[1]) %>%
        str_extract("\\d+") %>% as.numeric()

        results_f$cold_threshold <- results_f$Cold_bins %>%
        str_extract_all(pattern = "_\\d+C") %>%
         map_chr(~ .x[length(.x)]) %>%
        str_extract("\\d+") %>% as.numeric()
        
        glimpse(results_f)
        ggplot(results_f %>% filter(Variable=="cold"))+
        geom_point(aes(x=cold_threshold,y=Coefficient))+
        geom_errorbar(aes(x=cold_threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[4])


        ggplot(results_f %>% filter(Variable=="hot"))+
        geom_point(aes(x=hot_threshold,y=Coefficient))+
        geom_errorbar(aes(x=hot_threshold,ymin=(Coefficient-SE*1.64),ymax=(Coefficient+SE*1.64)),col=pal_lapaz[1])

        results_f$significant <- 0
        results_f$significant[which(results_f$P_value<0.01)] <- 1
        results_f$stars <- ""
        results_f$stars[which(results_f$P_value<0.1)]<-"*"
        results_f$stars[which(results_f$P_value<0.05)]<-"**"
        results_f$stars[which(results_f$P_value<0.01)]<-"***"
        results_f$Coeff_sci <- formatC(100*results_f$Coefficient, format="e", digits=2, flag="-")
        results_f$co <- paste0(substr(as.character(results_f$Coeff_sci),start=1,stop=9),results_f$stars)


        results_f_hot <- results_f %>% select(hot_threshold) %>%
                        group_by(hot_threshold) %>%
                        mutate(count_hot_threshold = length(hot_threshold)) %>%
                        ungroup()
        results_f_hot$count_hot_threshold
        group_by(hot_threshold) %>%as.data.frame()%>%
        mutate(count_hot_threshold = n()) %>%
        ungroup() %>% as.data.frame()%>%
        group_by(cold_threshold) %>%
        mutate(count_cold_threshold = n()) %>%
        ungroup() %>% as.data.frame()

        glimpse(results_f_hot)

        glimpse(results_f)
        plot_effects <- ggplot()+
        geom_point(data=results_f %>% filter(Variable=="hot" & hot_threshold<34),aes(x=hot_threshold,y=100*Coefficient))+
        geom_errorbar(data=results_f %>% filter(Variable=="hot"& hot_threshold<34),aes(x=hot_threshold,ymin=(100*Coefficient-100*SE*1.64),ymax=(100*Coefficient+100*SE*1.64)),col=pal_lapaz[1])+
        geom_text(data=results_f %>% filter(Variable=="hot"& hot_threshold==31) %>% slice(1),aes(x=hot_threshold,y=(100*Coefficient-100*SE*1.64*3.5),label=co),angle=90)+
        geom_point(data=results_f %>% filter(Variable=="cold" & cold_threshold>12) ,aes(x=cold_threshold,y=100*Coefficient))+
        geom_errorbar(data=results_f %>% filter(Variable=="cold"& cold_threshold>12),aes(x=cold_threshold,ymin=(100*Coefficient-100*SE*1.64),ymax=(100*Coefficient+100*SE*1.64)),col=pal_lapaz[4])+
        geom_text(data=results_f %>% filter(Variable=="cold"& cold_threshold==18) %>% slice(1),aes(x=cold_threshold,y=(100*Coefficient-100*SE*1.64*3),label=co),angle=90)+
        coord_cartesian(xlim=c(13,33)) + theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+ylab("Effect of One Additional Day\n Below or Above Threshold (percent points)")+xlab("Temperature Threshold")
        
        plot_effects
        save(plot_effects,file="Data/output/effects_threshold.Rds")
        ggsave("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Figures\\Draft2\\BinEffects.png",dpi=600)
        
#here         
        df_long <- df_f %>%
        gather(key = "bin", value = "days", b8_9C:b36_37C)
        glimpse(df_long)

        
        df_avg <- df_long %>%
        group_by(bin,extratropical) %>%
        summarize(average_days = mean(days, na.rm = TRUE))

        df_long %>% filter(bin %in% c("b8_9C")) %>%
        group_by(bin,extratropical) %>%
        summarize(average_days = mean(days, na.rm = TRUE))

        levels(factor(df_avg$bin))
        sorted_levels <- c(
        "b8_9C", "b9_10C", "b10_11C", "b11_12C", "b12_13C", "b13_14C", "b14_15C", 
        "b15_16C", "b16_17C", "b17_18C", "b18_19C", "b19_20C", "b20_21C", "b21_22C", 
        "b22_23C", "b23_24C", "b24_25C", "b25_26C", "b26_27C", "b27_28C", "b28_29C", 
        "b29_30C", "b30_31C", "b31_32C", "b32_33C", "b33_34C", "b34_35C", "b35_36C", 
        "b36_37C"
        )

        df_avg$bin <- factor(df_avg$bin, levels = sorted_levels)
        "%notin%" <- Negate("%in%")

        df_avg$label <- as.character(df_avg$bin)  # Convert the factor to character
        df_avg$label <- gsub("b", "", df_avg$label)  # Remove the 'b' prefix
        df_avg$label <- gsub("_", "-", df_avg$label)  # Replace underscore with a hyphen
        df_avg$label <- factor(df_avg$label, levels = c(
            "8-9C", "9-10C", "10-11C", "11-12C", "12-13C", "13-14C", "14-15C", "15-16C", 
            "16-17C", "17-18C", "18-19C", "19-20C", "20-21C", "21-22C", "22-23C", "23-24C", 
            "24-25C", "25-26C", "26-27C", "27-28C", "28-29C", "29-30C", "30-31C", "31-32C", 
            "32-33C", "33-34C", "34-35C", "35-36C", "36-37C"
        ))

        df_avg %>% as.data.frame()
        avg_days_lessthan_zero <- df_avg %>% group_by(bin) %>%
                                    summarize(average_days = mean(average_days, na.rm = TRUE)) %>% filter(average_days<1) %>% 
                                    mutate(bin = as.character(bin)) %>% select(bin) %>% as.data.frame()
        glimpse(avg_days_lessthan_zero)
        
        library(ggplot2)
        library(scico)

        plot_daysbin_tropics <- ggplot(df_avg %>% as.data.frame() %>% filter(bin %notin% avg_days_lessthan_zero$bin) , 
                                    aes(x = label, y = average_days/2)) +
        geom_bar(stat = "identity", aes(fill = factor(extratropical))) +
        labs(y = "Historical Average\nDayDistribution",x="") +
        scale_fill_scico_d(palette = "bam", aesthetics = "fill", name = "Latitude",
                        breaks = c(0, 1), labels = c("Tropical", "Extratropical")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.position = "bottom",
                legend.title.align = 0.5) +  # Center the legend title at the top
        guides(fill = guide_legend(reverse = TRUE, title.position = "top")) +
        theme_minimal()




        ggarrange(plot_effects,plot_daysbin_tropics,ncol=1,heights=c(2,1),legend="bottom",align="hv")
        ggsave("Figures/15Sept2023/Threshold_Models.png",dpi=600)

        getwd()


        library(dplyr)







        

