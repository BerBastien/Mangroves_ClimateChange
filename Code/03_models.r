
mcn <- read.csv("C:/Users/basti/Box/Data/Oceans/Mangroves/filtered_mcn.csv")
glimpse(mcn)
setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")

## Libraries
library(viridis)
library("ggridges")
library("lfe")
library("ggridges")
library("stargazer")

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


#### Model price start

        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        total_ma <- aggregate(mangrove_area ~ countrycode + year, data=mcn,FUN="sum")
        names(total_ma)[3] <- "total_mangrove_area"
        #mcn <- merge(mcn,total_ma,by=c("countrycode","year"),all=TRUE)
        #glimpse(mcn)
        mcn_country <- total_ma

        total_ma <- aggregate(np ~ countrycode + year, data=mcn,FUN="sum")
        names(total_ma)[3] <- "total_country_np"
        mcn_country <- merge(mcn_country,total_ma,by=c("countrycode","year"),all=TRUE)
        glimpse(mcn_country)
        total_ma <- aggregate(patch_size ~ countrycode + year, data=mcn,FUN="mean")
        names(total_ma)[3] <- "mean_country_patchsize"
        mcn_country <- merge(mcn_country,total_ma,by=c("countrycode","year"),all=TRUE)
        glimpse(mcn)
        total_ma <- aggregate(ntl ~ countrycode + year, data=mcn,FUN="sum")
        names(total_ma)[3] <- "ntl_country_sum"
        mcn_country <- merge(mcn_country,total_ma,by=c("countrycode","year"),all=TRUE)
        glimpse(mcn_country)
        total_ma <- aggregate(Nmangroves ~ countrycode + year, data=mcn,FUN="sum")
        names(total_ma)[3] <- "Nmangroves"
        mcn_country <- merge(mcn_country,total_ma,by=c("countrycode","year"),all=TRUE)
        glimpse(mcn_country)
        total_ma <- aggregate(GDP ~ countrycode + year, data=mcn,FUN="mean")
        names(total_ma)[3] <- "GDP"
        mcn_country <- merge(mcn_country,total_ma,by=c("countrycode","year"),all=TRUE)
        glimpse(mcn_country)
        total_ma <- aggregate(Population ~ countrycode + year, data=mcn,FUN="sum")
        names(total_ma)[3] <- "Population"
        mcn_country <- merge(mcn_country,total_ma,by=c("countrycode","year"),all=TRUE)
        glimpse(mcn_country)

        mcn_country$log_income <- log(mcn_country$GDP / mcn_country$Population)
        hist(mcn_country$log_income)

        mcn_country$income <- (mcn_country$GDP / mcn_country$Population)
        hist(mcn_country$income)

        mcn_country$log_esval <- log(mcn_country$Nmangroves*0.03/mcn_country$total_mangrove_area)
        hist(mcn_country$log_esval)

        hist((mcn_country$mean_country_patchsize))
        hist(log(mcn_country$total_country_np))

        model_Nm <- felm(log_esval~
                log_income+
                #I(GDP/Population)+
                #ntl+
                log(ntl_country_sum)+
                log(total_mangrove_area)+
                #log(total_country_np) +
                #log(total_mangrove_area)*total_country_np + 
                log(mean_country_patchsize)#+year:countrycode+I(year^2):countrycode
                #np*log(mangrove_area)+
                #np+log(mangrove_area)+
                #patch_size+
                #log(Population)#+year
                #|year+gridcell_id|0|countrycode,
                #data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
                |countrycode|0|countrycode,
                data=mcn_country[which(mcn_country$total_mangrove_area>0 & mcn_country$Nmangroves>0),])
        summary(model_Nm)
        plot_coefs(model_Nm)
        ggsave("Figures/model_coefs.png")

        stargazer(model_Nm,type="html",out="model_Nm.html")
        glimpse(mcn)



#### Model price end

##Model Area 1
        model_area <- felm(annual_area_change ~
                                log_ntl + I(log_ntl^2)+mcw_int+I(mcw_int^2)+
                                mhw_int+I(mhw_int^2)+
                              
                        #spei+ 
                        #temp+I(temp^2)+
                        #sst+I(sst^2)+
                        patch_size+
                        
                        #annual_area_change+
                        mhw_dur+mcw_dur+
                        #mhw_freq+mcw_freq+
                        #mhw:mhw_int_anom+
                        #mcw:mcw_int_anom+
                        Mean_Precipitation+
                        I(Mean_Precipitation^2)
                        #spei
                        #I(spei^2)+
                        #ntl_change+
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
        summary(model_area)
        
        modelsummary(model_area,stars=TRUE,output="reg_table_area.docx")


##Model Area 1 (end)

#### Model patch_size ~ ntl + mhw  (start)


        mcn <- read.csv("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\mangrove_alldata.csv")
        mcn <- mcn %>%
        group_by(gridcell_id) %>%
        filter(sum(which(abs(annual_patchsize_change)>1))<1)

        table(mcn$mhw)
        glimpse(mcn)
        mcn$temp <- mcn$mean_temp + mcn$temp_anom
        mcn$log_ntl <- log(mcn$ntl)

        ggplot(data=mcn,aes(y=annual_patchsize_change,x=temp))+
        geom_point()

        model_patchsize <- felm(annual_patchsize_change~
                                log_ntl + I(log_ntl^2)+mcw_int+I(mcw_int^2)+
                                mhw_int+I(mhw_int^2)+
                              
                        #spei+ 
                        #temp+I(temp^2)+
                        #sst+I(sst^2)+
                        #np+
                        log(mangrove_area)+
                        #annual_area_change+
                        mhw_dur+mcw_dur+
                        #mhw_freq+mcw_freq+
                        #mhw:mhw_int_anom+
                        #mcw:mcw_int_anom+
                        Mean_Precipitation+
                        I(Mean_Precipitation^2)
                        #spei
                        #I(spei^2)+
                        #ntl_change+
        |gridcell_id+year|0|countrycode,
        data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
        
        #data=mcn[which(mcn$ntl>0),])

        summary(model_patchsize)
        stargazer(model_patchsize,type="html",out="PatchSize_reg.html")
        hist(mcn$spei)

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"temp","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"sst","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"log_ntl","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1% increase in NTL\non change in patch size")+xlab("NTL intensity")
        #ggsave("Figures/NTL_intensity_PatchSize.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"mhw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#c7457c")+
        #scale_fill_manual(values=c("#c7457c","#2b9089")) #"#c7457c","#2b9089"
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MHW\non change in patch size")+xlab("MHW intensity")
        #ggsave("Figures/MHW_intensity_PatchSize.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"mcw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#2b9089")+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MCW\non change in patch size")+xlab("MCW intensity")
        #ggsave("Figures/MCW_intensity_PatchSize.png")

#### Model patch_size ~ ntl + mhw  (end)


#### Model patch_size ~ gdp + mhw  (start)
        ggplot(mcn)+
        geom_point(aes(x=annual_patchsize_change,y=log(Population_Count_50km)))
        mcn$logGDP50km <- log(mcn$Mean_GDP_50km)
        mcn$logPop50km <- log(mcn$Population_Count_50km)
        mcn$Gdppc <- mcn$Mean_GDP_50km/mcn$Population_Count_50km

        model_ntl_gdppop <- felm(log(ntl)~ logGDP50km+logPop50km
                        #logPop50km
                        |year+countrycode|0|countrycode,#data=mcn)
        data=mcn[which(mcn$ntl>0 & mcn$Mean_GDP_50km>0),])
        
        summary(model_ntl_gdppop)

        model_patchsize2 <- felm(annual_patchsize_change~
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
        summary(model_patchsize2)

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$Mean_GDP_50km>0),],model_patchsize2,"Mean_GDP_50km","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1% increase in NTL\non change in patch size")+xlab("NTL intensity")
        #ggsave("Figures/NTL_intensity_PatchSize.png")

####  Model patch_size ~ gdp + mhw (end)

#### Model patch_size ~ ntl + mhw  (start)


        model_patchsize_spei <- felm(annual_patchsize_change~
                                log_ntl + I(log_ntl^2)+
                                log(mangrove_area)+
                                spei + lag(spei)
                                
                |gridcell_id+year|0|countrycode,
                data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
                summary(model_patchsize_spei)

                mcn$mhw_INT <- mcn$mhw_int
                mcn$mhw_INT[is.na(mcn$mhw_int)] <- 0
                mcn$mcw_INT <- mcn$mcw_int
                mcn$mcw_INT[is.na(mcn$mcw_int)] <- 0
                class(mcn$mhw)
                mcn$mhw <- as.factor(mcn$mhw)
                mcn$mcw <- as.factor(mcn$mcw)
                model_patchsize_temp <- felm(annual_patchsize_change~
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
                summary(model_patchsize_temp)
                
                max(mcn$year,na.rm=TRUE)
                min(mcn$year[which(!is.na(mcn$mhw_int_anom))],na.rm=TRUE)
                
                model_patchsize_anom <- felm(annual_patchsize_change~
                                log_ntl + I(log_ntl^2)+
                                log(mangrove_area)+
                                mhw_int_anom+
                                mcw_int_anom
                                #Mean_Precipitation+I(Mean_Precipitation^2)
                |gridcell_id+year|0|countrycode,
                data=mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),])
                summary(model_patchsize_anom)

                
        stargazer(model_patchsize,model_patchsize_anom,model_patchsize_spei,type="text")

        stargazer(model_patchsize,model_patchsize_anom,model_patchsize_temp,model_patchsize_spei,type="html",out="PatchSize_robustness.html")
                
        install.packages("modelsummary")
        library("modelsummary")
        model_list <- list(model_patchsize,model_patchsize_anom,model_patchsize_temp,model_patchsize_spei)
        modelsummary(model_list,stars=TRUE,output="reg_table.docx")
        getwd()        
        
        stargazer(model_patchsize,model_patchsize_anom,model_patchsize_temp,model_patchsize_spei,type="html",out="PatchSize_robustness.html")
        hist(mcn$spei)

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"temp","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"sst","m1")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"log_ntl","m1")

        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2)+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1% increase in NTL\non change in patch size")+xlab("NTL intensity")
        #ggsave("Figures/NTL_intensity_PatchSize.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"mhw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#c7457c")+
        #scale_fill_manual(values=c("#c7457c","#2b9089")) #"#c7457c","#2b9089"
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MHW\non change in patch size")+xlab("MHW intensity")
        #ggsave("Figures/MHW_intensity_PatchSize.png")

        sq_estimate_sst <- sqest(mcn[which(mcn$mangrove_area>0 & mcn$ntl>0),],model_patchsize,"mcw_int","m1")


        ggplot(sq_estimate_sst)+
        geom_line(aes(x=temp,y=gestimated)) +
        geom_ribbon(aes(x=temp,ymin=ci1,ymax=ci2),alpha=0.2,fill="#2b9089")+
        theme_bw() + 
        geom_hline(aes(yintercept=0),linetype="dashed")+
        ylab("Effect of 1 Degree of warming during MCW\non change in patch size")+xlab("MCW intensity")
        #ggsave("Figures/MCW_intensity_PatchSize.png")

#### Model patch_size ~ ntl + mhw  (end)


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
