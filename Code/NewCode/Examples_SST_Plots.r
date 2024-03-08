install.packages("jsonlite")
library(jsonlite)
# If you've saved your JSON content in a file named "data.json":
data <- fromJSON("C:\\Users\\basti\\Box\\Data\\Oceans\\oisst2.1_world2_sst_day.json") #SST World 60S to 60N

# Now, `data` is a list with the JSON content
print(data)

# Unlist the data column
sst_values <- unlist(data$data)

# Create a month variable and repeat the name variable
day <- rep(1:366, times = nrow(data))
year <- rep(data$name, each = 366)

# Combine into desired dataframe
result <- data.frame(
  year = as.double(year),
  day = day,
  sst = sst_values
)


glimpse(result)
ggplot(result, aes(x=day,y=sst))+geom_line(aes(group=year))

library(lubridate)

# Create a date column using the year and day columns
result$date <- ymd(paste(result$year, "-01-01")) + days(result$day - 1)

# Calculate the number of days since January 1, 1981
result$days_since_start <- as.integer(difftime(result$date, ymd("1981-01-01"), units = "days"))

glimpse(result)

ggplot(result, aes(x=days_since_start,y=sst))+geom_line(aes(group=year))

sst_sub <- result %>% filter(year>1994 & year <2010)
ggplot(sst_sub, aes(x=days_since_start,y=sst))+geom_line(aes(group=year))

q90_sst <- quantile(sst_sub$sst,na.rm=T,c(0.9))
q10_sst <- quantile(sst_sub$sst,na.rm=T,c(0.1))
sst_sub$hw <- NA
sst_sub$hw[which(sst_sub$sst > q90_sst)] <- "MHW"
sst_sub$hw[which(sst_sub$sst < q10_sst)] <- "MCW"

sst_sub_year  <- sst_sub %>% group_by(year) %>% 
                summarise(sst = mean(sst,na.rm=TRUE),
                year = mean(year,na.rm=TRUE),
                month = 6,
                date=mean(date,na.rm=TRUE))

sst_sub_year_threshold <- sst_sub %>%
  group_by(year) %>%
  summarise(
    sst_mean = mean(sst, na.rm = TRUE),
    count_days_above_30 = sum(sst > 20.5, na.rm = TRUE),
    month = 6,
    date = mean(date, na.rm = TRUE)
  )

sst_sub_year_hottest <- sst_sub %>%
  group_by(year) %>% filter(day>59 & day<91) %>%
  summarise(
    sst_hottest = mean(sst, na.rm = TRUE),
    month = 3,
    date = mean(date, na.rm = TRUE))

plot_sst_hw <- ggplot(sst_sub, aes(x=date, y=sst)) +
    geom_line(aes(group=year), color="gray") +
    geom_line(aes(group=year, color=hw)) +
    theme_bw() + 
    scale_color_scico_d(palette="bam", direction=-1, na.value=NA, 
                        breaks = na.omit(unique(sst_sub$hw))) +
    labs(color = "") +  # Change legend title
    xlab("") + 
    ylab("Daily \nMean SST") +
    theme(legend.position = c(0.7, 0.4),  # Place legend inside the plot
          legend.background = element_blank(),  # Remove background
          legend.box.background = element_blank(), # Remove box around the legend
          legend.key = element_blank()) + # Remove legend key background
    guides(color = guide_legend(reverse = TRUE)) # Reverse order of legend items
plot_sst_hw 

plot_sst <- ggplot(sst_sub, aes(x=date,y=sst))+
    geom_line(aes(group=year))+
    theme_bw()+xlab("")+ylab("Daily \nMean SST")

plot_hottest <-  ggplot(sst_sub, aes(x=date,y=sst))+
    geom_line(aes(group=year),color="gray")+
    geom_point(data=sst_sub_year_hottest,aes(x=date,y=sst_hottest))+
    theme_bw()+xlab("")+ylab("Mean SST \nHottest Month")

plot_mean <- ggplot(sst_sub, aes(x=date,y=sst))+
    geom_line(aes(group=year),color="gray")+
    geom_point(data=sst_sub_year,aes(x=date,y=sst))+
    theme_bw()+xlab("")+ylab("Annual \n Mean SST")

plot_daysabove<- ggplot()+
    geom_point(data=sst_sub_year_threshold,aes(x=date,y=count_days_above_30))+
    theme_bw()+xlab("")+ylab("Days Above \n20.5C Threshold")

ggarrange(plot_sst,plot_mean,plot_hottest,plot_sst_hw,plot_daysabove,ncol=1)
ggsave("Figures/Presentation/SST_summary.png",dpi=600)
getwd()
