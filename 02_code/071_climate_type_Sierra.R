rm(list=ls())

# Libraries
library(ggplot2)
library(dplyr)

# Directories
wd = list()
wd$main     = 'D:/Project_soil_moisture/00_SM_project/'
wd$data     = paste0(wd$main,'01_data/')
wd$raw_data = paste0(wd$main,'01_data/VIC_outputs_elev_bands_100m/')
wd$output   = paste0(wd$main,'03_results/')
wd$figure   = paste0(wd$main,'04_figure/p_t_scatter/')
setwd(wd$data)

#flag to include precipitation in the PRE calculation
flag_p = 0 # 0: no precipitation, 1: include precipitation

# Read data
#-------------------------------------------------------------------------
#From the following text file I only use the station id and cluster number
# pcor_df = read.table('pcor_sm_sr_sweFix.txt',header = T)
pcor_df = read.table('08_pcor_sm_sr_sweFix.txt',header = T)
pcor_df[,2] = factor(pcor_df[,2])
head(pcor_df)


#read all data csv
setwd(wd$raw_data)
file_list = list.files(pattern = "^Sierra Nevada")
df_all= 0

#Loop through the stations
for(i in 1:length(file_list)){
  
  df_low = read.csv(paste0('Sierra Nevada_',(i+1),'.csv'))
  
  #separating required columns and creating new dataframe
  df_low = df_low[c('Year','Month','Day','PREC','AIR_TEMP')]
  
  #calculating monthly sum of precipitation and mean of air temperature
  df_low_m = aggregate(.~Year + Month, df_low, mean)
  df_low_s = aggregate(.~Year + Month, df_low, sum)
  
  #assigning precipitation sum to the df_low_m precipitation column
  df_low_m$PREC = df_low_s$PREC 
  
  df_all = rbind(df_all, df_low_m)
  
}

df_all = df_all[-1,]
head(df_all)

#calculating monthly mean of precipitation and air temperature of all elevation bands
df_all = aggregate(.~Year + Month, df_all, mean)
dim(df_all)

#convert to water year
df_tmp = df_all[c('Year','Month')]
df_tmp$Month = df_tmp$Month +3
id = which(df_tmp$Month>12)
df_tmp$Year[id] = df_tmp$Year[id] + 1
df_tmp$Month[id] = df_tmp$Month[id] - 12
head(df_tmp)
df_all$Month = df_tmp$Month; df_all$Year = df_tmp$Year

#calculate annual precipitaiton and temeprature for each water years
df_all_m = aggregate(.~Year, df_all, mean)
df_all_s = aggregate(.~Year, df_all, sum)

#assigning the annual precipitation to the precipitation column in df_all_m
df_all_m$PREC = df_all_s$PREC


#scatter plot of precipitation and temperature
p = ggplot(df_all_m, aes(x=AIR_TEMP , y=PREC)) +
  geom_point(size = 1)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Avg. Temperature [degree-C]") + ylab("Annual Precipitation [mm]")+
  ggtitle(paste0('Sierra Nevada'))+
  theme_bw()

p

#exporting scatterplot
ggsave(p,filename=paste0(wd$figure,"Sierra_Nevada_p_t.png",sep=""),
       width = 15, height = 15, units = "cm")


#=========================================================================================================
#identifying years with different climate categories
p_s = summary(df_all_m$PREC)
t_s = summary(df_all_m$AIR_TEMP)

p_s
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 286.7   705.6   850.0   879.3  1003.0  1638.5 

t_s
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.876   7.203   7.495   7.571   7.957   9.358

# (1) warm and wet considering above 75th percentile for both
id = which(df_all_m$PREC>p_s[5] & df_all_m$AIR_TEMP>t_s[5])
df_all_m$Year[id]
# [1] 1936 1940 1996 2017

# (2) warm and dry considering above 75th percentile for temperature and 25th for precipitation
id = which(df_all_m$PREC<p_s[2] & df_all_m$AIR_TEMP>t_s[5])
df_all_m$Year[id]
# [1] 1939 1959 1988 2007 2013 2015


# (3) cold and wet considering above 75th percentile for both
id = which(df_all_m$PREC>p_s[5] & df_all_m$AIR_TEMP<t_s[2])
df_all_m$Year[id]
# [1] 1920 1922 1952 1955 1973 1982 1983 1998

# (2) cold and dry considering above 75th percentile for temperature and 25th for precipitation
id = which(df_all_m$PREC<p_s[2] & df_all_m$AIR_TEMP<t_s[2])
df_all_m$Year[id]
# 1949 1972 1985 2011 2020
