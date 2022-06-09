rm(list=ls())

# Libraries
library(ggplot2)
library(dplyr)

# Directories
wd = list()
wd$main     = 'D:/Project_soil_moisture/00_SM_project/'
wd$data     = paste0(wd$main,'01_data/')
wd$raw_data = paste0(wd$main,'01_data/10_raw_data/')
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


st_avg_info = read.csv(paste0(wd$data,'AvgInfo_SelectedBasins_Obs.csv'))
clust.data = st_avg_info[,104]
st_sel = c(st_avg_info[,3])
k_val = c(pcor_df[,5])

elev = read.table(paste0(wd$data,'08_elevation.txt'),
                  header =T)

#read all data csv
setwd(wd$raw_data)
file_list = list.files()
df_all= 0

#Loop through the stations
for(i in 1:length(st_sel)){
  
  #check if the station data exists
  if(st_sel[i]<10000000){
    chk = which(file_list == paste0('Variables_0',st_sel[i],'.csv'))
  }else{
    chk = which(file_list == paste0('Variables_',st_sel[i],'.csv'))
  }
  
  if(length(chk)== 0){next()}
  
  
  if(st_sel[i]<10000000){
    df_low = read.csv(paste0('Variables_0',st_sel[i],'.csv'))
    nm = paste0('0',st_sel[i])
  }else{
    df_low = read.csv(paste0('Variables_',st_sel[i],'.csv'))
    nm = st_sel[i]
  }
  
  #separating required columns and creating new dataframe
  df_low = df_low[c('WaterYear','AvgWinterTemp','AvgAnnualTemp','WinterPrecip','SpringPrecip')]
  df_low$WintSpriPrecip = df_low$WinterPrecip + df_low$SpringPrecip
  
  #storing all df_low in one dataframe
  df_all = rbind(df_all,df_low)
  
  #scatterplot of temperature vs precipitation
  p = ggplot(df_low, aes(x=AvgWinterTemp , y=WinterPrecip)) +
    geom_point()+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=14,face="bold"))+
    xlab("Winter T [degree C]") + ylab("Winter P [mm]")+
    ggtitle(paste0('USGS-',nm))+
    theme_bw()
  
  p
  
  #exporting scatterplot
  ggsave(p,filename=paste0(wd$figure,nm,"_p_t.png",sep=""),
         width = 10, height = 10, units = "cm")
  
}

df_all = df_all[-1,]
head(df_all)


p = ggplot(df_all, aes(x=AvgWinterTemp , y=WinterPrecip)) +
  geom_point(size = 1)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Winter T [degree C]") + ylab("Winter P [mm]")+
  ggtitle(paste0('All stations and water years'))+
  theme_bw()

p

#exporting scatterplot
ggsave(p,filename=paste0(wd$figure,"all_stations_p_t.png",sep=""),
       width = 15, height = 15, units = "cm")
