# This code is used to calculate storage coefficient at selected stations

rm(list=ls())

#Libraries
library(lubridate)
library(ggplot2)
library(reshape2)

# Directories
wd = list()
wd$main     = 'D:/Project_soil_moisture/00_SM_project/'
wd$data     = paste0(wd$main,'01_data/')
wd$output   = paste0(wd$main,'03_results/')
wd$figure   = paste0(wd$main,'04_figure/')

setwd(wd$data)

# Read data
data = read.csv(paste0('AvgInfo_SelectedBasins_Obs2.csv'),header=T,fill = T)
head(data,1)

df = data[,c(1,2,3,57:62,111)]
head(df,1)

tmp = df$Cluster#var_means[,5]
tmp = cbind(tmp,'temp')
tmp = data.frame(tmp)
tmp[,1] = as.numeric(as.character(tmp[,1]))
tmp[,2] = as.character(tmp[,2])
tmp[which(tmp[,1]==1),2] = 'Interior'
tmp[which(tmp[,1]==2),2] = 'Maritime'
colnames(tmp) = c('Ratio','Regime')

df = cbind(df,Regime = tmp[,2])

df.tmp = df[,c(4,11)]

mdat = melt(df.tmp, id.var=c('Regime'))

#===============================================================
# Plot
#===============================================================
# Plot day of water year (DOWY) below 0-degrees
p1= ggplot(mdat, aes(x=Regime, y=value, fill = Regime)) +
  geom_boxplot(alpha = .6)+
  theme_bw()+
  theme(legend.position="none") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))+
  xlab(" ") + ylab("First DOWY T<= 0 C")+
  # ylim(0,40)+
  scale_fill_manual(values=c( "skyblue3", "orangered2"))+
  guides(fill=guide_legend(title="Regime"))

ggsave(p1,filename=paste0(wd$figure,"05_DOWY_T_below_0C.png",sep=""),
       width = 14, height = 15, units = "cm")


# Plot days  below 0-degrees
df.tmp = df[,c(5,11)]

mdat = melt(df.tmp, id.var=c('Regime'))

p2 = ggplot(mdat, aes(x=Regime, y=value, fill = Regime)) +
  geom_boxplot(alpha = .6)+
  theme_bw()+
  theme(legend.position="none") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))+
  xlab(" ") + ylab("Number of days with T<= 0 C")+
  scale_fill_manual(values=c( "skyblue3", "orangered2"))+
  guides(fill=guide_legend(title="Regime"))

ggsave(p2,filename=paste0(wd$figure,"05_days_T_below_0C.png",sep=""),
       width = 14, height = 15, units = "cm")


# plot SM pre 0-degree vs post 0-degree
df.tmp = df[,c(8,9,11)]

p3 = ggplot(df.tmp, aes(x=SM_0TempDay, y=SM_PosTempDay, color = Regime)) +
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1)+
  geom_point(aes(fill=Regime),color='black',pch=21,size=4,alpha =.6)+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))+
  xlab("SM_pre_T0C [mm]") + ylab("SM_post_T0C [mm]")+
  guides(fill=guide_legend(title="Regime"))+
  scale_fill_manual(values=c( "skyblue3", "orangered2"))+
  scale_y_continuous(breaks = seq(200, 1300, by=200), limits=c(200,1300))+
  scale_x_continuous(breaks = seq(200, 1300, by=200), limits=c(200,1300))

ggsave(p3,filename=paste0(wd$figure,"05_SM_pre_post_below_0C.png",sep=""),
       width = 17, height = 15, units = "cm")

