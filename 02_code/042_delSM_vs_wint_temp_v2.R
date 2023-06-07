rm(list=ls())

# Directories
wd = list()
wd$main = 'C:/sarfaraz/SM_carryover/'
wd$data = paste0(wd$main,'01_data/')
wd$fn = paste0(wd$main,'02_code/')
wd$output = paste0(wd$main,'03_results/')
wd$figure = paste0(wd$main,'04_figure/')
setwd(wd$data)

# Load function
source(paste0(wd$code, '051_pcor.R'))

# List 
file_list = list.files(paste0(wd$data,'10_raw_data/'), full.names = T)
file_name = substr(file_list,11,18)

# Read files using loop
tmp_all = sumry_all = 0
for(i in 1:length(file_list)){
  
  df = read.csv(file_list[[i]])
  head(df,1)
  df = df[,c(5,10,20,21,30,18,24,25,22,23,50,51,52,53,54,41,42,43)]
  df = cbind(df, delnovSM = (df[,11]-df[,1]),deldecSM = (df[,12]-df[,11]), deljanSM = (df[,13]-df[,12]))
  
  
  id_nan = which(is.nan(df[,6])==T)
  if(length(id_nan)>0){
    df = df[-id_nan,]
  }
  
  sumry_all = rbind(sumry_all, df)

}
head(sumry_all)

sumry_all = sumry_all[-1,]

#==================================================================
# Scatter plot
#==================================================================

# Plot for Nov
p1=ggplot(sumry_all, aes(x = NovTemp, y = delnovSM))+
  geom_point(alpha=.2,size=1.5,color = 'steelblue')+ theme_bw()+
  xlab(expression(paste("T"[Nov], " (degree C)")))+ ylab(expression(paste(Delta, "SM"[Nov], " (mm)")))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlim(c(-25,10))+ scale_y_continuous(limits = c(-100, 500),breaks=seq(-100, 500, 100))

ggsave(p1,filename=paste0(wd$figure,"05_delSMnov_vs_Tnov.png",sep=""),
       width = 12, height = 12, units = "cm")

# Plot for dec
p2= ggplot(sumry_all, aes(x = DecTemp, y = deldecSM))+
  geom_point(alpha=.2,size=1.5,color = 'steelblue')+ theme_bw()+
  xlab(expression(paste("T"[Dec], " (degree C)")))+ ylab(expression(paste(Delta, "SM"[Dec], " (mm)")))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlim(c(-25,10))+ scale_y_continuous(limits = c(-100, 500),breaks=seq(-100, 500, 100))

ggsave(p2,filename=paste0(wd$figure,"05_delSMdec_vs_Tdec.png",sep=""),
       width = 12, height = 12, units = "cm")


# Plot for Jan
p3= ggplot(sumry_all, aes(x = JanTemp, y = deljanSM))+
  geom_point(alpha=.2,size=1.5,color = 'steelblue')+ theme_bw()+
  xlab(expression(paste("T"[Jan], " (degree C)")))+ ylab(expression(paste(Delta, "SM"[Jan], " (mm)")))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlim(c(-25,10))+ scale_y_continuous(limits = c(-100, 500),breaks=seq(-100, 500, 100))

ggsave(p3,filename=paste0(wd$figure,"05_delSMjan_vs_Tjan.png",sep=""),
       width = 12, height = 12, units = "cm")


setwd(wd$output)
tmp_all = data.frame(tmp_all)

# write.csv(tmp_all, 'corr_pcor_compare_only_significant.csv')
# write.csv(sumry_all, 'mean_SR_PKSW_P_NT.csv')