# Script to boxplot PRE vs slow/fast draining for maritime/interior catchments
# Author: Sarfaraz Alam

# Clear the environment
rm(list=ls())

# Libraries
library(ggplot2)
library(forcats)
library(dplyr)

# Directories
wd = list()
wd$main     = 'D:/Project_soil_moisture/00_SM_project/'
wd$data     = paste0(wd$main,'01_data/')
wd$raw_data = paste0(wd$main,'01_data/10_raw_data/')
wd$k_values = paste0(wd$main,'03_results/')
wd$output   = paste0(wd$main,'03_results/')
wd$figure   = paste0(wd$main,'04_figure/')
setwd(wd$data)

# User intput requried
#------------------------------------------------------------------------------
ppt_cosider = 0 # 1: include spring ppt; 0: don't include spring ppt
sm_signif = 1   # 1: only when SM is significant predictor; 0: sm not necessarily significant predictor

# Function
#------------------------------------------------------------------------------
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Import files
#------------------------------------------------------------------------------

# From the following text file only used the station id and cluster number
# pcor_df = read.table(paste0(wd$output,'10_pcor_sm_sr_sweFix.txt'),header = T)
# pcor_df[,2] = factor(pcor_df[,2])

# import k values
k_values = read.csv(paste0(wd$k_value,'10_k_all.csv'))

# import station information
st_avg_info = read.csv('AvgInfo_SelectedBasins_Obs.csv')
clust.data = st_avg_info[,104] # column 104 is the cluster number

st_sel = c(st_avg_info[,3]) # column 3 is the gage id

# read all simulated data csv
setwd(wd$raw_data)
file_list = list.files()
pre_all = var_means = id_rm = k.keep = clust.all = n.all = st.all = 0

# looping through 5 lead times (LT)
for(n in 1:5){
  # n=1
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
    }else{
      df_low = read.csv(paste0('Variables_',st_sel[i],'.csv'))
    }
    
    # df_high = read.csv(paste0('Variables_',st_sel[2],'.csv'))
    
    df_low = df_low[,c(5,8,12,13,10,20,55,50,51,52,53,14)]
    head(df_low,1)
    
    id_nan = which(is.nan(as.numeric(df_low[,7]))==T)
    
    if(length(id_nan >0)){
      df_low = df_low[-id_nan,]
    }
    
    # prediction model development
    
    # Create the relationship model.
    if(ppt_cosider == 1){
      if(n==5){model <- lm(SpringRunoffApr1 ~ Nov1SM + PeakSWE + SpringPrecip, data = df_low)}
      if(n==4){model <- lm(SpringRunoffApr1 ~ Dec1SM + PeakSWE + SpringPrecip, data = df_low)}
      if(n==3){model <- lm(SpringRunoffApr1 ~ Jan1SM + PeakSWE + SpringPrecip, data = df_low)}
      if(n==2){model <- lm(SpringRunoffApr1 ~ Feb1SM + PeakSWE + SpringPrecip, data = df_low)}
      if(n==1){model <- lm(SpringRunoffApr1 ~ Mar1SM + PeakSWE + SpringPrecip, data = df_low)}
      
      model2 <- lm(SpringRunoffApr1 ~ PeakSWE+ SpringPrecip, data = df_low)
      
    }else{
      if(n==5){model <- lm(SpringRunoffApr1 ~ Nov1SM + PeakSWE, data = df_low)}
      if(n==4){model <- lm(SpringRunoffApr1 ~ Dec1SM + PeakSWE, data = df_low)}
      if(n==3){model <- lm(SpringRunoffApr1 ~ Jan1SM + PeakSWE, data = df_low)}
      if(n==2){model <- lm(SpringRunoffApr1 ~ Feb1SM + PeakSWE, data = df_low)}
      if(n==1){model <- lm(SpringRunoffApr1 ~ Mar1SM + PeakSWE, data = df_low)}
      
      model2 <- lm(SpringRunoffApr1 ~ PeakSWE, data = df_low)
    }
    
    
    # Show the model.
    # print(model)
    p_val = lmp(model)
    sum_model = summary(model)
    
    
    if(p_val>.05){
      id_rm = c(id_rm,i)
      next()}
    
    if(sm_signif==1){
      if(sum_model$coefficients[11]>0.05){next()}}
    
    # if(sum_model$coefficients[11]>.05){
    #   id_rm = c(id_rm,i)
    #   next()}
    
    # get the k value for this station
    id.tmp = which(k_values[,3]==st_sel[i])
    print(id.tmp)
    k.keep = c(k.keep, k_values[id.tmp,4])
    
    
    #Proportional Reduction in Error (PRE)
    PRE = (deviance(model2)-deviance(model))/deviance(model2)
    pre_all = c(pre_all, PRE)
    
    var_means = rbind(var_means,  c(colMeans(df_low),n))
    
    clust.all = c(clust.all, clust.data[i])
   
    n.all = c(n.all,n) 
    
    st.all = c(st.all, st_sel[i])
  }

}

pre_all = pre_all[-1]
var_means = var_means[-1,]
k.keep = k.keep[-1]
clust.all = clust.all[-1]
n.all = n.all[-1]
st.all = st.all[-1]
id_rm = id_rm[-1]


av_wint_T = clust.all#var_means[,5]
av_wint_T = cbind(av_wint_T,'temp')
av_wint_T = data.frame(av_wint_T)
av_wint_T[,1] = as.numeric(as.character(av_wint_T[,1]))
av_wint_T[,2] = as.character(av_wint_T[,2])
av_wint_T[which(av_wint_T[,1]==1),2] = 'Interior'# av_wint_T[which(av_wint_T[,1]<=0),2] = 'Interior'
av_wint_T[which(av_wint_T[,1]==2),2] = 'Maritime'# av_wint_T[which(av_wint_T[,1]>0),2] = 'Maritime'
# av_wint_T = data.frame(av_wint_T)
colnames(av_wint_T) = c('Ratio','Regime')

# Categorizing based on k threshold values
k_class = k.keep
k_class[k.keep>45] = 'Slow'
k_class[k.keep<=45] = 'Fast'


df_plot2 = data.frame(k = k.keep, Regime = factor(k_class),t_regime = av_wint_T[,2],PRE = pre_all*100, LT = n.all)
# df_plot2 = data.frame(k = k.keep, Regime = av_wint_T[,2],value = pre_all*100)
# df_plot2 = data.frame(K = k.keep,value = pre_all*100)

df_plot2_tmp = df_plot2[(df_plot2$t_regime=='Interior' & df_plot2$Regime=='Fast'),]
df_plot2_tmp = df_plot2[(df_plot2$t_regime=='Interior' & df_plot2$Regime=='Slow'),]
mean(df_plot2_tmp$PRE)

# Export plotted data
write.csv(df_plot2, paste0(wd$output,'PRE_LT_stns.csv'))

df_plot2.back = df_plot2
df_plot2 = df_plot2.back
head(df_plot2)

# df_plot2 = df_plot2[df_plot2$LT == 5,]
nrow(df_plot2[(df_plot2$Regime=='Slow' & df_plot2$t_regime=='Interior'),]) 
nrow(df_plot2[(df_plot2$Regime=='Fast' & df_plot2$t_regime=='Interior'),]) 
nrow(df_plot2[(df_plot2$Regime=='Slow' & df_plot2$t_regime=='Maritime'),])
nrow(df_plot2[(df_plot2$Regime=='Fast' & df_plot2$t_regime=='Maritime'),])

#===============================================================
#Calculate stations under each category
#===============================================================
k_values.tmp = data.frame(GAGEID = k_values$st.st.select,k=k_values$k)
st_avg_info.tmp = st_avg_info[c('GAGEID','Cluster')]
df.comb = left_join(st_avg_info.tmp,k_values.tmp,by = 'GAGEID')
head(df.comb)
df.comb$categ = 'a'
df.comb[(df.comb$Cluster==1 & df.comb$k>45),4]  = 'interior and slow'
df.comb[(df.comb$Cluster==1 & df.comb$k<=45),4] = 'interior and fast'
df.comb[(df.comb$Cluster==2 & df.comb$k>45),4]  = 'maritime and slow'
df.comb[(df.comb$Cluster==2 & df.comb$k<=45),4] = 'maritime and fast'

summary(df.comb$categ)
nrow(df.comb[df.comb$categ == 'interior and slow',])
nrow(df.comb[df.comb$categ == 'interior and fast',])
nrow(df.comb[df.comb$categ == 'maritime and slow',])
nrow(df.comb[df.comb$categ == 'maritime and fast',])
head(df.comb)
# write.csv(df.comb, paste0(wd$output,'1_stn_category.csv'))

#================================================
# select specific lead times
# # df_plot2.back = df_plot2
# df_plot2 = df_plot2.back
# df_plot2 = df_plot2[df_plot2$LT==5,]
#================================================

p= ggplot(df_plot2, aes(x=Regime, y=PRE,fill = t_regime)) +
  geom_boxplot()+
  theme_bw()+ 
  # geom_smooth(method = 'lm',aes(x=k, y=value),se=F,alpha=.5, size =.5)
  # ylim(0,10)+ xlim(20,200)
  # theme(legend.position="none") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlab(" ") + ylab("PRE [%]")+
  ylim(0,40)+
  scale_fill_brewer(palette = "Spectral")+
  guides(fill=guide_legend(title="Regime"))

p

# Export figures
#-----------------------------------------------------------
if(ppt_cosider == 1){
  ggsave(p,filename=paste0(wd$figure,"10_PRE_vs_slowfast_wPPT_",n,"mon.png",sep=""),
         width = 12, height = 15, units = "cm")
}else{
  if(sm_signif==1){
    ggsave(p,filename=paste0(wd$figure,"10_PRE_vs_slowfast_woPPT_",n,"mon_SM_signif.png",sep=""),
           width = 12, height = 15, units = "cm")
  }else{
  ggsave(p,filename=paste0(wd$figure,"10_PRE_vs_slowfast_woPPT_",n,"mon.png",sep=""),
         width = 12, height = 15, units = "cm")}
}

df_plot2$t_regime = as.factor(df_plot2$t_regime)
summary(df_plot2)

# Creating pivot table of the statistics
smry_pre = 
  df_plot2 %>%
  group_by(t_regime, Regime) %>%
  summarize(st_count =  n(), 
            mean_pre = mean(PRE, na.rm=TRUE),
            median_pre = median(PRE, na.rm=TRUE),
            sd_pre = sd(PRE, na.rm=TRUE),
            qnt_75 = quantile(PRE, probs = 0.75),
            qnt_25 = quantile(PRE, probs = 0.25),
            max_pre = max(PRE, na.rm=TRUE),
            min_pre = min(PRE, na.rm=TRUE)
            )

write.csv(smry_pre, paste0(wd$output,'10_PRE_intmar_fastslow.csv'))

# Check
#------------------------------------------------------------
ttt = aggregate(df_plot2[,4],by = list(df_plot2[,2]),FUN=mean) 
(ttt[1,2]-ttt[2,2])#slow fast


ttt = aggregate(df_plot2[,4],by = list(df_plot2[,3]),FUN=mean)
(ttt[1,2]-ttt[2,2])#interior Maritime