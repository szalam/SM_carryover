rm(list=ls())

library(ggplot2)
library(forcats)

ppt_cosider = 0 # 1: include spring ppt; 0: don't include spring ppt
sm_signif = 1 # 1: only when SM is significant predictor; 0: sm not necessarily significant predictor

# Directories
wd = list()
wd$main     = 'C:/sarfaraz/SM_carryover/'
wd$data     = paste0(wd$main,'01_data/')
wd$raw_data = paste0(wd$main,'01_data/85_40NA/Runoff_Sel/')
wd$output   = wd$data
wd$figure   = paste0(wd$main,'04_figure/')
setwd(wd$data)


lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


setwd(wd$data)
 
st_avg_info = read.csv(paste0('85_40NA/Csv_Files/','AvgInfo_Basins_Cluster_40NA_SMChangeClimate.csv'))
clust.data = st_avg_info$ClusterMode
st_sel = c(st_avg_info$GAGEID)

#read all data csv
# setwd(wd$raw_data)
all.in = read.csv('YearlyData_ElevBands_Clustered2.csv')

file_list = list.files()

pre_all = var_means = id_rm = k.keep = clust.all = n.all = st.all = cor.all = 0

for(n in 1:5){

for(i in 1:length(st_sel)){
 
  # df_high = read.csv(paste0('Variables_',st_sel[2],'.csv'))
  df_low = all.in[all.in$GAGEID==st_sel[i],]
  
  df_low = df_low[c('SpringRunoffApr1','Nov1SM','Dec1SM','Jan1SM','Feb1SM','Mar1SM','PeakSWE','PeakSWE_PrecipRatio')]
  head(df_low,1)
  
  # id_nan = which(is.nan(as.numeric(df_low[,7]))==T)
  
  # if(length(id_nan >0)){
  #   df_low = df_low[-id_nan,]
  # }
  
 
  # prediction model development
  
  # Create the relationship model.
  if(ppt_cosider == 1){
    if(n==5){model <- lm(SpringRunoffVIC ~ Nov1SM + PeakSWE + SpringPrecip, data = df_low)}
    if(n==4){model <- lm(SpringRunoffVIC ~ Dec1SM + PeakSWE + SpringPrecip, data = df_low)}
    if(n==3){model <- lm(SpringRunoffVIC ~ Jan1SM + PeakSWE + SpringPrecip, data = df_low)}
    if(n==2){model <- lm(SpringRunoffVIC ~ Feb1SM + PeakSWE + SpringPrecip, data = df_low)}
    if(n==1){model <- lm(SpringRunoffVIC ~ Mar1SM + PeakSWE + SpringPrecip, data = df_low)}
    
    model2 <- lm(SpringRunoffVIC ~ PeakSWE+ SpringPrecip, data = df_low)
    
  }else{
    if(n==5){model <- lm(SpringRunoffVIC ~ Nov1SM + PeakSWE, data = df_low)}
    if(n==4){model <- lm(SpringRunoffVIC ~ Dec1SM + PeakSWE, data = df_low)}
    if(n==3){model <- lm(SpringRunoffVIC ~ Jan1SM + PeakSWE, data = df_low)}
    if(n==2){model <- lm(SpringRunoffVIC ~ Feb1SM + PeakSWE, data = df_low)}
    if(n==1){model <- lm(SpringRunoffVIC ~ Mar1SM + PeakSWE, data = df_low)}
    
    model2 <- lm(SpringRunoffVIC ~ PeakSWE, data = df_low)
  }
  
  if(n==3){cor.all = c(cor.all, (cor(df_low$SpringRunoffVIC, df_low$Nov1SM))^2)}
  # Show the model.
  # print(model)
  p_val = lmp(model)
  sum_model = summary(model)
  
  
  if(p_val>.05){
    id_rm = c(id_rm,i)
    next()}
  
  if(sm_signif==1){
  if(sum_model$coefficients[11]>0.05){next()}}

  #Proportional Reduction in Error
  PRE = (deviance(model2)-deviance(model))/deviance(model2)
  pre_all = c(pre_all, PRE)
  
  var_means = rbind(var_means,  c(colMeans(df_low),n))
  
  # clust.all = c(clust.all, clust.data[i])
 
  n.all = c(n.all,n) 
  
  st.all = c(st.all, st_sel[i])
}

}

pre_all = pre_all[-1]
var_means = var_means[-1,]
# k.keep = k.keep[-1]
# clust.all = clust.all[-1]
n.all = n.all[-1]
st.all = st.all[-1]
id_rm = id_rm[-1]
cor.all= cor.all[-1]

# av_wint_T = clust.all#var_means[,5]
# av_wint_T = cbind(av_wint_T,'temp')
# av_wint_T = data.frame(av_wint_T)
# av_wint_T[,1] = as.numeric(as.character(av_wint_T[,1]))
# av_wint_T[,2] = as.character(av_wint_T[,2])
# av_wint_T[which(av_wint_T[,1]==1),2] = 'Interior'# av_wint_T[which(av_wint_T[,1]<=0),2] = 'Interior'
# av_wint_T[which(av_wint_T[,1]==2),2] = 'Coastal'# av_wint_T[which(av_wint_T[,1]>0),2] = 'Coastal'
# # av_wint_T = data.frame(av_wint_T)
# colnames(av_wint_T) = c('Ratio','Regime')

df_plot2 = data.frame(gage_id = st.all,PRE = pre_all*100, Lead_time_month = n.all)

cor.all = data.frame(station = st_sel, r2 = cor.all)
if(sm_signif==1){
  write.csv(df_plot2, paste0(wd$output,'PRE_SM_SWE_LT_stns_forElevBands_SM_signif_predictor.csv'))
}else{
write.csv(df_plot2, paste0(wd$output,'PRE_SM_SWE_LT_stns_forElevBands.csv'))}
write.csv(cor.all, paste0(wd$output,'r2_janSM_SR.csv'))
head(df_plot2)
