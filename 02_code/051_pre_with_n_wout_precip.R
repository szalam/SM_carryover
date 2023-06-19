rm(list=ls())

# Libraries
library(ggplot2)
library(dplyr)

# Directories
wd = list()
wd$main = 'C:/sarfaraz/SM_carryover/'
wd$data = paste0(wd$main, '01_data/')
wd$raw_data = paste0(wd$main,'01_data/85_40NA/Runoff_Sel/')
wd$output = paste0(wd$main, '03_results/')
wd$figure = paste0(wd$main, '04_figure/')

setwd(wd$data)

#flag to include precipitation in the PRE calculation
flag_p = 0 # 0: no precipitation, 1: include precipitation

# Function
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


# Read data
#-------------------------------------------------------------------------
#From the following text file I only use the station id and cluster number
# pcor_df = read.table('pcor_sm_sr_sweFix.txt',header = T)
# pcor_df = read.table('08_pcor_sm_sr_sweFix.txt',header = T)
# pcor_df[,2] = factor(pcor_df[,2])
# head(pcor_df)


st_avg_info = read.csv(paste0('85_40NA/Csv_Files/AvgInfo_Basins_Cluster_40NA_SMChangeClimate.csv'), header = TRUE, fill = TRUE)
clust.data = st_avg_info$ClusterMode
st_sel = st_avg_info$GAGEID

#read all data csv
setwd(wd$raw_data)
file_list = list.files()
pre_all = var_means = id_rm = r2_all = clust.all = st_high = st_all = rmse_w_sm = rmse_wo_sm = 0

#Loop through the 5 lead times (LT)
#--------------------------------------------------------------------------
for(n in 1:5){
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
    
    df_low = df_low[c('SpringRunoffApr1','Nov1SM','Dec1SM','Jan1SM','Feb1SM','Mar1SM','PeakSWE','PeakSWE_PrecipRatio')]
    head(df_low,1)
    
    id_nan = which(is.nan(as.numeric(df_low[,7]))==T)
    
    if(length(id_nan >0)){
      df_low = df_low[-id_nan,]
    }
    
    # prediction model development
    
    # Create the relationship model.
    
    if(flag_p == 0){
      if(n==5){model <- lm(SpringRunoffApr1 ~ Nov1SM + PeakSWE, data = df_low)}
      if(n==4){model <- lm(SpringRunoffApr1 ~ Dec1SM + PeakSWE, data = df_low)}
      if(n==3){model <- lm(SpringRunoffApr1 ~ Jan1SM + PeakSWE, data = df_low)}
      if(n==2){model <- lm(SpringRunoffApr1 ~ Feb1SM + PeakSWE, data = df_low)}
      if(n==1){model <- lm(SpringRunoffApr1 ~ Mar1SM + PeakSWE, data = df_low)}
      
      model2 <- lm(SpringRunoffApr1 ~ PeakSWE, data = df_low)
    }
    
    
    if(flag_p == 1){
      if(n==5){model <- lm(SpringRunoffApr1 ~ Nov1SM + PeakSWE + WinterPrecip , data = df_low)}
      if(n==4){model <- lm(SpringRunoffApr1 ~ Dec1SM + PeakSWE + WinterPrecip , data = df_low)}
      if(n==3){model <- lm(SpringRunoffApr1 ~ Jan1SM + PeakSWE + WinterPrecip , data = df_low)}
      if(n==2){model <- lm(SpringRunoffApr1 ~ Feb1SM + PeakSWE + WinterPrecip , data = df_low)}
      if(n==1){model <- lm(SpringRunoffApr1 ~ Mar1SM + PeakSWE + WinterPrecip , data = df_low)}
      
      model2 <- lm(SpringRunoffApr1 ~ PeakSWE + WinterPrecip, data = df_low)
    }
    
    
    # Show the model.
    print(model)
    p_val = lmp(model)
    sum_model = summary(model)
    sum_model2 = summary(model2)
    
    
    if(p_val>.05){
      id_rm = c(id_rm,i)
      next()}
    if(flag_p==0){if(sum_model$coefficients[11]>0.05){next()}} #if the coefficient of soil moisture has p value >0.05, ignore the station
    if(flag_p==1){if(sum_model$coefficients[14]>0.05){next()}} #if the coefficient of soil moisture has p value >0.05, ignore the station
    
    r2_all = rbind(r2_all, c(sum_model$adj.r.squared, sum_model2$adj.r.squared,n))

    
    # Get the Intercept and coefficients as vector elements.
    cat("# # # # The Coefficient Values # # # ","\n")
    
    a <- coef(model)[1]
    print(a)
    
    Xdisp <- coef(model)[2]
    Xhp <- coef(model)[3]
    
    print(Xdisp)
    print(Xhp)
    
    #Proportional Reduction in Error
    PRE = (deviance(model2)-deviance(model))/deviance(model2)
    pre_all = c(pre_all, PRE)
    st_all = c(st_all, st_sel[i])
    var_means = rbind(var_means,  c(colMeans(df_low),n))
    
    clust.all = c(clust.all, clust.data[i])
    
    #RMSE
    rmse_w_sm = c(rmse_w_sm, sqrt(c(crossprod(model$residuals))/length(model$residuals)))
    rmse_wo_sm = c(rmse_wo_sm, sqrt(c(crossprod(model2$residuals))/length(model2$residuals)))
    
    if(PRE>=.2){st_high = c(st_high,st_sel[i])}
  }
  
}

pre_all = pre_all[-1]
var_means = var_means[-1,]
r2_all = r2_all[-1,]
clust.all = clust.all[-1]
st_high = st_high[-1]
id_rm = id_rm[-1]
st_all = st_all[-1]
rmse_w_sm = rmse_w_sm[-1]
rmse_wo_sm = rmse_wo_sm[-1]


colnames(r2_all) =c('with_SM','without_sm','LT')
if(flag_p == 0){df_comb = data.frame(station = st_all, r2_all, PRE_wo_p = pre_all*100, cluster = clust.all, rmse_w_sm_wo_p = rmse_w_sm, rmse_wo_sm_wo_p = rmse_wo_sm, regime = 'A')}
if(flag_p == 1){df_comb = data.frame(station = st_all, r2_all, PRE_w_p = pre_all*100, cluster = clust.all, rmse_w_sm_w_p = rmse_w_sm, rmse_wo_sm_w_p = rmse_wo_sm, regime = 'A')}

df_comb[which(df_comb$cluster==1),'regime'] = 'Interior'
df_comb[which(df_comb$cluster==2),'regime'] = 'Maritime'
df_comb$PRE = df_comb$PRE*100 #convert to percent

head(df_comb)

if(flag_p == 0){write.csv(df_comb, paste0(wd$output,'/051_pre_wthout_precip.csv'),row.names = F)}
if(flag_p == 1){write.csv(df_comb, paste0(wd$output,'/051_pre_with_precip.csv'),row.names = F)}

#========================================================================================================
#read the PRE values with and without considering precipitation as covariate of MLR
df_w_p = read.csv(paste0(wd$output, '051_pre_with_precip.csv'))    # precipitation used as covariate
df_wo_p = read.csv(paste0(wd$output, '051_pre_wthout_precip.csv')) # precipitation not used as covariate
dim(df_w_p)
dim(df_wo_p)

#join two dataframes
df_join = full_join(df_w_p, df_wo_p, by = c("station",'LT'))
head(df_join)


# Scatterplot of PRE with and without considering precipitation as covariate of MLR
p = ggplot(df_join, aes(x=PRE_wo_p , y=PRE_w_p)) +
  geom_point()+
  theme_bw() + 
  # theme(legend.position="none") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlab("PRE without P as covariate in MLR") + ylab("PRE with P as covariate in MLR")

p

summary(df_join$PRE_w_p - df_join$PRE_wo_p)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -11.2864  -9.9965  -4.6268  -5.7883  -2.9552  -0.6099       24 

df_maritime = df_join[df_join$regime.x == 'Maritime',]
summary(df_maritime$PRE_w_p - df_maritime$PRE_wo_p)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -5.376  -5.376  -5.376  -5.376  -5.376  -5.376      23

df_interior = df_join[df_join$regime.x == 'Interior',]
summary(df_interior$PRE_w_p - df_interior$PRE_wo_p)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -11.2864 -10.1125  -4.4499  -5.8200  -2.6193  -0.6099       21 

#Export the histogram of PRE differences
png(paste0(wd$figure, '051_pre_diff_hist.png'),width = 880, height = 480,
    units = "px", bg = "white")
hist(df_join$PRE_w_p - df_join$PRE_wo_p, main = 'Histogram of PRE differences with/without precipitation as covariate',
     xlab = '(PRE_with_p - PRE_without_p)%',prob=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.4, cex.sub=1.5)
dev.off()



summary(df_join$rmse_w_sm_wo_p) #unit in mm of spring flow
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   22.77   32.04   63.30   57.85   72.78  142.72       4 

summary(df_join$rmse_wo_sm_wo_p) #unit in mm of spring flow
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   27.44   37.92   66.36   62.48   77.07  157.70       4 
