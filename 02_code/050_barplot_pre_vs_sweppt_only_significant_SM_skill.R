rm(list=ls())

# Libraries
library(ggplot2)

# Directories
wd = list()
wd$main     = 'C:/sarfaraz/SM_carryover/'
wd$data     = paste0(wd$main,'01_data/')
wd$raw_data = paste0(wd$main,'01_data/85_40NA/Runoff_Sel/')
wd$output   = paste0(wd$main,'03_results/')
wd$figure   = paste0(wd$main,'04_figure/')
setwd(wd$data)

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
pcor_df = read.csv(paste0('85_40NA/Csv_Files/','AvgInfo_Basins_Cluster_40NA_SMChangeClimate.csv'),header = T)
pcor_df = pcor_df[c('GAGEID','ClusterMode')]
# pcor_df = read.table('08_pcor_sm_sr_sweFix.txt',header = T)
pcor_df[,2] = factor(pcor_df[,2])
head(pcor_df)


st_avg_info = read.csv(paste0('85_40NA/Csv_Files/','AvgInfo_Basins_Cluster_40NA_SMChangeClimate.csv'))
clust.data = st_avg_info$ClusterMode
st_sel = c(st_avg_info$GAGEID)
# k_val = c(pcor_df[,5])

#read all data csv
setwd(wd$raw_data)
file_list = list.files()
pre_all = var_means = id_rm = r2_all = clust.all = st_high = 0

#Loop through the 5 lead times (LT)
#--------------------------------------------------------------------------
# for(i in 1:nrow(pcor_df)){
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
    
    # df_low = df_low[,c(5,8,12,13,10,20,55,50,51,52,53)]
    df_low = df_low[c('SpringRunoffApr1','Nov1SM','Dec1SM','Jan1SM','Feb1SM','Mar1SM','PeakSWE','PeakSWE_PrecipRatio')]
    head(df_low,1)
    
    id_nan = which(is.nan(as.numeric(df_low[,'SpringRunoffApr1']))==T)
    
    if(length(id_nan >0)){
      df_low = df_low[-id_nan,]
    }
    
    # prediction model development
    
    # Create the relationship model.
    if(n==5){model <- lm(SpringRunoffApr1 ~ Nov1SM + PeakSWE, data = df_low)}
    if(n==4){model <- lm(SpringRunoffApr1 ~ Dec1SM + PeakSWE, data = df_low)}
    if(n==3){model <- lm(SpringRunoffApr1 ~ Jan1SM + PeakSWE, data = df_low)}
    if(n==2){model <- lm(SpringRunoffApr1 ~ Feb1SM + PeakSWE, data = df_low)}
    if(n==1){model <- lm(SpringRunoffApr1 ~ Mar1SM + PeakSWE, data = df_low)}
    
    model2 <- lm(SpringRunoffApr1 ~ PeakSWE, data = df_low)
    
    # Show the model.
    print(model)
    p_val = lmp(model)
    sum_model = summary(model)
    sum_model2 = summary(model2)
    
    
    if(p_val>.05){
      id_rm = c(id_rm,i)
      next()}
    if(sum_model$coefficients[11]>0.05){next()}
    
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
    
    var_means = rbind(var_means,  c(colMeans(df_low),LT = n))
    
    clust.all = c(clust.all, clust.data[i])
    
    if(PRE>=.2){st_high = c(st_high,st_sel[i])}
  }
  
}

pre_all = pre_all[-1]
var_means = data.frame(var_means[-1,])
r2_all = r2_all[-1,]
clust.all = clust.all[-1]
st_high = st_high[-1]
id_rm = id_rm[-1]


colnames(r2_all) =c('with_SM','without_sm','LT')
# write.csv(r2_all, paste0(wd$output,'/2_r2_MLR_SR_SWE_SM.csv'))

swe_ppt_rat = var_means$PeakSWE_PrecipRatio #var_means[,3]/var_means[,4]
swe_ppt_rat = cbind(swe_ppt_rat,'temp')
swe_ppt_rat[,1] = as.numeric(as.character(swe_ppt_rat[,1]))
swe_ppt_rat[which(swe_ppt_rat[,1]<.3),2] = 'SWE/P < 30%'
swe_ppt_rat[which(swe_ppt_rat[,1]>=.3),2] = 'SWE/P >= 30%'
swe_ppt_rat = data.frame(swe_ppt_rat)
colnames(swe_ppt_rat) = c('Ratio','Regime')

# Process dataframe for plotting
df_plot = data.frame(Regime = swe_ppt_rat$Ratio,value = pre_all*100, LT = factor(var_means$LT))


av_wint_T = clust.all#var_means[,5]
av_wint_T = cbind(av_wint_T,'temp')
av_wint_T = data.frame(av_wint_T)
av_wint_T[,1] = as.numeric(as.character(av_wint_T[,1]))
av_wint_T[,2] = as.character(av_wint_T[,2])
av_wint_T[which(av_wint_T[,1]==1),2] = 'Interior'# av_wint_T[which(av_wint_T[,1]<=0),2] = 'Interior'
av_wint_T[which(av_wint_T[,1]==2),2] = 'Maritime'# av_wint_T[which(av_wint_T[,1]>0),2] = 'Maritime'
# av_wint_T = data.frame(av_wint_T)
colnames(av_wint_T) = c('Ratio','Regime')


df_plot2 = data.frame(Regime = av_wint_T$Regime,value = pre_all*100, LT = factor(var_means$LT))

df_plot2[df_plot2$value>=20,]
# Ordering data in specific sequence we are interested. Maritime to Interior. LT 5 to 1
#----------------------------------------------------------------------------------------
y = c('Maritime','Interior')
df_plot2 = df_plot2[order(match(df_plot2$Regime, y)),]
y = c('5','4','3','2','1')
df_plot2 = df_plot2[order(match(df_plot2$LT, y)),]

df_plot2$Regime = factor(as.character(df_plot2$Regime), levels = unique(df_plot2$Regime))
df_plot2$LT = factor(as.character(df_plot2$LT), levels = unique(df_plot2$LT))

# Boxplot for PRE vs Regime for different lead times
#----------------------------------------------------------------------------------------
p = ggplot(df_plot2, aes(x=Regime, y=value, fill = LT)) +
  geom_boxplot()+
  theme_bw() + 
  # theme(legend.position="none") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlab(" ") + ylab("PRE [%]")+#ylab("Correlation")+
  # scale_x_discrete(labels=c("correlation" = "Pearson correlation", "partial_correlation" = "Partial correlation"))+
  ylim(0,40)+
  scale_fill_brewer(palette="Blues")+
  guides(fill=guide_legend(title="LT (months)"))

p

# Export figure
ggsave(p,filename=paste0(wd$figure,"08_PRE_vs_intcoast_nosprppt.png",sep=""),
       width = 20, height = 15, units = "cm")


# Prepare data for correlation plot
#----------------------------------------------------------------------------------------
r2_all = data.frame(cbind(r2_all, Regime = as.character(df_plot2[,1])))

df_plot3 = data.frame(Regime = av_wint_T[,2],r2_withSM = as.numeric(as.character(r2_all[,1])),
                      r2_withoutSM = as.numeric(as.character(r2_all[,2])),LT = factor(r2_all[,3]))

df_plot3_tmp = df_plot3[df_plot3$Regime=='Maritime',]
tmp = (df_plot3_tmp$r2_withSM - df_plot3_tmp$r2_withoutSM)*100/df_plot3_tmp$r2_withoutSM
mean(tmp)


p2 = ggplot(df_plot3, aes(x=r2_withSM, y=r2_withoutSM, fill = Regime)) +
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1)+
  geom_point(aes(fill=Regime),color='black',pch=21,size=4,alpha =.3)+
  theme_bw()  +
  # theme(legend.position="none") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))+
  xlab(expression(paste(R^2 ," (with SM)"))) + ylab(expression(paste(R^2 ," (without SM)")))+#ylab("Correlation")+
  # scale_x_discrete(labels=c("correlation" = "Pearson correlation", "partial_correlation" = "Partial correlation"))+
  ylim(0,1)+ xlim(0,1)+
  scale_fill_manual(values=c( "skyblue3", "orangered2"))

# Export image
ggsave(p2,filename=paste0(wd$figure,"08_R2_compare_scatterplot_no_sprPPT.png",sep=""),
       width = 20, height = 15, units = "cm")


