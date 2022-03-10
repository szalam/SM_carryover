# This code is used to calculate storage coefficient at selected stations

rm(list=ls())

# Libraries
library(lubridate)

# storing working directories
wd          = list()
wd$main     = 'D:/Project_soil_moisture/00_SM_project/'
wd$data     = paste0(wd$main,'01_data/101_K_data/')
wd$output   = paste0(wd$main,'03_results/')

# Set working directory
setwd(wd$data)

# Import data
#------------------------------------------------------

Qapp.list = readRDS("Qapp_list.rds")
# Qapp.list = readRDS("Qma_list.rds")
dt.list   = readRDS("dt_list.rds")
dqdt.list = readRDS("dqdt_list.rds")
p.list    = readRDS("p_list.rds")
dq.list   = readRDS("dq_list.rds")
dy.pkswe_list = readRDS("dy.pkswe_list.rds")
# Selected stations
st.select = read.csv('gages_K_calc.csv')

# Day of the yaer 
aug15     = 319


# Creating lists for storing data
K.logQ = K.logdqdt = list()
K.k1_b1 = K.k2_b1 = K.intercept_b1 = b_fit = K.k1_b32 = 0

# Looping through the stations for calculation of K
for(n in 1:nrow(st.select)) #n is station number
{
  print(n)
  st     = st.select[n,2]
  
  # the data we are working with
  Qapp                    =   Qapp.list[[n]]
  dqdt                    =   dqdt.list[[n]]
  dq                      =   dq.list[[n]]
  dt                      =   dt.list[[n]]
  ppt                     =   p.list[[n]]
  # swe                     =   DATA.swe.(id)(731:end,2);
  # t                       =   DATA.flow.(id)(:,1);
  
  # round ppt data to remove trace < 1 mm
  ppt                     =   round(ppt,0)

  # these will hold the screened data re-stitched at end of m loop
  Qapp_new                =   0
  dqdt_new                =   0
  dt_new                  =   0
  dq_new                  =   0

  
  for(m in 1:ncol(Qapp)){ # m represents year
    
    # want two versions, one for original data and one for screened
    Q_keep              =   Qapp[,m]
    Q_n                 =   Qapp[,m]
    dqdt_n              =   dqdt[,m]
    dq_n                =   dq[,m]
    dt_n                =   dt[,m]
    ppt_n               =   ppt[,m]
    # swe_n               =   movingmean(swers(:,m),3)
    # t_n                 =   t[,m]
    
    # date                =   datevec(t_n(1))
    # year                =   int2str(date(1))
    
    # day of peak swe
    dy.pkswe            =   dy.pkswe_list[[n]][m]
    if(is.na(dy.pkswe)==T){
      dy.pkswe.mean     =   round(mean(dy.pkswe_list[[n]],na.rm = T),0) #mean day of peak swe
    }else{
      dy.pkswe.mean     =   dy.pkswe
    }
    
    
    # screen out all locations where Q_n is NA. there might be NAs in the data
    qna_pos             =   which(is.na(Q_n)==T)
    Q_n[qna_pos]        =   NA
    dqdt_n[qna_pos]     =   NA
    dt_n[qna_pos]       =   NA
    dq_n[qna_pos]       =   NA
    
    # screen out data between peak swe (april 1) to agust 15
    ssd_n               =   dy.pkswe.mean
    Q_n[ssd_n:aug15]    =   NA
    dqdt_n[ssd_n:aug15] =   NA
    # dt_n[ssd_n:aug15]   =   NA
    # dq_n[ssd_n:aug15]   =   NA
    dq[ssd_n:aug15,m]   =   NA
    dt[ssd_n:aug15,m]   =   NA
    
    # screen out precip and 3 days after
    ppt_pos             =   which(ppt_n > 0)
    ppt_pos1            =   ppt_pos + 1
    ppt_pos2            =   ppt_pos + 2
    ppt_pos3            =   ppt_pos + 3
    ppt_pos             =   unique(c(ppt_pos,ppt_pos1,ppt_pos2,ppt_pos3))
    id.ppt.pos          =   which(ppt_pos>365)
    ppt_pos             =   ppt_pos[-id.ppt.pos]
    #ppt with NA values remove, if anh
    # ppt_pos_tmp         =   which(is.na(ppt_n)==T)
    # ppt_pos             = c(ppt_pos,ppt_pos_tmp)
    # ppt_pos[ppt_pos>365]=  NA
    
    Q_n[ppt_pos]        =   NA
    dqdt_n[ppt_pos]     =   NA
    dt_n[ppt_pos]       =   NA
    dq_n[ppt_pos]       =   NA
    
    # screen out dqdt >= 0
    dqdt_pos            =   which(dqdt_n >= 0)
    Q_n[dqdt_pos]       =   NA
    dqdt_n[dqdt_pos]    =   NA
    dq_n[dqdt_pos]      =   NA
    dt_n[dqdt_pos]      =   NA
    
    
    # put it back together into a long timeseries
    Qapp_new            =   cbind(Qapp_new,Q_n)
    dqdt_new            =   cbind(dqdt_new,dqdt_n)
    dt_new              =   cbind(dt_new,dt_n)
    dq_new              =   cbind(dq_new,dq_n)
  }
  
  Qapp_new                = Qapp_new[,-1]
  dqdt_new                = dqdt_new[,-1]
  dt_new                  = dt_new[,-1]
  dq_new                  = dq_new[,-1]
  
  # ttt = rowMeans(Qapp_new,na.rm = T)
  # plot(ttt)
  
  # #calculate row mean
  # Qapp_new                = rowMeans(Qapp_new,na.rm = T)
  # dqdt_new                = rowMeans(dqdt_new,na.rm = T)
  # dt_new                  = rowMeans(dt_new,na.rm = T)
  # dq_new                  = rowMeans(dq_new,na.rm = T)

  
  # plot(c(Qapp_new),c(dqdt_new))
  
  # remove nan
  Qapp_new                =   Qapp_new[!is.na(Qapp_new)]
  dqdt_new                =   dqdt_new[!is.na(dqdt_new)]
  dt_new                  =   dt_new[!is.na(dt_new)]
  dq_new                  =   dq_new[!is.na(dq_new)]
  
  # take the logarithms and fit regressions 
  logQ                    =   log((Qapp_new))
  logdqdt                 =   log(abs(dqdt_new))
  
  # plot(logQ,logdqdt)
  
  lower_bound         =   quantile(logQ,prob = c(.1),na.rm = T)
  lower_inds          =   which(logQ<=as.numeric(lower_bound))
  logQlower           =   logQ[lower_inds]
  logdqdtlower        =   logdqdt[lower_inds]
  

  
  # Ordinary least square, force slope to be 1
  b_fit_tmp                   =   mean((logdqdtlower - logQlower),na.rm = T)
  K.k1_b1_tmp                 =   1/exp(b_fit_tmp)
  K.k2_b1_tmp                 =   exp(-1/K.k1_b1_tmp)
  K.intercept_b1_tmp          =   b_fit_tmp # note, this is the intercept
  
  # Concataion of all values
  b_fit                       = c(b_fit,b_fit_tmp)
  K.k1_b1                     = c(K.k1_b1,K.k1_b1_tmp)
  K.k2_b1                     = c(K.k2_b1,K.k2_b1_tmp)
  K.intercept_b1              = c(K.intercept_b1, K.intercept_b1_tmp)
  
  # K.k1_b32                     = c(K.k1_b32,K.k1_b1_tmp32)
  
  # put the logQ and logdqdt data in the structure
  K.logQ[[n]]                 =   logQ
  K.logdqdt[[n]]              =   logdqdt
  
}

K.k1_b1 = K.k1_b1[-1]
K.k2_b1 = K.k2_b1[-1]
K.intercept_b1 = K.intercept_b1[-1]
b_fit = b_fit[-1]

# K.k1_b32 = K.k1_b32[-1]

summary(K.k1_b1)
summary(K.intercept_b1_tmp)
summary(K.k2_b1)

df.k.all = data.frame(st = st.select , k = K.k1_b1 )
# df.k.all = data.frame(st = st.select , k = K.k1_b32 )


#Export data
write.csv(df.k.all, paste0(wd$output, '10_k_all.csv'))