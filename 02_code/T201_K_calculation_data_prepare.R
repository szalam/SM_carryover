# This code is used to calculate storage coefficient at selected stations

rm(list=ls())

# Libraries
library(lubridate)

# Directories
wd = list()
wd$main = 'D:/Project_soil_moisture/00_SM_project/'
wd$data     = 'C:/sarfaraz/Project_soil_moisture/storage_coeff_K/data/USGS_discharge_cfs/' # zip of the usgs is kept in 01_data folder
wd$vic_data = 'C:/sarfaraz/Project_soil_moisture/storage_coeff_K/data/vic_all_data/' # large size vic outputs
wd$st.loc = wd$st_list   = paste0(wd$main,'01_data/')
wd$output   = paste0(wd$main,'01_data/101_K_data/')

limit = 0#2.8 #/1000000 

# NOTE: flow data is in cfs and the raw data from USGS is reported to the
# nearest 0.1 cfs, which is equivalent to 0.00283 cms. In the code, I
# convert the flow to cms, then again to cubic mm per second, and round to
# the nearest mm, hence the limit is 0.00283 converted to mm = 2.8 mm//
# 
# NOTE: updated flow data is now in cms, but the other info is accurate.
# raw flow data is cfs to nearest 0.1 cfs, but also, gage data is to neares
# 0.01 foot, so could take that into consideration. Also noticed that sub
# daily data is availabe which would be better for computing k but gonna
# ignore that for now ...
# 
# NOTE: In new DATA, the swe and ppt data go back two years prior to first
# flow year. not sure if that was taken into account in this code here


setwd(wd$st_list)

st.new.list = read.csv(paste0('Runoff_M3_WY1965_2018_New2_file_stationIDs.csv'),header=T)
q.data = read.csv(paste0('Runoff_M3_WY1965_2018_New2.csv'),header=T)
q.data = q.data[(1:nrow(q.data)-1),]

#read basin clustering file. this has station ids to be considered
# df.tmp    = read.csv('YearlyData_Basin_Clustered.csv')
# st_avg_info = read.csv(paste0(wd$st.loc,'AvgInfo_SelectedBasins_Obs.csv'))
# st.select = st_avg_info[,3]
# st.select = unique(df.tmp[,1]) #total 35 stations

st_avg_info = read.csv(paste0(wd$st.loc,'AvgInfo_SelectedBasins_Obs.csv'))
st.select = st_avg_info[,3]
d.area = st_avg_info[,76]*1000000 #original in km2, converting to m2

#process usgs streamflows to calculate storage coefficient

#storing data in lists
Qapp.list = dt.list = dqdt.list = p.list = dq.list = swe.list = dy.pkswe_list = Qma.list = list()

for(i in 1:length(st.select))
{
  id.st.match = which(st.select[i] == st.new.list)
  df = q.data[,id.st.match]
  d.area.sel = d.area[i] # m2
  
  #convert df to mm
  df = df*1000/d.area.sel
  
  #create a continuous time series
  # t         = seq(as.Date(df[1,1]),as.Date(df[nrow(df),1]),by = 1)
  t         = seq(as.Date("1964-10-01"),as.Date("2018-09-30"),by = 1)#original 2018
  
  #dataframe with continous date and q data
  df.tmp    = data.frame(time = t, q = df)
  #store month and years
  mon.org   = month(df.tmp[,1])
  yr.org    = year(df.tmp[,1])
  dy.org    = day(df.tmp[,1])
  
  
  #   print(df.tmp[nrow(df.tmp),1])
  # }
  
  #remove feb 29
  id.rm     = which(month(df.tmp[,1]) == 2 & day(df.tmp[,1]) == 29)
  df.tmp    = df.tmp[-id.rm,]
  
  
  print(df.tmp[1,1])
  print(df.tmp[nrow(df.tmp),1])
  
  #converting the q format to 365 days row and each column as year
  yrs.tmp      = year(df.tmp[,1])
  yrs.tmp      = unique(yrs.tmp)
  yrs.tmp      = yrs.tmp[1:(length(yrs.tmp)-1)]
  # 
  # print(sum(df.tmp[,2],na.rm = T))
  tmp          = df.tmp[1:365,2]
  for(j in 2:length(yrs.tmp)){
    tmp              = cbind(tmp, df.tmp[((365*(j-1)+1):(365*j)),2])
  }
  # print(sum(tmp, na.rm = T))
  
  q.new = tmp
  
  #calculate dq/dt
  Qapp         = data.frame(matrix(NA, nrow = nrow(q.new), ncol = ncol(q.new)))
  dq           = data.frame(matrix(NA, nrow = nrow(q.new), ncol = ncol(q.new)))
  dt           = data.frame(matrix(NA, nrow = nrow(q.new), ncol = ncol(q.new)))
  dqdt         = data.frame(matrix(NA, nrow = nrow(q.new), ncol = ncol(q.new)))
  
  #initializing
  Qapp[1,]     = q.new[1,]
  dq[1,]       = 0
  dqdt[1,]     = 0
  dt[1,]       = 1
  
  # moving average function
  ma           = function(x , n = 3){filter(x, rep(1 / n, n), sides = 2)}
  t.dy         = 1:365
  
  print('line 153 done')
  q.new[is.nan(q.new)] = NA
  
  for(m in 1:ncol(q.new)){ # m indicates year
    
    # q moving average calculation with 3 day time window
    q.ma               = ma(q.new[,m])
    q.ma[1]            = q.new[1,m]
    q.ma[length(q.ma)] = q.new[nrow(q.new),m]
    
    for(n in 2:365){
      
      for(j in 1:(n-1)) 
      {
        dq[n,m] = q.ma[n] - q.ma[(n-j)]
        
        if(is.na(dq[n,m])==T){break}
        
        if(dq[n,m] >= 0 | (round(abs(dq[n,m]),1) > limit)){#(round(abs(dq[n,m]),1) > limit)){ # was >=
          
          Qapp[n,m]     =   1/(j+1) * sum(q.ma[(n-j:n)],na.rm = T)
          dt[n,m]       =   t.dy[n]- t.dy[n-j]
          dqdt[n,m]     =   dq[n,m]/dt[n,m]
          # print('ok')
          break
          
        }else{
          Qapp[n,m] = NA
          dt[n,m] = NA
          dqdt[n,m] = NA
        }
      }
      
    }
  }
  
  print('line 186 done')
  
  Qma.list[[i]] = q.ma
  Qapp.list[[i]] = Qapp
  dq.list[[i]]   = dq
  dt.list[[i]]   = dt
  dqdt.list[[i]] = dqdt
  
  rm(Qapp,dt,dqdt,q.ma,dq)
  
  #===========================
  # process precipitation data
  
  setwd(wd$vic_data)
  
  #condition for reading stations with ids below 10000000
  if(st.select[i] < 10000000){
    df = read.csv(paste0('fluxes_0',st.select[i],'.csv'),sep =' ',header=F)
  }else{
    df = read.csv(paste0('fluxes_',st.select[i],'.csv'),sep =' ',header=F)
  }
  
  df           = df[,c(1,2,3,4)]
  colnames(df) = c('year','month','day','p')
  
  df.p = data.frame(df)
  
  
  id.1 = which((df.p[,3])==1 & (df.p[,2])==10 & (df.p[,1])==1964)
  id.2 = which((df.p[,3])==30 & (df.p[,2])==9 & (df.p[,1])==2018)
  
  df.p = df.p[id.1:id.2,]
  
  id.rm     = which((df.p[,3])==29 & (df.p[,2])==2)
  df.p    = df.p[-id.rm,]
  head(df.p)
  
  t         = seq(as.Date("1964-10-01"),as.Date("2018-09-30"),by = 1)#original 2018
  t = t[-id.rm]
  
  
  df.p = data.frame(date = t, p = df.p[,4])
  
  tmp = df.p[1:365,2]
  
  for(j in 2:length(yrs.tmp)){
    tmp = cbind(tmp, df.p[((365*(j-1)+1):(365*j)),2])
  }
  
  p.list[[i]] = tmp
  
  #===========================
  # process SWE data
  
  setwd(wd$vic_data)
  
  #condition for reading stations with ids below 10000000
  if(st.select[i] < 10000000){
    df = read.csv(paste0('fluxes_0',st.select[i],'.csv'),sep =' ',header=F)
  }else{
    df = read.csv(paste0('fluxes_',st.select[i],'.csv'),sep =' ',header=F)
  }
  
  df           = df[,c(1,2,3,12)]
  colnames(df) = c('year','month','day','swe')
  
  df.swe = data.frame(df)
  
  
  id.1 = which((df.swe[,3])==1 & (df.swe[,2])==10 & (df.swe[,1])==1964)
  id.2 = which((df.swe[,3])==30 & (df.swe[,2])==9 & (df.swe[,1])==2018)
  
  df.swe = df.swe[id.1:id.2,]
  
  id.rm     = which((df.swe[,3])==29 & (df.swe[,2])==2)
  df.swe    = df.swe[-id.rm,]
  head(df.swe)
  
  t         = seq(as.Date("1964-10-01"),as.Date("2018-09-30"),by = 1)#original 2018
  t = t[-id.rm]
  
  
  df.swe = data.frame(date = t, swe = df.swe[,4])
  
  tmp = df.swe[1:365,2]
  
  for(j in 2:length(yrs.tmp)){
    tmp = cbind(tmp, df.swe[((365*(j-1)+1):(365*j)),2])
  }
  
  swe.list[[i]] = tmp
  
  
  
  # calculate moving average swe
  dy.pkswe= 0
  swe.ma = tmp
  for (l in 1:ncol(tmp)) {
    swe.ma[,l]               = ma(tmp[,l])
    dy.pkswe = c(dy.pkswe, which(swe.ma[,l] == max(swe.ma[,l],na.rm = T)))
  }
  
  dy.pkswe = dy.pkswe[-1]
  
  dy.pkswe_list[[i]]  = dy.pkswe
  
  
  print(paste0('i=',i))
}

setwd(wd$output)
st.select = data.frame(st.select)
write.csv(st.select,'gages_K_calc.csv') 

saveRDS(Qapp.list, "Qapp_list.rds")
saveRDS(dq.list, "dq_list.rds")
saveRDS(dt.list, "dt_list.rds")
saveRDS(dqdt.list, "dqdt_list.rds")
saveRDS(p.list, "p_list.rds")
saveRDS(dy.pkswe_list, "dy.pkswe_list.rds")
saveRDS(Qma.list, "Qma_list.rds")



