# Clear workspace
rm(list=ls())

# Load libraries
library(ggplot2)

# Set directories
wd = list(
  main = 'C:/sarfaraz/SM_carryover/',
  data = '01_data/',
  code = '02_code/',
  fn = '02_code/',
  output = '03_results/',
  figure = '04_figure/'
)
setwd(paste0(wd$data))

# Load function
source(paste0(wd$code, '041_pcor.R'))

# Define column names
column_names = c(
  "Nov1SM", "AvgWinterTemp", "NovTemp", "DecTemp", "VolSMChangeWint",
  "NovSM", "NovSWE", "DecSWE", "JanTemp", "FebTemp",
  "Dec1SM", "Jan1SM", "Feb1SM", "Mar1SM", "Apr1SM",
  "NovPrecip", "DecPrecip", "JanPrecip"
)

# List files
file_list = list.files(paste0(wd$data, '85_40NA/Runoff_Sel/'), full.names = TRUE)
file_name = substr(file_list, 11, 18)

# Read files using loop
sumry_all = data.frame()
for (i in 1:length(file_list)) {
  df = read.csv(file_list[[i]])
  df = df[, column_names]
  df = cbind(df,
              delnovSM = (df[, "Dec1SM"] - df[, "Nov1SM"]),
              deldecSM = (df[, "Jan1SM"] - df[, "Dec1SM"]),
              deljanSM = (df[, "Feb1SM"] - df[, "Jan1SM"]))
  
  id_nan = which(is.nan(df[,'NovSM']) == TRUE)
  if (length(id_nan) > 0) {
    df = df[-id_nan, ]
  }
  
  sumry_all = rbind(sumry_all, df)
}

sumry_all = sumry_all[-1,]

#==================================================================
# Scatter plot
#==================================================================

# Plot for Nov
p1 = ggplot(sumry_all, aes(x = NovTemp, y = delnovSM)) +
  geom_point(alpha = 0.2, size = 1.5, color = 'steelblue') + 
  theme_bw() +
  xlab(expression(paste("T"[Nov], " (degree C)"))) +
  ylab(expression(paste(Delta, "SM"[Nov], " (mm)"))) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  xlim(c(-25, 10)) +
  scale_y_continuous(limits = c(-100, 500), breaks = seq(-100, 500, 100))

ggsave(p1, filename = paste0(wd$figure, "05_delSMnov_vs_Tnov.png"), width = 12, height = 12, units = "cm")

# Plot for Dec
p2 = ggplot(sumry_all, aes(x = DecTemp, y = deldecSM)) +
  geom_point(alpha = 0.2, size = 1.5, color = 'steelblue') + 
  theme_bw() +
  xlab(expression(paste("T"[Dec], " (degree C)"))) +
  ylab(expression(paste(Delta, "SM"[Dec], " (mm)"))) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  xlim(c(-25, 10)) +
  scale_y_continuous(limits = c(-100, 500), breaks = seq(-100, 500, 100))

ggsave(p2, filename = paste0(wd$figure, "05_delSMdec_vs_Tdec.png"), width = 12, height = 12, units = "cm")

# Plot for Jan
p3 = ggplot(sumry_all, aes(x = JanTemp, y = deljanSM)) +
  geom_point(alpha = 0.2, size = 1.5, color = 'steelblue') + 
  theme_bw() +
  xlab(expression(paste("T"[Jan], " (degree C)"))) +
  ylab(expression(paste(Delta, "SM"[Jan], " (mm)"))) +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  xlim(c(-25, 10)) +
  scale_y_continuous(limits = c(-100, 500), breaks = seq(-100, 500, 100))

ggsave(p3, filename = paste0(wd$figure, "05_delSMjan_vs_Tjan.png"), width = 12, height = 12, units = "cm")

setwd(wd$output)
tmp_all = data.frame(tmp_all)

# write.csv(tmp_all, 'corr_pcor_compare_only_significant.csv')
# write.csv(sumry_all, 'mean_SR_PKSW_P_NT.csv')
