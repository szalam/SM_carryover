################################################################################
# Title: Analyze Soil moisture change (ΔSM) and mean air temperature (T) during 
#       (a) November, (b) December, and (c) January
# Author: Sarfaraz Alam
################################################################################

# Clear workspace
rm(list = ls())

# Libraries
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)

# Directories
wd = list()
wd$main = 'C:/sarfaraz/SM_carryover/'
wd$data = paste0(wd$main, '01_data/')
wd$output = paste0(wd$main, '03_results/')
wd$figure = paste0(wd$main, '04_figure/')

setwd(wd$data)

# Read data
data = read.csv(paste0('85_40NA/Csv_Files/AvgInfo_Basins_Cluster_40NA_SMChangeClimate.csv'), header = TRUE, fill = TRUE)

head(data, 1)

df = data[, c("LatGauge", "LonGauge", "GAGEID", "FirstDayBelow0Temp", "NumbDaysBelow0Temp", "MaxConsDaysBelow0Temp", "FirstDayAbove0Temp", "SM_0TempDay", "SM_PosTempDay", "ClusterMode")]
head(df, 1)

tmp = df$ClusterMode
tmp = cbind(tmp, 'temp')
tmp = data.frame(tmp)
tmp[, 1] = as.numeric(as.character(tmp[, 1]))
tmp[, 2] = as.character(tmp[, 2])
tmp[which(tmp[, 1] == 1), 2] = 'Interior'
tmp[which(tmp[, 1] == 2), 2] = 'Maritime'
colnames(tmp) = c('Ratio', 'Regime')

df = cbind(df, Regime = tmp[, 2])

df.tmp = df[, c(4, 11)]

mdat = melt(df.tmp, id.var = c('Regime'))

# Plot: Day of water year (DOWY) below 0-degrees
p1 = ggplot(mdat, aes(x = Regime, y = value, fill = Regime)) +
  geom_boxplot(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")) +
  xlab(" ") + ylab("First DOWY T <= 0 C") +
  scale_fill_manual(values = c("skyblue3", "orangered2")) +
  guides(fill = guide_legend(title = "Regime"))

ggsave(p1, filename = paste0(wd$figure, "05_DOWY_T_below_0C.png"), width = 14, height = 15, units = "cm")

result <- mdat %>%
  group_by(Regime) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    StandardDeviation = sd(value, na.rm = TRUE)
  )

print(result)

# Plot: Days below 0-degrees
df.tmp = df[, c(5, 11)]

mdat = melt(df.tmp, id.var = c('Regime'))

p2 = ggplot(mdat, aes(x = Regime, y = value, fill = Regime)) +
  geom_boxplot(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")) +
  xlab(" ") + ylab("Number of days with T <= 0 C") +
  scale_fill_manual(values = c("skyblue3", "orangered2")) +
  guides(fill = guide_legend(title = "Regime"))

p2
ggsave(p2, filename = paste0(wd$figure, "05_days_T_below_0C.png"), width = 14, height = 15, units = "cm")

result <- mdat %>%
  group_by(Regime) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    StandardDeviation = sd(value, na.rm = TRUE)
  )

print(result)

# Plot: SM pre 0-degree vs post 0-degree
df.tmp = df[, c(8, 9, 11)]

p3 = ggplot(df.tmp, aes(x = SM_0TempDay, y = SM_PosTempDay, color = Regime)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +
  geom_point(aes(fill = Regime), color = 'black', pch = 21, size = 4, alpha = 0.6) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")) +
  xlab("SM_pre_T0C [mm]") + ylab("SM_post_T0C [mm]") +
  guides(fill = guide_legend(title = "Regime")) +
  scale_fill_manual(values = c("skyblue3", "orangered2")) +
  scale_y_continuous(breaks = seq(200, 1300, by = 200), limits = c(200, 1300)) +
  scale_x_continuous(breaks = seq(200, 1300, by = 200), limits = c(200, 1300))
p3

# Calculating correlation by Regime
correlation_by_regime <- df.tmp %>%
  group_by(Regime) %>%
  summarize(Correlation = cor(SM_0TempDay, SM_PosTempDay, use = "complete.obs"))

print(correlation_by_regime)

ggsave(p3, filename = paste0(wd$figure, "05_SM_pre_post_below_0C.png"), width = 17, height = 15, units = "cm")

