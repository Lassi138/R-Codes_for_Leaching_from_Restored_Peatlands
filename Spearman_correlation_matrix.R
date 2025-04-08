library(dplyr) # Data handling
library(ggplot2) # Plotting package
library(viridis)
library(GGally)
library(scales)
hrbrthemes::import_roboto_condensed()
library(lubridate) # Date and time handling
library(ggcorrplot)

# Set working directory
setwd('C:/Users/lpakkila/OneDrive - Oulun yliopisto/Lassin väitöskirjatyö/Peatland water table and quality/Water_quality/Porewater_vs_runoff_quality/')
# Read data to a data frame
regres <- read.csv('porewater_and_runoff_quality_regression_V2.csv', 
                   header=TRUE, sep =";", quote="", dec=".", fill=TRUE, comment.char="", skipNul=FALSE)
regres$peatland <- as.numeric(regres$peatland)
regres$date <- dmy(regres$date, tz=NULL)
regres$fyear <- as.numeric(regres$fyear)
regres$r_NO23N_ug_l <- as.numeric(regres$r_NO23N_ug_l)
regres$r_SS_mg_l <- as.numeric(regres$r_SS_mg_l)
regres$r_Fe_ug_l <- as.numeric(regres$r_Fe_ug_l)
regres$p_tot_N_ug_l <- as.numeric(regres$p_tot_N_ug_l)

# Add period column
regres$period <- NA
regres$period[regres$fyear < 0] <- "<0"
regres$period[regres$fyear >= 0 & regres$fyear <= 1] <- "0-1"
regres$period[regres$fyear > 1 & regres$fyear <= 5] <- "1-5"
regres$period[regres$fyear > 5] <- ">5"
regres$period <- ifelse(is.na(regres$fyear), "Pristine", regres$period)
regres$period <- as.factor(regres$period)

# Add year column
regres$year <- NA
regres$year <- as.factor(format(regres$date,'%Y'))

# Calculate Spearman's correlation matrix for 'regres': drained and restored data
regres2 <- regres[regres$res_pris == 1,]
regres.cor_s <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")
regres.cor_p <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("pearson"), use = "pairwise.complete.obs")
regres.cor_dif <- round((regres.cor_s-regres.cor_p),1)

ggcorrplot(regres.cor_dif, 
           outline.col = "black",
           lab = TRUE,
           digits = 2,
           legend.title = "Spearman's \u03C1 - Pearson's \u03C1",
           title = "Drained and restored sites")

regres.cor_round <- round(regres.cor_s,1)
p.mat <- cor_pmat(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")

ggcorrplot(regres.cor_round, 
           outline.col = "black",
           lab = TRUE,
           digits = 2,
           legend.title = "Spearman's \u03C1",
           title = "Drained and restored sites")

# Calculate Spearman's correlation matrix for 'regres' Subset: restored state
regres2 <- regres[regres$res_pris == 1 & regres$fyear >= 0,]
regres.cor <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")
p.mat <- cor_pmat(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")

ggcorrplot(regres.cor, 
           outline.col = "black",
           lab = TRUE,            
           digits = 2,
           lab_size = 3,
           p.mat = p.mat,
           sig.level = 0.05,
           insig = "blank",
           legend.title = "Spearman's \u03C1",
           title = "Restored sites (years >= 0), p=0.001",)

rows <- c(1,2,21,22,23,24,25,26,27)
columns <- c(3:20)

rhos <- regres.cor[rows, columns]
ps <- p.mat[rows, columns]
write.table(rhos, file = "restored_rhos.csv", sep = ";", col.names = TRUE, row.names= TRUE)
write.table(ps, file = "restored_ps.csv", sep = ";", col.names = TRUE, row.names= TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.05, TRUE, FALSE)
write.table(ps, file = "restored_ps_test0.05.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.01, TRUE, FALSE)
write.table(ps, file = "restored_ps_test0.01.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.001, TRUE, FALSE)
write.table(ps, file = "restored_ps_test0.001.csv", sep = ";", col.names = TRUE, row.names = TRUE)

# Calculate Spearman's correlation matrix for 'regres' Subset: drained state
regres2 <- regres[regres$res_pris == 1 & regres$fyear < 0,] 
regres.cor <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")
p.mat <- cor_pmat(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")

ggcorrplot(regres.cor, 
           outline.col = "black",
           lab = TRUE,            
           digits = 2,
           lab_size = 3,
           p.mat = p.mat,
           sig.level = 0.05,
           insig = "blank",
           legend.title = "Spearman's \u03C1",
           title = "Drained sites (years <0), p=0.001",)

rows <- c(1,2,21,22,23,24,25,26,27)
columns <- c(3:20)

rhos <- regres.cor[rows, columns]
ps <- p.mat[rows, columns]
write.table(rhos, file = "drained_rhos.csv", sep = ";", col.names = TRUE, row.names= TRUE)
write.table(ps, file = "drained_ps.csv", sep = ";", col.names = TRUE, row.names= TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.05, TRUE, FALSE)
write.table(ps, file = "drained_ps_test0.05.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.01, TRUE, FALSE)
write.table(ps, file = "drained_ps_test0.01.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.001, TRUE, FALSE)
write.table(ps, file = "drained_ps_test0.001.csv", sep = ";", col.names = TRUE, row.names = TRUE)

# Calculate Spearman's correlation matrix for 'regres' Subset: restored state 0 to 5 years since restoration
regres2 <- regres[regres$res_pris == 1 & regres$fyear <= 5 & regres$fyear >= 0,]
regres.cor <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")
p.mat <- cor_pmat(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")

ggcorrplot(regres.cor, 
           outline.col = "black",
           lab = TRUE,            
           digits = 2,
           lab_size = 3,
           p.mat = p.mat,
           sig.level = 0.05,
           insig = "blank",
           legend.title = "Spearman's \u03C1",
           title = "Restored sites (years 0-5), p=0.001",)

rows <- c(1,2,21,22,23,24,25,26,27)
columns <- c(3:20)

rhos <- regres.cor[rows, columns]
ps <- p.mat[rows, columns]
write.table(rhos, file = "restored_0-5_rhos.csv", sep = ";", col.names = TRUE, row.names= TRUE)
write.table(ps, file = "restored_0-5_ps.csv", sep = ";", col.names = TRUE, row.names= TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.05, TRUE, FALSE)
write.table(ps, file = "restored_0-5_ps_test0.05.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.01, TRUE, FALSE)
write.table(ps, file = "restored_0-5_ps_test0.01.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.001, TRUE, FALSE)
write.table(ps, file = "restored_0-5_ps_test0.001.csv", sep = ";", col.names = TRUE, row.names = TRUE)

# Calculate Spearman's correlation matrix for 'regres' Subset: restored state over 5 years since restoration
regres2 <- regres[regres$res_pris == 1 & regres$fyear > 5,]
regres.cor <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")
p.mat <- cor_pmat(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")

ggcorrplot(regres.cor, 
           outline.col = "black",
           lab = TRUE,            
           digits = 2,
           lab_size = 3,
           p.mat = p.mat,
           sig.level = 0.05,
           insig = "blank",
           legend.title = "Spearman's \u03C1",
           title = "Restored sites (years >= 5), p=0.001",)

rows <- c(1,2,21,22,23,24,25,26,27)
columns <- c(3:20)

rhos <- regres.cor[rows, columns]
ps <- p.mat[rows,columns]
write.table(rhos, file = "restored_over5_rhos.csv", sep = ";", col.names = TRUE, row.names= TRUE)
write.table(ps, file = "restored_over5_ps.csv", sep = ";", col.names = TRUE, row.names= TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.05, TRUE, FALSE)
write.table(ps, file = "restored_over5_ps_test0.05.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.01, TRUE, FALSE)
write.table(ps, file = "restored_over5_ps_test0.01.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.001, TRUE, FALSE)
write.table(ps, file = "restored_over5_ps_test0.001.csv", sep = ";", col.names = TRUE, row.names = TRUE)

# Calculate Spearman's correlation matrix for 'regres' Subset: pristine state
regres2 <- regres[regres$res_pris == 2,]
regres.cor <- cor(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")
p.mat <- cor_pmat(regres2[,-c(1:8,15,16,38,39,40)], method = c("spearman"), use = "pairwise.complete.obs")

ggcorrplot(regres.cor, 
           outline.col = "black",
           lab = TRUE,            
           digits = 2,
           lab_size = 3,
           p.mat = p.mat,
           sig.level = 0.05,
           insig = "blank",
           legend.title = "Spearman's \u03C1",
           title = "Pristine sites, p=0.001",)

rows <- c(1,2,21,22,23,24,25,26,27)
columns <- c(3:20)

rhos <- regres.cor[rows, columns]
ps <- p.mat[rows, columns]
write.table(rhos, file = "pristine_rhos.csv", sep = ";", col.names = TRUE, row.names= TRUE)
write.table(ps, file = "pristine_ps.csv", sep = ";", col.names = TRUE, row.names= TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.05, TRUE, FALSE)
write.table(ps, file = "pristine_ps_test0.05.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.01, TRUE, FALSE)
write.table(ps, file = "pristine_ps_test0.01.csv", sep = ";", col.names = TRUE, row.names = TRUE)

ps <- ifelse(p.mat[rows, columns] <= 0.001, TRUE, FALSE)
write.table(ps, file = "pristine_ps_test0.001.csv", sep = ";", col.names = TRUE, row.names = TRUE)

