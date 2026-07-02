# Causal Inference
# Nathan Acciai
# Final Project

#covariates age, married, nonwithe, educ, income,depress_pre

#assignement variable z

#Treatment receipt indicators

#outcome variable job_seek 
library(ggplot2)
library(tidyr)
library(dplyr)
library(glue)

setwd("Mattei_Causal_inference/Assignement_Final")
data <- read.table("JobsII.txt", header=TRUE, sep =" ")
n_total <- nrow(data)
n_treated <- table(data$w)
n_treated

# ex 1-3
#check for non-compliance problem, that dimostrate we have one-side non compliance problem 
table(data$z, data$w)

#Trasform categorical variable Incom in a dummy variable for each category
data$income <- as.factor(data$income)
dummy_income <- model.matrix(~ income - 1, data = data)
dati_completi <- cbind(data, dummy_income)
dati_completi

#Going to calculate the mean of each variable in full daatset
variables_name_for_mean <-setdiff(colnames(dati_completi),"income")
#full dataset mean
full_dataset_mean <- colMeans(dati_completi[, variables_name_for_mean], na.rm= TRUE)
#subsample mean for w
mean_for_w <- aggregate(dati_completi[, variables_name_for_mean], by = list(W = dati_completi$w), FUN = mean, na.rm = TRUE)
#subsample mean for z
mean_for_z <- aggregate(dati_completi[, variables_name_for_mean], by = list(Z = dati_completi$z), FUN = mean, na.rm = TRUE)

#we defined the covariates that are important to show how the treatment receipt is unbalanced now we go to show some plots for this 
covariate_unbalanced<-c("sex","age", "nonwhite", "educ","income50k+","incomelt15k")
plot_unbalanced_covariates <-function(full_dataset,w_dataset, z_dataset, name_covariate){
  par(mfrow = c(1, 3), mar = c(5, 4, 4, 1), oma = c(0, 0, 3, 0))
  
  
  col_z_w <- c("0" = "lightcoral", "1" = "lightskyblue3")
  y_label<- "Percentage"
  y_range <- c(0, 1)
  if(name_covariate=="age"){
    y_label<- "Value"
    y_range <- c(0,70)
  }
  
  val_full_dataset <- full_dataset[[name_covariate]]
  bp1 <- barplot(val_full_dataset, 
                 names.arg = "Full Dataset", 
                 col = "darkseagreen3", 
                 ylim = y_range, 
                 ylab = y_label,
                 main = glue("Full Dataset "))
  text(x = bp1, y = val_full_dataset, labels = round(val_full_dataset, 3), pos = 3, cex = 1.2)
  
  z_values <- z_dataset[order(z_dataset$Z), name_covariate]
  bp3 <- barplot(z_values, 
                 names.arg = c("Z = 0", "Z = 1"), 
                 col = col_z_w, 
                 ylim = y_range, 
                 ylab = y_label,
                 main = "By Assignment variable\n(Z)")
  text(x = bp3, y = z_values, labels = round(z_values, 3), pos = 3, cex = 1.2)
  
  
  w_values <- w_dataset[order(w_dataset$W), name_covariate]
  bp2 <- barplot(w_values, 
                 names.arg = c("W = 0", "W = 1"), 
                 col = col_z_w , 
                 ylim = y_range, 
                 ylab = y_label,
                 main = "By Treatment receipt \n(W)")
  text(x = bp2, y = w_values, labels = round(w_values, 3), pos = 3, cex = 1.2)
  

  
  
  mtext(glue::glue("Covariate: {name_covariate}"),
        outer = TRUE,
        col = "firebrick3",   
        cex = 1.6,            
        font = 2,             
        line = 1)             
  
  par(mfrow = c(1, 1))
}
for ( cov in covariate_unbalanced){
  plot_unbalanced_covariates(full_dataset_mean,mean_for_w,mean_for_z,cov)
}
plot_density_outcome <- function(data){
  xlim_val <- c(1, 5)
  
  # Calcoliamo la densità empirica
  dens_full <- density(data$job_seek, na.rm = TRUE)
  y_range <- c(0,0.7)
  # Disegniamo la curva di probabilità
  plot(dens_full, 
       xlim = xlim_val, 
       main = "Overall Density\n(Full Dataset)", 
       xlab = "Job Seek Score", 
       ylab = "Density", 
       ylim = y_range,
       col = "darkseagreen4", 
       lwd = 3)
  # Riempiamo la curva con un colore trasparente per estetica
  polygon(dens_full, col = rgb(0.5, 0.7, 0.5, 0.3), border = "darkseagreen4")
  
  
  dens_z0 <- density(data$job_seek[data$z == 0], na.rm = TRUE)
  dens_z1 <- density(data$job_seek[data$z == 1], na.rm = TRUE)
  
  
  plot(dens_z0, 
       xlim = xlim_val, 
       main = "Density by Assignment\n(Z Effect / ITT)", 
       xlab = "Job Seek Score", 
       ylab = "Density", 
       ylim = y_range,
       col = "khaki3", 
       lwd = 2)
  polygon(dens_z0, col = rgb(0.9, 0.9, 0.6, 0.2), border = NA)
  
  
  lines(dens_z1, col = "mediumpurple3", lwd = 3)
  polygon(dens_z1, col = rgb(0.6, 0.4, 0.8, 0.2), border = NA)
  
  
  legend("topleft", legend = c("Z = 0 (Control)", "Z = 1 (Assigned)"), 
         col = c("khaki3", "mediumpurple3"), lwd = c(2, 3), bty = "n", cex = 0.9)
  

  dens_w0 <- density(data$job_seek[data$w == 0], na.rm = TRUE)
  dens_w1 <- density(data$job_seek[data$w == 1], na.rm = TRUE)
  
  
  plot(dens_w0, 
       xlim = xlim_val, 
       main = "Density by Participation\n(W Effect / Naive)", 
       xlab = "Job Seek Score", 
       ylab = "Density", 
       ylim = y_range,
       col = "lightcoral", 
       lwd = 2)
  polygon(dens_w0, col = rgb(0.9, 0.5, 0.5, 0.2), border = NA)
  
  
  lines(dens_w1, col = "lightskyblue4", lwd = 3)
  polygon(dens_w1, col = rgb(0.5, 0.7, 0.9, 0.2), border = NA)
  
  legend("topleft", legend = c("W = 0 (Non-Part)", "W = 1 (Participant)"), 
         col = c("lightcoral", "lightskyblue4"), lwd = c(2, 3), bty = "n", cex = 0.9)
}
plot_density_outcome(dati_completi)

#5) Estimate the overall intention-to-treat effect on the primary outcome, Y

mean_treated <- mean(data$job_seek[data$z == 1], na.rm = TRUE)
mean_control <- mean(data$job_seek[data$z == 0], na.rm = TRUE)
ITT_Y <- mean_treated - mean_control
ITT_Y

ITT_W <- with(data, mean(data$w[data$z == 1]))

# CACE
CACE <- ITT_Y / ITT_W
CACE

#now we go to estimate the standard error via delta method
Y_1 <- data$job_seek[data$z == 1]
Y_0 <- data$job_seek[data$z == 0]

W_1 <- data$w[data$z == 1]
W_0 <- data$w[data$z == 0]

n_1 <- length(Y_1)
n_0 <- length(Y_0)

#calculate the variance for the group

var_ITT_Y <- var(Y_1)/n_1 + var(Y_0)/n_0
var_ITT_W <- var(W_1)/n_1 

#calculate the covariance
cov_1 <- cov(Y_1, W_1)/n_1
cov_2 <- cov(Y_0, W_0)/n_0
Cov_ITT <- cov_1 + cov_2

Var_CACE <- (var_ITT_Y / ITT_W^2) + ITT_Y^2 / ITT_W^4 * var_ITT_W - 2 * ITT_Y/ ITT_W^3 * Cov_ITT
SE_CACE <- sqrt(Var_CACE)
SE_CACE
#calcolus of the 1-alpha confidence interval of 95% of confidence

alpha <- 0.05
z <- qnorm(1 - alpha/2)

CI_lower <- CACE - z * SE_CACE
CI_upper <- CACE + z * SE_CACE

c(CI_lower, CI_upper)







