
# Causal Inference
# Assignment 2
# Authors: Group 2 (Acciai Nathan, Canti Edoardo, Tarlini Davide)


library(dplyr)
library(sandwich)
library(lmtest)


data <- read.table("Karolinska.txt", header = TRUE, sep= " ")
n_total <- nrow(data)
n_groups <- table(data$hvdiag)
n_groups


#1)
#Valutare il bilanciamento e la sovrapposizione nelle covariate
# verificare se i due gruppi (trattati e controlli) sono simili rispetto alle covariate

#a) Ispezionare il numero totale di unità e quante appartengono a ogni gruppo di trattamento
#calcolare stat descrittive, media per campione e per ogni gruppo

#Media per tutte le covariate e per gruppo
covariates <- c("age", "rural", "male")

mean_total <- colMeans(data[, covariates])
mean_group <- aggregate(data[,covariates], by= list(hvdiag= data$hvdiag), FUN=mean)

cat("\n -- Total Mean ---\n")
print(mean_total)
cat("\n -- Total group ---\n")
print(mean_group)


#Barplot for the age of the tratment-control units
boxplot(age ~ hvdiag, data = data,
        main = "Age Distributions by treatment",
        col = c("lightblue", "orange"),
        xlab = "Hospital (0=S, 1=L)", ylab = "Years")

# Barplot per Rural
counts_rural <- table(data$rural, data$hvdiag)
counts_rural
barplot(prop.table(counts_rural, 2), beside = TRUE, 
        main = "Distribution of Area Type",
        col = c("#cccccc", "#78c679"),
        xlab = "Hospital (0=Small, 1=Large)", ylab = "proportion",
        ylim = c(0, 1.2),
        legend.text = c("Urban", "Rural"),
        args.legend = list(x = "topright", bty = "n", cex = 0.8))


# Barplot per Male
counts_male <- table(data$male, data$hvdiag)
barplot(prop.table(counts_male, 2), beside = TRUE, 
        main = "Distribution of Gender",
        col = c("#f7adad", "#85a3e0"),
        xlab = "Hospital (0=Small, 1=Large)", ylab = "proportion",
        ylim = c(0, 1.2), # Più spazio per la legenda
        legend.text = c("Woman", "Men"),
        args.legend = list(x = "topright", bty = "n", cex = 0.8))


surv_stats <- aggregate(survival ~ hvdiag, data = data, FUN = mean)
colnames(surv_stats) <- c("Hospital", "Ratio_Survival")
cat("--- Survival ratio ---\n")
print(surv_stats)

barplot(surv_stats$Ratio_Survival, 
        names.arg = c("Small (0)", "Large (1)"),
        main = "Survaival ratio of 1 year",
        col = c("#fbb4ae", "#b3cde3"),
        ylab = "Proportion survival",
        ylim = c(0, 0.5), # La sopravvivenza è intorno al 35-36%
)
# Aggiungo il valore esatto sopra le colonne
text(x = c(0.7, 1.9), y = surv_stats$Ratio_Survival + 0.02, 
     labels = round(surv_stats$Ratio_Survival, 3), cex = 1.5)

#Quello che si può vedere è questo.
#ETA: ha una differenza di 3 anni quindi i pazzienti negli ospedali grandi sono poco più anziani 
#che comunque potrebbe fare differenza essendo che aumenta il fattore di rischio per la mortalità
#RURALE: aui c'è una grossa differenza si parla di un 30% in più di popolazione negli ospedali piccoli che viene da campagna rispetto alla zona urbana
#Questa differenza potrebbe essere dato da diversi fattori, ma non va molto bne
#Maschio: C'è una maggiore concentrazione di maschi negli ospedali piccoli rispetto a quelli grandi di un 12%
#potrebbe portare sbiulanciamento.
#A prima vista, sembrerebbe che gli ospedali grandi siano peggiori (sopravvivenza più bassa del
# 1.3%). Tuttavia, grazie all'analisi delle covariate fatta prima, sappiamo che gli ospedali piccoli
# trattano pazienti molto più anziani e rurali. Questo è il classico esempio di bias da confondimento:
# i pazienti negli ospedali grandi potrebbero essere in condizioni cliniche iniziali più gravi che non
# sono catturate dalle sole covariate age, rural e male, oppure lo sbilanciamento delle covariate
# maschera il vero beneficio degli ospedali grandi.
#Quindi si deve andare a adottare metodo di aggiustamento per stimare il vero effetto causale.

#1b) calcolare per ogni covariata la differenza normalizzata,per l'eta che è variabile continua anche la misura di 
#bilanciamento normalizzata comparare

#DElta function
delta_calculation <- function(Xt,Xc){
  Xt_mean <- mean(Xt)
  Xc_mean <- mean(Xc)
  std_t <- var(Xt)
  std_c <- var(Xc)
  delta <- (Xt_mean- Xc_mean) / sqrt((std_c+std_t)/2)
  return(delta)
}

X_t <- data[data$hvdiag==1, ]
X_c <- data[data$hvdiag==0, ]
covariates <- c("age","rural", "male")

delta_table <-data.frame(
  Delta= sapply(covariates, function(nome_col) {
        delta_calculation(X_t[[nome_col]], X_c[[nome_col]])
   }))
delta_table

gamma_calculation<- function(Xt,Xc){
  sigmat <- sd(Xt)
  sigmac <- sd(Xc)
  gamma<- log(sigmat / sigmac)
  return(gamma)
}
gamma_age<-gamma_calculation(X_t[["age"]],X_c[["age"]])
gamma_age

#compare the covariate distribution
plot(density(X_t$age), col = "red", lwd = 2, 
     main = "Density Comparison: Age", 
     xlab = "Age", ylim = c(0, 0.05))
lines(density(X_c$age), col = "blue", lwd = 2)
legend("topright", legend = c("Control", "Treated"), 
       col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")



qqplot(X_c$age, X_t$age, 
       xlab = "Quantile Control", ylab = "Quantile Treated",
       main = "Q-Q Plot: Age Balance")
abline(0, 1, col = "gray", lty = 2)


par(mar = c(5, 4, 4, 8))
barplot(rbind(prop.table(table(X_t$male)),
              prop.table(table(X_c$male))),
        beside = TRUE,
        main = "Distribution Treatment vs Control in Gender",
        col = c("red", "blue"),
        names.arg = c("Female", "Male")) 
legend("topright",
       inset = c(-0.50, 0),
       legend = c("Trattati", "Controlli"),
       fill = c("red", "blue"),
       xpd = TRUE,
       bty = "n")


barplot(rbind(prop.table(table(X_t$rural)),
              prop.table(table(X_c$rural))),
        beside = TRUE,
        main= "Distribution Treatment vs Control in Area",
        col = c("red", "blue"),
        names.arg = c("Urban", "Rural")) 
legend("topright",
       inset = c(-0.50, 0),
       legend = c("Trattati", "Controlli"),
       fill = c("red", "blue"),
       xpd = TRUE,
       bty = "n")

#conclusion
#There is substantial imbalance across all covariates. In particular, the normalized differences exceed the 
#common threshold of 0.1 for all variables, with especially large imbalance for the rural variable. 
#Additionally, for age, the dispersion measure (Γ = 0.15) indicates that the treated group exhibits higher variability.
#Δ| < 0.1 → buon bilanciamento
#Δ| > 0.1 → sbilanciamento
#overall, the evidence suggests that the treatment assignment is not independent of the observed covariates, 
#and adjustment methods are required before estimating causal effects




#2) SRE
#The age covariates is continuos, but the exercise required to trasform in deterministic variable with this range of age
#[25− 60), (60, 75], > 75
#The strata are 3*2*2 equal to a combinations of the covariates
age_cut <- cut(data$age,
               breaks = c(25, 60, 75, Inf),
               labels = c("[25-60)","[60,75]",">75"),
               right=TRUE
               )
age_cut
strata <- interaction(age_cut, data$male, data$rural, drop=TRUE)
length(levels(strata))

#now we go to inspect number of units in each strata
table_strata<-table(strata=strata, tratment_vs_control=data$hvdiag)
table_strata
#inspect the number of units treatment and control for age group
table_age_group<-table(age_cut, data$hvdiag)
table_age_group

#we can see in the range [25-60) we have more treatment than control, opposite in the range >75, the situation in the middle section
#is more equilibrated
#This means that the treatment is most commonly prescribed to young patients.

#2b)
#the strata that have only unit treated or control is the [25-60).0.1 that have 0 control and 2 treated
#This suggests a violation of the overlap assumption, as no comparison between treated and control units is possible within that stratum.
#the proportion of this case respect all units is

proportion_of_problem_strata <- function(table_strata){
  idx <- apply(table_strata, 1, function(x) any(x == 0))
  strata_names <- rownames(table_strata)[idx]
  
  units_problem <- sum(table_strata[idx, ])
  total_units <- sum(table_strata)
  prop <- units_problem / total_units
  
  return(list(
    strata = strata_names,
    n_strata = length(strata_names),
    proportion = prop
  ))
}
strata_problem <- proportion_of_problem_strata(table_strata)
strata_problem


#then we look strata with either one control or one treatment
proportion_of_one_strata <- function(table_strata){
  idx <- apply(table_strata, 1, function(x) any(x == 1))
  strata_names <- rownames(table_strata)[idx]
  
  units_problem <- sum(table_strata[idx, ])
  total_units <- sum(table_strata)
  prop <- units_problem / total_units
  
  return(list(
    strata = strata_names,
    n_strata = length(strata_names),
    proportion = prop
  ))
}
one_ct_strata <- proportion_of_one_strata(table_strata)
one_ct_strata

#now we go to see which strata have more than one units t/c in the strata
prop_good_strata <- function(tab){
  
  idx <- tab[,1] > 1 & tab[,2] > 1
  
  strata_names <- rownames(tab)[idx]
  n_strata <- sum(idx)
  
  units_good <- sum(tab[idx, ])
  
  
  total_units <- sum(tab)
  
  prop <- units_good / total_units
  
  return(list(
    strata = strata_names,
    n_strata = n_strata,
    proportion = prop
  ))
}
good_strata <- prop_good_strata(table_strata)
good_strata
data_filtered <- data[strata %in% good_strata$strata, ]
data_filtered$strata <- factor(strata[strata %in% good_strata$strata])

#Compute ATE e Variance estiamtor

compute_point_estimation<- function(data_filtered){
  strata_unique <- unique(data_filtered$strata)
  tau_hat <-0
  var_hat <-0
  N <- nrow(data_filtered)
  for(s in strata_unique){
    strata <- data_filtered[data_filtered$strata == s, ]
    Y_t <- strata$survival[strata$hvdiag==1]
    Y_c <- strata$survival[strata$hvdiag==0]
    n_t <- length(Y_t)
    n_c <- length(Y_c)
    n_s <- n_t + n_c
    w_s= n_s / N
    tau_s <- mean(Y_t) - mean(Y_c)
    tau_hat <- tau_hat +  tau_s *w_s
    var_s <- (var(Y_t) / n_t + var(Y_c) / n_c)
    var_hat <- var_hat + var_s * (w_s^2)
  }
  return(list(ATE= tau_hat, Variance_estimator= var_hat))
}

result_PE <- compute_point_estimation(data_filtered)
result_PE

#costruction CI 95%
alpha <- 0.05
z <- qnorm(1 - alpha/2)

se <- sqrt(result_PE$Variance_estimator)
ATE <- result_PE$ATE
CI_low <- ATE - z * se
CI_high <-ATE + z * se

CI <- c(CI_low, CI_high)
CI
result_ex <- data.frame(
  statistic = c("tau_hat", "variance_estimator", "CI_low", "CI_high"),
  value = c(
    result_PE$ATE,
    result_PE$Variance_estimator,
    CI[1],
    CI[2]
  )
)

result_ex
#The estimated Average Treatment Effect (ATE) is positive but small ($\hat{\tau} \approx 0.10$). 
#However, since the 95% confidence interval includes zero, there is no statistical evidence of a significant
#causal effect. This high level of uncertainty is consistent with the initial covariate imbalance and the 
#limited overlap between the treated and control groups within the strata."


# 3) 
#Lin estimator
#first of all Lin estimator required a centered covariates, that mean we subtrack the value of the covariates
#with the mean of this

age_c <- data$age - mean(data$age)
rural_c <- data$rural - mean(data$rural)
male_c <- data$male - mean(data$male)

#after this we construct a model for Lin estimator with the interaction of treatment and centered covariates
#for the interaction we have the *
Y_true <- data$survival
Treatment <- data$hvdiag
model <- lm(Y_true ~ Treatment * (age_c + rural_c + male_c))
summary(model)

lin_results <- coeftest(model, vcov = vcovHC(model, type = "HC3"))
lin_results

ace_lin <- lin_results["Treatment", "Estimate"]
se_lin <- lin_results["Treatment", "Std. Error"]


cat("ACE (Lin 2013):", ace_lin, "\nRobust SE:", se_lin)

#To correct for selection bias, we applied the estimator proposed by Winston Lin (2013),
# using a regression with interactions between the treatment and centered covariates. 
#This method provides a more efficient and robust estimate of the average causal effect (ACE). 
#The result shows an almost null effect (0,033) and a relatively large robust standard error (0.08666), 
#confirming that, after adjustment, the estimated causal effect is not statistically distinguishable from zero.


# 4)
covariates_names <- c("age", "rural", "male")
X <- as.matrix(data[, covariates_names])
X
# we don't use the scale because we don't have a extermis value in the dataset and scaled a binary value don't have to much sense

#build the model glm with binomial family for logistic regression

ps_model <- glm(Treatment ~ X, family = binomial())
summary(ps_model)

ps_score <- ps_model$fitted.values
ps_score
lpscore <- ps_model$linear.predictors
lpscore

by(ps_score, Treatment, summary)
par(mar = c(5, 4, 4, 8))
hist(ps_score[data$hvdiag == 1], col = rgb(0,0,1,0.5), 
     main = "Overlap of Propensity Score", xlab = "PS", breaks = 15)
hist(ps_score[data$hvdiag == 0], col = rgb(1,0,0,0.5), add = TRUE, breaks = 15)
legend("topright", 
       inset = c(-0.5, 0),    
       legend = c("Treated", "Control"), 
       fill = c(rgb(0,0,1,0.5), 
                rgb(1,0,0,0.5)),
       cex = 0.7,             
       xpd = TRUE,            
       bty = "n") 

#Clear imbalance: distributions diverge in the tails. Partial overlap makes trimming essential to remove non-comparable units.

#4a)
ps_score
#Discard control units with estimated propensity scores lower than the minimum 
#of the active treated units’ estimated propensity scores or higher than 
#the maximum of the active treated units’ estimated propensity scores.
minimum_ps_treated <- min(ps_score[data$hvdiag == 1])
maximum_ps_treated <- max(ps_score[data$hvdiag == 1])
ps_bounds_treated <- cbind(minimum_ps_treated, maximum_ps_treated)
ps_bounds_treated

filtered_data <- data[ps_score>=minimum_ps_treated & ps_score<=maximum_ps_treated, ]
filtered_data

total_cardinality <- nrow(data)
filtered_cardinality <- nrow(filtered_data)
data_cardinalities <- cbind(total_cardinality, filtered_cardinality)
data_cardinalities

# How many units did you discard? 
# What are we doing, and why is it important to discard these units?
# We are discarding 4 control units because these 4 units are too different from 
# those in the treatment group to make the two sets comparable.

# 4b) Re-estimate propensity scores on the new sample.
# For this we need to re-build the covariates, of course, using 
# the filtered data
covariates_names <- c("age", "rural", "male")
filtered_X <- as.matrix(filtered_data[, covariates_names])
filtered_X
# Confirming cardinalities
cat("Original Covariates:", nrow(X), "\nFiltered Covariates: ", nrow(filtered_X))    

# 4b) Re-estimate propensity scores on the new sample.
ps_model_filtered <- glm(hvdiag ~ age + rural + male, 
                         data = filtered_data, family = binomial())
summary(ps_model_filtered)

ps_score_filtered <- ps_model_filtered$fitted.values
ps_score_filtered
lpscore_filtered <- ps_model_filtered$linear.predictors
lpscore_filtered

by(ps_score_filtered, filtered_data$hvdiag, summary)

# Checking overlapping
par(mar = c(5, 4, 4, 8))
hist(ps_score_filtered[filtered_data$hvdiag == 1], col = rgb(0,0,1,0.5), 
    main = "Filtered data Overlapping", xlab = "Re-estimated PS", breaks = 15,
     xlim = c(0, 1)) 

hist(ps_score_filtered[filtered_data$hvdiag == 0],col = rgb(1,0,0,0.5), add = TRUE, 
     breaks = 15)

legend("topright",inset = c(-0.5, 0),legend = c("Treated", "Control"), 
      fill = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)), cex = 0.7,xpd = TRUE,  bty = "n")

# for 4c We are going to re-use delta_calculation
ps_t_new <- ps_score_filtered[filtered_data$hvdiag == 1]
ps_c_new <- ps_score_filtered[filtered_data$hvdiag == 0]
delta_ps_final <- delta_calculation(ps_t_new, ps_c_new)
# Balance Measure on Re-estimated PS
print(delta_ps_final)

# previous value:
ps_t_old <- ps_score[data$hvdiag == 1]
ps_c_old <- ps_score[data$hvdiag == 0]
delta_ps_initial <- delta_calculation(ps_t_old, ps_c_old)
ps_old_new <- cbind(delta_ps_initial, delta_ps_final)
ps_old_new




### Exercise 5
w = filtered_data$hvdiag
ps_score_filtered <- ps_model_filtered$fitted.values

# Subclassification of trimmed sample
breaks <- quantile(ps_score_filtered, probs = c(0, 0.25, 0.50, 0.70, 0.85, 1))
subclasses <- cut(ps_score_filtered, breaks = breaks, labels = 1:5, include.lowest = TRUE)
strata_sizes <- table(subclasses)
breaks
subclasses
strata_sizes

# 5a
subclass_table <- table(subclasses, w)
colnames(subclass_table) <- c("Control", "Treated")
print(subclass_table)

# Plot PS histogram per strata
par(mfrow=c(2,3))
for(j in 1:5){
  
  hist(ps_score_filtered[w==1 & subclasses==j], breaks=20, freq=FALSE, ylab="", 
       xlab=paste("Propensity scores", "-", "Stratum", j), 
       axes = FALSE, ,col=4, density=30, main="")
  hist(ps_score_filtered[w==0 & subclasses==j], breaks=20, freq=FALSE, col=2, add=T, density=30)
  axis(1)
}
plot(1:3, 1:3, type="n", axes=FALSE, xlab="", ylab="")
legend("topright", legend=c("Control group", "Treatment group"), col=c(2,4), lty=c(1,1))
par(mfrow=c(1,1))

# 5b
# Part 1: Normalized Differences pre and post stratification
s20 <- apply(filtered_X[w==0,], 2, var)
s21 <- apply(filtered_X[w==1,], 2, var)

norm_diff = function(X, w) {
  Xm <- apply(X,2,mean)
  Xm0 <-apply(X[w==0,], 2, mean) 
  Xm1<-apply(X[w==1,], 2, mean)
  
  DeltaX<- (Xm1-Xm0)/sqrt({s20+s21}/2)
  
  return(DeltaX)
}

complete_norm_diff = norm_diff(filtered_X, w)
complete_norm_diff

split_data <- lapply(split(filtered_X, subclasses), matrix, ncol=3)
split_w <- lapply(split(w, subclasses), as.matrix)
split_data
split_w

strat_diffs <- Map(norm_diff, split_data, split_w)
strat_diffs

diff_matrix <- do.call(rbind, strat_diffs)
weights <- strata_sizes / sum(strata_sizes)
strat_norm_diff <- t(weights) %*% diff_matrix
strat_norm_diff

# Love plot
par(mar=c(5, 12, 4, 2) + 0.1)
plot(complete_norm_diff, 1:length(complete_norm_diff), pch=19, ylab="",
     xlab="Normalized Mean Differences", col=2, axes=FALSE, 
     main="",xlim=c(-1,1))
points(strat_norm_diff,1:length(complete_norm_diff), pch=19, col=4)
abline(v=0, lty=2)
axis(2, at=1:1:ncol(filtered_X), labels = c('age', "rural", "male"), las=2)
axis(1)
legend("topright", legend=c(expression(Delta[ct]), expression(Delta[ct]^strat)),
       col=c(2,4), pch=c(19,19))
box()

# Part 2: Assess balance in the covariates using each covariate as an outcome
# Complete Sample statistics
Xm0 <-apply(filtered_X[w==0,], 2, mean)
Xm1<-apply(filtered_X[w==1,], 2, mean)
tau_complete = Xm1-Xm0
s20 <- apply(filtered_X[w==0,], 2, var)
s21 <- apply(filtered_X[w==1,], 2, var)
s2<- {s20*(sum(1-w)-1) + s21*(sum(w)-1)}/{length(w)-2}
z_complete <- tau_complete/sqrt(s2*{1/sum(1-w) + 1/sum(w)})
tau_complete
z_complete

# Stratified Statistics
balance.check = function(w, x, pstrata)
{
  ej.levels = sort(unique(pstrata))
  J      = length(ej.levels)
  PiJ     = rep(0, J)
  TauJ    = rep(0, J)
  varJ    = rep(0, J)
  for(j in 1:J)
  {
    ej         = ej.levels[j]
    wj         = w[pstrata == ej] 
    xj         = x[pstrata == ej]
    PiJ[j]     = length(wj)/length(w)
    TauJ[j]    = mean(xj[wj==1]) - mean(xj[wj==0])
    varj <- (var(xj[wj==1])*{sum(wj)-1} + var(xj[wj==0])*{sum(1-wj)-1})/{length(wj)-2}
    varJ[j]    = varj*{1/sum(wj) + 1/sum(1-wj)}
    
  }
  
  tau <- sum(PiJ*TauJ)
  V.tau <- sum({PiJ^2}*varJ)
  
  list(TauJ=TauJ, varJ=varJ, Tau=tau, V.tau=V.tau)
}

tau.x <- var.tau.x <- z.x<- matrix(NA, nrow=ncol(filtered_X), ncol = {5+1})
colnames(tau.x)<-colnames(var.tau.x)<- colnames(z.x) <- c(paste("Stratum ", 1:5), "All")
rownames(tau.x)<-rownames(var.tau.x)<- rownames(z.x) <-colnames(filtered_X)
lb.x<- ub.x<-Delta.ct.s<-p.value.x<-rep(0,ncol(filtered_X))
alpha<-0.05

for(ell in 1:ncol(filtered_X)){
  balance.x<-  balance.check(w, filtered_X[,ell], subclasses)
  
  tau.x[ell,]<- c(balance.x$TauJ, balance.x$Tau)
  var.tau.x[ell,]<- c(balance.x$varJ, balance.x$V.tau)  
  z.x[ell,]<- c(balance.x$TauJ/sqrt(balance.x$varJ), balance.x$Tau/sqrt(balance.x$V.tau))
  
  lb.x[ell] <- balance.x$Tau-qnorm(1-alpha/2)*sqrt(balance.x$V.tau)
  ub.x[ell] <- balance.x$Tau+qnorm(1-alpha/2)*sqrt(balance.x$V.tau)
  
  mr <- lm(filtered_X[,ell]~as.factor(subclasses))
  me <- lm(filtered_X[,ell]~w*as.factor(subclasses))
  p.value.x[ell]<- anova(mr, me)$Pr[2]
}

# Plot tables to compare complete sample vs stratified statistics
round(tau.x,2)
round(cbind(Xm1-Xm0, tau.x),2)
round(sqrt(var.tau.x),2)
round(cbind(sqrt(s2*{1/sum(1-w) + 1/sum(w)}), sqrt(var.tau.x)),2)

z.x[is.nan(z.x)] <-0  
z.x
round(cbind(z_complete,z.x, p.value.x),3)

round(cbind(lb.x, ub.x),3)

# Plot of tau.x and their 95% CI 
plot(seq(1,3),tau.x[1:3,ncol(tau.x)], ylim=c(min(lb.x)-0.5, max(ub.x)+0.5), pch=19, cex=2, axes=FALSE, xlab="", ylab="")
for(ell in 1:3){
  segments(ell,lb.x[ell], ell,ub.x[ell])
}
abline(h=0, lty=2)
axis(1, at=seq(1,3), labels = c("age", "rural", "male"), cex.axis=0.75)
axis(2, at=seq(round(min(lb.x), 2)-0.5, round(max(ub.x), 2)+0.5, by=0.5, ))

# Plot tau.x and their 95% CI just for rural and male covariates (much smaller CIs)
plot(seq(2,3),tau.x[2:3,ncol(tau.x)], ylim=c(-0.3, 0.3), pch=19, cex=2, axes=FALSE, xlab="", ylab="")
for(ell in 2:3){
  segments(ell,lb.x[ell], ell,ub.x[ell])
}
abline(h=0, lty=2)
axis(1, at=seq(2,3), labels = c("rural", "male"), cex.axis=0.75)
axis(2, at=seq(-0.3, 0.3, by=0.03,))

# 5c
# Analysis Phase
filtered_Y <- filtered_data["survival"]
  
strat_ATE = function(w, y, pstrata)
{
  ej.levels = sort(unique(pstrata))
  J      = length(ej.levels)
  PiJ     = rep(0, J)
  TauJ    = rep(0, J)
  varJ    = rep(0, J)
  for(j in 1:J)
  {
    ej         = ej.levels[j]
    wj         = w[pstrata == ej] 
    yj         = y[pstrata == ej, ]
    PiJ[j]     = length(wj)/length(w)
    TauJ[j]    = mean(yj[wj==1]) - mean(yj[wj==0])
    varJ[j] <- (var(yj[wj==1])/sum(wj)) + (var(yj[wj==0])/sum(1-wj))
  }
  
  tau <- sum(PiJ*TauJ)
  V.tau <- sum({PiJ^2}*varJ)
  
  list(TauJ=TauJ, varJ=varJ, Tau=tau, V.tau=V.tau)
} 
  
tau_ATE <- strat_ATE(w, filtered_Y, subclasses)  
tau_ATE  

alpha = 0.05
ATE.lb <- tau_ATE$Tau-qnorm(1-alpha/2)*sqrt(tau_ATE$V.tau)
ATE.ub <- tau_ATE$Tau+qnorm(1-alpha/2)*sqrt(tau_ATE$V.tau)
c(ATE.lb, ATE.ub)

# 5d
library(car)
CovAdj_Subps  = function(w, y, x, pstrata){
  ej.levels = unique(pstrata)
  J      = length(ej.levels)
  PiJ     = rep(0, J)
  TauJ    = rep(0, J)
  varJ    = rep(0, J)
  
  for(j in 1:J)
  {
    ej         = ej.levels[j]
    wj         = w[pstrata == ej]
    xj         = x[pstrata == ej,]
    zxj         = scale(xj, center=TRUE, scale=FALSE)
    yj         = y[pstrata == ej,]
    PiJ[j]     = length(wj)/length(w)
    reg.int    =  lm ( yj ~ wj + zxj + wj*zxj )
    TauJ[j]    =  coef( reg.int )[2]
    varJ[j]    =  hccm(reg.int, type="hc2")[2 , 2]
  }
  
  return(c(sum(PiJ*TauJ), sum(PiJ^2*varJ)))
} 

tau_lin <- CovAdj_Subps(w, filtered_Y, filtered_data['age'], subclasses)
tau_lin

sqrt(0.007947282)

# Matching. Use a matching method to estimate the average treatment effect 
# on the treated population
#
# (a) Create a match subsample using 1-to-1 matching with replacement 
# with exact matching on male and rural

# First we need to consider the Design Phase
# A reminder: exact matches means that for 
# each unit i in the Treated we find m_(i) s.t X_{i} = X_{m(i)}
# The assignment says that the match must be exact ON male and rural,
# so ok no "error" allowed on that. This leaves age "free", so we can
# decide which distance metric to use.
# 
# for what we are going to do we refer to the official doc:
# https://cran.r-project.org/web/packages/Matching/Matching.pdf
install.packages("Matching")
library(Matching)
# by reading the matching documentazione Match expects vectors as args
# so the first thing to do is to prepare those vectors (taking them 
# from covariates)
X

# From documentation:
# Y := outcome
# Tr := vector of 0s for control units and 1 for treatment units we can do it
# transforming the True/False into 0/1 as.integer(data$hvdiag == 1)  
# M arg is the number of matches and by default is 1 to 1 so leaving it empty
# Weight = 2 is the Mahalanobis
# EXACT IS THE VECTOR OF TRUE/FALSE for telling the method on which
# covariates use or not use exact matching so since 
# X == ["age", "rural", "male"], we want it as [FALSE, TRUE, TRUE]
match <- Match(Y = data$survival, Tr = as.integer(data$hvdiag == 1),
           X = X, Weight = 2, replace = TRUE, exact = c(FALSE, TRUE, TRUE))
summary(match)

#b) 
MatchBalance(data$hvdiag == 1 ~ age + rural + male, 
             match.out = match, 
             nboots = 500, 
             data = data)

#c)
match_adj <- Match(Y = data$survival, Tr = as.integer(data$hvdiag == 1),
                   X = X, Weight = 2, replace = TRUE, 
                   exact = c(FALSE, TRUE, TRUE), BiasAdjust = TRUE, 
                   Z = data$age)
match_adj

estimated_treated <- match_adj$est
estimated_treated

sample_variance <- match_adj$se^2
sample_variance

# Confidence Intevral at 95%
# 1.96

lower_bound <- estimated_treated - 1.96 * match_adj$se
upper_bound <- estimated_treated + 1.96 * match_adj$se
ci = cbind(lower_bound, upper_bound)
ci




