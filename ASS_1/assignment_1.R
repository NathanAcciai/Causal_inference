
# Causal Inference
# Assignment 1
# Authors: Group 2 (Acciai Nathan, Canti Edoardo, Tarlini Davide)




library(sandwich)
library(lmtest)


# 0. Importing Data
data <- read.table("Morphine.txt",header = TRUE, sep = " ")
data

# 1. Descriptive Analysis

## 1a) Summarize the data with descriptive statistics (including summary statistics
#  and graphs)
N <- nrow(data) # total number of units 
N

p <- ncol(data) # total number of columns
p

# Number of treated units
N_t <- sum(data$W == 1)
N_t

# Number of control units
N_c <- sum(data$W == 0)
N_c

# Verify is the number of units under control is coherent with expected
if (N_c == N - N_t) {
  print("Expected Control units are Total - Treatements")
}else{
  print("No")
}

# computing descriptive statistics on total data
# without discriminating between treated and control
# means and variances for numerical

desc_stats <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE),
    var  = var(x, na.rm = TRUE),
    min  = min(x, na.rm = TRUE),
    max  = max(x, na.rm = TRUE)
  )
}
vars <- data[, c("age", "AS4", "VS4", "VD4")]

stats_table <- t(sapply(vars, desc_stats))
stats_table

# countings for categoricals
total_womens <- sum(data$sex == 0)
total_mens <- sum(data$sex == 1)

total_by_sex <- cbind(total_womens, total_mens)
total_by_sex

# Treated units
stats_treated <- t(sapply(data[data$W == 1, c("age","AS4","VS4","VD4")], desc_stats))
stats_treated

#Control units
stats_control <- t(sapply(data[data$W == 0, c("age","AS4","VS4","VD4")], desc_stats))
stats_control

# number of treated mens, treated womans 
num_treated_women <- sum(data$sex[data$W == 1] == 0)
num_treated_mens <- sum(data$sex[data$W == 1] == 1)

num_control_women <- sum(data$sex[data$W == 0] == 0)
num_control_mens <- sum(data$sex[data$W == 0] == 1)

#generate summary table
tabel_gender_tr_vs_cont <- cbind(
  Control = c(num_control_women, num_control_mens),
  Treated  = c(num_treated_women, num_treated_mens)
)

# Aggiungiamo i nomi delle righe per chiarezza
rownames(tabel_gender_tr_vs_cont) <- c("Women", "Men")

# Visualizza il risultato
print(tabel_gender_tr_vs_cont)


# Statistiche per le DONNE (sex == 0)
stats_women <- t(sapply(data[data$sex == 0, c("age", "AS4", "VS4")], desc_stats))

# Statistiche per gli UOMINI (sex == 1)
stats_men <- t(sapply(data[data$sex == 1, c("age", "AS4", "VS4")], desc_stats))

# Extract and mearge the age from the result
age_comparison <- rbind(
  Donne = stats_women["age", ],
  Uomini = stats_men["age", ]
)
print(age_comparison)


# Plotting the distribution between men and women 
# in treatment and control
bar_plot_table <- table(data$W, data$sex)

barplot(bar_plot_table,
        beside = TRUE,
        main = "Gender by Treatment",
        names.arg = c("Women", "Men"),
        col = c("orange", "blue"),
        ylim = c(0, max(bar_plot_table) * 1.4), # Aggiunge il 40% di spazio in alto
        legend.text = c("Controllo", "Trattati"),
        args.legend = list(x = "topright", bty = "n"))

###
# Age Distributions

# Age histogram of all units
hist(data$age,
     col = rgb(0, 0, 0, 0.2),
     main = "Age Total Distribution",
     xlab = "Age",
     breaks = 10)

# Age histogram of control units
hist(data$age[data$W == 0],
     col = rgb(0, 0, 1, 0.5),
     main = "Age distribution in control group",
     xlab = "Age",
     breaks = 10)

# Age histogram of treated units
hist(data$age[data$W == 1],
     col = rgb(1, 0, 0, 0.5),
     main = "Age distribution in treated group",
     xlab = "Age",
     breaks = 10)

par(mfrow = c(1, 1))

# Boxplots
boxplot(data$age,
        main = "Age - Boxplot totale",
        col = "gray")

boxplot(data$age ~ data$W,
        main = "Age - Control vs Treated",
        col = c("blue", "red"),
        names = c("Control", "Treated"))

###
# AS4 Distributions

## AS4 histogram of all units
hist(data$AS4,
     col = rgb(0, 0, 0, 0.2),
     main = "AS4 Total Distribution",
     xlab = "AS4",
     breaks = 10)

## AS4 histogram of Control units
hist(data$AS4[data$W == 0],
     col = rgb(0, 0, 1, 0.5),
     main = "AS4 distribution in control group",
     xlab = "AS4",
     breaks = 10)

## AS4 histogram of Treated units
hist(data$AS4[data$W == 1],
     col = rgb(1, 0, 0, 0.5),
     main = "AS4 distribution in treated group",
     xlab = "AS4",
     breaks = 10)

boxplot(data$AS4,
        main = "AS4 - Boxplot totale",
        col = "gray")

boxplot(data$AS4 ~ data$W,
        main = "AS4 - Control vs Treated",
        col = c("blue", "red"),
        names = c("Control", "Treated"))

###
# VS4 Distributions

# VS4 histogram of all units
hist(data$VS4,
     col = rgb(0, 0, 0, 0.2),
     main = "VS4 Total Distribution",
     xlab = "VS4",
     breaks = 10)

# VS4 histogram of Control units
hist(data$VS4[data$W==0],
     col = rgb(0, 0, 1, 0.5),
     main = "VS4 distribution in control group",
     xlab = "VS4",
     breaks = 10)

# VS4 histogram of treated units
hist(data$VS4[data$W==1],
     col = rgb(1, 0, 0, 0.5),
     main = "VS4 distribution in treated group",
     xlab = "VS4",
     breaks = 10)

par(mfrow = c(1, 1))

boxplot(data$VS4,
        main = "VS4 - Boxplot totale",
        col = "gray")

boxplot(data$VS4 ~ data$W,
        main = "VS4 - Control vs Treated",
        col = c("blue", "red"),
        names = c("Control", "Treated"))

###

## 1 b)Inspect the dataset, and provide some evidence that the assignment 
#was indeed random, e.g. by examining the pre-treatment covariate distributions.

# Now pre-treatment covariates are only age and sex, so removing
# AS4 and VS4.

# Verifying the balance by Covariance Balance by Design 
# as shown in (slides_03, n.15 and n.22, ref to CRE) 

# pre-treatment Age
Xage_t <- data$age[data$W == 1]
Xage_c <- data$age[data$W == 0]
Xage_meandiff <- mean(Xage_t) - mean(Xage_c)
Xage_meandiff


# pre-treatment Sex
## proportion of Treated men
prop_men_treated <- num_treated_mens / N_t
prop_men_treated

## proportion of Treated women
prop_wom_treated <- num_treated_women / N_t
prop_wom_treated

## proportion of Control men
prop_man_control <- num_control_mens / N_c
prop_man_control

## proportion of Control women
prop_wom_control <- num_control_women / N_c
prop_wom_control

sex_table <- rbind(
  men   = c(proportion_treated = prop_men_treated, proportion_control = prop_man_control),
  women = c(proportion_treated = prop_wom_treated, proportion_control = prop_wom_control)
)

sex_table

# What about the mean age of men in control and mean age of men in treatment?
mean_age_men_control <- mean(data$age[data$W == 0 & data$sex == 1])
mean_age_men_control
mean_age_men_treated <- mean(data$age[data$W == 1 & data$sex == 1])
mean_age_men_treated

# What about the mean age of women in control and mean age of women in treatment?
mean_age_women_control <- mean(data$age[data$W == 0 & data$sex == 0])
mean_age_women_control
mean_age_women_treated <- mean(data$age[data$W == 1 & data$sex == 0])
mean_age_women_treated

mean_age_groups_table<- rbind(
  men= c(mean_age_control= mean_age_men_control, mean_age_treated= mean_age_men_treated),
  woman= c(mean_age_control= mean_age_women_control, mean_age_treated= mean_age_women_treated)
)
mean_age_groups_table


## Comments on 1b)
# the question was:
# b) Inspect the dataset, and provide some evidence that the assignment 
# was indeed random, e.g. by examining the pre-treatment covariate distributions.

# comments:
# Given a mean age of 66.45 on all units,
# data in age_table show that treated units are slightly older than
# controlled units. A mean difference of 3.72 between treated and control
# could be considered as acceptable, especially because the standard deviation
# of the age over all units is stadev_age := 9.241441
#
# Data collected in variable sex_table show that there is a perfect balance
# between men and women in control units, and an almost perfect balance between
# sex in the treated units
#
# Considering age mean of controlled units on sex it reflects that the
# mean age of women under control is lower wrt to the men but "less lower"
# in treated

# 2. Fisher
## 2 a)
# Sharp Null Hypothesis of no effect whatsoever
# H0: Y_{i}(1) = Y_{i}(0), this means that Y_{i}^{miss} = Y_{i}^{obs}
# Remember that our outcome is Y := VD4

# in order to use Fisher we need to choose a test statistic
# as asked we are going to use difference in mean which is defined as
# T^{diff} := mean(Y_{W=1}^{obs}) - mean(Y_{W=0}^{obs})

# Now we have a dataset, this means that we already have our observations
# (we are in the situation in which the evidence given by observations is 
#  the dataset itself)

# first of all we want to isolate the units assigned to treatment (W=1)
treated_units <- data[data$W == 1,]
treated_units

# now our Y_{W=1}^{obs} can be extracted as:
Yt_OBS <- treated_units$VD4
Yt_OBS

# similarly on control units:
control_units <- data[data$W == 0,]
control_units
Yc_OBS <- control_units$VD4
Yc_OBS 

# now we can compute the T^{diff} test stat:
T_diff_obs <- mean(Yt_OBS) - mean(Yc_OBS)
T_diff_obs

# Is a difference of T_diff := -21.60714 in terms of VD4
# more extreme than what we would have observe by random chance?

# so now we have the T_diff value for the "observed world"
# i.e: for a realization of the assignment vector
# how many possible assignment vectors are there?
#(i.e: of which the prob is not null?)

# the text says: 
# "Answer questions 1-4 assuming that the data come from a completely 
# randomized experiment."
# in a CRE we have choose(N, N_t) possible realization of W
number_of_possible_avectors <- choose(N, N_t)
number_of_possible_avectors # 1.037199e+17 it is a bit too much...

# In order to calculate the Fisher's exact PValue we should, in principle,
# compute the test statistic for all possible realizations of the assignment
# vector (that represent all the possible worlds), of course this approach is
# not feasible because the number of possible assignment vectors is 
# number_of_possible_avectors := 1.037199e+17.

# We need to approximate the p-value using MonteCarlo approach...
# after this premise we recall that sampling a K fixed number of 
# possible assignment vectors is actually the same to permute K times
# the assignment vector and on each of the K permutations we can compute
# again the test statistic.

# Problem: we observed just one realization of the assignment vector, so we have the 
# outcomes only for that.
# Solution: No effect whatsoever is our sharp null hypothesis 
# (Y_{i}^{miss} = Y_{i}^{obs})
Y_true <- data$VD4 # original outcomes (as observed)
W_true <- data$W # original assignments

K <- 100000
T_diff_sims <- rep(NA, K)
set.seed(42)
for(k in 1:K){
  W_alternative <- sample(W_true, replace = FALSE) 
  T_diff_sims[k] <- mean(Y_true[W_alternative==1]) - mean(Y_true[W_alternative==0])
  cat("Iteration:", k, "- Current T_diff", T_diff_sims[k], "\n")
}
#Andiamo quindi a calcolare il p-value di MonteCarlo ovvero
#la probabilità (approssimata via simulazione) di osservare una differenza
#tra gruppi almeno così grande come quella osservata, se in realtà il trattamento non avesse alcun effetto

p_value <- mean(abs(T_diff_sims) >= abs(T_diff_obs))
p_value
#essendo molto piccolo ci indica che il risultato è molto improbabile sotto H0

#I grafici che vediamo sotto indicano la distribuzione della statistica sotto H0
#Mentre la riga rossa indica il valore osservato, questo cosa ci rappresenta, beh se il valore osservato
#(la linea rossa) rimane nelle code evidenzia il rifiuto di HO

# Istogramma della distribuzione della statistica
T_diff_sims # the list of T_diffs on K possible realization of the assignment vector
par(mfrow=c(1,2))
hist(T_diff_sims, freq = FALSE, main = "", breaks=15,
     ylab="", xlab = expression(bar(Y_true)[t] - bar(Y_true)[c]))
abline(v = T_diff_obs, col = "red") 


# Considerando il valore assoluto vado a considerrare un test bilaterale
hist(abs(T_diff_sims), freq = FALSE, main = "", ylab="", breaks=15,
     xlab = expression(abs(bar(Y_true)[t] - bar(Y_true)[c])))
abline(v = abs( T_diff_obs), col = "red") 
par(mfrow=c(1,1))




# A nice way for doing this would be to calculate the pvalue MC approximation
# using K as a param so we are going to define the following fuction:
fisher_test_mc <- function(Y, W, K, T_obs){
  T_diff_simulations <- rep(NA, K)
  for(k in 1:K){
    W_simulation <- sample(W, replace = FALSE) 
    T_diff_simulations[k] <- mean(Y[W_simulation == 1]) - mean(Y[W_simulation == 0])
  }
  p_frt_approx <- sum(abs(T_diff_simulations) >= abs(T_obs)) / K
  
  return(p_frt_approx)
}

#p_val_fisher <- fisher_test_mc(data$VD4, data$W, 1000000, T_diff_obs, verbose = TRUE)
#p_val_fisher

Ks_list <- c(10000, 100000, 1000000)
p_values_results <- rep(NA, length(Ks_list))
set.seed(42)
for (i in 1:length(Ks_list)) {
  current_K <- Ks_list[i]
  cat("\n Approximating P_frt on K =", current_K, "\n")
  p_values_results[i] <- fisher_test_mc(data$VD4, data$W, current_K, T_diff_obs)
}

p_values_results
# This results on different K shows that
# the difference in pvalue between K = 100000 and K = 1000000 is only 0.7e^{-05}
# Could this indicate that the pValue is going to converge?
# E idica sicuramente che per ogni test , avendo un p-value molto piccolo è improbabile che il trattamento
# non abbia alcun effetto.

# Asymptotic 

var_Tdif <- (N*var(Y_true))/(N_t*N_c)
asym_pv <- 2*pnorm(abs(T_diff_obs/sqrt(var_Tdif)), mean = 0, sd = 1, lower.tail = FALSE)
asym_pv




## 2 b) #devo considerare dei valori di tau per la costante addittiva che porteranno ad avere un sottoinsieme di questi
#valori che non vengono rifiutati dall' ipotesi
# Inizializzo i valori di tau
# La formula è Yi(0)=Yi(1)+tau, questo vuol dire che per calcolare Y(0) non ho l'effetto di tau mentre per 
#Y(1) avrò effetto addittivo 

#Definisco intervallo di tau per costruire poi l'intervallo
fisher_test_mc_CI <- function(tau_grid, Y_true, W_true, K){
  p_values_results <- rep(NA, length(tau_grid))
  for (i in 1:length(tau_grid)){
    tau <- tau_grid[i]
    #Valore osservato T^diff_obs
    T_diff<-mean(Y_true[W_true==1])-mean(Y_true[W_true==0]) - tau
    #Vado a randomizzare l'esperimento con il sample casuale
    T_sim <- replicate(K, {
      W_sim<- sample(W_true, replace = FALSE)
      mean(Y_true[W_sim==1]) - mean(Y_true[W_sim==0]) - tau
    })
    p_values_results[i]<- mean(abs(T_sim)>=abs(T_diff))
  }
  
  alpha <- 0.10
  CI<- range(tau_grid[p_values_results >= alpha])
  return(CI)
}
#Ho provato anche con una griglia più stretta ma non funziona non c'è nessun valore di tau che soddisfa 
tau_grid <- seq(-10, 10, by = 0.25)
Ks_list <- c(10000, 100000)
CI_lower <- rep(NA, length(Ks_list))
CI_upper <- rep(NA, length(Ks_list))
set.seed(42)
for (i in 1:length(Ks_list)) {
  current_K <- Ks_list[i]
  cat("\n Calculate CI on K =", current_K, "\n")
  CI <- fisher_test_mc_CI(tau_grid, Y_true, W_true, K)
  CI_lower[i]<- CI[1]
  CI_upper[i] <- CI[2]
}
results_table <- data.frame(
  K = Ks_list,
  CI_lower = CI_lower,
  CI_upper = CI_upper
)

results_table
#Si vede che anche se si Cambia K non risultano esserci dei cambiamenti.


#L’intervallo di Fisher al 90% per l’effetto causale costante è [−10.0,−7.3],
#ottenuto invertendo il test di randomizzazione sulla differenza delle medie.
#Poiché l’intervallo è interamente negativo e non include lo zero, c’è forte evidenza di un effetto causale negativo 
#del trattamento.





## 2 c) Calculate exact (two sided) Fisher p-value 
# for the sharp null hypothesis of no effects whatsoever using a statistic 
# that adjusts for covariates, through either a model output strategy 
# or a pseudo-outcome strategy (not both)

# First we notice that T gain statistic cannot be used because there is 
# not a clear pre-treatment variable that is analogous to the outcome
model_output <- lm(data$VD4 ~ data$W + data$sex + data$age)
summary(model_output)
# estimate on the observed realization of the assignment vector
T_obs_model <- model_output$coefficients['data$W'] 

# Replicating MonteCarlo Fisher for Model Output Strategy)
covadj_fisher_test_mc <- function(W, K, T_obs){
  T_diff_simulations <- rep(NA, K)
  for(k in 1:K){
    W_simulation <- sample(W, replace = FALSE) 
    model_simulation <- lm(data$VD4 ~ W_simulation + data$sex + data$age)
    T_diff_simulations[k] <- model_simulation$coefficients['W_simulation']
  }
  
  p_frt_approx <- (sum(abs(T_diff_simulations) >= abs(T_obs))) / K
  
  return(p_frt_approx)
}

p_values_results_model <- rep(NA, length(Ks_list))
set.seed(42)
for (i in 1:length(Ks_list)) {
  current_K <- Ks_list[i]
  cat("\n Approximating P_frt on K =", current_K, "\n")
  p_values_results_model[i] <- covadj_fisher_test_mc(data$W, current_K, T_obs_model)
}

p_values_results_model



## 2 d) uso lo stesso applicato a rpima ovvero il model output per calcolare l'intervallo di confidenza
#sotto l'ipotesi che il trattamento sia costante ed addittivo per cui il modello è il solito
#fit del modello osservato

Y_true<- data$VD4
W_true <- data$W
model_output <- lm(Y_true ~ W_true + data$sex + data$age)
summary(model_output)

#Statistica osservata T^reg-coeff = \hatB_w
T_obs <- coef(model_output)["W_true"]
T_obs

#Ora devo trovare intervallo di confidenza per un effetto addittivo costante quindi essendo che io ho Y_i^obs devo ovviamente
#cambiare quella cosa li per la formula slide pacco 6 pag 11 e poi 
#Then use these Yi(0) as dependent variable in implementing the model output strategy
#In questo caso si va a ricercare quanto l'effetto tau che ipotizzo sia veramente l'effetto vero 
#Quello che si fa è prendere i coefficienti del modello sotto l'ipotesi nulla per un qualche K 
#Poi si va a stipulare una griglia di tau perchè per il mio p_FRT mi serve fare questa cosa:
#1) calcolare i coefficienti dell' assegnamento con un certo valore di TAU
#2) confrontare per ogni tau il suo coefficiente con tutti i coefficienti del modello sotto ipotesi nulla
#3) Vogliamo quindi trovare un tau che produce dei coefficienti molto vicini a zero 
#Perchè se è molto piccolo vuol dire che tutti coefficienti dell' ipotesi nulla saranno tanti ad essere maggiori del coefficiente di tau
#Cosa significa vi chiederete? beh significa che 
#Se il caso (T_null) produce spesso valori più grandi del tuo errore residuo (T_tau), 
#significa che il tuo errore è trascurabile. Quel τ è quindi un ottimo candidato per essere l'effetto vero.
#Il P-value: Più è alto il p-value, più quel τ è "credibile". Il valore di τ che ha il p-value più alto 
#di tutti è la tua stima migliore (quella che rende il trattamento "più invisibile" nei dati)





set.seed(42)
#Questo mi serve per fare regressione provando vari valori di TAU per trovare gli intervalli di confidenza
fisher_stat <- function(tau, data, W_true, Y_true){
  #Se Assegnata allora devo togliere tau al potenziale
  Y_obs= Y_true
  #ora devo usare questo Potenziale per fittare il modello
  Y_obs[W_true==1]<- Y_true[W_true==1] - tau

  model_output_potential <- lm(Y_obs ~ W_true+ data$sex+ data$age)
  return(coef(model_output_potential)["W_true"])
}
#Mi serve per il confronto dei coefficienti
fisher_null <- function(Y_true, W_true,data){
  W_simulation <- sample(W_true, replace = FALSE) 
  model_output_potential_null<- lm(Y_true~ W_simulation+ data$sex+ data$age)
  return( coef(model_output_potential_null)["W_simulation"])
}

K<-10000
T_l <- replicate(K, fisher_null(Y_true,W_true,data))
#ora T obs è la mia migliore scommessa quindi devo cercare tau centrando T_obs poichè 
#È estremamente probabile che l'intervallo di confidenza si trovi "nelle vicinanze" di questo numero.
tau_grid <- seq(T_obs - 2, T_obs + 2, by = 0.01)
T_l

#Calcolo i coefficienti per ogni tau
T_tau <- sapply(tau_grid, function(tau) {
  fisher_stat(tau, data,W_true, Y_true)
})
T_tau

# Calcolo il p-value per ogni T_l
p_values <- sapply(seq_along(tau_grid), function(i) {
  mean(abs(T_l) >= abs(T_tau[i]))
})
p_values

# 90% Fisher confidence interval confronto ogni p-values con alpha e prendo il minimo e il massimo
alpha <- 0.10

ci_90 <- range(tau_grid[p_values > alpha])

ci_90


# 9. Output results
cat("Observed effect (beta_W):", T_obs, "\n")
cat("90% Fisher CI:", ci_90[1], "to", ci_90[2], "\n")





# 3. Neyman

#3a. Estimate the ATE
# The objective is to estimate the Average Treatment Effect (ATE) in a Completely Randomized Experiment (CRE).
# Under a CRE the difference in observed sample means is an unbiased estimator of the true population ATE.

# Extract observed outcomes for the treated group (W = 1)
treated_units <- data[data$W == 1,]
treated_units
Yt_OBS <- treated_units$VD4
Yt_OBS

# Extract observed outcomes for the control group (W = 0)
control_units <- data[data$W == 0,]
control_units
Yc_OBS <- control_units$VD4
Yc_OBS 

# Now we can compute the T^{diff} estimator. In a CRE, this is an unbiased estimator of the ATE:
Yt_mean <- mean(Yt_OBS)
Yc_mean <- mean(Yc_OBS)
T_diff_obs <- Yt_mean - Yc_mean
T_diff_obs

#3b. Neyman Confidence Interval
# We want to construct a large sample 90% Confidence Interval. 
# We know that (T^{diff}-tau)/sqrt(var(T^{diff})) converges to a standard normal N(0,1).

# Since we cannot reliably estimate the true variance var(T^{diff}), we approximate it with the so called Neyman Estimator.
St_hat <- sum((Yt_OBS - Yt_mean)^2)*(1/(N_t-1))
Sc_hat <- sum((Yc_OBS - Yc_mean)^2)*(1/(N_c-1))

V_neymann <- (St_hat/N_t) + (Sc_hat/N_c)
V_neymann

# We can then use this estimate to compute an approximate, conservative, large sample confidence interval.
# We use 1.64 as the Z-value for alpha = 0.1 (90% CI).
# The confidence interval is equal to [T^{diff}-1.64*sqrt(V_ney), T^{diff}+1.64*sqrt(V_ney)]:
neyman_CI <- data.frame(
  CI_lower = (T_diff_obs-1.64*sqrt(V_neymann)),
  CI_upper = (T_diff_obs+1.64*sqrt(V_neymann))
)

neyman_CI

#3c. Neyman Hypothesis Testing
# The above CI can be inverted to perform an hypotesis testing procedure on the null hypotesis H_0: Mean_Y(1) = Mean_Y(0)
# against H_1: Mean_Y(1) != Mean_Y(0)

# The test statistic is:
neyman_test_statistic = T_diff_obs/sqrt(V_neymann)
neyman_test_statistic

# Since under the null H_0, and for large N, the randomization distribution of the above statistic is approximately the standard normal,
# the two sided p-value under this approximation is:
neyman_p_value = 2*(1-pnorm(abs(neyman_test_statistic)))
neyman_p_value

#3d. Regression Adjustment (Lin's Estimator)
# Estimate the ATE using the treatment's coefficient of the Lin's estimator

# Center the covariates
sex_centered <- data$sex - mean(data$sex)
age_centered <- data$age - mean(data$age)

# We can consider the model defined by:
#   - data$VD4 ~ data$W + age_centered + sex_centered + age_centered*data$W + sex_centered*data$W
# where we interact each covariate with the treatment variable
lin_mo <- lm(data$VD4 ~ data$W + sex_centered + sex_centered*data$W + age_centered + age_centered*data$W)
lin_ATE_estimate <- lin_mo$coefficients['data$W']

# Compute now an HC2 estimate of the variance of Lin's Estimator 
HC2 <- coeftest(lin_mo, vcov = vcovHC(lin_mo, type = "HC2"))

# These are the treatment coefficient and the HC2 estimate of its variance
lin_ATE_estimate
HC2[2,2]^2


# When compared with T^{diff}...
c(T_diff_obs, lin_ATE_estimate)
# we see that the estimate of the model is very similar to T^{diff}, as expected


# When compared with V_neymann...
c(V_neymann, HC2[2,2]^2)
c(sqrt(V_neymann), HC2[2,2])
# we see that the precision of Lin's estimator is actually worse



# Some Comments:
# From point 3a we see that T^{diff}=-21.60714, with an estimated standard deviation of 4.788592. So, our estimate suggests that the treatment has, 
# on average in the sample, a lowering effect on the outcome of interest (VD4).

# From point 3b and 3c we see that the constructed 90% large sample confidence interval is [-29.46043, -13.75385], wich suggest ...  
# We must also remember this interval is actually computed with a conservative estimate of the true variance of the statistics used, wich 
# means that the actual coverage rate is something more than 90%.
# Testing the null hypotesis of no average treatment effect in the sample, we can also obtain a p-value of 6.415489e-06, still conservative.
# This two results points to the presence of an ATE different than zero.

# Lastly, in point 3d we tried to incorporate the covariate into our inference. By computing Lin's estimator with respect to
# different inclusion of covariates we obtained a very similar estimate to T^{diff} by considering only the sex covariate, while the 
# introduction of the age covariate biased the result a little more:
#                         sex_only=-21.61123 ; age_only=-21.28248 ; both=-21.31531 ; T^{diff}=-21.60714
# By computing the HC2 estimator for the variance of Lin's estimator, we get the following values: 
#                         sex_only=23.50699 ; age_only=27.28495 ; both=27.98669  ; V_neymann=22.93061
# wich shows that by including or using only the age covariate we actually incur into a loss of precision of our inference with respect to the 
# T^{diff} estimator, while including into Lin's model only the sex covariate we obtain an estimator with a similar estimated variance
# than the (conservative) estimated one for T^{diff}



#5) Fisher
# With SRE caluculate exact two side of p-value in fisher experiment using as test statistic the weighted average
#Recall a sex table and the table of the gender calculate in the 1 exercise
sex_table
tabel_gender_tr_vs_cont
Y_obs <- data$VD4
W_true <- data$W
sex<- data$sex
#Now we are going to stratified the experiment from sex, where the weight is calulated n_s/N where n_s is the number of unit
# in the block

compute_statistic <- function(Y, W, sex){
  #identify the index of sex
  strata_level <- unique(sex)
  #for the proportion
  N <- length(Y)
  tau <-0
  for (s in strata_level){
    #Take the index of the units for strata
    idx<- which(sex==s)
    #Take the potential outcome for the unit_s
    Y_s <- Y[idx]
    W_s <- W[idx]
    
    n_s <- length(Y_s)
    Y_0 <- Y_s[W_s==0]
    Y_1 <- Y_s[W_s==1]
    #calculate the mean
    tau_s <- mean(Y_1)- mean(Y_0)
    #Calculate the average mean
    tau <- tau + (n_s/ N) *tau_s
  }
  return(tau)
}
T_obs<- compute_statistic(Y_obs, W_true, sex)
T_obs
#It's like the same at each exercise
#Now we going to randomized in the strata

permute_W <- function(W, sex){
  W_perm <- W
  for (s in unique(sex)){
    idx <- which(sex==s)
    n_treated <- sum(W[idx])
    W_perm[idx] <-0
    treated_idx <- sample(idx, n_treated)
    W_perm[treated_idx] <- 1
  }
  return(W_perm)
}
W_perm<- permute_W(W_true,sex)
#verify the permute
nt_women <- sum(sex[W_perm == 1] == 0)
nt_mens <- sum(sex[W_perm == 1] == 1)

nc_women <- sum(sex[W_perm == 0] == 0)
nc_mens <- sum(sex[W_perm == 0] == 1)

#generate summary table of permute and verify the proportion is the same
tabel_gender_tr_vs_cont_perm <- cbind(
  Control = c(nc_women, nc_mens),
  Treated  = c(nt_women, nt_mens)
)
tabel_gender_tr_vs_cont
tabel_gender_tr_vs_cont_perm
#define monte carlo experiment

fisher_mc_SRE <- function(Y, W, sex, K){
  T_obs <- compute_statistic(Y,W, sex)
  count <-0
  for(k in 1:K){
    W_permute <- permute_W(W,sex)
    T_l<- compute_statistic(Y,W_permute,sex)
    if(abs(T_l)>=abs(T_obs)){
      count<- count+1
    }
  }
  p_hat <- (count+1)/(K+1)
  return(p_hat)
}

Ks_list <- c(10000, 100000, 1000000)
p_values_results <- rep(NA, length(Ks_list))
for(k in 1:length(Ks_list)){
  result <- fisher_mc_SRE(Y_obs, W_true, sex, Ks_list[k])  
  p_values_results[k]<- result
  cat("Iteration:", Ks_list[k], "Current p-value", result, "\n")
}

table_iteration <- data.frame(
  MC_iteration = Ks_list,
  p_value = p_values_results
)

table_iteration




# Lastly, in point 3d we tried to incorporate the covariate into our inference. By computing Lin's estimator we get an estimate of the ATE of -21.31531.
# By computing the HC2 estimate of its variance we get a value of 27.98669, so an estimated standard error of 5.290245.
# This shows that by adjusting our analysis by including the covariates we actually incur into a loss of precision in our inference with respect to what we 
# got for the T^{diff} estimator.



# 6. SRE Neyman
male_units <- data[data$sex == 1,]
female_units <- data[data$sex == 0,]
mt_units <- male_units[male_units$W == 1,]
mc_units <- male_units[male_units$W == 0,]
ft_units <- female_units[female_units$W == 1,]
fc_units <- female_units[female_units$W == 0,]

N0 <- length(female_units[,1])
Nft <- length(ft_units[,1])
Nfc <- length(fc_units[,1])

N1 <- length(male_units[,1])
Nmt <- length(mt_units[,1])
Nmc <- length(mc_units[,1])


# Male strata
Ymt_OBS <- mt_units$VD4
Ymt_OBS

Ymc_OBS <- mc_units$VD4
Ymc_OBS

# Now we can compute the T^{diff} estimator for this strata.
Ymt_mean <- mean(Ymt_OBS)
Ymc_mean <- mean(Ymc_OBS)
T_diff_obs_male <- Ymt_mean - Ymc_mean
T_diff_obs_male


# Female strata
Yft_OBS <- ft_units$VD4
Yft_OBS

Yfc_OBS <- fc_units$VD4
Yfc_OBS

# Now we can compute the T^{diff} estimator for this strata. 
Yft_mean <- mean(Yft_OBS)
Yfc_mean <- mean(Yfc_OBS)
T_diff_obs_female <- Yft_mean - Yfc_mean
T_diff_obs_female

# Let's combine the two estimates:
T_diff_obs_sre <- (N0/N)*T_diff_obs_female + (N1/N)*T_diff_obs_male
T_diff_obs_sre

# For the estimating the variance (and the s.e) we do the following:
Smt_hat <- sum((Ymt_OBS - Ymt_mean)^2)*(1/(Nmt-1))
Smc_hat <- sum((Ymc_OBS - Ymc_mean)^2)*(1/(Nmc-1))

male_v_neyman <- (Smt_hat/Nmt) + (Smc_hat/Nmc)
male_v_neyman

Sft_hat <- sum((Yft_OBS - Yft_mean)^2)*(1/(Nft-1))
Sfc_hat <- sum((Yfc_OBS - Yfc_mean)^2)*(1/(Nfc-1))

female_v_neyman <- (Sft_hat/Nft) + (Sfc_hat/Nfc)
female_v_neyman

var_sre <- (N0/N)^2*female_v_neyman + (N1/N)^2*male_v_neyman
var_sre



# 7 SRE Lin's Estimator

# Male strata
m_age_centered <- male_units$age - mean(male_units$age)

lin_mo_male <- lm(male_units$VD4 ~ male_units$W + m_age_centered + m_age_centered*male_units$W)

lin_ATE_male <- lin_mo_male$coefficients['male_units$W']
HC0_male <- coeftest(lin_mo_male, vcov = vcovHC(lin_mo_male, type = "HC0"))

# Female strata
f_age_centered <- female_units$age - mean(female_units$age)

lin_mo_female <- lm(female_units$VD4 ~ female_units$W + f_age_centered + f_age_centered*female_units$W)

lin_ATE_female <- lin_mo_female$coefficients['female_units$W']
HC0_female <- coeftest(lin_mo_female, vcov = vcovHC(lin_mo_female, type = "HC0"))

# Combine them to get the final estimate of ATE...
sre_lin <- (N0/N)*(lin_ATE_female) + (N1/N)*(lin_ATE_male)
sre_lin

# and the variance of this estimator
sre_lin_var <- (N0/N)^2*(HC0_female[2,2]^2) + (N1/N)^2*(HC0_male[2,2]^2)
sre_lin_var
sqrt(sre_lin_var)

