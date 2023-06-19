
rm(list=ls())

source(here::here("0-config.R"))
source(paste0(here::here(),"/src/0-power-calc_functions.R"))
# install.packages("pwr")
# install.packages("pwrss")
library(pwr)
library(pwrss)
library(arm)
library(lm.beta)

d <- readRDS("C:/Users/andre/Dropbox/WASHB-EE-analysis/bangladesh-cleaned-master-data.RDS")


#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu","gest_age_weeks",
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "tr", "life_viol_any_t3_cat", "viol_any_preg_cat")

Wvars[!(Wvars %in% colnames(d))]

#Add in time varying covariates:
Wvars2 <- c(Wvars, c("ageday_bt2", "month_blood_t0", "month_bt2"))
Wvars3 <- c(Wvars, c("ageday_bt3", "month_blood_t0", "month_bt3"))

pick_covariates <- function(j){
  # j is outcome as string
  # choose correct adjustment set based on outcome
  if(grepl("t2", j)){Wset = Wvars2}
  else{Wset = Wvars3}
  return(Wset)
}




#Loop over exposure-outcome pairs

##Hypothesis 1
#Maternal nutrition is negatively associated with child inflammation
Xvars <- c("vitD_nmol_per_L", "logFERR_inf", "logSTFR_inf", "logRBP_inf", 
           "vit_A_def", "iron_def", "vit_D_def")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(j)
    res_adj <- fit_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}




#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  if(grepl("_def", H1_adj_models$X[i])){
    preds <- predict_glm_power(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], Xvar=res$X, Yvar=res$Y)
  }else{
    preds <- predict_glm_power(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], Xvar=res$X, Yvar=res$Y)
  }
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}




## Hypothesis 2
# Maternal stress is positively associated with child inflammation

Xvars <- c("ln_preg_cort")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-c(pick_covariates(j), "time_of_day_cort_cont")
    res_adj <- fit_glm(d=d, X=i, Y=j,  W=Wset, forcedW = c("time_of_day_cort_cont"))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#Get primary contrasts
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_glm_power(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res , preds$res)
}




##Hypothesis 3

Xvars <- c("logCRP", "logAGP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(j)
    res_adj <- fit_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_glm_power(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}



##Hypothesis 4
# Maternal estriol is negatively associated with child inflammation

Xvars <- c("ln_preg_estri")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(j)
    res_adj <- fit_glm(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_glm_power(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}




#Save results
power_res <- bind_rows(
  H1_adj_res %>% mutate(hypothesis="H1"),
  H2_adj_res %>% mutate(hypothesis="H2"),
  H3_res %>% mutate(hypothesis="H3"),
  H4_res %>% mutate(hypothesis="H4")
) %>% dplyr::select(Y,X,power)

plot(hist(power_res$power))
summary(power_res$power)

saveRDS(power_res, here("results/post_hoc_power.RDS"))

write.csv(power_res, file =  here("tables/post_hoc_power.csv"))

