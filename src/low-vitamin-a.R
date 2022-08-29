rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS("/Users/sophiatan/Downloads/bangladesh-cleaned-master-data.RDS")
d <- d %>% filter(pregnancy_immune==1)
#d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/pregnancy_child_immune_covariates_data.RDS"))

# different iron deficiency cutoff
d <- d %>% mutate(vit_A_def = ifelse(RBP_inf_preg < 0.7, 1, 0))
d <- d %>% mutate(vit_A_low = ifelse(RBP_inf_preg < 1.05, 1, 0))

##Hypothesis 1
Xvars <- c("vit_A_low")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_models <- bind_rows(H1_models, res)
  }
}

#Get primary contrasts
H1_res <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
  H1_res <-  bind_rows(H1_res , preds$res)
}

#Make list of plots
H1_plot_list <- NULL
H1_plot_data <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_plot_list[[i]] <-  simul_plot$p
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

H1_res <- H1_res %>% mutate(BH.Pval=p.adjust(Pval, method="BH")) 

#Save models
saveRDS(H1_models, here("models/low_vit_A.RDS"))

#Save results
saveRDS(H1_res, here("results/unadjusted/low_vit_A_res.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/low_vit_A_unadj_spline_data.RDS"))


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

H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}

#Make list of plots
H1_adj_plot_list <- NULL
H1_adj_plot_data <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_adj_plot_list[[i]] <-  simul_plot$p
  H1_adj_plot_data <-  rbind(H1_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}

H1_adj_res <- H1_adj_res %>% mutate(BH.Pval=p.adjust(Pval, method="BH")) 

#Save results
saveRDS(H1_adj_res, here("results/adjusted/low_vit_A_adj_res.RDS"))

#Save plot data
saveRDS(H1_adj_plot_data, here("figure-data/low_vit_A_adj_spline.data.RDS"))

