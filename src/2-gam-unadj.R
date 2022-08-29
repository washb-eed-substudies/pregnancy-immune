rm(list=ls())
 
source(here::here("0-config.R"))

d <- readRDS("/Users/sophiatan/Downloads/bangladesh-cleaned-master-data.RDS")
d <- d %>% filter(pregnancy_immune==1)
#d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/pregnancy_child_immune_covariates_data.RDS"))

# different iron deficiency cutoff
d <- d %>% mutate(vit_A_def = ifelse(RBP_inf_preg < 0.7, 1, 0))

# # iron deficiency in moms
# d %>% group_by(dataid) %>% summarise(n=n()) %>% filter(n>1)
# filter(d, dataid %in% c(23404, 31102, 35105)) %>% select(dataid, childid)
# unique_moms <- rbind(filter(d, !(dataid %in% c(23404, 31102, 35105))), filter(d, dataid %in% c(23404, 31102, 35105))[1:3,])
# 
# summary(unique_moms$iron_def)
# sum(unique_moms$iron_def, na.rm=T)
# filter(unique_moms, FERR_inf_preg < 12 & STFR_inf_preg <= 8.3) %>% nrow()
# filter(unique_moms, FERR_inf_preg >= 12 & STFR_inf_preg > 8.3) %>% nrow()
# filter(unique_moms, FERR_inf_preg < 12 & STFR_inf_preg > 8.3) %>% nrow()
# 
# # diarrhea in children
# summary(d$diar7d_t2)
# summary(d$diar7d_t3)

#Loop over exposure-outcome pairs

##Hypothesis 1
#Maternal nutrition is negatively associated with child inflammation
Xvars <- c("vitD_nmol_per_L", "logFERR_inf", "logSTFR_inf", "logRBP_inf", 
           "vit_A_def", "iron_def", "vit_D_def")            
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
  if(grepl("_def", H1_models$X[i])){
    preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
  }else{
    preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  }
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


#Save models
saveRDS(H1_models, here("models/H1_models.RDS"))

#Save results
saveRDS(H1_res, here("results/unadjusted/H1_res.RDS"))


#Save plots
#saveRDS(H1_plot_list, here("figure-objects/H1_unadj_splines.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/H1_unadj_spline_data.RDS"))



## Hypothesis 2
# Maternal stress is positively associated with child inflammation

Xvars <- c("ln_preg_cort")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H2_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H2_models <- bind_rows(H2_models, res)
  }
}

#Get primary contrasts
H2_res <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  preds <- predict_gam_diff(fit=H2_models$fit[i][[1]], d=H2_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_res <-  bind_rows(H2_res , preds$res)
}

#Make list of plots
H2_plot_list <- NULL
H2_plot_data <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H2_models, here("models/H2_models.RDS"))

#Save results
saveRDS(H2_res, here("results/unadjusted/H2_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, here("figure-objects/H2_unadj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, here("figure-data/H2_unadj_spline_data.RDS"))



##Hypothesis 3

Xvars <- c("logCRP", "logAGP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H3_models, here("models/H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/unadjusted/H3_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, here("figure-objects/H3_unadj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, here("figure-data/H3_unadj_spline_data.RDS"))


##Hypothesis 4
# Maternal estriol is negatively associated with child inflammation
                                                                                            
Xvars <- c("ln_preg_estri")            
Yvars <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
           "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn", "sumscore_t3_Z")

#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}

#Make list of plots
H4_plot_list <- NULL
H4_plot_data <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_plot_list[[i]] <-  simul_plot$p
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H4_models, here("models/H4_models.RDS"))

#Save results
saveRDS(H4_res, here("results/unadjusted/H4_res.RDS"))


#Save plots
#saveRDS(H4_plot_list, here("figure-objects/H4_unadj_splines.RDS"))

#Save plot data
saveRDS(H4_plot_data, here("figure-data/H4_unadj_spline_data.RDS"))

