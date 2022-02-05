rm(list=ls())

source(here::here("0-config.R"))

Xvars <- c("ln_preg_cort")     
Yvars <- c("ln_crp", "ln_agp", "ln_ifn", "sumscore", 
           "ratio_th1_th2", "ratio_th1_il10", "ratio_th2_il10", "ratio_pro_il10")

d <- d %>% select(!c(sumscore_t3_noil2il21_Z, sumscore_t2_noil2il21_Z))

#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    long_d <- d %>% gather(grep("t2|t3", grep(j, names(d), value=T), value=T), key = "time", value=j)
    res_unadj <- fit_RE_gam(d=long_d, X=i, Y="j",  W=NULL)
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

H1_res <- H1_res %>% mutate(BH.Pval=p.adjust(Pval, method="BH")) 

#Save models
saveRDS(H1_models, here("models/longitudinals.RDS"))

#Save results
saveRDS(H1_res, here("results/unadjusted/longitudinals_res.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/longitudinal_unadj_spline_data.RDS"))


#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "tr", "life_viol_any_t3_cat", "viol_any_preg_cat")

Wvars[!(Wvars %in% colnames(d))]

#Add in time varying covariates:
Wvars <- c(Wvars, c("ageday_bt2", "month_blood_t0", "month_bt2", "ageday_bt3", "month_bt3"))


#Fit models
H1_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    long_d <- d %>% gather(grep("t2|t3", grep(j, names(d), value=T), value=T), key = "time", value=j)
    res_unadj <- fit_RE_gam(d=long_d, X=i, Y="j",  W=Wvars)
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
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, simul_plot$pred))
}

H1_res <- H1_res %>% mutate(BH.Pval=p.adjust(Pval, method="BH")) 

#Save models
saveRDS(H1_models, here("models/longitudinals_adj.RDS"))

#Save results
saveRDS(H1_res, here("results/adjusted/longitudinals_adj_res.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/longitudinal_adj_spline_data.RDS"))