


rm(list=ls())

source(here::here("0-config.R"))

library(boxr)
box_auth()
updated_pregnancy <- box_read(845078269128)
updated_pregnancy <- updated_pregnancy %>% select(dataid, preg_estri, preg_cort) %>%
  mutate(dataid = as.integer(dataid))
d<-read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bd-pregnancy-serum-micronutrient-immun-cortisol-covariates-child-immune.csv"))
names(d)

d <- left_join(d, updated_pregnancy, "dataid")
summary(d$mom_t0_ln_il2)

######### Add combined ratios of cytokines ##########

#drop Z-score, sd, and ratio measures
d <- d[,!(grepl("^(z_)",colnames(d)) | grepl("^(sd_)",colnames(d)))]

x=c("il1_mom_t0", "il6_mom_t0", "tnfa_mom_t0")[1]
summary(as.vector(scale(d[,x], center = FALSE, scale = apply(as.matrix(d[,x]), 2, sd, na.rm = TRUE))))
x=c("il1_mom_t0", "il6_mom_t0", "tnfa_mom_t0")[2]
summary(as.vector(scale(d[,x], center = FALSE, scale = apply(as.matrix(d[,x]), 2, sd, na.rm = TRUE))))
x=c("il1_mom_t0", "il6_mom_t0", "tnfa_mom_t0")[3]
summary(as.vector(scale(d[,x], center = FALSE, scale = apply(as.matrix(d[,x]), 2, sd, na.rm = TRUE))))


#function to create composite score
create_score <- function(d, numerator_vars=c("il1_mom_t0", "il6_mom_t0", "tnfa_mom_t0"), denominator_vars="il10_mom_t0", varname="mom_t0_ratio_pro_il10"){
  for(i in numerator_vars){
    if(i==numerator_vars[1]){
      x = as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }else{
      x = x + as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }
  }
  summary(x)
  
  
  for(i in denominator_vars){
    if(i==denominator_vars[1]){
      y = as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }else{
      y = y + as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }
  }
  summary(y)
  
  score=log(x/y)
  summary(score)
  d$score <- score
  colnames(d)[ncol(d)] <- varname
  return(d)
}

# *Pro-inflammatory cytokines / IL-10
# *(IL-1 + IL-6 + TNF-a) / IL-10
d <- create_score(d, numerator_vars=c("il1_mom_t0", "il6_mom_t0", "tnfa_mom_t0"), denominator_vars="il10_mom_t0", varname="mom_t0_ratio_pro_il10")
summary(d$mom_t0_ratio_pro_il10)
ggplot(d, aes(x=mom_t0_ratio_pro_il10)) + geom_density()

# *Th1 / IL-10
# *(IL-12 + IFN) / IL-10
# gen mom_t0_ratio_th1_il10 = (il12_mom_t0 + ifng_mom_t0) / il10_mom_t0
d <- create_score(d, numerator_vars=c("il12_mom_t0", "ifng_mom_t0"), denominator_vars="il10_mom_t0", varname="mom_t0_ratio_th1_il10")
summary(d$mom_t0_ratio_th1_il10)
ggplot(d, aes(x=mom_t0_ratio_th1_il10)) + geom_density()

# *Th2 / IL-10 
# *(IL-4 + IL-5 + IL-13) / IL-10
# gen mom_t0_ratio_th2_il10 = (il4_mom_t0 + il5_mom_t0 + il13_mom_t0) / il10_mom_t0
d <- create_score(d, numerator_vars=c("il4_mom_t0", "il5_mom_t0", "il13_mom_t0"), denominator_vars="il10_mom_t0", varname="mom_t0_ratio_th2_il10")
summary(d$mom_t0_ratio_th2_il10)
ggplot(d, aes(x=mom_t0_ratio_th2_il10)) + geom_density()


# *Th17 / IL-10
# *(IL-17A + IL-21) / IL-10
# gen mom_t0_ratio_th17_il10 = (il17_mom_t0 + il21_mom_t0) / il10_mom_t0
d <- create_score(d, numerator_vars=c("il17_mom_t0", "il21_mom_t0"), denominator_vars="il10_mom_t0", varname="mom_t0_ratio_th17_il10")
summary(d$mom_t0_ratio_th17_il10)
ggplot(d, aes(x=mom_t0_ratio_th17_il10)) + geom_density()


# *Th1 / Th2
# *(IL-12 + IFN) / (IL-4 + IL-5 + IL-13)
# gen mom_t0_ratio_th1_th2 = (il12_mom_t0 + ifng_mom_t0) / (il4_mom_t0 + il5_mom_t0 + il13_mom_t0)
d <- create_score(d, numerator_vars=c("il12_mom_t0", "ifng_mom_t0"), denominator_vars=c("il4_mom_t0", "il5_mom_t0", "il13_mom_t0"), varname="mom_t0_ratio_th1_th2")
summary(d$mom_t0_ratio_th1_th2)
ggplot(d, aes(x=mom_t0_ratio_th1_th2)) + geom_density()


# *Th1 / Th17
# *(IL-12+IFN) / (IL-17A + IL-21)
# gen mom_t0_ratio_th1_th17 = (il12_mom_t0 + ifng_mom_t0) / (il17_mom_t0 + il21_mom_t0)
d <- create_score(d, numerator_vars=c("il12_mom_t0", "ifng_mom_t0"), denominator_vars=c("il17_mom_t0", "il21_mom_t0"), varname="mom_t0_ratio_th1_th17")
summary(d$mom_t0_ratio_th1_th17)
ggplot(d, aes(x=mom_t0_ratio_th1_th17)) + geom_density()


########### Add deficiency cutoff exposures #############
# 1 if deficient, 0 if not deficient
d$vit_A_def <- ifelse(d$RBP_inf_preg < 0.83, 1, 0)
d$vit_D_def <- ifelse(d$vitD_nmol_per_L < 30, 1, 0)
d$iron_def <- ifelse(d$FERR_inf_preg < 12 | d$STFR_inf_preg > 8.3, 1, 0)
  
  
############## Merge in hhwealth ##################
d_hhwealth <- read.csv("C:/Users/Sophia/Documents/ee-secondary/sophia scripts/hhwealth.csv")
d1 <- left_join(d, d_hhwealth, by="dataid")


############## Merge in maternal sum scores ####################
d_sum <- read.csv("C:/Users/Sophia/Documents/immune-growth/results/maternal sum score/maternal inflammation sum score.csv") %>%
  select(-X)
d2 <- left_join(d1, d_sum, by="dataid")


############## Merge in child sum scores ####################
d_sum_child <- read.csv("C:/Users/Sophia/Documents/immune-growth/results/child sum score/child immune sum scores.csv") %>%
  select(-X)
dfull <- left_join(d2, d_sum_child, by="childid")


############# Check covariate missingness ###################
# a few exposures and outcomes
exp <- c("vitD_nmol_per_L", "logFERR_inf", "logSTFR_inf", "logRBP_inf",
         "ln_preg_cort", "logCRP", "logAGP", "ifng_mom_t0", "ln_preg_estri")
out <- c("t2_ln_agp", "t3_ln_agp", "t2_ln_crp", "t3_ln_agp", "t2_ln_ifn", "t3_ln_ifn")

Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "roof", "HHwealth",
         "tr", "life_viol_any_t3", "viol_any_preg", "ageday_bt2", "ageday_bt3", 
         "month_blood_t0", "month_bt2", "month_bt3") %>% unique()

W <- dfull %>% select(all_of(Wvars))  

generate_miss_tbl <- function(Wvars, d){
  W <- d %>% select(all_of(Wvars))  
  miss <- data.frame(name = names(W), missing = colSums(is.na(W))/nrow(W), row.names = c(1:ncol(W)))
  for (i in 1:nrow(miss)) {
    miss$class[i] <- class(W[,which(colnames(W) == miss[i, 1])])
  }
  miss 
}
generate_miss_tbl(Wvars, dfull)

# roof has low variability
mean(W$roof, na.rm=T)
sd(W$roof, na.rm=T)
# remove roof from covariates

# add missingness category to IPV covariates
dfull$life_viol_any_t3<-as.factor(dfull$life_viol_any_t3)
dfull$life_viol_any_t3<-addNA(dfull$life_viol_any_t3)
levels(dfull$life_viol_any_t3)[length(levels(dfull$life_viol_any_t3))]<-"Missing"

dfull$viol_any_preg<-as.factor(dfull$viol_any_preg)
dfull$viol_any_preg<-addNA(dfull$viol_any_preg)
levels(dfull$viol_any_preg)[length(levels(dfull$viol_any_preg))]<-"Missing"

for (i in exp){
  for (j in out){
    print(i)
    print(j)
    d_sub <- subset(dfull, !is.na(dfull[,i]) & !is.na(dfull[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}



saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Audrie/pregnancy_child_immune_covariates_data.RDS"))


