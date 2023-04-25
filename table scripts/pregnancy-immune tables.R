rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

# load enrollment characteristics and results
H1 <- readRDS(here('results/unadjusted/H1_res.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res.RDS'))
H4 <- readRDS(here("results/unadjusted/H4_res.RDS"))
H1adj <- readRDS(here('results/adjusted/H1_adj_res.RDS'))
H2adj <- readRDS(here('results/adjusted/H2_adj_res.RDS'))
H3adj <- readRDS(here('results/adjusted/H3_adj_res.RDS'))
H4adj <- readRDS(here('results/adjusted/H4_adj_res.RDS'))
cytokineratios <- readRDS(here('results/unadjusted/post-hoc-cytokine-ratios_res.RDS'))
cytokineratiosadj <- readRDS(here('results/adjusted/post-hoc-cytokine-ratios_adj_res.RDS'))

full_res <- rbind(H1, H2, H3, H4, cytokineratios)
full_adj_res <- rbind(H1adj, H2adj, H3adj, H4adj, cytokineratiosadj)

#### MAIN TABLES ####
#### Table 1 ####
# Characteristics of participants
# nperc <- function(vector){
#   n <- sum(vector==1, na.rm=T)
#   perc <- round(n/sum(!is.na(vector))*100)
#   paste(n, " (", perc, "%)", sep="")
# }
 
# mediqr <- function(vector){
#   quantiles <- round(quantile(vector, na.rm=T), 2)
#   paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
# }
 
# n_med_col <- c(nperc(d$sex), mediqr(d$t2_f2_8ip), mediqr(d$t2_f2_23d), mediqr(d$t2_f2_VI), mediqr(d$t2_f2_12i),
#                mediqr(d$t3_cort_slope), mediqr(d$t3_residual_cort), mediqr(d$t3_saa_slope), mediqr#(d$t3_residual_saa),
#                mediqr(d$t3_map), mediqr(d$t3_hr_mean), mediqr(d$t3_gcr_mean), mediqr(d$t3_gcr_cpg12),
#                mediqr(d$laz_t2), mediqr(d$waz_t2), mediqr(d$whz_t2), mediqr(d$hcz_t2),
#                mediqr(d$laz_t3), mediqr(d$waz_t3), mediqr(d$whz_t3), mediqr(d$hcz_t3),
#                nperc(d$diar7d_t2), nperc(d$diar7d_t3), mediqr(d$momage), mediqr(d$momheight), 
#                mediqr(d$momeduy), mediqr(d$cesd_sum_t2), mediqr(d$cesd_sum_ee_t3), mediqr(d$pss_sum_mom_t3), 
#                nperc(d$life_viol_any_t3))
 
 
 #immune markers, anthropometry (Y1, Y2)
# tbl1 <- data.table(" " = c("Child","","","","","","","","","","","","","","","","","","","","","","","Mother","","","","","",""),
#                    " " = c("", "Urinary F2-isoprostanes (Year 1)","","","", "Salivary cortisol reactivity (Year 2)","", "sAA reactivity (Year 2)","",
#                            "SAM biomarkers (Year 2)","", "Glucocorticoid receptor","", "Anthropometry (14 months, Year 1)","","","",
#                            "Anthropometry (28 months, Year 2)","","","", "Diarrhea (14 months, Year 1)", "Diarrhea (28 months, Year 2)","",
#                           "Anthropometry at enrollment", "Education", "Depression at Year 1", "Depression at Year 2", "Perceived stress at Year 2", 
#                            "Intimate partner violence"),
#                    " " = c("Female", "iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a-VI", "8,12-iso-iPF(2a)-VI", 
#                            "Change in slope between pre- and post-stressor cortisol", "Cortisol residualized gain score", 
#                            "Change in slope between pre- and post-stressor sAA change", "sAA residualized gain score",
#                            "Mean arterial pressure", "Resting heart rate", "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site methylation",
 #                           "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
#                            "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
#                            "Caregiver-reported 7-day recall", "Caregiver-reported 7-day recall", "Age (years)", "Height (cm)", "Schooling completed (years)",
#                            "CES-D score", "CES-D score", "Perceived Stress Scale score", "Any lifetime exposure"),
#                    "n (%) or median (IQR)" = n_med_col)


#### Table 2 ####
exposure <- c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def") 
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency","Ln ferritin (ug/L)", "Ln sTfR (mg/L)", "Iron deficiency") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl2 <- growth_tbl("Maternal Micronutrients and Child Immune Status", expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl2flex <- growth_tbl_flex("Maternal Micronutrients and Child Immune Status", expo_var, out_var, exposure, outcome, H1, H1adj, T, 1.1, 1.4)
tbl2supp <- growth_tbl("Maternal Micronutrients and Child Immune Status", expo_var, out_var, exposure, outcome, H1, H1adj,)
tbl2flexsupp <- growth_tbl_flex("Maternal Micronutrients and Child Immune Status", expo_var, out_var, exposure, outcome, H1, H1adj,)

#### Table 3 ####
exposure <- c("ln_preg_cort") 
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Ln Cortisol (ug/dL)") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl3 <- growth_tbl("Maternal Cortisol and Child Immune Status", expo_var, out_var, exposure, outcome, H2, H2adj, T)
tbl3flex <- growth_tbl_flex("Maternal Cortisol and Child Immune Status", expo_var, out_var, exposure, outcome, H2, H2adj, T, 1.1, 1.4)
tbl3supp <- growth_tbl("Maternal Cortisol and Child Immune Status", expo_var, out_var, exposure, outcome, H2, H2adj)
tbl3flexsupp <- growth_tbl_flex("Maternal Cortisol and Child Immune Status", expo_var, out_var, exposure, outcome, H2, H2adj)

#### Table 4 ####
exposure <- c("ln_preg_estri") 
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Ln Estriol (ng/mL)") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl4 <- growth_tbl("Maternal Estriol and Child Immune Status", expo_var, out_var, exposure, outcome, H4, H4adj, T)
tbl4flex <- growth_tbl_flex("Maternal Estriol and Child Immune Status", expo_var, out_var, exposure, outcome, H4, H4adj, T, .8, 1.4)
tbl4supp <- growth_tbl("Maternal Estriol and Child Immune Status", expo_var, out_var, exposure, outcome, H4, H4adj)
tbl4flexsupp <- growth_tbl_flex("Maternal Estriol and Child Immune Status", expo_var, out_var, exposure, outcome, H4, H4adj)

#### Table 5 ####
exposure <- c("logAGP", "logCRP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")   
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Ln AGP (g/L)", "Ln CRP (mg/L)", "Initial Ln IFN-y (pg/mL)", "Sum score of 13 cytokines") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl5 <- growth_tbl("Maternal Immune Status and Child Immune Status", expo_var, out_var, exposure, outcome, H3, H3adj, T)
tbl5flex <- growth_tbl_flex("Maternal Immune Status and Child Immune Status", expo_var, out_var, exposure, outcome, H3, H3adj, T, 1.1, 1.4)
tbl5supp <- growth_tbl("Maternal Immune Status and Child Immune Status", expo_var, out_var, exposure, outcome, H3, H3adj)
tbl5flexsupp <- growth_tbl_flex("Maternal Immune Status and Child Immune Status", expo_var, out_var, exposure, outcome, H3, H3adj)

#post-hoc results
#### TABLE 6 ####
exposure <- c("logRBP_inf", "logSTFR_inf") 
outcome <- c("t2_ratio_th1_th2", "t2_ratio_th1_il10", "t2_ratio_th2_il10", "t2_ratio_pro_il10", "t3_ratio_th1_th2", "t3_ratio_th1_il10", "t3_ratio_th2_il10", "t3_ratio_pro_il10")
expo_var <- c("Ln RBP (umol/L)", "Ln sTfR (mg/L)") 
out_var <- c("Th1/Th2 Age 14 months", "Th1/IL-10 Age 14 months", "Th2/IL-10 Age 14 months", "Proinflammatory/IL-10 Age 14 months", "Th1/Th2 Age 28 months", "Th1/IL-10 Age 28 months", "Th2/IL-10 Age 28 months", "Proinflammatory/IL-10 Age 28 months")

tbl6 <- growth_tbl("Maternal RBP and sTfR and Child Cytokine Ratios", expo_var, out_var, exposure, outcome, cytokineratios, cytokineratiosadj, T)
tbl6flex <- growth_tbl_flex("Maternal RBP and sTfR and Child Cytokine Ratios", expo_var, out_var, exposure, outcome, cytokineratios, cytokineratiosadj, T, 1.1, 1.4)
tbl6supp <- growth_tbl("Maternal RBP and sTfR and Child Cytokine Ratios", expo_var, out_var, exposure, outcome, cytokineratios, cytokineratiosadj,)
tbl6flexsupp <- growth_tbl_flex("Maternal RBP and sTfR and Child Cytokine Ratios", expo_var, out_var, exposure, outcome, cytokineratios, cytokineratiosadj,)


#### SAVE TABLES ####

write.csv(tbl2, here('tables/main/pregnancy-immune-table1.csv'))
write.csv(tbl3, here('tables/main/pregnancy-immune-table2.csv'))
write.csv(tbl4, here('tables/main/pregnancy-immune-table3.csv'))
write.csv(tbl5, here('tables/main/pregnancy-immune-table4.csv'))

write.csv(tbl2supp, here('tables/supplementary/immune-growth-supptable2.csv'))
write.csv(tbl3supp, here('tables/supplementary/immune-growth-supptable3.csv'))
write.csv(tbl4supp, here('tables/supplementary/immune-growth-supptable4.csv'))
write.csv(tbl5supp, here('tables/supplementary/immune-growth-supptable5.csv'))
write.csv(tbl6supp, here('tables/supplementary/cytokine-ratios.csv'))

sect_properties <- prop_section(
        page_size = page_size(orient = "landscape", width=11, height=8.5),
        page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)

save_as_docx("Table 1: Maternal Micronutrients and Child Immune Status" = tbl2flex, 
             "Table 2: Maternal Cortisol and Child Immune Status" = tbl3flex,
             "Table 3: Maternal Estriol and Child Immune Status" = tbl4flex,
             "Table 4: Maternal Immune Status and Child Immune Status" = tbl5flex,
             path='/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/main/pregnancy-immune.docx',
             pr_section = sect_properties) 
             
save_as_docx("Table S3: Maternal Micronutrients and Child Immune Status" = tbl2flexsupp, 
             "Table S4: Maternal Cortisol and Child Immune Status" = tbl3flexsupp, 
             "Table S5: Maternal Estriol and Child Immune Status" = tbl4flexsupp, 
             "Table S6: Maternal Immune Status and Child Immune Status" = tbl5flexsupp,
             "Table S7: Maternal STfR and Child Cytokine Ratios" = tbl6flexsupp,
             path='/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/supplementary/pregnancy-immune-supplementary.docx',
             pr_section = sect_properties)

