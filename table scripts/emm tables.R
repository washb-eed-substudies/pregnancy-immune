rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

emm <- readRDS(here('results/adjusted/emm_tr_adj_res.RDS'))

tbl1 <- subgroup_tbl("Maternal micronutrients", 
                     c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency", "Ln ferritin (ug/L)","Ln sTfR (mg/L)",  "Iron deficiency"),
                     c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", 
                       "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", 
                       "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (g/L)", "Sum score of 13 cytokines Age 28 months"), 
                     c("Arm"), 
                     c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf", "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def"),
                     c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
                       "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z"),
                     c("tr"), emm)

save_as_docx("EMM Table: Effect modification of maternal micronutrients and child immune status" = tbl1,
             path = "/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/main/pregnancy-immune-emm1.docx",
             pr_section = sect_properties)
