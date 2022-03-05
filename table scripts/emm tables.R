rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

emm <- readRDS(here('results/adjusted/emm_tr_adj_res.RDS'))

tbl1 <- subgroup_tbl("Maternal micronutrients", 
                     c("Vitamin D", "Ln RBP", "Ln ferritin", "Ln sTfR"),
                     c("Ln AGP age 14 months", "Ln CRP age 14 months", 
                       "Ln IFN-y age 14 months", "Sum score of 13 cytokines age 14 months", 
                       "Ln AGP age 28 months", "Ln CRP age 28 months", "Ln IFN-y age 28 months", "Sum score of 13 cytokines age 28 months"), 
                     c("Arm"), 
                     c("vitD_nmol_per_L", "logRBP_inf", "logFERR_inf", "logSTFR_inf"),
                     c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
                       "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z"),
                     c("tr"), emm)

save_as_docx("EMM Table: Effect modification of maternal micronutrients and child immune status" = tbl1,
             path = "/Users/sophiatan/Documents/WASH/pregnancy-immune-emm.docx",
             pr_section = sect_properties)
