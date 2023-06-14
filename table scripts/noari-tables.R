rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

# load enrollment characteristics and results
H1adj <- readRDS(here('results/adjusted/H1_adj_res_noari.RDS'))
H2adj <- readRDS(here('results/adjusted/H2_adj_res_noari.RDS'))
H3adj <- readRDS(here('results/adjusted/H3_adj_res_noari.RDS'))
H4adj <- readRDS(here('results/adjusted/H4_adj_res_noari.RDS'))

full_adj_res <- rbind(H1adj, H2adj, H3adj, H4adj)

#### Table 2 ####
exposure <- c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def") 
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency","Ln ferritin (ug/L)", "Ln sTfR (mg/L)", "Iron deficiency") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl2 <- growth_tbl("Maternal Micronutrients and Child Immune Status", expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl2flex <- growth_tbl_flex("Maternal Micronutrients and Child Immune Status", expo_var, out_var, exposure, outcome, H1, H1adj, T, 1.1, 1.4)

#### Table 3 ####
exposure <- c("ln_preg_cort") 
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Ln Cortisol (ug/dL)") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl3 <- growth_tbl("Maternal Cortisol and Child Immune Status", expo_var, out_var, exposure, outcome, H2, H2adj, T)
tbl3flex <- growth_tbl_flex("Maternal Cortisol and Child Immune Status", expo_var, out_var, exposure, outcome, H2, H2adj, T, 1.1, 1.4)

#### Table 4 ####
exposure <- c("ln_preg_estri") 
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Ln Estriol (ng/mL)") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl4 <- growth_tbl("Maternal Estriol and Child Immune Status", expo_var, out_var, exposure, outcome, H4, H4adj, T)
tbl4flex <- growth_tbl_flex("Maternal Estriol and Child Immune Status", expo_var, out_var, exposure, outcome, H4, H4adj, T, .8, 1.4)

#### Table 5 ####
exposure <- c("logAGP", "logCRP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")   
outcome <- c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
             "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z")
expo_var <- c("Ln AGP (g/L)", "Ln CRP (mg/L)", "Initial Ln IFN-y (pg/mL)", "Sum score of 13 cytokines") 
out_var <- c("Ln AGP Age 14 months (g/L)", "Ln CRP Age 14 months (mg/L)", "Ln IFN-y Age 14 months (pg/mL)", "Sum score of 13 cytokines Age 14 months", "Ln AGP Age 28 months (g/L)", "Ln CRP Age 28 months (mg/L)", "Ln IFN-y Age 28 months (pg/mL)", "Sum score of 13 cytokines Age 28 months")

tbl5 <- growth_tbl("Maternal Immune Status and Child Immune Status", expo_var, out_var, exposure, outcome, H3, H3adj, T)
tbl5flex <- growth_tbl_flex("Maternal Immune Status and Child Immune Status", expo_var, out_var, exposure, outcome, H3, H3adj, T, 1.1, 1.4)

#### SAVE TABLES ####

write.csv(tbl2, here('tables/main/pregnancy-immune-noari-table1.csv'))
write.csv(tbl3, here('tables/main/pregnancy-immune-noari-table2.csv'))
write.csv(tbl4, here('tables/main/pregnancy-immune-noari-table3.csv'))
write.csv(tbl5, here('tables/main/pregnancy-immune-noari-table4.csv'))

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width=11, height=8.5),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)

save_as_docx("Table 1: Maternal Micronutrients and Child Immune Status" = tbl2flex, 
             "Table 2: Maternal Cortisol and Child Immune Status" = tbl3flex,
             "Table 3: Maternal Estriol and Child Immune Status" = tbl4flex,
             "Table 4: Maternal Immune Status and Child Immune Status" = tbl5flex,
             path=here::here('tables/supplementary/pregnancy-immune-noari.docx'),
             pr_section = sect_properties) 


