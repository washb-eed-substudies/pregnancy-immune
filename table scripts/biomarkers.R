library(data.table)
source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)

d <- readRDS("/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/pregnancy_child_immune_covariates_data.rds")

writeqntle<-function(vector) {
  quantiles<-round(quantile(vector, na.rm=TRUE), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

mom_lab <-c("Outcome", 
        "Vit D", "Ferritin", "sTfR", "RBP", 
        "Cortisol", "Estriol",
        "IL-1?? (pg/ml)", "Il-6 (pg/ml)", "TNF-?? (pg/ml)", "IL-12 (pg/ml)", "IFN-?? (pg/ml)", "IL-4 (pg/ml)", "IL-5 (pg/ml)", "IL-13 (pg/ml)", "IL-17A (pg/ml)", "IL-21 (pg/ml)", "IL-10 (pg/ml)", "IL-2 (pg/ml)", "GM-CSF (pg/ml)", "AGP (g/L)", "CRP (mg/L)")

child_lab <-c("Outcome", 
          "IL-1?? (pg/ml)", "Il-6 (pg/ml)", "TNF-?? (pg/ml)", "IL-12 (pg/ml)", "IFN-?? (pg/ml)", "IL-4 (pg/ml)", "IL-5 (pg/ml)", "IL-13 (pg/ml)", "IL-17A (pg/ml)", "IL-21 (pg/ml)", "IL-10 (pg/ml)", "IL-2 (pg/ml)", "GM-CSF (pg/ml)", "AGP (g/L)", "CRP (mg/L)")

mom <-c("Median (25th, 75th percentile)", 
        writeqntle(d$vitD_nmol_per_L), writeqntle(d$ferr), writeqntle(d$stfr), writeqntle(d$rbp),
        writeqntle(d$preg_cort), writeqntle(d$preg_estri), 
        writeqntle(d$il1_mom_t0), writeqntle(d$il6_mom_t0), writeqntle(d$tnfa_mom_t0), writeqntle(d$il12_mom_t0), writeqntle(d$ifng_mom_t0), writeqntle(d$il4_mom_t0), writeqntle(d$il5_mom_t0), writeqntle(d$il13_mom_t0), writeqntle(d$il17_mom_t0), writeqntle(d$il21_mom_t0), writeqntle(d$il10_mom_t0), writeqntle(d$il2_mom_t0), writeqntle(d$gmcsf_mom_t0), writeqntle(d$agp), writeqntle(d$crp))
        
child_t2 <- c("Median (25th, 75th percentile)", writeqntle(d$il1_t2), writeqntle(d$il6_t2), writeqntle(d$tnfa_t2), writeqntle(d$il12_t2), writeqntle(d$ifng_t2), writeqntle(d$il4_t2), writeqntle(d$il5_t2), writeqntle(d$il13_t2), writeqntle(d$il17_t2), writeqntle(d$il21_t2), writeqntle(d$il10_t2), writeqntle(d$il2_t2), writeqntle(d$gmcsf_t2), writeqntle(d$agp_t2), writeqntle(d$crp_t2))

child_t3<-c("Median (25th, 75th percentile)", writeqntle(d$il1_t3), writeqntle(d$il6_t3), writeqntle(d$tnfa_t3), writeqntle(d$il12_t3), writeqntle(d$ifng_t3), writeqntle(d$il4_t3), writeqntle(d$il5_t3), writeqntle(d$il13_t3), writeqntle(d$il17_t3), writeqntle(d$il21_t3), writeqntle(d$il10_t3), writeqntle(d$il2_t3), writeqntle(d$gmcsf_t3), " ", " ")

mom_tbl<-data.table(" "= mom_lab,
                "At Enrollment"=mom)

child_tbl<-data.table(" "= child_lab,
                "Age 14 Months"=child_t2,
                "Age 28 Months"=child_t3)

#view(mom_tbl)
#view(child_tbl)

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)

save_as_docx("Maternal Biomarkers" = flextable(mom_tbl), path="/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/biomarkers_mom.docx", 
             pr_section = sect_properties) 

save_as_docx("Child Biomarkers" = flextable(child_tbl), path="/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/biomarkers_child.docx", 
             pr_section = sect_properties) 

#write.csv(tbl, file=here('tables/biomarkers.csv'))
#print(xtable(tbl), type="html", file=here("tables/biomarkers.html"))
