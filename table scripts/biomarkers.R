rm(list=ls())
library(data.table)
source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)
library(boxr)

#box_auth()
d <- readRDS("/Users/kjung0909/Documents/Research/WASHB/bangladesh-cleaned-master-data.RDS") %>% filter(.$pregnancy_immune == 1)
d %>% group_by(dataid) %>% summarise(n=n()) %>% filter(n>1)
filter(d, dataid %in% c(23404, 31102, 35105)) %>% select(dataid, childid)
# different iron deficiency cutoff
d <- d %>% mutate(vit_A_def = ifelse(RBP_inf_preg < 0.7, 1, 0))
#unique moms
m <- rbind(filter(d, !(dataid %in% c(23404, 31102, 35105))), filter(d, dataid %in% c(23404, 31102, 35105))[1:3,])

writeqntle<-function(vector) {
  quantiles<-round(quantile(vector, na.rm=TRUE), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

mom_lab <-c("Maternal Biomarker", 
        "Vitamin D (nmol/L)", "RBP (umol/L)", "Ferritin (ug/L)", "sTfR (mg/L)",  
        "Cortisol (ug/dL)", "Estriol (ng/mL)", "Cytokine Sum Score",
        "IL-1B (pg/mL)", "IL-6 (pg/mL)", "TNF-a (pg/mL)", "IL-12 (pg/mL)", "IFN-y (pg/mL)", "IL-4 (pg/mL)", "IL-5 (pg/mL)", "IL-13 (pg/mL)", "IL-17A (pg/mL)", "IL-21 (pg/mL)", "IL-10 (pg/mL)", "IL-2 (pg/mL)", "GM-CSF (pg/mL)", "AGP (g/L)", "CRP (mg/L)")

child_lab <-c("Child Biomarker", "Cytokine Sum Score",
          "IL-1B (pg/mL)", "IL-6 (pg/mL)", "TNF-a (pg/mL)", "IL-12 (pg/mL)", "IFN-y (pg/mL)", "IL-4 (pg/mL)", "IL-5 (pg/mL)", "IL-13 (pg/mL)", "IL-17A (pg/mL)", "IL-21 (pg/mL)", "IL-10 (pg/mL)", "IL-2 (pg/mL)", "GM-CSF (pg/mL)", "AGP (g/L)", "CRP (mg/L)")

mom <-c("Median (25th, 75th percentile)", 
        writeqntle(m$vitD_nmol_per_L), writeqntle(m$rbp), writeqntle(m$FERR_inf_preg), writeqntle(m$STFR_inf_preg),
        writeqntle(m$preg_cort), writeqntle(m$preg_estri), writeqntle(m$sumscore_t0_mom_Z),
        writeqntle(m$il1_mom_t0), writeqntle(m$il6_mom_t0), writeqntle(m$tnfa_mom_t0), writeqntle(m$il12_mom_t0), writeqntle(m$ifng_mom_t0), writeqntle(m$il4_mom_t0), writeqntle(m$il5_mom_t0), writeqntle(m$il13_mom_t0), writeqntle(m$il17_mom_t0), writeqntle(m$il21_mom_t0), writeqntle(m$il10_mom_t0), writeqntle(m$il2_mom_t0), writeqntle(m$gmcsf_mom_t0), writeqntle(m$agp), writeqntle(m$crp))
        
child_t2 <- c("Median (25th, 75th percentile)", writeqntle(d$sumscore_t2_Z), writeqntle(d$il1_t2), writeqntle(d$il6_t2), writeqntle(d$tnfa_t2), writeqntle(d$il12_t2), writeqntle(d$ifng_t2), writeqntle(d$il4_t2), writeqntle(d$il5_t2), writeqntle(d$il13_t2), writeqntle(d$il17_t2), writeqntle(d$il21_t2), writeqntle(d$il10_t2), writeqntle(d$il2_t2), writeqntle(d$gmcsf_t2), writeqntle(d$agp_t2), writeqntle(d$crp_t2))

child_t3<-c("Median (25th, 75th percentile)", writeqntle(d$sumscore_t3_Z), writeqntle(d$il1_t3), writeqntle(d$il6_t3), writeqntle(d$tnfa_t3), writeqntle(d$il12_t3), writeqntle(d$ifng_t3), writeqntle(d$il4_t3), writeqntle(d$il5_t3), writeqntle(d$il13_t3), writeqntle(d$il17_t3), writeqntle(d$il21_t3), writeqntle(d$il10_t3), writeqntle(d$il2_t3), writeqntle(d$gmcsf_t3))

mom_tbl<-data.table(" "= mom_lab,
                "At Enrollment"=mom)

child_tbl<-data.table(" "= child_lab,
                "Age 14 Months"=child_t2,
                "Age 28 Months"=child_t3)

#view(mom_tbl)
#view(child_tbl)

#COUNTING NUMBER OF NUTRIENT-DEFICIENT MOMS
#total number of moms
sum(m$vit_D_def, na.rm=T)
sum(m$vit_A_def, na.rm=T)
sum(m$iron_def, na.rm=T)
  filter(m, FERR_inf_preg < 12 & STFR_inf_preg <= 8.3) %>% nrow()
  filter(m, FERR_inf_preg >= 12 & STFR_inf_preg > 8.3) %>% nrow()
  filter(m, FERR_inf_preg < 12 & STFR_inf_preg > 8.3) %>% nrow()

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)

save_as_docx("Maternal Biomarkers" = flextable(mom_tbl), path="/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/supplementary/biomarkers_mom.docx", 
             pr_section = sect_properties) 

save_as_docx("Child Biomarkers" = flextable(child_tbl), path="/Users/kjung0909/Documents/Research/WASHB/Pregnancy + Immune/pregnancy-immune/tables/supplementary/biomarkers_child.docx", 
             pr_section = sect_properties) 

#write.csv(tbl, file=here('tables/biomarkers.csv'))
#print(xtable(tbl), type="htmL", file=here("tables/biomarkers.htmL"))
