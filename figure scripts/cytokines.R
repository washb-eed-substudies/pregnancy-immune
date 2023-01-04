rm(list=ls())
library(data.table)
source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)
library(boxr)
library(ggplot2)


#box_auth()
d <- readRDS("/Users/kjung0909/Documents/Research/WASHB/bangladesh-cleaned-master-data.RDS") %>% filter(.$pregnancy_immune == 1)


#selecting cytokine columns and renaming them
t2 <- data.frame(d$t2_ln_agp, d$t2_ln_crp, d$t2_ln_gmc, d$t2_ln_ifn, d$t2_ln_il10, d$t2_ln_il12, d$t2_ln_il13, d$t2_ln_il17, d$t2_ln_il1, d$t2_ln_il2, d$t2_ln_il21, d$t2_ln_il4, d$t2_ln_il5, d$t2_ln_il6, d$t2_ln_tnf)
setnames(t2, 
         old = c('d.t2_ln_agp', 'd.t2_ln_crp', 'd.t2_ln_gmc', 'd.t2_ln_ifn', 'd.t2_ln_il10', 'd.t2_ln_il12', 'd.t2_ln_il13', 'd.t2_ln_il17', 'd.t2_ln_il1', 'd.t2_ln_il2', 'd.t2_ln_il21', 'd.t2_ln_il4', 'd.t2_ln_il5', 'd.t2_ln_il6', 'd.t2_ln_tnf'),
         new = c('Ln AGP', 'Ln CRP', 'Ln GM-CSF', 'Ln IFN-y', 'Ln IL-10', 'Ln IL-12', 'Ln IL-13', 'Ln IL-17', 'Ln IL-1', 'Ln IL-2', 'Ln IL-21', 'Ln IL-4', 'Ln IL-5', 'Ln IL-6', 'Ln TNF-a'))

t3 <- data.frame(d$t3_ln_gmc, d$t3_ln_ifn, d$t3_ln_il10, d$t3_ln_il12, d$t3_ln_il13, d$t3_ln_il17, d$t3_ln_il1, d$t3_ln_il2, d$t3_ln_il21, d$t3_ln_il4, d$t3_ln_il5, d$t3_ln_il6, d$t3_ln_tnf)
setnames(t3, 
         old = c('d.t3_ln_gmc', 'd.t3_ln_ifn', 'd.t3_ln_il10', 'd.t3_ln_il12', 'd.t3_ln_il13', 'd.t3_ln_il17', 'd.t3_ln_il1', 'd.t3_ln_il2', 'd.t3_ln_il21', 'd.t3_ln_il4', 'd.t3_ln_il5', 'd.t3_ln_il6', 'd.t3_ln_tnf'),
         new = c('Ln GM-CSF', 'Ln IFN-y', 'Ln IL-10', 'Ln IL-12', 'Ln IL-13', 'Ln IL-17', 'Ln IL-1', 'Ln IL-2', 'Ln IL-21', 'Ln IL-4', 'Ln IL-5', 'Ln IL-6', 'Ln TNF-a'))


#plots
t2_pairs <- ggcorr(t2, method = c("pairwise", "pearson"), label = TRUE, label_round = 2, label_size = 3)

t3_pairs <- ggcorr(t3, method = c("pairwise", "pearson"), label = TRUE, label_round = 2, label_size = 3)


#saving plots as jpg images
t2_pairs %>% ggsave(filename="figures/cytokines-t2.jpg", width = 8, height = 9)
t3_pairs %>% ggsave(filename="figures/cytokines-t3.jpg", width = 8, height = 9)
