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

full_res <- rbind(H1, H2, H3, H4)
full_adj_res <- rbind(H1adj, H2adj, H3adj, H4adj)