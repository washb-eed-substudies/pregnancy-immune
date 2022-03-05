rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)

#load spline data
H1_spline <- readRDS(here("figure-data/H1_adj_spline.data.RDS"))

#load results for quartiles
H1_quartiles <- readRDS(here("results/adjusted/H1_adj_res.RDS"))

d_for_plot <- function(x_name, y_name, x_var, y_var, spline, quart){
  d <- NULL
  for (i in 1: length(x_var)) {
    for (j in 1:length(y_var)){
      exists <- (quart%>%filter(X==x_var[i], Y==y_var[j]) %>% nrow()) != 0
      if (exists){
        new <- data.frame(x=x_name[i], y=y_name[j], quart%>%filter(X==x_var[i], Y==y_var[j]))
        d <- rbind(d, new)
      }
    }
  }
  d
}

color_levels <- c("Ln AGP", "Ln CRP", "Ln IFN-y", "Sum score of 13 cytokines")

d1 <- d_for_plot(c("Vitamin D", "Vitamin D deficiency", "Ln RBP", "Vitamin A deficiency","Ln ferritin", "Ln sTfR", "Iron deficiency"),
                 c("Ln AGP", "Ln CRP", "Ln IFN-y", "Sum score of 13 cytokines",
                   "Ln AGP", "Ln CRP", "Ln IFN-y", "Sum score of 13 cytokines"), 
                 c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def"),   
                 c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
                   "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z"),
                 H1_spline, H1_quartiles)

d1$x <- d1$x %>% as.factor()
d1$y <- factor(d1$y, levels=color_levels)
d1$`Time of outcome measurement` <- factor(ifelse(grepl("t3", d1$Y), "Age 28 months", "Age 14 months"))

p <- ggplot(d1, aes(x=x, y=point.diff)) + 
  scale_x_discrete(limits=rev(levels(d1$x)))+
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, shape=`Time of outcome measurement`),
                  position = position_dodge2(width = 0.5, reverse = T),
                  size = 1, show.legend = T) +
  coord_flip(expand = T) + 
  facet_grid(~y, scales = "free_x", ) + 
  labs(y = "Adjusted difference in mean child immune status outcome between 25th and 75th percentile of maternal exposure", 
       x="Maternal micronutrient exposure") +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_shape_manual(breaks=c("Age 14 months","Age 28 months"), values=c(21,16)) +
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_text(size=10),
        strip.text = element_text(hjust=0.5, size=9),
        axis.text.y = element_text(hjust = 1, size=10),
        panel.spacing = unit(0, "lines"),
        legend.position = "bottom")      
p


p %>% ggsave(filename="figures/pregnancy-immune_point_diff_micronutrients.jpg", width=10, height=7)

d3 <- H1_quartiles %>% rbind(H2_quartiles) %>% filter(grepl("th1_th2", X))
d3 <- d3 %>% mutate(group=factor(ifelse(Y=="sum_who", "WHO motor milestones", ifelse(grepl("cdi", Y), "CDI Z-scores", "EASQ Z-scores")),
                                 levels=c("WHO motor milestones", "CDI Z-scores", "EASQ Z-scores")), 
                    Xvar = factor(ifelse(grepl("t2", X), "Th1/Th2 Year 1", "Th1/Th2 Year 2"), levels=c("Th1/Th2 Year 1", "Th1/Th2 Year 2")),
                    Yvar = case_when(grepl("who", Y) ~ "Sum of WHO motor milestones",
                                     grepl("und", Y) ~ "CDI comprehension Z-score",
                                     grepl("say", Y) ~ "CDI expressive language Z-score",
                                     grepl("comm", Y) ~ "EASQ communication Z-score",
                                     grepl("motor", Y) ~ "EASQ gross motor Z-score",
                                     grepl("personal", Y) ~ "EASQ personal social Z-score",
                                     grepl("combined", Y) ~ "EASQ combined Z-score"))
d3$Yvar <- factor(d3$Yvar, levels=c("Sum of WHO motor milestones", paste(c("CDI comprehension", "CDI expressive language",
                                                                           "EASQ communication", "EASQ gross motor", "EASQ personal social", "EASQ combined"), "Z-score")))
d3$Ytime <- factor(ifelse(grepl("t3|easq", d3$Y), "Year 2", "Year 1"))

p <- ggplot(d3, aes(x=Yvar, y=point.diff)) + 
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=group, group=Yvar, shape=Ytime),
                  position = position_dodge2(reverse=T,width = 0.5),
                  size = 1) +
  #geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  scale_x_discrete(limits = rev(levels(d3$Yvar))) +
  coord_flip() + 
  facet_grid(~Xvar) + 
  labs(y = "Adjusted difference in mean development outcome\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_shape_manual(breaks =c("Year 1", "Year 2"), values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"),
        legend.position = "right") +
  guides(color = guide_legend(title="Development measurement"), 
         shape=guide_legend(title="Development\nmeasurement time"))

p


HR <- readRDS(here("results/adjusted/HR_adj_res.RDS"))
HR_th1_th2 <- HR %>% filter(grepl("th1_th2", X) & !grepl("sit", Y))
HR_th1_th2$Yvar <- factor(c("Hands-and-knees crawling","Standing with assistance",
                            "Walking with assistance","Standing alone","Walking alone"),
                          levels=c("Hands-and-knees crawling","Standing with assistance","Walking with assistance","Standing alone","Walking alone"))
HR_th1_th2$Xvar <- factor(ifelse(grepl("t2", HR_th1_th2$X), "Th1/Th2 Year 1", "Th1/Th2 Year 2"), levels=c("Th1/Th2 Year 1", "Th1/Th2 Year 2"))

p1 <- ggplot(HR_th1_th2, aes(x=Yvar, y=point.HR, color="WHO motor milestones", shape="Year 1")) + 
  geom_pointrange(aes(ymin=lb.HR, ymax=ub.HR),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(HR_th1_th2$Yvar))) +
  facet_grid(~Xvar, drop = T) +
  coord_flip() + 
  labs(y = "Adjusted hazard ratio of motor milestone achievement\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))

ggpubr::ggarrange(p1, p, widths = c(1.5, 2.5)) %>%  ggsave(filename = here("figures/gam_th1_th2.jpg"), width = 16, height=7)




d1 <- H1_quartiles %>% rbind(H2_quartiles) %>% filter(grepl("crp", X))
d1 <- d1 %>% mutate(group=factor(ifelse(Y=="sum_who", "WHO motor milestones", ifelse(grepl("cdi", Y), "CDI Z-scores", "EASQ Z-scores")),
                                 levels=c("WHO motor milestones", "CDI Z-scores", "EASQ Z-scores")), 
                    Xvar = factor(ifelse(grepl("t2", X), "Ln CRP Year 1", "Ln CRP Year 2"), levels=c("Ln CRP Year 1", "Ln CRP Year 2")),
                    Yvar = case_when(grepl("who", Y) ~ "Sum of WHO motor milestones",
                                     grepl("und", Y) ~ "CDI comprehension Z-score",
                                     grepl("say", Y) ~ "CDI expressive language Z-score",
                                     grepl("comm", Y) ~ "EASQ communication Z-score",
                                     grepl("motor", Y) ~ "EASQ gross motor Z-score",
                                     grepl("personal", Y) ~ "EASQ personal social Z-score",
                                     grepl("combined", Y) ~ "EASQ combined Z-score"))
d1$Yvar <- factor(d1$Yvar, levels=c("Sum of WHO motor milestones", paste(c("CDI comprehension", "CDI expressive language",
                                                                           "EASQ communication", "EASQ gross motor", "EASQ personal social", "EASQ combined"), "Z-score")))
d1$Ytime <- factor(ifelse(grepl("t3|easq", d1$Y), "Year 2", "Year 1"))

p <- ggplot(d1, aes(x=Yvar, y=point.diff)) + 
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=group, group=Yvar, shape=Ytime),
                  position = position_dodge2(reverse = TRUE, width = .5),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(d1$Yvar)))+
  #geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  facet_grid(~Xvar, drop = T) +
  coord_flip() + 
  labs(y = "Adjusted difference in mean development outcome\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"),
        legend.position = "right")+
  guides(color = guide_legend(title="Development measurement"), shape=guide_legend(title="Development\nmeasurement time", reverse = T))
p


HR_crp <- HR %>% filter(grepl("crp", X) & !grepl("sit", Y))
HR_crp$Yvar <- factor(c("Hands-and-knees crawling","Standing with assistance",
                        "Walking with assistance","Standing alone","Walking alone"),
                      levels=c("Hands-and-knees crawling","Standing with assistance","Walking with assistance","Standing alone","Walking alone"))
HR_crp$Xvar <- factor(ifelse(grepl("t2", HR_crp$X), "Ln CRP Year 1", "Ln CRP Year 2"), levels=c("Ln CRP Year 1", "Ln CRP Year 2"))

p1 <- ggplot(HR_crp, aes(x=Yvar, y=point.HR, color="WHO motor milestones", shape="Year 1")) + 
  geom_pointrange(aes(ymin=lb.HR, ymax=ub.HR),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(HR_crp$Yvar)))+
  facet_grid(~Xvar, drop = T, scales = "free") +
  coord_flip() + 
  labs(y = "Adjusted hazard ratio of motor milestone achievement\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))
p1 

ggpubr::ggarrange(p1, p, widths = c(1.5, 2.5)) %>%  ggsave(filename = here("figures/gam_crp.jpg"), width = 16, height=7)



d2 <- H1_quartiles %>% rbind(H2_quartiles) %>% filter(grepl("agp", X))
d2 <- d2 %>% mutate(group=factor(ifelse(Y=="sum_who", "WHO motor milestones", ifelse(grepl("cdi", Y), "CDI Z-scores", "EASQ Z-scores")),
                                 levels=c("WHO motor milestones", "CDI Z-scores", "EASQ Z-scores")), 
                    Xvar = factor(ifelse(grepl("t2", X), "Ln AGP Year 1", "Ln AGP Year 2"), levels=c("Ln AGP Year 1", "Ln AGP Year 2")),
                    Yvar = case_when(grepl("who", Y) ~ "Sum of WHO motor milestones",
                                     grepl("und", Y) ~ "CDI comprehension Z-score",
                                     grepl("say", Y) ~ "CDI expressive language Z-score",
                                     grepl("comm", Y) ~ "EASQ communication Z-score",
                                     grepl("motor", Y) ~ "EASQ gross motor Z-score",
                                     grepl("personal", Y) ~ "EASQ personal social Z-score",
                                     grepl("combined", Y) ~ "EASQ combined Z-score"))
d2$Yvar <- factor(d2$Yvar, levels=c("Sum of WHO motor milestones", paste(c("CDI comprehension", "CDI expressive language",
                                                                           "EASQ communication", "EASQ gross motor", "EASQ personal social", "EASQ combined"), "Z-score")))
d2$Ytime <- factor(ifelse(grepl("t3|easq", d2$Y), "Year 2", "Year 1"))

p <- ggplot(d2, aes(x=Yvar, y=point.diff)) + 
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=group, group=Yvar, shape=Ytime),
                  position = position_dodge2(reverse = TRUE, width = .5),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(d1$Yvar)))+
  #geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  facet_grid(~Xvar, drop = T) +
  coord_flip() + 
  labs(y = "Adjusted difference in mean development outcome\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"),
        legend.position = "right")+
  guides(color = guide_legend(title="Development measurement"), shape=guide_legend(title="Development\nmeasurement time", reverse = T))
p


HR_agp <- HR %>% filter(grepl("agp", X) & !grepl("sit", Y))
HR_agp$Yvar <- factor(c("Hands-and-knees crawling","Standing with assistance",
                        "Walking with assistance","Standing alone","Walking alone"),
                      levels=c("Hands-and-knees crawling","Standing with assistance","Walking with assistance","Standing alone","Walking alone"))
HR_agp$Xvar <- factor(ifelse(grepl("t2", HR_agp$X), "Ln AGP Year 1", "Ln AGP Year 2"), levels=c("Ln AGP Year 1", "Ln AGP Year 2"))

p1 <- ggplot(HR_agp, aes(x=Yvar, y=point.HR, color="WHO motor milestones", shape="Year 1")) + 
  geom_pointrange(aes(ymin=lb.HR, ymax=ub.HR),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(HR_agp$Yvar)))+
  facet_grid(~Xvar, drop = T, scales = "free") +
  coord_flip() + 
  labs(y = "Adjusted hazard ratio of motor milestone achievement\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))
p1 

ggpubr::ggarrange(p1, p, widths = c(1.5, 2.5)) %>%  ggsave(filename = here("figures/gam_agp.jpg"), width = 16, height=7)





ind_quartiles <- readRDS(here("results/adjusted/individual_adj_res.RDS"))
d2 <- ind_quartiles %>% filter(grepl("il4_", X))
d2 <- d2 %>% mutate(group=factor(ifelse(Y=="sum_who", "WHO motor milestones", ifelse(grepl("cdi", Y), "CDI Z-scores", "EASQ Z-scores")),
                                 levels=c("WHO motor milestones", "CDI Z-scores", "EASQ Z-scores")), 
                    Xvar = factor(ifelse(grepl("t2", X), "Ln IL-4/IL-10 Year 1", "Ln IL-4/IL-10 Year 2"), levels=c("Ln IL-4/IL-10 Year 1", "Ln IL-4/IL-10 Year 2")),
                    Yvar = case_when(grepl("who", Y) ~ "Sum of WHO motor milestones",
                                     grepl("und", Y) ~ "CDI comprehension Z-score",
                                     grepl("say", Y) ~ "CDI expressive language Z-score",
                                     grepl("comm", Y) ~ "EASQ communication Z-score",
                                     grepl("motor", Y) ~ "EASQ gross motor Z-score",
                                     grepl("personal", Y) ~ "EASQ personal social Z-score",
                                     grepl("combined", Y) ~ "EASQ combined Z-score"))
d2$Yvar <- factor(d2$Yvar, levels=c("Sum of WHO motor milestones", paste(c("CDI comprehension", "CDI expressive language",
                                                                           "EASQ communication", "EASQ gross motor", "EASQ personal social", "EASQ combined"), "Z-score")))
d2$Ytime <- factor(ifelse(grepl("t3|easq", d2$Y), "Year 2", "Year 1"))

p <- ggplot(d2, aes(x=Yvar, y=point.diff)) + 
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=group, group=Yvar, shape=Ytime),
                  position = position_dodge2(reverse = TRUE, width = .5),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(d2$Yvar)))+
  #geom_text(aes(label=ref), position = position_nudge(y = (abs(yrange[1])+abs(yrange[2]))/10)) +
  facet_grid(~Xvar, drop = T) +
  coord_flip() + 
  labs(y = "Adjusted difference in mean development outcome\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"),
        legend.position = "right")+
  guides(color = guide_legend(title="Development measurement"), shape=guide_legend(title="Development\nmeasurement time", reverse = T))
p


HR_agp <- HR %>% filter(grepl("il4_", X) & !grepl("sit", Y))
HR_agp$Yvar <- factor(c("Hands-and-knees crawling","Standing with assistance",
                        "Walking with assistance","Standing alone","Walking alone"),
                      levels=c("Hands-and-knees crawling","Standing with assistance","Walking with assistance","Standing alone","Walking alone"))
HR_agp$Xvar <- factor(ifelse(grepl("t2", HR_agp$X), "Ln IL-4/IL-10 Year 1", "Ln IL-4/IL-10 Year 2"), levels=c("Ln IL-4/IL-10 Year 1", "Ln IL-4/IL-10 Year 2"))

p1 <- ggplot(HR_agp, aes(x=Yvar, y=point.HR, color="WHO motor milestones", shape="Year 1")) + 
  geom_pointrange(aes(ymin=lb.HR, ymax=ub.HR),
                  size = 1) +
  scale_x_discrete(limits = rev(levels(HR_agp$Yvar)))+
  facet_grid(~Xvar, drop = T, scales = "free") +
  coord_flip() + 
  labs(y = "Adjusted hazard ratio of motor milestone achievement\nbetween 25th and 75th percentile of Th1/Th2 ratio", 
       x=element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_shape_manual(values=c(21,16)) +
  scale_colour_manual(values=tableau10) + 
  theme_ki() +
  theme(plot.title = element_text(hjust = 0),
        panel.spacing = unit(0, "lines"))
p1 

ggpubr::ggarrange(p1, p, widths = c(1.5, 2.5)) %>%  ggsave(filename = here("figures/gam_agp.jpg"), width = 16, height=7)

