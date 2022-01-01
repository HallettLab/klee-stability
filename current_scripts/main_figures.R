## Main Figures ## 

## load packages
library(tidyverse)
library(lubridate)
library(scico)
library(ggpubr)
library(cowplot)
## read in data
source("current_scripts/finalprep_postcalcs.R") 

## Figure Settings ## 
theme_set(theme_classic()) #set the theme


## Figure 1: Conceptual ##


## Figure 2: Cover and Richness over time
## get dates correct for plotting
ppt_date <- drought_record %>%
  mutate(Date_final = ymd(sample_date)) %>%
  filter(drought == 1)

## calculate mean cover at each time point 
meancov <- totcov %>%
  mutate(type = "Full") %>%
  group_by(TREATMENT, Date_final, type) %>%
  summarise(meancov = mean(totcov))

## change treatment to a factor and reorder
meancov$TREATMENT <- as.factor(meancov$TREATMENT)
meancov <- meancov %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))

## make a vector of all sample dates to use for axis labels
dates <- meancov[['Date_final']]
june <- unique(dates[dates %like% "06-"])

## Graph total cover over time
tcov_drought <- ggplot(meancov, aes(x=Date_final, y=meancov, color = TREATMENT)) +
  geom_vline(data = ppt_date, mapping = aes(xintercept = Date_final), linetype = "dashed") +
  geom_line(size = 1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  ylab("Total Cover") + xlab("") +
  scale_x_date(breaks = june, labels = NULL) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  labs(col = "Treatment") 

## Graph richness over time
richtime <- ggplot(meanannrich, aes(x=Date_final, y=meanrich, col=TREATMENT)) +
  geom_vline(data = ppt_date, mapping = aes(xintercept = Date_final), linetype = "dashed") +
  geom_line(size = 0.72) +
  ylab("Richness") + xlab("") +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.title = element_text(size=14)) +
  theme(text = element_text(size = 14)) +
  scale_x_date(breaks = june, date_labels = "%Y") +
  theme(legend.position = "none") +
  labs(col = "Treatment") + #change legend title
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(tcov_drought, richtime, ncol = 1, nrow = 2, 
          common.legend = TRUE,
          legend = "right",
          labels = "AUTO", 
          align = "v")



## Figure 3: Stability (full ts, 5yr & 10yr) 
s <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_st)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) + #add standard error bars
  ylab("Stability (all years)") + xlab("Treatment") + #label axes
  theme(text = element_text(size = 12)) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) + 
  scale_fill_scico_d(palette = "batlow", direction = -1)

sdreg <- ggplot(figdrst, aes(x=Dscore, y=stability)) +
  geom_point() +
  ylab("Stability (10 year)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.4) +
  theme(text = element_text(size = 12)) +
  #geom_line(stat = "smooth", method = "lm", formula = y~x,
           # aes(color = TREATMENT),
           # size = 0.75, 
           # alpha = 0.5) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12))

sdreg5 <- ggplot(figdrst5, aes(x=Dscore, y=stability)) +
  geom_point() +
  ylab("Stability (5 year)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.4) +
  theme(text = element_text(size = 12)) +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            aes(color = TREATMENT),
            size = 0.75, 
            alpha = 0.5) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12))

ggarrange(s, sdreg, 
          ncol = 2, nrow = 1, 
          common.legend = TRUE, 
          legend = "bottom", 
          align = "hv", 
          labels = "AUTO")

ggsave("fig3.png", width = 7, height = 4)


## Figure 4: One Dscore-stability - cvr regression figure
## variance ratio by herbivore treatment
VRtrt <- ggplot(tsVR_plot, aes(x=TREATMENT, y=mean_cVR, fill = TREATMENT, shape = community_type, color = TREATMENT)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_cVR-SE_cVR, ymax=mean_cVR+SE_cVR), width = 0.2, color = "black") +
  geom_point(size = 3) +
  geom_point(size = 3, fill = NA, colour = "black") +
  scale_shape_manual(values = c(21,22,18)) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) + 
  xlab("Treatment") + ylab("Variance Ratio (all years)") +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  guides(color=guide_legend("Treatment"), fill = FALSE, shape =guide_legend("Species")) +
  annotate("text", x =5, y=1.08, label = "synchronous", size = 3.5,  fontface = 'italic') +
  annotate("text", x =5, y=0.95, label = "compensatory", size = 3.5, fontface = 'italic')


## population stability by herbivore
pdomspstab <- ggplot(meanstab_mech, aes(x=TREATMENT, y=avgpopstab)) +
  geom_errorbar(aes(ymin = avgpopstab-SEpopstab, ymax=avgpopstab+SEpopstab), width = 0.3) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=3) + 
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Population Stability (all years)") + xlab("Treatment") +  
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12))

## richness by herbivore treatment
prich <- ggplot(meanstab_mech, aes(x=TREATMENT , y=mean_rich)) +
  geom_errorbar(aes(ymin = mean_rich-SE_rich, ymax=mean_rich+SE_rich), width = 0.3) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Species Richness (all years)") + xlab("Treatment") +  
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) + 
  labs(col = "Time Period")

## variance ratio (10 year)
vrdreg <- ggplot(figdrcvr, aes(x=Dscore, y=classicVR)) +
  geom_point() +
  ylab("Variance Ratio (10 year)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25) +
  #geom_line(stat = "smooth", method = "lm", formula = y~x,
          #  aes(color = TREATMENT),
          #  size = 0.75, 
          #  alpha = 0.5) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  annotate("text", x =0.1, y=1.1, label = "synchronous", size = 3.5, fontface = 'italic') +
  annotate("text", x =0.1, y=0.95, label = "compensatory", size = 3.5, fontface = 'italic')


## population stability (10 year)
psdreg <- ggplot(figdrpopst, aes(x=Dscore, y=popstab)) +
  geom_point() +
  ylab("Population Stability (10 year)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25, color = "darkgray") +
  #geom_line(stat = "smooth", method = "lm", formula = y~x,
   #         aes(color = TREATMENT),
    #        size = 0.75, 
     #       alpha = 0.5) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) 


## richness (10 year)
ridreg <- ggplot(figdrri, aes(x=Dscore, y=richness)) +
  geom_point() +
  ylab("Species Richness (10 year)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25) +
  #geom_line(stat = "smooth", method = "lm", formula = y~x,
   #         aes(color = TREATMENT),
    #        size = 0.75, 
     #       alpha = 0.5) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) 


ggarrange(VRtrt,vrdreg, 
          pdomspstab,psdreg, 
          prich,ridreg,
          ncol = 2, nrow = 3, 
          common.legend = TRUE, 
          legend = "bottom", 
          align = "hv", 
          labels = "AUTO")


ggsave("fig4.png", height = 10, width = 6.5)

## Figure 5: Stability & Biotic Mechanisms
## VR and stability
VRstab <- ggplot(meanstab_mech, aes(x=mean_classicVR, y=mean_st)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = T, color = "black")+
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.06) +
  geom_errorbarh(aes(xmin = mean_classicVR-SEclassicVR, xmax=mean_classicVR+SEclassicVR), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Stability") + xlab("Variance Ratio") +  
  #theme(legend.title = element_text(size=14)) +  
  theme(text = element_text(size = 14))  + #change font sizes
  #geom_abline(slope = -10.8462, intercept = 5.6445) +
  #labs(col = "Treatment") +
  annotate("text", x =1.07, y=3.3, label = "synchronous", size = 5, angle='90', fontface = 'italic') +
  annotate("text", x =0.9, y=3.3, label = "compensatory", size = 5, angle='90',fontface = 'italic') +
  theme(legend.position = "none")

## Aggregate dominant sp stability by stability
domspstabst <- ggplot(meanstab_mech, aes(x=avgpopstab, y=mean_st)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = T, color = "black") +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.06) +
  geom_errorbarh(aes(xmin = avgpopstab-SEpopstab, xmax=avgpopstab+SEpopstab), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("Stability") + xlab("Population Stability") +  
  #theme(legend.title = element_text(size=14)) +  
  theme(text = element_text(size = 14)) + #change font sizes
  #labs(col = "Treatment") +
  theme(legend.position = "none")

## richness and stability
prichstab <- ggplot(meanstab_mech, aes(x=mean_rich, y=mean_st)) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) +
  geom_errorbarh(aes(xmin = mean_rich-SE_rich, xmax=mean_rich+SE_rich), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("Stability") + xlab("Species Richness") +  
  #theme(legend.title = element_text(size=14)) +  
  theme(text = element_text(size = 14))  + #change font sizes
  #labs(col = "Treatment") +
  theme(legend.position = "none")

fig5 <- ggarrange(VRstab, domspstabst, prichstab,
          ncol = 3, nrow=1,
          align = "hv",
          labels = "AUTO",
          common.legend = FALSE)

## extract the legend from Fig 4 for use in Fig 5
leg <- get_legend(VRtrt +
                    theme(legend.position = "bottom"))

plot_grid(fig5, leg, rel_heights = c(8,1), ncol = 1)

ggsave("fig5.png", width = 9, height = 4)

