## Main Figures ## 

## load packages
library(tidyverse)
library(lubridate)
library(scico)
library(ggpubr)
library(cowplot)
library(scico)

## read in data
source("finalprep_postcalcs.R") 

## Figure Settings ## 
theme_set(theme_classic()) #set the theme


## Figure 1: Conceptual ##
  ## created in Illustrator


## Figure 2: Cover and Richness over time
## get dates correct for plotting
ppt_date <- drought_record %>%
  mutate(Date_final = ymd(sample_date)) %>%
  filter(drought == 1)

ppt_record <- drought_record %>%
  mutate(Date_final = ymd(sample_date))

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

## Graph annual rainfall over time
ppt_plot <- ggplot(ppt_record, aes(x=Date_final, y=preceding12ppt)) +
  geom_line(size = 0.75) +
  geom_vline(data = ppt_date, mapping = aes(xintercept = Date_final), linetype = "dashed") +
  xlab("") + ylab("Rainfall (mm)") +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_date(breaks = june, labels = NULL)


## Graph total cover over time
tcov_drought <- ggplot(meancov, aes(x=Date_final, y=meancov, color = TREATMENT)) +
  geom_vline(data = ppt_date, mapping = aes(xintercept = Date_final), linetype = "dashed") +
  geom_line(size = 0.75) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  ylab("Total Cover") + xlab("") +
  scale_x_date(breaks = june, labels = NULL) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  labs(col = "Treatment") 

## Graph richness over time
richtime <- ggplot(meanannrich, aes(x=Date_final, y=meanrich, col=TREATMENT)) +
  geom_vline(data = ppt_date, mapping = aes(xintercept = Date_final), linetype = "dashed") +
  geom_line(size = 0.75) +
  ylab("Richness") + xlab("") +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 12)) +
  scale_x_date(breaks = june, date_labels = "%Y") +
  theme(legend.position = "none") +
  labs(col = "Treatment") + #change legend title
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pdf("figure2.pdf", width = 7.087, height = 6.5, onefile = F)

ggarrange(ppt_plot, tcov_drought, richtime, ncol = 1, nrow = 3, 
          common.legend = TRUE,
          legend = "bottom",
          labels = "AUTO", 
          align = "v")

dev.off()


## Figure 3: Stability (full ts, 10yr) 
s <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_st)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) + #add standard error bars
  ylab("Stability (all years)") + xlab("Herbivore Treatment") + #label axes
  theme(legend.title = element_text(size=12)) + 
  theme(text = element_text(size = 12)) +
  theme(legend.text = element_text(size=12)) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) + 
  labs(fill = "Herbivore Treatment") +
  scale_fill_scico_d(palette = "batlow", direction = -1)

sdreg <- ggplot(mwfigs10, aes(x=Dscore, y=stability)) +
  geom_point() +
  ylab("Stability (10 year)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25) +
  theme(legend.text = element_text(size=12)) +
  labs(fill = "Treatment:") +
  theme(legend.title = element_text(size=12)) +  
  theme(text = element_text(size = 12))


pdf("figure3.pdf", width = 7.087, height = 3.75, onefile = F)

ggarrange(sdreg, s, 
          ncol = 2, nrow = 1, 
          common.legend = TRUE, 
          legend = "bottom", 
          align = "hv", 
          labels = "AUTO")

dev.off()



## Figure 4: Top-down and bottom-up effects on biotic stability mechanisms
## variance ratio by herbivore treatment
VRtrt <- ggplot(tsVR_plot, aes(x=TREATMENT, y=mean_cVR, fill = TREATMENT, shape = community_type, color = TREATMENT)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_cVR-SE_cVR, ymax=mean_cVR+SE_cVR), width = 0.2, color = "black") +
  geom_point(size = 3) +
  geom_point(size = 3, fill = NA, colour = "black") +
  scale_shape_manual(values = c(21,22,18)) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  theme(legend.title = element_text(size=12)) +  
  theme(text = element_text(size = 12)) + 
  theme(legend.text = element_text(size=12)) +
  xlab("Herbivore Treatment") + ylab("Variance Ratio (all yrs)") +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  guides(color=guide_legend("Treatment:"), fill = FALSE, shape =guide_legend("Species:")) +
  annotate("text", x =5, y=1.08, label = "synchronous", size = 3.5,  fontface = 'italic') +
  annotate("text", x =5, y=0.94, label = "asynchronous", size = 3.5, fontface = 'italic')


## population stability by herbivore
pdomspstab <- ggplot(meanstab_mech, aes(x=TREATMENT, y=avgpopstab)) +
  geom_errorbar(aes(ymin = avgpopstab-SEpopstab, ymax=avgpopstab+SEpopstab), width = 0.3) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=3) + 
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Population Stability (all yrs)") + xlab("Herbivore Treatment") +  
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12))

## richness by herbivore treatment
prich <- ggplot(meanstab_mech, aes(x=TREATMENT , y=mean_rich)) +
  geom_errorbar(aes(ymin = mean_rich-SE_rich, ymax=mean_rich+SE_rich), width = 0.3) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Species Richness (all yrs)") + xlab("Herbivore Treatment") +  
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) + 
  labs(col = "Time Period")

## variance ratio (10 year)
vrdreg <- ggplot(mwfigs10, aes(x=Dscore, y=classicVR)) +
  geom_point() +
  ylab("Variance Ratio (10 yr)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5, show.legend = FALSE) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  annotate("text", x =0.23, y=1.11, label = "synchronous", size = 3.5, fontface = 'italic') +
  annotate("text", x =0.23, y=0.92, label = "asynchronous", size = 3.5, fontface = 'italic') 


## population stability (10 year)
psdreg <- ggplot(mwfigs10, aes(x=Dscore, y=popstab)) +
  geom_point() +
  ylab("Population Stability (10 yr)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25, color = "gray") +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) 


## richness (10 year)
ridreg <- ggplot(mwfigs10, aes(x=Dscore, y=richness)) +
  geom_point() +
  ylab("Species Richness (10 yr)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1.25) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) 



pdf("figure4.pdf", width = 7.087, height = 9, onefile = F)

ggarrange(vrdreg, VRtrt,
          psdreg, pdomspstab,
          ridreg,prich,
          ncol = 2, nrow = 3, 
          common.legend = TRUE, 
          legend = "bottom", 
          align = "hv", 
          labels = "AUTO", 
          vjust = 1.1)

dev.off()



## Figure 5: Stability & Biotic Mechanisms
## VR and stability
VRstab <- ggplot(meanstab_mech, aes(x=mean_classicVR, y=mean_st)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = T, color = "black", size = 1.15)+
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.06) +
  geom_errorbarh(aes(xmin = mean_classicVR-SEclassicVR, xmax=mean_classicVR+SEclassicVR), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Stability") + xlab("Variance Ratio") +  
  theme(text = element_text(size = 12))  + #change font sizes
  annotate("text", x =1.07, y=3.3, label = "synchronous", size = 4, angle='90', fontface = 'italic') +
  annotate("text", x =0.9, y=3.3, label = "asynchronous", size = 4, angle='90',fontface = 'italic') +
  theme(legend.position = "none")

## Aggregate dominant sp stability by stability
domspstabst <- ggplot(meanstab_mech, aes(x=avgpopstab, y=mean_st)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = T, color = "black", size = 1.15) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.06) +
  geom_errorbarh(aes(xmin = avgpopstab-SEpopstab, xmax=avgpopstab+SEpopstab), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("Stability") + xlab("Population Stability") +  
  theme(text = element_text(size = 12)) + #change font sizes
  theme(legend.position = "none")

## richness and stability
prichstab <- ggplot(meanstab_mech, aes(x=mean_rich, y=mean_st)) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) +
  geom_errorbarh(aes(xmin = mean_rich-SE_rich, xmax=mean_rich+SE_rich), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("Stability") + xlab("Species Richness") +  
  theme(text = element_text(size = 12))  + #change font sizes
  theme(legend.position = "none")

fig5 <- ggarrange(VRstab, domspstabst, prichstab,
          ncol = 3, nrow=1,
          align = "hv",
          labels = "AUTO",
          common.legend = FALSE)

## extract the legend from Fig 4 for use in Fig 5
leg <- get_legend(VRtrt +
                    theme(legend.position = "bottom"))

pdf("figure5.pdf", width = 7.087, height = 3.1, onefile = F)

plot_grid(fig5, leg, rel_heights = c(5.5,1), ncol = 1)

dev.off()

