## Proposal Exam Figures

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


## Figure 3: Stability (full ts, 10yr) 
s <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_st)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) + #add standard error bars
  ylab("Stability (all yrs)") + xlab("Herbivore Treatment") + #label axes
  theme(legend.title = element_text(size=10)) + 
  theme(text = element_text(size = 9)) +
  theme(legend.text = element_text(size=8)) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=2) + 
  labs(fill = "Treatment") +
  scale_fill_scico_d(palette = "batlow", direction = -1)

sdreg <- ggplot(mwfigs10, aes(x=Dscore, y=stability)) +
  geom_point() +
  ylab("Stability (10 yrs)") + xlab("Drought Score") +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=1.25) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  theme(legend.position = "right") +
  geom_line(stat = "smooth", method = "lm", formula = y~x,
            size = 1) +
  theme(legend.text = element_text(size=8)) +
  labs(fill = "Treatment:") +
  theme(legend.title = element_text(size=8)) +  
  theme(text = element_text(size = 9))


pdf("proposal_figure2.pdf", width = 3.54, height = 2.5, onefile = F)

ggarrange(sdreg, s, 
          ncol = 2, nrow = 1, 
          common.legend = TRUE, 
          legend = "bottom", 
          align = "hv", 
          labels = "AUTO")

dev.off()





## THIS PART INCLUDED FOR THE LEGEND IT GENERATES ##
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
## END FIG CREATED FOR THE LEGEND ##





## Figure 5: Stability & Biotic Mechanisms
## VR and stability
VRstab <- ggplot(meanstab_mech, aes(x=mean_classicVR, y=mean_st)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = T, color = "black", size = 0.75)+
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.06) +
  geom_errorbarh(aes(xmin = mean_classicVR-SEclassicVR, xmax=mean_classicVR+SEclassicVR), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=1.75) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Stability") + xlab("Variance Ratio") +  
  theme(text = element_text(size = 9))  + #change font sizes
  #annotate("text", x =1.07, y=3.3, label = "synchronous", size = 4, angle='90', fontface = 'italic') +
 # annotate("text", x =0.9, y=3.3, label = "asynchronous", size = 4, angle='90',fontface = 'italic') +
  theme(legend.position = "none")

## Aggregate dominant sp stability by stability
domspstabst <- ggplot(meanstab_mech, aes(x=avgpopstab, y=mean_st)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = T, color = "black", size = 0.75) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.06) +
  geom_errorbarh(aes(xmin = avgpopstab-SEpopstab, xmax=avgpopstab+SEpopstab), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=22, size=1.75) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("") + xlab("Pop. Stability") +  
  theme(text = element_text(size = 9)) + #change font sizes
  theme(legend.position = "none")

## richness and stability
prichstab <- ggplot(meanstab_mech, aes(x=mean_rich, y=mean_st)) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) +
  geom_errorbarh(aes(xmin = mean_rich-SE_rich, xmax=mean_rich+SE_rich), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=1.75) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("") + xlab("Richness") +  
  theme(text = element_text(size = 9))  + #change font sizes
  theme(legend.position = "none")

#fig5 <- 

## extract the legend from Fig 4 for use in Fig 5
##leg <- get_legend(VRtrt +
                    #theme(legend.position = "bottom"))

#width = 3.54, height = 2.5,

pdf("proposal_figure5.pdf", width = 4.5, height = 1.5, onefile = F)

ggarrange(VRstab, domspstabst, prichstab,
          ncol = 3, nrow=1,
          align = "hv",
          labels = "AUTO",
          common.legend = FALSE)
#plot_grid(fig5, leg, rel_heights = c(5.5,1), ncol = 1)

dev.off()
