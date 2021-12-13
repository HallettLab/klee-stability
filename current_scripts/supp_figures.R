## Supplementary Figures ##

library(tidyverse)
library(scico)
library(ggpubr)

source("current_scripts/finalprep_postcalcs.R") ## read in data

theme_set(theme_classic()) #set the theme


## Figure S1: Stability, Mean Pin Hits, and Variance
#graph the mean variance by treatment
v <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_var)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_var-SE_var, ymax = mean_var+SE_var), width=0.25) +
  ylab("Temporal Variance") + xlab("") + 
  theme(text = element_text(size = 16)) #change font size

#graph the mean total pin hits by treatment
b <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_bio)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=mean_bio-SE_bio, ymax=mean_bio+SE_bio), width = 0.2) + #include standard error bars
  ylab("Total Cover (Pin Hits)") + xlab("Treatment") +
  theme(text = element_text(size = 16)) #change font size

## visualize mean stability & standard error
s <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_st)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) + #add standard error bars
  ylab("Stability (mean/sd)") + xlab("") + #label axes
  theme(text = element_text(size = 16)) #change font size

## plot 3 panel figure of stability, total pin hits, and temporal variance
ggarrange(s, b, v, 
          labels = "AUTO",
          ncol = 3, nrow = 1, 
          align = "hv")



## Figure S2: Moving-window Time-Scale

mean_mwstab$TREATMENT <- as.factor(mean_mwstab$TREATMENT)
mean_mwstab <- mean_mwstab %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments
## Stability by Timescale
stabts <- ggplot(mean_mwstab, aes(x = window_size, y=mean_stab)) +
  geom_point(size = 1.5) +
  ylab("Mean Stability") + xlab("Window Size of Stability Calculation") +
  geom_errorbar(aes(ymin=mean_stab-SE_stab, ymax = mean_stab+SE_stab), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")


mean_mwcVR$TREATMENT <- as.factor(mean_mwcVR$TREATMENT)
meancrv <- mean_mwcVR %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_vr = mean(mean_cVR), se_vr = calcSE(mean_cVR)) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments


## Variance Ratio by Timescale
cvrts <- ggplot(meancrv, aes(x = window_size, y=mean_vr)) +
  geom_point(size = 1.5) +
  ylab("Mean Variance Ratio") + xlab("Window Size of VR Calculation") +
  geom_errorbar(aes(ymin=mean_vr-se_vr, ymax = mean_vr+se_vr), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")




mean_mwrich$TREATMENT <- as.factor(mean_mwrich$TREATMENT)
meanri <- mean_mwrich %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_ri = mean(mean_rich), se_ri = calcSE(mean_rich)) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments


## Richness by Timescale
richts <- ggplot(meanri, aes(x=window_size, y=mean_ri)) +
  geom_point(size=1.5) +
  ylab("Mean Richness") + xlab("Window Size of Richness Calculation") +
  geom_errorbar(aes(ymin=mean_ri-se_ri, ymax = mean_ri+se_ri), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")




mean_mwpopst$TREATMENT <- as.factor(mean_mwpopst$TREATMENT)
mean_mwpopst <- mean_mwpopst %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments

## Dominant Species Stability by Timescale

popstts <- ggplot(mean_mwpopst, aes(x=window_size, y=mean_popst)) + 
  geom_point(size=1.5) +
  ylab("Mean Dom Pop Stability") + xlab("Window Size of Pop Stability Calculation") +
  geom_errorbar(aes(ymin=mean_popst-SE_popst, ymax = mean_popst+SE_popst), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")

ggarrange(stabts, cvrts, richts, popstts,
          ncol=2, nrow = 2, align = "hv", 
          common.legend = TRUE, 
          labels = "AUTO", 
          legend = "bottom")



## Figure S3: Stability & Mech MW over time; drought score
## make a vector of all sample dates to use for axis labels
dates <- sort(unique(totcov[['Date_final']]))
window10dates <- year(dates[1:13])

breaks <- c(1:13)

st <- ggplot(figdrst, aes(x=timestep, y=stability, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = Dscore), size = 15) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 3.5) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.9) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab("Stability") + labs(fill = "Treatment") + xlab("Starting Year (10yr Window)") +
  scale_fill_scico_d(palette = "batlow", direction = -1)


cvr <- ggplot(figdrcvr, aes(x=timestep, y=classicVR, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = Dscore), size = 15) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 3.5) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.9) +
  ylab("Variance Ratio") + xlab("Starting Year (10yr Window)")+
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) +
  geom_hline(yintercept = 1, linetype = "dashed")


pst <- ggplot(figdrpopst, aes(x=timestep, y=popstab, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = Dscore), size = 15) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 3.5) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.9) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Population Stability") + xlab("Starting Year (10yr Window)")+
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) 

ri <- ggplot(figdrri, aes(x=timestep, y=richness, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = Dscore), size = 15) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 3.5) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.9) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Species Richness") + xlab("Starting Year (10yr Window)")+
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) 

ggarrange(st, cvr, pst, ri,
          ncol =1, 
          common.legend = TRUE, 
          legend = "right", 
          labels = "AUTO")


