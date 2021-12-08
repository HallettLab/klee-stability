## Supplementary Figures ##

library(tidyverse)

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
## Stability by Timescale
stabts <- ggplot(mean_mwstab, aes(x = window_size, y=mean_stab)) +
  geom_point(size = 3) +
  ylab("Mean Stability") + xlab("Window Size of Stability Calculation") +
  geom_errorbar(aes(ymin=mean_stab-SE_stab, ymax = mean_stab+SE_stab), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")

## Variance Ratio by Timescale
cvrts <- ggplot(mean_mwcVR, aes(x = window_size, y=mean_cVR)) +
  geom_point(size = 3) +
  ylab("Mean Variance Ratio") + xlab("Window Size of VR Calculation") +
  geom_errorbar(aes(ymin=mean_cVR-SE_cVR, ymax = mean_cVR+SE_cVR), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")

## Richness by Timescale
richts <- ggplot(mean_mwrich, aes(x=window_size, y=mean_rich)) +
  geom_point(size=3) +
  ylab("Mean Richness") + xlab("Window Size of Richness Calculation") +
  geom_errorbar(aes(ymin=mean_rich-SE_rich, ymax = mean_rich+SE_rich), width = 0.2) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=3.5) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(fill = "Treatment")

## Dominant Species Stability by Timescale

ggarrange(stabts, cvrts, richts, 
          ncol=3, nrow = 1, align = "hv", 
          common.legend = TRUE, 
          labels = "AUTO", 
          legend = "bottom")



## Figure S3: Stability & Mech MW over time; drought score
## make a vector of all sample dates to use for axis labels
dates <- sort(unique(klee_annual[['Date_final']]))
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


