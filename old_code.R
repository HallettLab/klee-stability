

## Dominant Species, Rank, & Stability
```{r, fig.width =10, fig.height=6}
ggplot(domsp_stab, aes(x=rank, y=stability, col=SPECIES)) +
  geom_point(size=2) +
  ylab("Species Stability") + xlab("Mean Rank") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE), width=0.75) +
  #theme(legend.title = element_text(size=12)) +
  #theme(text = element_text(size = 14)) +
  labs(col = "Species") + #change legend title
  facet_wrap(~TREATMENT)
```


## Dominant Species & Stability
```{r, fig.width =10, fig.height=8}
ggplot(domsp_stab, aes(x=SPECIES, y=stability, col=as.factor(rank_int))) +
  geom_point(size=3) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  #theme(legend.title = element_text(size=12)) +
  #theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  facet_wrap(~TREATMENT)
```

***
  
  ## Dominant species VR Table
  ```{r, echo=FALSE, caption = 'Table 3: Dominant Species Variance Ratio'}
## create one data frame of dominant species VR ratios
i <- rbind(O.vr, W.vr)
j <- rbind(i, MW.vr)
k <- rbind(j, C.vr)
l <- rbind(k, WC.vr)
dominant_vr <- rbind(l, MWC.vr)

domVR <- inner_join(dominant_vr, treats, by="Unique_ID") %>%
  group_by(TREATMENT) %>%
  summarise(meanVR = mean(VR), meanlower=mean(lowerCI), meanupper=mean(upperCI))

kable(domVR, col.names = c("Treatment", "Variance Ratio", "Lower CI", "Upper CI"))

```







```{r, fig.width =12, fig.height=6}
#reorder treatments to match grazing pressure.
#domsp$TREATMENT <- as.factor(domsp) #change treatment to factor
dominant.sp$TREATMENT <- factor(dominant.sp$TREATMENT, levels = c("O", "W",  "MW",  "C", "MWC", "WC"))

ggplot(dominant.sp, aes(x=Date_final, y=meanhits, col=SPECIES)) +
  geom_point(size=1.5) +
  geom_line() +
  # scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  facet_wrap(~TREATMENT)
```




```{r, fig.width =12, fig.height=8}
ggplot(domsp, aes(x=Date_final, y=meanhits, col=TREATMENT)) +
  geom_point(size=1.5) +
  geom_line() +
  facet_wrap(~SPECIES) +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Mean Pin Hits")
```






## Dominant Species over time
``` {r, fig.width =8, fig.height=4, echo=FALSE}
bold <- c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99", "#66C5CC", "#F6CF71", "#B497E7")

## O Treatment
ggplot(meanO, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  ggtitle("O Treatment")

## W Treatment
ggplot(meanW, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  ggtitle("W Treatment")

## MW Treatment
ggplot(meanMW, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  ggtitle("MW Treatment")

## C Treatment
ggplot(meanC, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  ggtitle("C Treatment")

## WC Treatment
ggplot(meanWC, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  ggtitle("WC Treatment")


## MWC Treatment
ggplot(meanMWC, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = bold) +
  xlab("Date") + ylab("Pin Hits") +
  ggtitle("MWC Treatment")

```



**Figure 9** Lines are species mean abundances through time. Species shown were in 5 most abundant species in at least one year from 1999-2015. Colors are not consistent with species. 




## Dominant species stability
```{r, fig.width =10, fig.height=8, echo=FALSE}
## Plot species stability of any species that was in top 5 most abundant species throughout time series

bold <- c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310","#008695","#CF1C90","#f97b72","#4b4b8f","#A5AA99", "#66C5CC", "#F6CF71", "#B497E7")

ggplot(meanO.spstab, aes(x=SPECIES, y=stability, col=as.factor(rank))) +
  geom_point(size=4) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  ggtitle("O Treatment")


ggplot(meanW.spstab, aes(x=SPECIES, y=stability, col=as.factor(rank))) +
  geom_point(size=4) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  ggtitle("W Treatment")


ggplot(meanMW.spstab, aes(x=SPECIES, y=stability, col=as.factor(rank))) +
  geom_point(size=4) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  ggtitle("MW Treatment")

ggplot(meanC.spstab, aes(x=SPECIES, y=stability, col=as.factor(rank))) +
  geom_point(size=4) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  ggtitle("C Treatment")

ggplot(meanWC.spstab, aes(x=SPECIES, y=stability, col=as.factor(rank))) +
  geom_point(size=4) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  ggtitle("WC Treatment")


ggplot(meanMWC.spstab, aes(x=SPECIES, y=stability, col=as.factor(rank))) +
  geom_point(size=4) +
  ylab("Species Stability") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = bold) +
  geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE)) +
  theme(legend.title = element_text(size=12)) +
  theme(text = element_text(size = 14)) +
  labs(col = "Mean Rank Over Time") + #change legend title
  ggtitle("MWC Treatment")
```



**Figure 10** Points show the mean species stability (temporal mean/temporal sd) in each treatment. Species shown were in 5 most abundant species in at least one year from 1999-2015. Bars show one standard error above and below the mean. Colors are not consistent by species.






