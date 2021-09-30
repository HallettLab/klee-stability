# Poster Figures 
## Poster Fig 1
#```{r, fig.width=12.5, fig.height=3.5}
pstab <- ggplot(meanstab_mech, aes(x=TREATMENT, y=mean_st)) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.2) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Stability") + xlab("") +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 16)) + #change font sizes
  labs(col = "Time Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## synchrony by herbivory
psyn <- ggplot(meanstab_mech, aes(x=TREATMENT , y=mean_syn)) +
  geom_point(size=4) +
  scale_color_manual(values = colors) + 
  geom_errorbar(aes(ymin = mean_syn-SE_syn, ymax=mean_syn+SE_syn), width = 0.3) +
  ylab("Synchrony (All)") + xlab("") +  
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 16)) + #change font sizes
  labs(col = "Time Period")

pb5syn <- ggplot(meanstab_mech, aes(x=TREATMENT , y=b5mean_syn)) +
  geom_point(size = 3.5) +
  scale_color_manual(values = colors) + 
  geom_errorbar(aes(ymin = b5mean_syn-b5SE_syn, ymax=b5mean_syn+b5SE_syn), width = 0.3) +
  ylab("Big 5 Synchrony") + xlab("Treatment") +  
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 15)) + #change font sizes
  labs(col = "Time Period")


ggplot(syn_plot, aes(x=TREATMENT, y=mean_syn, color = community_type, shape = community_type)) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("#000000", "#4f4f4f", "#919191")) + 
  scale_shape_manual(values = c(16,17,18)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 15)) + #change font sizes
  geom_errorbar(aes(ymin = mean_syn-SE_syn, ymax=mean_syn+SE_syn), width = 0.2) +
  xlab("") + ylab("Synchrony") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(syn_plot, aes(x=TREATMENT, y=mean_syn, fill = TREATMENT, shape = community_type)) +
  geom_errorbar(aes(ymin = mean_syn-SE_syn, ymax=mean_syn+SE_syn), width = 0.2) +
  geom_point(size = 4) +
  geom_point(size = 4, fill = NA, colour = "black") +
  scale_shape_manual(values = c(24,22,18)) +
  #geom_point(aes(color=TREATMENT), size=3.6) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  # scale_shape_manual(values = c(16,17,18)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 16)) + #change font sizes
  xlab("") + ylab("Synchrony") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(syn_plot, aes(x=TREATMENT, y=mean_syn, shape = community_type)) +
  geom_errorbar(aes(ymin = mean_syn-SE_syn, ymax=mean_syn+SE_syn), width = 0.2) +
  geom_point(aes(y=mean_syn, shape = community_type), size = 4.25) +
  geom_point(aes(color=TREATMENT), size=3.6) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  scale_shape_manual(values = c(16,17,18)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 15)) + #change font sizes
  xlab("Treatment") + ylab("Synchrony") #+
facet_wrap(~community_type)




ggarrange(pstab, psynall, pdom, prich,
          ncol = 4,
          align = "hv",
          labels = "AUTO",
          common.legend = TRUE, 
          legend = "none")



#```

## Mechanisms Poster Figure
#```{r, echo = FALSE, fig.width=12, fig.height=3.5}
pb5synstab <- ggplot(meanstab_mech, aes(x=b5mean_syn, y=mean_st)) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.01) +
  geom_errorbarh(aes(xmin = b5mean_syn-b5SE_syn, xmax=b5mean_syn+b5SE_syn), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Stability") + xlab("Synchrony: Dominant Sp") +  
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 16)) + #change font sizes 
  labs(col = "Treatment")


ggarrange(psynstab, pb5synstab, pdomstab, prichstab,
          ncol = 4, nrow = 1,
          align = "hv",
          labels = "AUTO",
          common.legend = TRUE, 
          legend = "none")

ggarrange(psynstab, pb5synstab, pdomstab, prichstab,
          ncol = 4, nrow = 1,
          align = "hv",
          labels = "AUTO",
          common.legend = TRUE, 
          legend = "right")
#```

## More poster figs
#```{r, fig.width=9, fig.height=3.5}

ggarrange(psynstab, pdomstab, prichstab, 
          ncol = 3, nrow = 1,
          align = "hv",
          labels = "AUTO",
          common.legend = TRUE, 
          legend = "none")

ggarrange(psynstab, pdomstab, prichstab, 
          ncol = 3, nrow = 1,
          align = "hv",
          labels = "AUTO",
          common.legend = TRUE, 
          legend = "bottom")

#```


# Older 
## More total cover
#```{r, echo=FALSE}
b5totcov <- big5annual %>%
  group_by(BLOCK, TREATMENT, Unique_ID, Date_final) %>% #group by block, treatment, and date 
  summarise(totcov = sum(Pin_Hits, na.rm=T)) %>% #sum abundances to calculate tot cover
  mutate(type = "Dominant") #%>%

meanb5cov <- b5totcov %>%
  group_by(TREATMENT, Date_final, type) %>%
  summarise(meancov = mean(totcov))

ndtotcov <- nondom %>%
  group_by(BLOCK, TREATMENT, Unique_ID, Date_final) %>% #group by block, treatment, and date 
  summarise(totcov = sum(Pin_Hits, na.rm=T)) %>% #sum abundances to calculate tot cover
  mutate(type = "Subordinate") #%>%

meanndcov <- ndtotcov %>%
  group_by(TREATMENT, Date_final, type) %>%
  summarise(meancov = mean(totcov))

j <- rbind(meancov, meanb5cov)
cover <- rbind(j, meanndcov) 
cover$TREATMENT <- as.factor(cover$TREATMENT)

cover <- cover %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) %>% #reorder treatments
  mutate(type = fct_relevel(type, "Full", "Dominant", "Subordinate"))

ggplot(cover, aes(x= Date_final, y=meancov, color = type)) +
  geom_line(size = 1) +
  facet_wrap(~TREATMENT) +
  ylab("Total Cover") + xlab("Date") +
  scale_color_manual(values = c("#000000", "#4f4f4f", "#ababab"))


ggplot(cover, aes(x= Date_final, y=meancov, color = TREATMENT)) +
  geom_line(size = 0.75) +
  facet_wrap(~type) +
  scale_color_scico_d(palette = "batlow", direction = -1) +
  ylab("Total Cover") + xlab("Date")
#```












## Old Dominance Figures
```{r, echo = FALSE}
## dominance by herbivory
pdom <- ggplot(meanstab_mech, aes(x=TREATMENT , y=mean_dom)) +
  geom_errorbar(aes(ymin = mean_dom-SE_dom, ymax=mean_dom+SE_dom), width = 0.3) +
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Dominance") + xlab("") +  
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) + #change font sizes
  labs(fill = "Treatment") #+
#theme(axis.text.x = element_text(angle = 30, hjust = 1))


## dominance and stability
pdomstab <- ggplot(meanstab_mech, aes(x=mean_dom, y=mean_st)) +
  geom_errorbar(aes(ymin = mean_st-SE_st, ymax=mean_st+SE_st), width = 0.01) +
  geom_errorbarh(aes(xmin = mean_dom-SE_dom, xmax=mean_dom+SE_dom), height = 0.1) + #add standard error bars
  geom_point(aes(fill=TREATMENT), 
             colour="black",pch=21, size=4) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +  ylab("Stability") + xlab("Dominance") +  
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) + #change font sizes
  labs(col = "Treatment")
```





















#```{r, echo = FALSE}
## Calculate Loreau Synchrony metric
synchrony <- synchrony(
  klee_annual,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  metric = "Loreau",
  replicate.var = "Unique_ID"
)

big5synchrony <- synchrony(
  big5annual,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  metric = "Loreau",
  replicate.var = "Unique_ID"
)

colnames(big5synchrony) <- c("Unique_ID", "big5synchrony")

nondomsynchrony <- synchrony(
  nondom,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  metric = "Loreau",
  replicate.var = "Unique_ID"
)
colnames(nondomsynchrony) <- c("Unique_ID", "nondomsynchrony")

#```








### for subordinate species
#```{r,  echo=FALSE}
## Calculate Timescale Specific VR ##
#set up data frame to put tsvr into
ndoutnames <- c("Unique_ID", "TREATMENT", "classicVR", "longVR", "shortVR") #column names
ndsiteout <- as.data.frame(matrix(nrow=0, ncol = 5)) #make empty dataframe
names(ndsiteout) <- ndoutnames #set names for empty dataframe
ndplots <- unique(nondom$Unique_ID) #make vector of unique plots

## Use for loop to calculate TSVR for each plot
for (i in 1:length(ndplots)) {
  
  #subset by replicate (gives all observations from one plot over time)
  plot <- subset(nondom, Unique_ID == ndplots[i]) %>%
    tbl_df()
  
  #select species and fill 0's 
  plot2 <- plot %>%
    select(Date_numeric, SPECIES, Pin_Hits) %>% #selecting only these three columns
    spread(SPECIES, Pin_Hits, fill = 0) #changing to wide format
  
  #transpose the data 
  dat <- t(as.matrix(plot2[,2:dim(plot2)[2]]))
  
  #create a dataframe with replicate info to attach VR metrics to
  VR_plots <- plot %>%
    select(Unique_ID, TREATMENT, BLOCK) %>%
    unique()
  
  #calculate classic VR
  res0 <- vreq_classic(dat)
  VR_plots$ndclassicVR <- res0[[3]] #extracting classic VR
  
  #calculate tsvr 
  res <- tsvreq_classic(dat)
  
  #aggregate short vs. long variance ratios
  resLong <- aggts(res, res$ts[res$ts>=4]) #grabbing tsvr with time period >= 4 years
  resShort <- aggts(res, res$ts[res$ts<4]) #grabbing tsvr with time period <4 years
  
  #attach short & long variance ratios
  VR_plots$ndlongVR <- resLong[[3]]
  VR_plots$ndshortVR <- resShort[[3]]
  
  #append to external dataframe
  ndsiteout<-rbind(ndsiteout, VR_plots)
}\



#rename data frame
ndtsVR <- ndsiteout #%>%
#mutate(community_type = "Subordinate")

## Calculate the mean and standard error of VR metrics
ndmeantsVR <- ndtsVR %>%
  pivot_longer(cols = ndclassicVR:ndshortVR, names_to = "VR_type", values_to = "VR_value" ) %>% #change to long format
  group_by(TREATMENT, VR_type) %>%
  summarise(meanVR = mean(VR_value), SEVR = calcSE(VR_value))




#reorder treatments to match grazing pressure.
ndmeantsVR$TREATMENT <- as.factor(ndmeantsVR$TREATMENT) #change treatment to factor
ndmeantsVR$TREATMENT <- factor(ndmeantsVR$TREATMENT, levels = c("O", "W",  "MW",  "C", "WC", "MWC"))
#```