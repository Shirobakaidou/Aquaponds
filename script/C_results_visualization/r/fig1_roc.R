library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggpubr)

myPath <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/fig1_all_roc_param.csv"
df <- read.csv(file = myPath, header = TRUE, sep = "\t")
#df$parcelID <- as.character(df$parcelID)
#dfBest <- df %>% group_by(parcelID) %>% filter(eucDis==min(eucDis))

# Replace values
df[df=='95th'] <- '95th Percentile'
df[df=='median'] <- 'Median'
df[df=='ndwi'] <- 'NDWI'
df[df=='mndwi'] <- 'MNDWI'
df[df=='awei'] <- 'AWEI'
df[df=='wiFi'] <- 'WIFI'
df[df==540] <- 'Vietnam'
df[df==658] <- 'Malaysia'
df[df==667] <- 'Thailand'

# Make Water Index to factors, to fix facet order later
df$waterIndex_f <- factor(df$waterIndex, levels = c('NDWI', 'MNDWI', 'AWEI', 'WIFI'))

xaxis <- seq(0, 1, by=0.1)
yaxis <- seq(0, 1, by=0.1)

# Figure of ROC
p1 <-
df %>% 
  ggplot(aes(x=fpr, y=tpr, color=parcelID))+
  geom_point(alpha=0.7, size=1) + 
  #geom_line() +
  #geom_text_repel(data = dfBest, aes(x=fpr, y=tpr, label="minDis"), min.segment.length=0.5, box.padding = 1) +
  
  scale_x_continuous(name="FPR", breaks=xaxis, labels = paste(xaxis)) +
  scale_y_continuous(name="TPR", breaks=yaxis, labels = paste(yaxis)) +
  labs(color="Test Site") + # Modify Legend
  
  facet_wrap(~reducer+waterIndex_f, nrow = 2) +
  
  coord_fixed() +
  theme_bw() +
  theme(
    #legend.position = "none",
    #legend.title=element_text(size=13), 
    #legend.text=element_text(size=12),
    
    legend.title=element_text(size=18),
    legend.text=element_text(size=16),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    legend.key.size = unit(2, 'line'),
    
    axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
    axis.text=element_text(size=10),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
    
    panel.grid.major.x = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.x = element_line(color = 'grey', size = 0.4, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2),
    
    strip.text.x = element_text(size = 12) # Facet Label
  )  +
  guides(
    color = guide_legend(byrow = TRUE, # important for increasing vertical spacing of legend items
                         override.aes = list(size = 3)),  # Change Legend Item Size
  )



##################################################################################

dfSub <- subset(df, reducer=='Median' & waterIndex%in%c('AWEI','WIFI'))

xaxis2 <- seq(0.7, 1.3, by=0.1)
yaxis2 <- seq(0, 0.5, by=0.1)

# Change Facet Labels
wi.labs <- c("Median, AWEI", "Median, WIFI")
names(wi.labs) <- c("AWEI", "WIFI")

# Figure of Threshold vs. Euclidean Distance
p2 <- 
dfSub %>%
  ggplot(aes(x=thresholds, y=eucDis, color=parcelID)) +
  geom_point(alpha=0.75) +
  #geom_line(alpha=0.75) +
  
  scale_y_continuous(breaks=yaxis2, labels = paste(yaxis2)) +
  
  facet_wrap(waterIndex~., scales = 'free_x', nrow = 2,
             labeller = labeller(waterIndex = wi.labs)) +
  
  # Add a line to one of the facets (https://stackoverflow.com/questions/34686217/how-can-i-add-a-line-to-one-of-the-facets)
  geom_vline(data = data.frame(thrh = -133235.6, waterIndex = "WIFI"), aes(xintercept = thrh), linetype = "dotdash") +
  
  labs(x = "Threshold", 
       y = "Euclidean Distance to Point (0,1) of ROC Curve", 
       color = "Test Site") +
  theme_bw() +
  theme(
    legend.position = "none",
    
    #legend.text=element_text(size=16),
    #legend.spacing.x = unit(1.0, 'cm'),
    #legend.spacing.y = unit(0.75, 'cm'),
    #legend.key.size = unit(3, 'line'),
    
    axis.text=element_text(size=12),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
    
    panel.grid.major.x = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.x = element_line(color = 'grey', size = 0.4, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2),
    
    strip.text.x = element_text(size = 12) # Facet Label
  ) #+
  #guides(
  #  color = guide_legend(byrow = TRUE, # important for increasing vertical spacing of legend items
  #                       override.aes = list(size = 2)),  # Change Legend Item Size
  #)


# Arrange Figure Panel p1,p2
ggarrange(p1, p2, ncol=2, labels=c("A","B"), common.legend = TRUE, legend = "bottom")




# Figure of Scaling Factors vs. Euclidean Distance
#p3 <- 
#dfSub %>%
#  ggplot(aes(x=factors, y=eucDis, color=parcelID)) +
#  geom_point(alpha=0.75) +
  
#  scale_x_continuous(breaks=xaxis2, labels = paste(xaxis2)) +
#  scale_y_continuous(breaks=yaxis2, labels = paste(yaxis2)) +
  
#  coord_fixed() +
#  facet_wrap(waterIndex~., ncol = 2) +
#  labs(x = "Scaling Factor", 
#       y = "Euclidean Distance to Point(0,1) of ROC Curve", 
#       color = "Test Site") +
  
#  theme_bw() +
#  theme(
#    legend.position = "none",
#    legend.title=element_text(size=13), 
#    legend.text=element_text(size=12)
#  ) +
#  guides(color = guide_legend(override.aes = list(size = 2)))


# Arange Figure Panel p1,p2,p3
#ggarrange(p1,                                                 # First row with scatter plot
#          ggarrange(p2, p3, nrow = 2, labels = c("B", "C")), # Second row with box and dot plots
#          ncol = 2, 
#          labels = "A",                                       # Labels of the scatter plot
#          common.legend = TRUE, legend = "bottom"
#) 



