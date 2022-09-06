library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)

# List Files
path <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/all_distance.csv"
df <- read.csv(file = path, header = TRUE, sep = ",")

yaxis <- append(append(1988, seq(1990, 2015, by=5)), 2019)
xaxis <- seq(0, 1, by = 0.2)

# Stacked Barplot
df %>%
  filter(year > 1987) %>%
  mutate(dist_cat = factor(distance.km., levels = c("[0,5)", "[5,20)", "[20,50)", "[50,100)", "[100,200)"))) %>%
  
  ggplot(aes(fill=dist_cat, y=factor(year), x=pond_number)) + 
  geom_bar(position="fill", stat="identity") +
  #geom_bar(stat = 'identity') +
  
  # change Labels
  labs(fill = "Distance from Coastline \n (in km)") + 
  
  scale_fill_viridis(discrete = T, labels = c("< 5 km", "5 - < 20 km", "20 - < 50 km", "50 - < 100 km", "> 100 km")) +
  scale_x_continuous(breaks=xaxis, labels=paste(xaxis), name="Proportion") +
  scale_y_discrete(breaks = yaxis, labels = paste(yaxis), name = "Year") +
  
  facet_wrap(.~country, nrow = 2) +
  
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12), # Facet Label
    
    legend.position = c(0.8, 0.2),
    legend.key = element_blank(),
    legend.title=element_text(size=16),
    legend.text=element_text(size=14),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    
    axis.text=element_text(size=12),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),

  )+
  guides(
    fill = guide_legend(byrow = TRUE, # important for increasing vertical spacing of legend items
                         override.aes = list(size = 3)),  # Change Legend Item Size
  )

