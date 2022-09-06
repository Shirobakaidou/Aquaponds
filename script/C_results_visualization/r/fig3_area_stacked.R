library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)

# List Files
path <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/all_#A.csv"
df <- read.csv(file = path, header = TRUE, sep = ",")

####################################################################
#---------------------------------------------------------------------------
##### Area Plot #####

xaxis2 <- append(seq(1988, 2019, by=2), 2019)
yaxis2 <- seq(0, 4000, by=500)

#p2 <-
df %>%
  filter(year > 1987) %>%
  
  ggplot(aes(x = year, y = pond_area_km2, fill = country)) +
  geom_area() +
  
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(breaks=xaxis2, labels = paste(xaxis2), name = "Year") +
  scale_y_continuous(breaks=yaxis2, labels=paste(yaxis2), name="Pond Area (km2)") +
  
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=16),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    legend.key.size = unit(1.5, 'line'),
    
    axis.text.x = element_text(angle = -25, hjust = 0.5, vjust = 0.5),
    axis.text=element_text(size=11),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5))
  )+
  guides(
    fill = guide_legend(byrow = TRUE, # important for increasing vertical spacing of legend items
                        override.aes = list(size = 2)),  # Change Legend Item Size
  )

