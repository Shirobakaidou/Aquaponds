# Load ggplot2
library(ggplot2)
library(dplyr)


path <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/all_naStats.csv"
df <- read.csv(file = path, header = TRUE, sep = ",")
df$na_rate <- round(df$na_rate, 3)
df$na_rate_log2 <- round(log2(df$na_rate),1)
#write.csv(df, file="C:/EAGLE/AquaPonds/Data/dataCoverage/naStats_merged_v2.csv", sep = ",", row.names = FALSE)

xaxis <- seq(1987, 2019, by=2)
yaxis <- seq(0, 5, by=1)

# Barplot
df %>%
  filter(year >= 1987) %>%
  
  ggplot(aes(x=year, y=na_rate_log2, fill=country)) + 
  geom_bar(stat = "identity", position='dodge') +
  #geom_text(aes(label=na_rate))+
  
  #scale_y_continuous(trans = 'log2') +
  scale_y_continuous(breaks=yaxis, labels = paste(yaxis), name = "Proportion(%) of Ponds having NA (log2 transformed Scale)") +
  scale_x_continuous(breaks=xaxis, labels = paste(xaxis), name = "Year") +
  coord_flip()+
  
  facet_wrap(~country) +
  #labs(title = "Proportion of Ponds having no Data") +
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank(),
    legend.text=element_text(size=16),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    
    axis.text=element_text(size=12),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
    
    strip.text.x = element_blank(), # Romove facet labels
    
    panel.grid.major.x = element_line(color = 'black', size = 0.5, linetype = 1),
    #panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2)
    
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # important for increasing vertical spacing of legend items

