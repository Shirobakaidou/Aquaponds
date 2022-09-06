library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(ggpubr)

# List Files
path1 <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/fig3-4_all_#A.csv"
df1 <- read.csv(file = path1, header = TRUE, sep = ",")

path2 <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/fig8_Output_fao_FrBrWater.csv"
df2 <- read.csv(file = path2, header = TRUE, sep = ",")

# Join data frames
df <- right_join(df1, df2, by=c('country', 'year'))


#xaxis <- seq(1987, 2019, by=5)
xaxis1 <- c(1988, 1990, 1995, 2000, 2005, 2010, 2015, 2019)
yaxis1 <- seq(0, 140, by=20)
yaxis2 <- seq(0, 4000000, by=1000000)

# Show numbers in scientific notation
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # Don't show 0 as 0x10^0
  l <- gsub("0e\\+00","0",l)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

p1 <-
df %>%
  ggplot(aes(x = year)) +
  
  geom_line(aes(y = area_percent_of2019, color = "Area of Ponds"), alpha = 0.6, size = 1.2) +
  geom_line(aes(y = percDif_to_2019, color = "Live Weight Tonnes (FAO)"), alpha = 0.6, size = 1.2) +
  geom_point(aes(y = area_percent_of2019, color = "Area of Ponds"), alpha = 0.5, size = 1.2) +
  geom_point(aes(y = percDif_to_2019, color = "Live Weight Tonnes (FAO)"), alpha = 0.5, size = 1.2) +
  
  #scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(breaks=yaxis1, labels = paste(yaxis1), name = "Percentage (%)") +
  scale_x_continuous(breaks=xaxis1, labels = paste(xaxis1), name = "Year") +
  scale_color_manual(values = c("Area of Ponds" = "#4d85db", "Live Weight Tonnes (FAO)" = "#fe654a")) +
  
  facet_wrap(~country, ncol = 1) +
  
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12), # Facet Label
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=16),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    legend.key.size = unit(1.5, 'line'),
    
    axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
    axis.text=element_text(size=12),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
    
    panel.grid.major.x = element_line(color = 'orange', size = 0.5, linetype = 2),
    #panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.5, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.35, linetype = 2)
  ) +
  guides(
    color = guide_legend(byrow = TRUE, # important for increasing vertical spacing of legend items
                         override.aes = list(size = 2)),  # Change Legend Item Size
  )

pFAO <-
  df %>% filter(year > 1987) %>%
  ggplot(aes(x=year, fill = country)) +
  
  geom_col(aes(y=tonnes_live_weight), size=.1, fill="#fe654a", color="black") +
  #geom_line(aes(y = a + log2(pond_count)*b), color = countColor, size=0.75) +
  
  scale_x_continuous(breaks=xaxis1, labels = paste(xaxis1), name = "Year") +
  scale_y_continuous(breaks=yaxis2, labels=fancy_scientific, name="Yield of Live Weights (tonnes)") +
  facet_wrap(.~country, ncol=1) +
  #facet_wrap(.~country, scales="free_y", ncol=1) +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12), # Facet Label
    
    axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
    axis.text=element_text(size=12),
    axis.title=element_text(size=14),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.3)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.3)),
    
    #panel.grid.major.x = element_line(color = 'grey', size = 0.4, linetype = 2),
    #panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2)
  ) 


ggarrange(pFAO, p1, ncol=2, 
          #labels=c("A","B"), 
          common.legend = TRUE, 
          legend = "bottom"
)
