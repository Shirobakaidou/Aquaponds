library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(ggpubr)


# List Files
path <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/all_#A.csv"
df <- read.csv(file = path, header = TRUE, sep = ",")

#xaxis <- seq(1987, 2019, by=5)
#xaxis1 <- c(1988, 1990, 1995, 2000, 2005, 2010, 2015, 2019)
xaxis <- c(1988, 1990, 1995, 2000, 2005, 2010, 2015, 2019)
yaxis1 <- seq(0, 100, by=20)
yaxis2 <- seq(0, 1500, by=200)

p1 <-
df %>%
  filter(year > 1987) %>%
  
  ggplot(aes(x = year)) +
  
  geom_line(aes(y = area_percent_of2019, color = "Area of Ponds"), alpha = 0.6, size = 1.2) +
  geom_line(aes(y = count_percent_of2019, color = "Count of Ponds"), alpha = 0.6, size = 1.2) +
  geom_point(aes(y = area_percent_of2019, color = "Area of Ponds"), alpha = 0.5, size = 1.2) +
  geom_point(aes(y = count_percent_of2019, color = "Count of Ponds"), alpha = 0.5, size = 1.2) +
    
  #scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(breaks=yaxis1, labels = paste(yaxis1), name = "Percentage (%)") +
  scale_x_continuous(breaks=xaxis, labels = paste(xaxis), name = "Year") +
  scale_color_manual(values = c("Area of Ponds" = "#4d85db", "Count of Ponds" = "#309608")) +
 
  facet_wrap(~country, ncol = 1) +
  
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12), # Facet Label
    
    #legend.position = c(0.8, 0.15),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=16),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    legend.key.size = unit(3, 'line'),
    
    axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
    axis.text=element_text(size=12),
    axis.title=element_text(size=14),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.3)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.3)),
    
    panel.grid.major.x = element_line(color = 'orange', size = 0.5, linetype = 2),
    #panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.5, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.35, linetype = 2)
  ) +
  guides(
    color = guide_legend(byrow = TRUE, # important for increasing vertical spacing of legend items
                         override.aes = list(size = 2)),  # Change Legend Item Size
    )

  
# Inspired by: http://www.perceptualedge.com/articles/visual_business_intelligence/dual-scaled_axes.pdf


#######################################################################
## Bar Plot: Area ##
#######################################################################
areaColor <- "#4d85db"

pArea <-
  df %>% filter(year > 1987) %>%
  ggplot(aes(x=year, fill = country)) +
  
  geom_col(aes(y=pond_area_km2), size=.1, fill=areaColor, color="black") +
  #geom_line(aes(y = a + log2(pond_count)*b), color = countColor, size=0.75) +
  
  scale_x_continuous(breaks=xaxis, labels = paste(xaxis), name = "Year") +
  scale_y_continuous(breaks=yaxis2, labels=paste(yaxis2), name="Pond Area (km2)") +
  facet_wrap(.~country, ncol=1) +
  #facet_wrap(.~country, scales="free_y", nrow=2) +
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


ggarrange(pArea, p1, ncol=2, 
          #labels=c("A","B"), 
          common.legend = TRUE, 
          legend = "bottom"
          )



# --------------------- Discarded ----------------------------------------
#countColor <- "#309608"

#pCount <-
#  df %>% filter(year > 1987) %>%
#  ggplot(aes(x=year, fill = country)) +

#geom_col(aes(y=pond_area_km2), size=.1, fill=areaColor, color="black", alpha = 0.6) +
#  geom_col(aes(y = pond_count), fill = countColor, size=0.75) +

#  scale_x_continuous(breaks=xaxis, labels = paste(xaxis), name = "Year") +
#  scale_y_continuous(breaks=yaxis.sec, labels=paste(yaxis.sec), name="Count of Ponds (scale 1og10 transformed)") +
#  facet_wrap(.~country, ncol=1) +
#facet_wrap(.~country, scales="free_y", nrow=2) +
#  theme_bw() +
#  theme(
#    axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
#    axis.text=element_text(size=12),
#    axis.title=element_text(size=14),
#    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.3)),
#    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.3)),

#panel.grid.major.x = element_line(color = 'grey', size = 0.4, linetype = 2),
#panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
#    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
#    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2)
#  ) 


#-----------------------------------------------------------------
#---Discarded: Dual Axis------------------------------------------
#df$area_log2 <- round(log2(df$active_area_km2), 1)
#df$count_log2 <- round(log2(df$active_count), 1)

#xaxis <- c(1988, 1990, 1995, 2000, 2005, 2010, 2015, 2019)
#yaxis.prim <- seq(0, 1500, by=200)
#yaxis.sec <- seq(0, 20, by=2) # log2 scale
#yaxis.sec <- seq(3, 6, 0.5) # log10 scale
#ylim.sec <- c(3000, 640000, 50000)

#ylim.prim <- c(0, 1550) # Area
#ylim.sec <- c(3000, 640000) # Count
#ylim.sec <- c(11, 20) # Count log2
#b <- diff(ylim.prim)/diff(ylim.sec)
#a <- ylim.prim[1] - b*ylim.sec[1]

#areaColor <- "#4d85db"
#countColor <- "#309608"

#df %>% filter(year > 1987) %>%
#  ggplot(aes(x=year, fill = country)) +
  
#  geom_col(aes(y=pond_area_km2), size=.1, fill=areaColor, color="black", alpha = 0.6) +
#  geom_line(aes(y = a + log2(pond_count)*b), color = countColor, size=0.75) +
  
#  scale_x_continuous(breaks=xaxis, labels = paste(xaxis)) +
#  scale_y_continuous(breaks=yaxis.prim, labels=paste(yaxis.prim), name="Pond Area (km2)",
#                     sec.axis = sec_axis(trans = ~(. - a)/b, 
#                                         breaks = yaxis.sec, labels = paste(yaxis.sec),
#                                         name = "Count of Ponds (scale log-2 transformed)")) +
#  facet_wrap(.~country, ncol=1) +
  #facet_wrap(.~country, scales="free_y", nrow=2) +
#  theme_bw() +
#  theme(
#    axis.line.y.right = element_line(color = countColor), 
#    axis.ticks.y.right = element_line(color = countColor),
#    axis.text.y.right = element_text(color = countColor), 
#    axis.title.y.right = element_text(color = countColor),
    
#    axis.line.y.left = element_line(color = areaColor),
#    axis.ticks.y.left = element_line(color = areaColor),
#    axis.text.y.left = element_text(color = areaColor), 
#    axis.title.y.left = element_text(color = areaColor),
    
    #panel.grid.major.x = element_line(color = 'grey', size = 0.4, linetype = 2),
    #panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
#    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
#    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2)
#  ) 


# Reference:
# Second Y-axis: 
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
# https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

