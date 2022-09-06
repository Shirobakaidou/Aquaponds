library(ggplot2)
library(dplyr)

# List Files
path <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/all_gid2_1yGr.csv"
df <- read.csv(file = path, header = TRUE, sep = ",")

yaxis <- append(append(1989, seq(1990, 2015, by=5)), 2019)
xaxis <- seq(-50, 130, by=25)

# sample size
sample_size = df %>% group_by(NAME_0, GID_2) %>% count() %>% group_by(NAME_0) %>% summarize(num=n())
fnl <- c(`Cambodia` = paste0("Cambodia (", sample_size[1,2], " Districts)"),
         `Malaysia` = paste0("Malaysia (", sample_size[2,2], " Districts)"),
         `Myanmar` = paste0("Myanmar (", sample_size[3,2], " Districts)"),
         `Thailand` = paste0("Thailand (", sample_size[4,2], " Districts)"),
         `Vietnam` = paste0("Vietnam (", sample_size[5,2], " Districts)"))

df %>%
  filter(year > 1988) %>%
  
  ggplot(aes(x = stats_value, y = factor(year), fill = NAME_0)) +
  geom_boxplot(outlier.alpha = 0.4, outlier.color = 'orange', outlier.size = 0.5,
               alpha = 0.6, width = 0.7, lwd = 0.4) +
  
  stat_summary(fun=mean, geom='point', size = 0.8, color='yellow') + 
  # Add aes(shape='mean') to let it show on legend
  
  scale_y_discrete(name = "Year", breaks=yaxis, labels = paste(yaxis)) +
  scale_x_continuous(limits = quantile(df$stats_value, c(0.025, 0.975), na.rm = TRUE),
                     breaks = xaxis, labels=paste(xaxis), name = "Growth Rate (%)") +
  
  facet_wrap(.~NAME_0, ncol= 3, labeller = as_labeller(fnl)) +
  
  theme_bw() +
  theme(
    #strip.text.x = element_text(size = 12), # Facet Label
    strip.text.x = element_blank(), # Romove facet labels
    
    #axis.text.x = element_text(angle = -45, vjust=0.6), # rotate axis label
    axis.text=element_text(size=10),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
    
    #legend.position = "none", # legend position
    legend.position = c(0.8, 0.2),
    legend.title = element_blank(), # remove legend title
    #legend.key = element_rect(fill = "grey"),
    legend.text = element_text(size=16),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    legend.key.size = unit(1.5, 'line'),
    
    panel.grid.major.x = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.x = element_line(color = 'grey', size = 0.4, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
    #panel.grid.minor.y = element_line(color = 'grey', size = 0.35, linetype = 2)
  )+
  guides(
    fill = guide_legend(byrow = TRUE) # important for increasing vertical spacing of legend items
                         #override.aes = list(size = 2))  # Change Legend Item Size
  )




#df_na <- 
#  df %>% 
#  filter_all(any_vars(is.na(.))) %>%
#  group_by(NAME_0, year) %>% count()

#df %>%
#  ggplot(aes(x = factor(stats_item), y = stats_value)) +
#  geom_boxplot(outlier.shape = NA) +
#  stat_summary(fun=mean, geom='point') +
#  scale_x_discrete(labels = paste(xaxis), name = "Year") +
#  scale_y_continuous(limits = quantile(df$stats_value, c(0.025, 0.975), na.rm = TRUE),
#                     breaks = yaxis, labels=paste(yaxis), name = "Annual Growth Rate (%)") +
#  facet_wrap(.~NAME_0, nrow = 2)



# Add "mean" on Boxplot:
# https://stackoverflow.com/questions/3989987/joining-means-on-a-boxplot-with-a-line-ggplot2
# Mean/Std vs. Median/IQR: 
# https://www.statology.org/interquartile-range-vs-standard-deviation/
# Plot Mean with Standard Deviation:
# https://www.geeksforgeeks.org/plot-mean-and-standard-deviation-using-ggplot2-in-r/
# Boxplot explained: 
#https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51


# Arrange Output Layout:
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/