library(ggplot2)
library(dplyr)
#library(hrbrthemes)
#library(viridis)

# List Files
path <- "C:/EAGLE/AquaPonds/Codes/r_for_marco/data/all_gid2_5yGr.csv"
df <- read.csv(file = path, header = TRUE, sep = ",")

# sample size
sample_size = df %>% group_by(NAME_0, GID_2) %>% count() %>% group_by(NAME_0) %>% summarize(num=n())

# Violin / Box Plot
breaks <- seq(-20, 100, by=20)
#fol <- unique(df$stats_item)

fnl <- c(`5y_avgGr_1990` = "1986 - 1990",
            `5y_avgGr_1995` = "1991 - 1995",
            `5y_avgGr_2000` = "1996 - 2000",
            `5y_avgGr_2005` = "2001 - 2005",
            `5y_avgGr_2010` = "2006 - 2010",
            `5y_avgGr_2015` = "2011 - 2015",
            `5y_avgGr_2019` = "2016 - 2019")

df %>%
  filter(stats_item != "5y_avgGr_1990") %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(NAME_0, "\n", num, " Districts")) %>%
  
  ggplot( aes(x=myaxis, y=stats_value, fill=NAME_0)) +
  geom_violin(width=1, alpha=0.7) +
  geom_boxplot(width=0.1, lwd=0.3, color="black", alpha=0.2, outlier.shape = NA) +
  #geom_boxplot(outlier.alpha = 0.5, outlier.color = 'grey', outlier.size = 0.5,
  #             alpha = 0.2, width = 0.1, lwd = 0.3) +
  
  scale_y_continuous(limits = quantile(df$stats_value, c(0.025, 0.975), na.rm = TRUE),
                     breaks = breaks, name = "Growth Rate (%)") +
  xlab("") +
  
  facet_wrap(~stats_item, labeller = as_labeller(fnl)) +
  
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12), # Facet Label
    
    #legend.position="none",
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=16),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.75, 'cm'),
    
    axis.text=element_text(size=10),
    axis.title=element_text(size=16),
    axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
    #axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    
    #panel.grid.major.x = element_line(color = 'orange', size = 0.5, linetype = 2),
    #panel.grid.minor.x = element_line(color = 'grey', size = 0.5, linetype = 2),
    panel.grid.major.y = element_line(color = 'grey', size = 0.4, linetype = 1),
    panel.grid.minor.y = element_line(color = 'grey', size = 0.4, linetype = 2)
  )#+
#  guides(fill = guide_legend(byrow = TRUE))


