###--------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------###
### Graphics for measurements of the speed of light (H&F Fig 1)
###--------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------###

##-------------------------------------------------------------------------------##
## Useful variables, libraries, and functions
##-------------------------------------------------------------------------------##

# Get dplyr, ggplot2
library(tidyverse)

# Code for arranging plots
library(cowplot)
library(gridExtra)

# Standard theme for graphics
stdtheme = theme(panel.grid.minor = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text = element_text(size = 14),
                 axis.title = element_text(size = 14), 
                 title = element_text(size = 16))

# stdtheme = theme(panel.grid.minor = element_blank(), 
#                  axis.ticks = element_blank(),
#                  axis.text = element_text(size = 14))


###--------------------------------------------------------------###                       
###--------------------------------------------------------------###
#' @name: birgeRatio
#' @param: t, vector of numerics, measurements of c
#' @param: v, vector of numerics, variance of measurements of c
#' @param: true_mean, numeric, take quadratic form around 
#'            pre-specified value, instead of Tbar 
#' @return: birge ratio
###--------------------------------------------------------------###
###--------------------------------------------------------------###
birgeRatio <- function(t, v, true_mean = NA){
  k = length(t)
  
  # set tbar
  if(is.na(true_mean)){
    
    tbar = weighted.mean(x=t, w=1/v)
  
  } else {
    
    tbar = true_mean

  }
  
  # Compute Q
  Q = sum((t - tbar)^2/v)
  
  ###### RETURN
  return(Q/(k-1))
}



##-------------------------------------------------------------------------------##
## Read in data and check it
##-------------------------------------------------------------------------------##
cdat = read.csv("./data/speed of light.csv")
head(cdat)

# 2014 value used in H&F
rec_val = 2.997925 * 10^5
rec_val_year = 2014

# Birge ratios don't seem to line up exactly...
cdat %>% 
  group_by(year <= 1941) %>%
  mutate(est = ifelse((hf_use_orig == 0), 
                        estimate_adj, 
                        estimate_orig)) %>%
  summarize(br = birgeRatio(est, sd^2),
            br_adj = birgeRatio(estimate_adj, sd^2), 
            br_tm = birgeRatio(est, sd^2, rec_val), 
            br_tm_adj = birgeRatio(estimate_adj, sd^2, rec_val))



##-------------------------------------------------------------------------------##
## Make plots with Birge's adjustments
##-------------------------------------------------------------------------------##

# Fig 1.1, pre-1900 measurements, using values later adjusted by Birge
pre1900 = ggplot(filter(cdat, year <= 1903)) + 
  geom_pointrange(aes(x = year, 
                      y = estimate_adj, 
                      ymin = estimate_adj - probable_error, 
                      ymax = ifelse(estimate_adj + probable_error > 300000, 
                                      300000, 
                                      estimate_adj + probable_error))) + 
  scale_y_continuous(limits = c(299600, 300000), 
                     breaks = 299600 + 50 * 0:8) + 
  scale_x_continuous(breaks = 1870 + 10*0:3) + 
  geom_hline(yintercept = rec_val, lty=2) + 
  labs(x = "", y = "") + 
  theme(panel.grid.minor = element_blank()) + 
  stdtheme


# Fig 1.2, post-1900 measurements, using values later adjusted by Birge
post1900 = ggplot(filter(cdat, year > 1902)) + 
  geom_pointrange(aes(x = year, 
                      y = estimate_adj, 
                      ymin = ifelse(estimate_adj - probable_error < 299740,
                                      299740,
                                      estimate_adj - probable_error), 
                      ymax = estimate_adj + probable_error)) + 
  scale_y_continuous(limits = c(299750, 299840),
                     breaks = 299750 + 10 * 0:9) + 
  scale_x_continuous(limits = c(1900, 1967),
                     breaks = 1900 + 10*0:6) + 
  coord_cartesian(clip = 'off') +
  geom_hline(yintercept = rec_val, lty=2) + 
  labs(x = "", y = "") + 
  annotate(geom = "text", size = 3.8,
           x = 1914, y = rec_val + 1.0, 
           label = c(paste(rec_val_year, "Recommended Value"))) + 
  stdtheme
post1900

out = grid.arrange(pre1900, post1900, 
             nrow = 1, 
             widths = c(1, 2))

ggsave(plot = out, "./graphics/HF speed of light adjusted.pdf", width = 12, height = 10)


###---Combined plot for Figure 1A
ggplot(cdat) + 
  geom_pointrange(aes(x = year, 
                      y = estimate_adj, 
                      ymin = ifelse(estimate_adj - probable_error < 299740,
                                    299740,
                                    estimate_adj - probable_error), 
                      ymax = ifelse(estimate_adj + probable_error > 300000, 
                                    300000,
                                    estimate_adj + probable_error))) + 
  scale_y_continuous(limits = c(299750, 300000)) + 
  scale_x_continuous(limits = c(1870, 1967),
                     breaks = 1880 + 20*0:4) + 
  coord_cartesian(clip = 'off') +
  geom_hline(yintercept = rec_val, lty=2) + 
  labs(x = "", y = "Estimate") + 
  annotate(geom = "text", size = 3.95,
           x = 1870, y = rec_val - 1.0,
           hjust = 0, vjust = 1,
           label = c(paste(rec_val_year, "Recommended Value"))) + 
  stdtheme

ggsave("./graphics/Figure 1A Single Pane.pdf", height = 10, width = 12)


###---Combined plot for Figure 1A with inset 
main_plot <- cdat %>% 
  filter(year < 1960) %>% 
  mutate(flag = year == lag(year),
         flag = replace(flag, is.na(flag), FALSE),
         year = replace(year, flag == TRUE, year + 0.5)) %>% 
  ggplot() + 
  geom_pointrange(aes(x = year, 
                      y = estimate_adj, 
                      ymin = ifelse(estimate_adj - probable_error < 299740,
                                    299740,
                                    estimate_adj - probable_error), 
                      ymax = ifelse(estimate_adj + probable_error > 300000, 
                                    300000,
                                    estimate_adj + probable_error)),
                  fatten = 0.75) + 
  scale_y_continuous(name = "Measured speed of light (km/sec)",
                     limits = c(299750, 300000),
                     breaks = seq(299750, 300000, 25),
                     expand = expansion(mult = 0.02)) + 
  scale_x_continuous(name = "Year of experiment",
                     limits = c(1870, 1960),
                     breaks = seq(1870, 1960, 10),
                     expand = expansion(mult = 0.02)) + 
  coord_cartesian(clip = 'off') +
  geom_hline(yintercept = rec_val, lty=2, color = "grey60") + 
  annotate(geom = "text", size = 3.95,
           x = 1878, y = rec_val - 2,
           hjust = 0, vjust = 1,
           label = c(paste(rec_val_year, "Recommended Value")),
           family = "Times") + 
  theme_classic() +
  theme(text = element_text(family = "Times"),                            
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14), 
        title = element_text(size = 16),
        axis.title.x = element_text(hjust = 1),
        axis.title.y = element_text(hjust = 1))

# inset plot
inset_plot <- cdat %>% 
  filter(year >= 1945 & year < 1960) %>% 
  mutate(flag = year == lag(year),
         flag = replace(flag, is.na(flag), FALSE),
         year = replace(year, flag == TRUE, year + 0.5)) %>% 
ggplot() + 
  geom_pointrange(aes(x = year, 
                      y = estimate_adj, 
                      ymin = ifelse(estimate_adj - probable_error < 299740,
                                    299740,
                                    estimate_adj - probable_error), 
                      ymax = ifelse(estimate_adj + probable_error > 300000, 
                                    300000,
                                    estimate_adj + probable_error)),
                  position = position_jitter(),
                  fatten = 0.75) + 
  scale_y_continuous(name = "Measured speed of light (km/sec)",
                     limits = c(299787.5, 299796.5),
                     breaks = seq(299788, 299796, 2),
                     expand = expansion(mult = 0.02)) + 
  scale_x_continuous(name = "Year of experiment",
                     limits = c(1946.5, 1961),
                     breaks = seq(1950, 1960, 5),
                     expand = expansion(mult = 0.02)) + 
  coord_cartesian(clip = 'off') +
  geom_hline(yintercept = rec_val, lty=2, color = "grey60") + 
  annotate(geom = "text", size = 3.5,
           x = 1956, y = rec_val - 1.1,
           hjust = 0, vjust = 1,
           label = c(paste(rec_val_year, "Recommended Value")),
           family = "Times") +
  geom_segment(aes(x = 1957, y = rec_val-1, 
                 xend = 1956.5, yend = rec_val - 0.1),
             arrow = arrow(length = unit(0.02, "npc"), type = "closed" ),
             size = 0.1, color = "grey40") +
  ggtitle("Rescaled for estimates between 1945 and 1960") +
  theme_classic() +
  theme(text = element_text(family="Times"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14), 
        title = element_text(size = 12),
        axis.title.x = element_text(hjust = 1, size = 12),
        axis.title.y = element_text(hjust = 1, size = 12),
        plot.background = element_rect(colour = "black", 
                                        size = 0.75))

# Place the inset
plot_with_inset <-
  ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset_plot,
            hjust = 1, vjust = 1,
            x = 0.99, y = 0.99, 
            width = 0.475, height = 0.475)
  # draw_plot_label(
  #   c("(A)", "(B)"),
  #   c(0, 0.45),
  #   c(1, 0.95),
  #   size = 12,
  #   family = "Times"
  # )

ggsave("./graphics/Figure 1A Single Pane with inset.pdf", plot_with_inset, height = 9, width = 12)


##-------------------------------------------------------------------------------##
## Make plots without Birge's adjustments
##-------------------------------------------------------------------------------##
cdat_hf = cdat %>% mutate(est = ifelse((hf_use_orig == 0), estimate_adj, estimate_orig))
           
# Fig 1.1, pre-1900 measurments w/o Birge's adjustments
pre1900_un = ggplot(filter(cdat_hf, year <= 1903)) + 
  geom_pointrange(aes(x = year, 
                      y = est, 
                      ymin = est - probable_error, 
                      ymax = ifelse(est + probable_error > 300000, 
                                      300000, 
                                      est + probable_error))) + 
  scale_y_continuous(limits = c(299600, 300000), 
                     breaks = 299600 + 50 * 0:8) + 
  scale_x_continuous(breaks = 1870 + 10*0:3) + 
  geom_hline(yintercept = rec_val, lty=2) + 
  labs(x = "", y = "") + 
  theme(panel.grid.minor = element_blank()) + 
  stdtheme


# Fig 1.2, post-1900 measurments w/o Birge's adjustments
post1900_un = ggplot(filter(cdat_hf, year > 1902)) + 
  geom_pointrange(aes(x = year, 
                      y = est, 
                      ymin = ifelse(est - probable_error < 299750, 
                                      299750, 
                                      est - probable_error), 
                      ymax = est + 1.48 * sd)) + 
  scale_y_continuous(limits = c(299750, 299840),
                     breaks = 299750 + 10 * 0:9) + 
  scale_x_continuous(limits = c(1900, 1967),
                     breaks = 1900 + 10*0:6) + 
  geom_hline(yintercept = rec_val, lty=2) + 
  labs(x = "", y = "") + 
  annotate(geom = "text", size = 3.8,
           x = 1914, y = rec_val + 1.0, 
           label = c(paste(rec_val_year, "Recommended Value"))) + 
  stdtheme

out_un = grid.arrange(pre1900_un, post1900_un, 
                   nrow = 1, 
                   widths = c(1, 2))

ggsave(plot = out_un, "./graphics/HF speed of light unadjusted.pdf", width = 12, height = 10)     
