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

# 1984 value used in H&F
c1984 = 299792.458

# Birge ratios don't seem to line up exactly...
cdat %>% 
  group_by(year <= 1941) %>%
  mutate(est = ifelse((hf_use_orig == 0), 
                        estimate_adj, 
                        estimate_orig)) %>%
  summarize(br = birgeRatio(est, sd^2),
            br_adj = birgeRatio(estimate_adj, sd^2), 
            br_tm = birgeRatio(est, sd^2, c1984), 
            br_tm_adj = birgeRatio(estimate_adj, sd^2, c1984))



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
  geom_hline(yintercept = c1984, lty=2) + 
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
  geom_hline(yintercept = c1984, lty=2) + 
  labs(x = "", y = "") + 
  annotate(geom = "text", size = 3.8,
           x = 1914, y = c1984 + 1.5, 
           label = c("1984 Value")) + 
  stdtheme
post1900

out = grid.arrange(pre1900, post1900, 
             nrow = 1, 
             widths = c(1, 2))

ggsave(plot = out, "./graphics/HF speed of light adjusted.pdf", width = 12, height = 10)


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
  geom_hline(yintercept = 299792.458, lty=2) + 
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
  geom_hline(yintercept = 299792.458, lty=2) + 
  labs(x = "", y = "") + 
  annotate(geom = "text", size = 3.8,
           x = 1914, y = c1984 + 1.5, 
           label = c("1984 Value")) + 
  stdtheme

out_un = grid.arrange(pre1900_un, post1900_un, 
                   nrow = 1, 
                   widths = c(1, 2))

ggsave(plot = out_un, "./graphics/HF speed of light unadjusted.pdf", width = 12, height = 10)     
