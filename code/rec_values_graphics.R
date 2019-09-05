###--------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------###
### Graphics for recommended values of various physical constants
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

###--------------------------------------------------------------###                       
###--------------------------------------------------------------###
#' @name: revcalPlot
#' @param: cnstnt, string, the name of the constant you want to plot
#' @param: lab_title, string, the title for the plot
#' @param: data, data.frame, default to rvs defined above
#' @param: plot_theme, a theme object, default defined as stdtheme,
#             which is defined above.
#' @param: ref, numeric, reference value
#' @param: include_refline, boolean, if true, plot includes a 
#'            horizontal line for a reference value. 
#' @return: plot of recomended values over the years
###--------------------------------------------------------------###
###--------------------------------------------------------------###
recvalPlot = function(cnstnt, 
                      lab_title, 
                      data = rvs, 
                      plot_theme = stdtheme, 
                      ref = NA, 
                      include_refline = TRUE,
                      nudgex = 0, 
                      error_bars = FALSE){

  # Build forest plot vs. time
  plt = data %>% 
    filter(constant == cnstnt) %>%
    ggplot() + 
    geom_pointrange(aes(x = year, y = estimate, 
                        ymin = estimate - error,
                        ymax = estimate + error)) + 
    geom_line(aes(x = year, y = estimate)) + 
    labs(title = lab_title, 
         x = "", 
         y = "Estimated Value") + 
    plot_theme # use the theme above

  
  # Add a horizonal reference line if desired
  if(include_refline){
    
    if(is.na(ref)){ # check that a reference value has been specified
      
      # if no reference value has been specified, use the 1973 value
      ref_val = dplyr::filter(data, constant == cnstnt, year == 1973)$estimate
    
    } else {
      
      ref_val = dplyr::filter(data, constant == cnstnt, year == ref)$estimate
      
    }
    
    dat <- dplyr::filter(data, constant == cnstnt)
    
    min_year = dat %>% 
      summarize(min_yr = min(year))
    min_yr = as.numeric(as.character(min_year$min_yr))
    
    range_vals <- dat %>% 
      summarize(range = max(estimate) - min(estimate))
    nudge <- range_vals$range/nrow(dat)/10
    
    # add reference line to the plot
    plt = plt + geom_hline(yintercept = ref_val, lty = 2) + 
      annotate(geom = "text", 
               x = min_yr + nudgex, y = ref_val + nudge, 
               label = paste0(ref, " Recommended Value"), 
               hjust = 0, vjust = 0)
  
  }
  
  ########## RETURN
  return(plt)
}



##-------------------------------------------------------------------------------##
## Read in data
##-------------------------------------------------------------------------------##
rvs = read.csv("./data/recommended values.csv")
head(rvs)
str(rvs)

# Names list of constants in Fig 3, some names for the plots and filenames
cnsts  = list(cnst = c("c", "N", "1/alpha", "h", "m_e", "e"),
              name = c("Speed of Light (c)",
                       "Avogadro's Number (N)", 
                       expression("Inverse Fine Structure Constant ("~alpha^-1~")"),
                       "Planck's Constant (h)",
                       expression("Electron Mass ("~m[e]~")"),
                       "Magnitude of Electron Charge (e)"),
              fname = paste0(c("speed_of_light",
                               "avogadro", 
                               "invFSC", 
                               "planck", 
                               "electron_mass", 
                               "electron_charge"), 
                             ".pdf")
        )


##-------------------------------------------------------------------------------##
## Build plots
##-------------------------------------------------------------------------------##
for(i in 1:length(cnsts$cnst)){
  cc = cnsts$cnst[i] # Constnat name
  labtitle = cnsts$name[i] # plot title
  fname = cnsts$fname[i] # file name
  foo = recvalPlot(cc, labtitle, include_refline = TRUE, ref = 2014) # build plot using 2014 reference value
  show(foo) # show the plot
  
  # save plot
  ggsave(plot = foo,
         filename = paste0("./graphics/", gsub(".pdf", "_updated.pdf", fname)),
         width = 12, height = 10)
}

###--------------------------------------------###
###---Figure 1B
###--------------------------------------------###
f1b = rvs %>% 
  filter(constant == "c") %>%
  mutate(estimate = estimate * 10^5, 
         error = error * 10^5 * 1.5) %>%
  recvalPlot("c", "", data = ., ref = 2014, nudgex = 2) + 
  scale_y_continuous(breaks = 299765 + 5 * 0:8)
f1b
ggsave(plot = f1b,
       filename = "./graphics/Figure 1B.pdf",
       width = 12, height = 10)

rvs %>% 
  filter(constant == "c", year %in% c(1986,2014))

###--------------------------------------------###
###---Figure 2
###--------------------------------------------###
# Info for title
constants_for_grid <- list(cnst = c("N", "1/alpha", "h", "m_e", "e"),
                           name = c("Avogadro's Number (N)", 
                                    expression("Inverse Fine Structure Constant ("~alpha^-1~")"),
                                    "Planck's Constant (h)",
                                    expression("Electron Mass ("~m[e]~")"),
                                    "Magnitude of Electron Charge (e)"))
# Rescale to differences vs 2014
dat_for_grid = rvs %>%
  filter(year == 2014) %>%
  group_by(constant) %>%
  summarize(rec_val = estimate) %>%
  left_join(., rvs) %>%
  filter(constant %in% c(constants_for_grid$cnst, "c")) %>%
  mutate(deviation = estimate - rec_val) %>%
  select(-estimate) %>%
  rename(estimate = deviation)

# Build plots for each constant
invfsc = recvalPlot("1/alpha", "", 
                    data = dat_for_grid, include_refline = F) +
  labs(y = "Deviation from 2014 Value", 
       title = constants_for_grid$name[2]) + 
  geom_hline(yintercept = 0)

plnk = recvalPlot("h", "", 
                  data = dat_for_grid, include_refline = F) +
  labs(y = "Deviation from 2014 Value", 
       title = constants_for_grid$name[3]) + 
  geom_hline(yintercept = 0)

echg = recvalPlot("e", "", 
                  data = dat_for_grid, include_refline = F) +
  labs(y = "Deviation from 2014 Value", 
       title = constants_for_grid$name[5]) + 
  geom_hline(yintercept = 0)

emass = recvalPlot("m_e", "", 
                   data = dat_for_grid, include_refline = F) +
  labs(y = "Deviation from 2014 Value", 
       title = constants_for_grid$name[4]) + 
  geom_hline(yintercept = 0)

avo = recvalPlot("N", "", 
                 data = dat_for_grid, include_refline = F) +
  labs(y = "Deviation from 2014 Value", 
       title = constants_for_grid$name[1]) + 
  geom_hline(yintercept = 0)


fig2 = grid.arrange(invfsc, plnk, echg, emass, avo, ncol = 1)
ggsave(plot = fig2,
       filename = "./graphics/Figure 2.pdf", 
       height = 18, width = 12, limitsize = FALSE)


# Build plots for each constant ----

# Clean up old version of Figure 2
rm("invfsc", "plnk", "echg", "emass", "avo")

# Inverse fine structure
invfsc <- dat_for_grid %>% 
  filter(constant == "1/alpha", year >= 1950, year <= 1975) %>%
  mutate(rescale_multiple = 7300) %>% 
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, color = "grey60") +
  geom_pointrange(aes(x = year, y = estimate * rescale_multiple, 
                      ymin = (estimate - error) * rescale_multiple,
                      ymax = (estimate + error) * rescale_multiple),
                  fatten = 0.75) +
  geom_line(
    aes(x = year, y = estimate * rescale_multiple), 
    size = 0.4
  )  +
  scale_x_continuous(
    name   = "Year of estimate",
    breaks = seq(1950, 1975, 5),
  ) +
  scale_y_continuous(
    name   = "Deviation from 2014 recommended value  (ppm)",
    limits = c(-2.5, 25),
    breaks = seq(0, 25, 5),
    expand = c(0, 0)
  ) +
  ggtitle(
    expression("Inverse Fine Structure Constant, " ~ alpha^-1 ~ "")
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),                            
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12), 
    axis.line.y = element_line(colour = "black"),
    title = element_text(size = 14),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1)
  )

# Planks
# rescale_multiple <- 100000

plnk <- dat_for_grid %>% 
  filter(constant == "h", year >= 1950, year <= 1975) %>% 
  mutate(rescale_multiple = 100000) %>% 
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, color = "grey60") +
  geom_pointrange(aes(x = year, y = estimate * rescale_multiple, 
                      ymin = (estimate - error) * rescale_multiple,
                      ymax = (estimate + error) * rescale_multiple),
                  fatten = 0.75) +
  geom_line(
    aes(x = year, y = estimate * rescale_multiple), 
    size = 0.4
  )  +
  scale_x_continuous(
    name   = "Year of estimate",
    breaks = seq(1950, 1975, 5),
  ) +
  scale_y_continuous(
    name   = "Deviation from 2014 recommended value (ppm)",
    limits = c(-150, 50),
    breaks = seq(-150, 50, 50),
    expand = c(0, 0)
  ) +
  ggtitle("Planck's Constant, h") +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),                            
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12), 
    axis.line.y = element_line(colour = "black"),
    title = element_text(size = 14),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1)
  )

# Electron charge
echg <- dat_for_grid %>% 
  filter(constant == "e", year >= 1950, year <= 1975) %>% 
  mutate(rescale_multiple = 260500) %>% 
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, color = "grey60") +
  geom_pointrange(aes(x = year, y = estimate * rescale_multiple, 
                      ymin = (estimate - error) * rescale_multiple,
                      ymax = (estimate + error) * rescale_multiple),
                  fatten = 0.75) +
  geom_line(
    aes(x = year, y = estimate * rescale_multiple), 
    size = 0.4
  )  +
  scale_x_continuous(
    name   = "Year of estimate",
    breaks = seq(1950, 1975, 5),
  ) +
  scale_y_continuous(
    name   = "Deviation from 2014 recommended value (ppm)",
    limits = c(-200, 25),
    breaks = seq(-200, 25, 50),
    expand = c(0, 0)
  ) +
  ggtitle(
    "Electron Charge, e"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),                            
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12), 
    axis.line.y = element_line(colour = "black"),
    title = element_text(size = 14),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1)
  )

# Electron mass

emass <- dat_for_grid %>% 
  filter(constant == "m_e", year >= 1950, year <= 1975) %>% 
  mutate(rescale_multiple = 124500) %>% 
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, color = "grey60") +
  geom_pointrange(aes(x = year, y = estimate * rescale_multiple, 
                      ymin = (estimate - error) * rescale_multiple,
                      ymax = (estimate + error) * rescale_multiple),
                  fatten = 0.75) +
  geom_line(
    aes(x = year, y = estimate * rescale_multiple), 
    size = 0.4
  )  +
  scale_x_continuous(
    name   = "Year of estimate",
    breaks = seq(1950, 1975, 5),
  ) +
  scale_y_continuous(
    name   = "Deviation from 2014 recommended value (ppm)",
    limits = c(-200, 30),
    breaks = seq(-200, 30 , 25),
    expand = c(0, 0)
  ) +
  ggtitle(
    expression("Electron Mass," ~ m[e] ~ "")
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),                            
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12), 
    axis.line.y = element_line(colour = "black"),
    title = element_text(size = 14),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1)
  )

# Avogadro's number
avo <- dat_for_grid %>% 
  filter(constant == "N", year >= 1950, year <= 1975) %>% 
  mutate(rescale_multiple = 50000) %>% 
  ggplot() +
  geom_hline(yintercept = 0, lty = 2, color = "grey60") +
  geom_pointrange(aes(x = year, y = estimate * rescale_multiple, 
                      ymin = (estimate - error) * rescale_multiple,
                      ymax = (estimate + error) * rescale_multiple),
                  fatten = 0.75) +
  geom_line(
    aes(x = year, y = estimate * rescale_multiple), 
    size = 0.4
  )  +
  scale_x_continuous(
    name   = "Year of estimate",
    breaks = seq(1950, 1975, 5),
  ) +
  scale_y_continuous(
    name   = "Deviation from 2014 recommended value (ppm)",
    limits = c(-25, 150),
    breaks = seq(0, 150, 50),
    expand = c(0, 0)
  ) +
  ggtitle(
    "Avogadro's Number, N"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),                            
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12), 
    axis.line.y = element_line(colour = "black"),
    title = element_text(size = 14),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1)
  )

# Final figure 
fig2 = plot_grid(invfsc, plnk, echg, emass, avo, ncol = 1)
ggsave(plot = fig2,
       filename = "./graphics/Figure 2.pdf", 
       height = 20, width = 8, limitsize = FALSE)

