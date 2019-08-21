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
recvalPlot = function(cnstnt, lab_title, data = rvs, plot_theme = stdtheme, ref = NA, include_refline = TRUE){

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
               x = min_yr, y = ref_val + nudge, 
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

