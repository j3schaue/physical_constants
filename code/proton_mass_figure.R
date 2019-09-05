###--------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------###
### Figure 3: Proton Mass; Statistics of Replication Paper 
###--------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------###

# Load package(s)
library(tidyverse)

# Recreate data -- eyeball it
proton_dat <- tibble(year     = c(1961.5, 1964.8, 1971, 1974.5),
                     estimate = c(938.215, 938.255, 938.259, 938.28),
                     error    = c(0.01, 0.005, 0.005,0.0025))

fig3 <- ggplot(data = proton_dat, aes(x =year, y = estimate)) +
  geom_pointrange(
    aes(ymin = estimate - error, ymax = estimate + error),
    fatten = 0.5
    ) +
  scale_x_continuous(
    name = "Publication date",
    limits = c(1960, 1975),
    breaks = seq(1960, 1975, 5),
    expand = c(0,0)
    ) +
  scale_y_continuous(
    name   = "Proton Mass (Mev)",
    limits = c(938.18, 938.30),
    breaks = seq(938.18, 938.30, 0.02)
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Times"),
    plot.margin = margin(t = 6, l = 6, b = 6, r = 24, unit = "pt"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14), 
    title = element_text(size = 16)
    )

ggsave(plot = fig3,
       filename = "./graphics/Figure 3.pdf",
       width = 3.8, height = 4)

  
