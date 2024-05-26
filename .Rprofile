source("renv/activate.R")

if (interactive()) {
  require(usethis)
  require(devtools)
  devtools::load_all()
  l <- launch_app()
}
library(ggplot2)
theme_set(
  theme_bw(base_size = 12) +
    theme(
      panel.background = element_blank(),
      panel.spacing = unit(0,"lines"),
      legend.position = 'top',
      strip.text.y.right = element_text(angle = 0, vjust = 0.5),
      strip.text.y.left = element_text(angle = 0, vjust = 0.5),
      strip.background = element_blank(),
      strip.placement = 'outside', 
      axis.ticks = element_blank())
)

palette_okabeito <-"khroma::okabeito"

colors_green_3 <- c("#A1D99BFF", "#74C476FF", "#41AB5DFF" )
colors_green_c <- paletteer::paletteer_d("RColorBrewer::Greens")
options(ggplot2.discrete.fill= colors_green_3)
