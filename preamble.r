library("dplyr")
library("readr")
library("ggplot2")
library("nasaweather")
library("magrittr")
library("knitr")
library("hexbin")

# knitr options

opts_chunk$set(fig.width=3.5, fig.height=3.5, fig.align="center", cache=TRUE)

# Set plotting to bw plot default, but with transparent background elements. Note
# transparency requires the panel.background, plot.background, and device background
# all be set!

theme_update(plot.background = element_rect(fill = "transparent", colour = NA))

options(tibble.print_max = 5, tibble.print_min = 5)