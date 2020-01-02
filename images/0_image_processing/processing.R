library(tidyverse)
library(magick)

imag <- image_read_svg('noun_action_1899450.svg', width=4000) %>%
  image_resize("100") %>%
  image_colorize(100, "#0000ff") %>%
  image_crop("100x100+0+0") # crop out width:100px and height:150px starting +50px from the left
print(imag)
image_write(imag, path = "action.png", format = "png")

imag <- image_read_svg('noun_Chain_429215.svg', width=4000) %>%
  image_resize("100") %>%
  image_colorize(100, "#c500ff") %>%
  image_crop("100x100+0+0") # crop out width:100px and height:150px starting +50px from the left
print(imag)
image_write(imag, path = "chain.png", format = "png")

imag <- image_read_svg('noun_efficiency_1555215.svg', width=4000) %>%
  image_resize("100") %>%
  image_colorize(100, "#ff8f00") %>%
  image_crop("100x100+0+0") # crop out width:100px and height:150px starting +50px from the left
print(imag)
image_write(imag, path = "efficiency.png", format = "png")

imag <- image_read_svg('noun_Information_2119887.svg', width=4000) %>%
  image_resize("100") %>%
  image_colorize(100, "#52C0CA") %>%
  image_crop("100x100+0+0") # crop out width:100px and height:150px starting +50px from the left
print(imag)
image_write(imag, path = "info.png", format = "png")

imag <- image_read_svg('noun_Safety_1930823.svg', width=4000) %>%
  image_resize("100") %>%
  image_colorize(100, "#36AC48") %>%
  image_crop("100x100+0+0") # crop out width:100px and height:150px starting +50px from the left
print(imag)
image_write(imag, path = "safety.png", format = "png")

imag <- image_read_svg('noun_Warning_77514.svg', width=4000) %>%
  image_resize("100") %>%
  image_colorize(100, "#FA0808") %>%
  image_crop("100x105+0+0") %>% # crop out width:100px and height:150px starting +50px from the left
  image_resize("x100")
print(imag)
image_write(imag, path = "warning.png", format = "png")
