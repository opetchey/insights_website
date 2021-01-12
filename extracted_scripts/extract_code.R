library(knitr)

filename <- "05-wfdemo-polity-fooddiv"
purl(input = paste0(filename, ".Rmd"),
     output = paste0("extracted_scripts/", filename, ".R"))

filename <- "06-wfdemo-preydiv-predstab"
purl(input = paste0(filename, ".Rmd"),
     output = paste0("extracted_scripts/", filename, ".R"))

filename <- "07-wfdemo-fishdiet-restr"
purl(input = paste0(filename, ".Rmd"),
     output = paste0("extracted_scripts/", filename, ".R"))
