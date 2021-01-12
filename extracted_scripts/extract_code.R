library(knitr)

filename <- "06-wfdemo-polity-fooddiv"
purl(input = paste0(filename, ".Rmd"),
     output = paste0("extracted_scripts/", filename, ".R"))

filename <- "05-wfdemo-preydiv-predstab"
purl(input = paste0(filename, ".Rmd"),
     output = paste0("extracted_scripts/", filename, ".R"))

filename <- "08-wfdemo-fishdiet-restr-solution"
purl(input = paste0(filename, ".Rmd"),
     output = paste0("extracted_scripts/", filename, ".R"))
