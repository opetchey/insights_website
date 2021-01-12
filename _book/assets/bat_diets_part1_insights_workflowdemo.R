# An analysis of the diets of
# male and female bats.
# Data from "Female dietary bias
# towards large migratory moths
# in the European free-tailed bat
# (Tadarida teniotis)"
# By Mata and colleagues in 2016
# in the journal Biology Letters
# http://rsbl.royalsocietypublishing.org/content/12/3/20150988


# Load the libraries we use
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggbeeswarm)


# read in the bat data, specifying the NA string as "na"
bats <- read_csv("data/bat_sex_diet_Mata_etal_2016.csv", na = "na")

# rename Sample to Bat_ID
bats <- bats %>%
  rename("Bat_ID" = "Sample")

# rename Order to Row_order
# and Order_1 to Order.
bats <- bats %>%
  rename("Row_order" = "Order",
         "Order" = "Order_1")

# change all spaces to underscores
names(bats) <- str_replace_all(names(bats), c(" " = "_"))

# get rid of brackets in names
names(bats) <- str_replace_all(names(bats), c("\\(" = "", "\\)" = ""))

# use function dmy() from stringr package to 
# encode Dates properly
bats <- bats %>%
  mutate(Date_proper = dmy(Date))

# update our working copy of the data to 
# remove the old Date column but leave the Date_proper one in
bats <- bats %>%
  select(-Date)

# use mutate and case_when 
# for nested if-else like changes to values in a variable
# e.g. if Sex is M, make it Male....
bats <- bats %>%
  mutate(Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female"))

# case_when replacement of Age values.
bats <- bats %>%
  mutate(Age = case_when(Age == "Ad" ~ "Adult",
                         Age == "Juv" ~ "Juvenile"))

# check for duplicate rows in the bats data
bats %>%
  duplicated() %>%
  sum()

# check for duplicates among specific combinations of variables
# select the ID, Sp._Nr. and Date_proper
bats %>%
  select(Bat_ID, Sp._Nr., Date_proper) %>%
  duplicated() %>%
  sum()

# use summarise to calculate things about specific variables
bats %>%
  summarise(var_min = min(Wingspan_mm, na.rm = TRUE),
            var_max = max(Wingspan_mm, na.rm = TRUE))

# use the dplyr function distinct() on the Sex variable to detect unexpected groups
bats %>%
  distinct(Sex)

# get sum of wingspan values that are NA
bats %>%
  summarise(num_nas = sum(is.na(Wingspan_mm)))

# use the function across to sum the NAs in every column
# note the use of the function symbol "~" before sum() to make this work
# .cols specifies the columns and .fns defines the function(s)
bats %>% 
  summarise(
    across(.cols = everything(), 
           .fns = ~sum(is.na(.)))) %>% 
  glimpse()


