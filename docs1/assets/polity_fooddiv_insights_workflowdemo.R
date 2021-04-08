

## ----message=FALSE, warning=FALSE-----------------------------------------------------------------
# Load the libraries we use
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(vegan)
library(ggbeeswarm)


## Install the `vegan` and `ggbeeswarm` library if you have not already done so, otherwise `library(vegan)` and `library(ggbeeswarm)` will not work. Look at the **Packages** section of the *Getting Acquainted* chapter of the book if you need help with installation.


## -------------------------------------------------------------------------------------------------
rm(list = ls())
pol <- read_csv("data/p4v2015.csv")


## ----eval=FALSE-----------------------------------------------------------------------------------
## library(readxl)
## pol <- read_excel("data/p4v2015.xls")


## -------------------------------------------------------------------------------------------------
pol <- pol %>%
  select(Country = country,
         Year = year,
         Polity2 = polity2)


## Please note that here and below we do not describe at all or in much detail the R code used. This is because it is described in detail in the book.


## -------------------------------------------------------------------------------------------------
pol_nas <- pol %>%
  filter_all(any_vars(is.na(.))) 
pol_nas


## -------------------------------------------------------------------------------------------------
pol <- pol %>%
  na.omit()


## -------------------------------------------------------------------------------------------------
rm(pol_nas)


## -------------------------------------------------------------------------------------------------
pol %>%
  select(Country, Year) %>%
  duplicated() %>%
  sum()


## -------------------------------------------------------------------------------------------------
pol %>%
  select(Country, Year) %>%
  filter(duplicated(.))


## -------------------------------------------------------------------------------------------------
pol %>%
  filter(Country == "Yugoslavia",
         Year == 1991)


## -------------------------------------------------------------------------------------------------
pol %>%
  filter(Country == "Ethiopia",
         Year == 1993)


## -------------------------------------------------------------------------------------------------
pol <- pol %>%
  group_by(Country, Year) %>%
  summarise(Polity2 = mean(Polity2))


## We just made a decision about how to deal with the duplicated scores. We took the mean. There are other things we could have done (e.g. use a different measure of central tendency, such as the median). So we have made an arbitrary choice in our analysis pathway. We went down one path when we could have chosen another. We need to 1) recognise and record when we make decisions like this, and 2) check to see if our conclusions are robust to variation in what we choose to do. (Here, conclusions would not be affected by using the median, as there are only two values being averaged.)


## -------------------------------------------------------------------------------------------------
pol %>%
  select(Country, Year) %>%
  duplicated() %>%
  sum()


## -------------------------------------------------------------------------------------------------
pol_summ <- pol %>%
  summarise(min_year = min(Year),
            max_year = max(Year),
            min_pol = min(Polity2),
            max_pol = max(Polity2))


## -------------------------------------------------------------------------------------------------
write_rds(pol, "data/pol.RDS")


## ----pol1, fig.cap='Frequency distributions of the polity score and of the year in which observations were made.', out.width='100%', fig.asp=.75, fig.align='center'----
pol %>%
  pivot_longer(names_to = "Variable", values_to = "Value", cols = 2:3) %>%
  ggplot() +
  geom_histogram(aes(x = Value), bins = 21) +
  facet_wrap( ~ Variable, scales = "free", nrow = 2)


## -------------------------------------------------------------------------------------------------
pol %>%
  summarise(num_countries = length(unique(Country)),
            num_years = length(unique(Year)),
            first_year = min(Year),
            last_year = max(Year))


## -------------------------------------------------------------------------------------------------
per_country_pol <- pol %>%
  group_by(Country) %>%
  summarise(num_years = length(unique(Year)),
            first_year = min(Year),
            last_year = max(Year),
            years_interval = last_year - first_year +1,
            gaps = num_years != years_interval)


## -------------------------------------------------------------------------------------------------
per_country_pol %>%
  summarise(sum(gaps))


## ----eval=FALSE-----------------------------------------------------------------------------------
## pol %>%
##   group_by(Year) %>%
##   summarise(num_countries = length(unique(Country))) %>%
##   ggplot() +
##   geom_point(aes(x=Year, y=num_countries))


## ----pol2, fig.cap='Change in polity in some countries of interest (to the authors at least)', out.width='100%', fig.asp=.75, fig.align='center'----
countries_of_interest <- c("Bhutan", "Brasil", "Cuba", "Cambodia",
                           "Estonia", "Yugoslavia", "Australia")
pol %>%
  filter(Country %in% countries_of_interest) %>%
  ggplot() +
  facet_wrap(~ Country) +
  geom_point(aes(x=Year, y=Polity2, col=Country)) +
  geom_line(aes(x=Year, y=Polity2, col=Country))


## ----eval=FALSE-----------------------------------------------------------------------------------
## # Get the full food balance sheet dataset *All data* from the FAO website:
## # http://www.fao.org/faostat/en/#data/FBS/metadata.
## # Reduce the size of the FAO data, so easier to deal with
## # import FAO data
## fbs <- read_csv("data/FoodBalanceSheets_E_All_Data.csv",
##                locale = locale(encoding = 'ISO-8859-1'))
## # First fix some variable names:
## names(fbs) <- str_replace_all(names(fbs), c(" " = "_"))
## # Remove all non-countries
## fbs <- filter(fbs, Country_Code < 5000)
## # keep only some of the elements
## fbs <- filter(fbs, Element %in% c("Food supply quantity (kg/capita/yr)"))
## # remove some other variables that won't be used
## fbs <- select(fbs, -Country_Code, -Item_Code,
##              -Element_Code, -Element, -Unit,
##              -ends_with("F"))
## # save the reduced file
## saveRDS(fbs, "data/FoodBalanceSheets_E_All_Data_reduced.Rdata")
## rm(fbs)


## ----eval=TRUE------------------------------------------------------------------------------------
#rm(list = ls())
fbs <- readRDS("data/FoodBalanceSheets_E_All_Data_reduced.Rdata")


## -------------------------------------------------------------------------------------------------
fbs %>%
  select(Country, Item, Y1961, Y1971, Y1981, Y1991, Y2001, Y2011) %>%
  glimpse()


## -------------------------------------------------------------------------------------------------
fbs %>%
  summarise(num_countries = length(unique(Country)),
            num_food_items = length(unique(Item)))


## ----long-wide, fig.cap='The same data (a) in long/tidy format and (b) in wide format, with colours and arrows linking the same data in the two different formats.', out.width='100%', fig.asp=.75, fig.align='center', echo=FALSE----
knitr::include_graphics("images/long_wide.jpg")


## ----eval=TRUE------------------------------------------------------------------------------------
fbs_long <- fbs %>%
  pivot_longer(names_to = "Year",
               values_to = "Food_Supply_Quantity",
               cols = 3:57) %>%
  mutate(Year = as.numeric(substr(Year, 2, 5)))
glimpse(fbs_long)


## -------------------------------------------------------------------------------------------------
fbs_long %>%
  pull(Year) %>%
  unique()


## ----dupe-1, cache = FALSE------------------------------------------------------------------------
duplicated_records <- fbs_long %>%
  filter(duplicated(.))


## -------------------------------------------------------------------------------------------------
fbs_long %>%
  filter(Country == "Afghanistan",
         Item == "Eggs",
         Year == 1961)


## ----dupe-2, cache = FALSE------------------------------------------------------------------------
fbs_long <- unique(fbs_long)


## ----dupe-3, cache = FALSE------------------------------------------------------------------------
part_duplicated_records <- fbs_long %>%
  select(Country, Item, Year) %>%
  duplicated() %>%
  filter(fbs_long, .)


## -------------------------------------------------------------------------------------------------
filter(fbs_long,
       Country == "Albania",
       Item == "Eggs",
       Year == 1961)


## ----failing, cache = FALSE-----------------------------------------------------------------------
fbs_long <- fbs_long %>%
  group_by(Country, Year, Item) %>%
  summarise(Food_Supply_Quantity = mean(Food_Supply_Quantity))


## Time to tell the truth. It took us a some considerable time to realise there were duplicated records in the FAO dataset. We only realised there must be duplicates when we tried to get the numbers to add up below. They did not before we removed the duplicates, and this led us to find the duplicates. Once we found them, we removed them above. In the text above, it may look like we were really smart to think they might occur, check for them, find them, and remove them. In reality we were not so smart. We were, however, absolutely determined to understand why the numbers did not add up. You must also be as strict in your work. After we found the duplicates in this dataset, we also looked in the Polity data, found some, and then decided to add a check for duplicates to all our Workflow Demostrations, and to advise you to make this a standard sanity check when you start looking at a dataset.


## -------------------------------------------------------------------------------------------------
obs_per_year <- fbs_long %>%
  group_by(Year) %>%
  summarise(num = n())


## -------------------------------------------------------------------------------------------------
countries_per_year <- fbs_long %>%
  group_by(Year) %>%
  summarise(num = length(unique(Country)))


## ----here_xxx-------------------------------------------------------------------------------------
items_per_country <- fbs_long %>%
  group_by(Country) %>%
  summarise(num = length(unique(Item)))


## -------------------------------------------------------------------------------------------------
items_per_country %>%
  summarise(total = sum(num))


## We didn't just now find out anything new about the data, but we did confirm that we understand it properly. Checks and balances increase confidence.


## -------------------------------------------------------------------------------------------------
fbs_long %>%
  summarise_all(list( ~ sum(is.na(.)))) %>%
  glimpse()


## -------------------------------------------------------------------------------------------------
obs_per_year <- fbs_long %>%
  group_by(Year) %>%
  summarise(num = n(),
            num_na = sum(is.na(Food_Supply_Quantity)),
            prop_na = num_na / num)


## ----fao-pol1, fig.cap='The number of observations, the number of missing values and the proportion of missing values in each year of the data.', out.width='100%', fig.asp=.75, fig.align='center', echo=FALSE----
obs_per_year %>%
  pivot_longer(names_to = "Variable", values_to = "Value",
               cols = c(num, num_na, prop_na)) %>%
  ggplot() +
  geom_point(aes(x = Year, y = Value)) +
  facet_wrap(~ Variable, scales = "free", ncol = 1)


## -------------------------------------------------------------------------------------------------
Belarus_obs_per_year <- fbs_long %>%
  filter(Country == "Belarus") %>%
  group_by(Year) %>%
  summarise(num = n(),
            num_na = sum(is.na(Food_Supply_Quantity)),
            prop_na = num_na / num)


## -------------------------------------------------------------------------------------------------
fbs_long <- filter(fbs_long, Year < 2012)


## -------------------------------------------------------------------------------------------------
country_year_not_all_missing <- fbs_long %>%
  group_by(Country, Year) %>%
  summarise(keep = n() != sum(is.na(Food_Supply_Quantity))) %>%
  filter(keep) %>%
  select(-keep) %>%
  ungroup()


## -------------------------------------------------------------------------------------------------
all_cases <- expand.grid(Country = unique(pull(fbs_long, Country)),
                         Year = unique(pull(fbs_long, Year)),
                         Item = unique(pull(fbs_long, Item))) 


## -------------------------------------------------------------------------------------------------
temp2 <- left_join(country_year_not_all_missing, all_cases)


## -------------------------------------------------------------------------------------------------
all_cases <- expand.grid(Country = unique(pull(fbs_long, Country)),
                         Year = unique(pull(fbs_long, Year)),
                         Item = unique(pull(fbs_long, Item)),
                         stringsAsFactors = FALSE)
temp2 <- left_join(country_year_not_all_missing, all_cases)


## -------------------------------------------------------------------------------------------------
temp3 <- left_join(temp2, fbs_long)


## -------------------------------------------------------------------------------------------------
fbs_final <- temp3 %>%
  mutate(Food_Supply_Quantity = ifelse(!is.na(Food_Supply_Quantity),
                                       Food_Supply_Quantity,
                                       0))


## -------------------------------------------------------------------------------------------------
items_per_country_per_year <- fbs_final %>%
  group_by(Country, Year) %>%
  summarise(num = length(unique(Item)))


## -------------------------------------------------------------------------------------------------
countries_per_year <- fbs_final %>%
  group_by(Year) %>%
  summarise(num = length(unique(Country)))


## -------------------------------------------------------------------------------------------------
countries_per_year %>%
  summarise(total = sum(num) * 114)


## -------------------------------------------------------------------------------------------------
fbs_div <- fbs_final %>%
  group_by(Country, Year) %>%
  summarise(richness = sum(Food_Supply_Quantity>0),
            diversity = diversity(Food_Supply_Quantity,
                                        index = "shannon"))


## -------------------------------------------------------------------------------------------------
write_rds(fbs_div, "data/fbs_div.RDS")


## ----message=FALSE, echo=FALSE, warning=FALSE-----------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
#library(skimr)
library(tidyr)
library(vegan)
library(ggbeeswarm)
#library(ggridges)


## -------------------------------------------------------------------------------------------------
rm(list=ls()) # clear R
pol <- read_rds("data/pol.rds")
fbs_div <- read_rds("data/fbs_div.RDS")


## -------------------------------------------------------------------------------------------------
fbs_div %>% filter(str_detect(Country, "United")) %>%
  pull(Country) %>%
  unique()


## -------------------------------------------------------------------------------------------------
pol %>% filter(str_detect(Country, "United")) %>%
  pull(Country) %>%
  unique()


## -------------------------------------------------------------------------------------------------
pol %>% filter(str_detect(Country, "Tanz")) %>%
  pull(Country) %>%
  unique()

## -------------------------------------------------------------------------------------------------
fbs_div %>% filter(str_detect(Country, "Tanz")) %>%
  pull(Country) %>%
  unique()


## ----message = FALSE------------------------------------------------------------------------------
fbs_temp <- read_csv("data/FoodBalanceSheets_E_All_Data.csv",
               locale = locale(encoding = 'ISO-8859-1'))
fbs_country <- fbs_temp %>%
  select(Country, Country_Code='Country Code') %>%
  unique() %>%
  filter(Country_Code < 5000)


## -------------------------------------------------------------------------------------------------
fbs_div <- left_join(fbs_div, fbs_country)


## -------------------------------------------------------------------------------------------------
FAO_country_standards <- read_csv("data/FAOSTAT_data_country_name_standards.csv", na = "")
names(FAO_country_standards) <- str_replace_all(names(FAO_country_standards), c(" " = "_"))
FAO_country_standards <- FAO_country_standards %>%
  rename(Country_name_FAO = Country) %>%
  filter(Country_Code < 5000)


## -------------------------------------------------------------------------------------------------
FAO_country_standards %>% filter(str_detect(Country_name_FAO, "Swaz"))


## -------------------------------------------------------------------------------------------------
fbs_div %>% filter(str_detect(Country, "Swaz")) %>%
  pull(Country) %>%
  unique()


## -------------------------------------------------------------------------------------------------
FAO_country_standards %>% filter(str_detect(Country_name_FAO, "Esw"))


## -------------------------------------------------------------------------------------------------
fbs_div_stand <- left_join(fbs_div, FAO_country_standards) %>%
  select(-Start_Year, -End_Year)


## ----eval = FALSE, echo = FALSE-------------------------------------------------------------------
## pol_countries <- pol %>%
##   select(Country) %>%
##   unique()
## write_csv(pol_countries ,"data/pol_countries.csv")


## ----message = FALSE------------------------------------------------------------------------------
pol_iso_mapping <- read_csv("data/pol_iso_mapping.csv",
                            na = ".") %>%
  rename(Country = Country_pol)


## **Be aware** You may have noticed that in the `read_csv` just above we wrote `na = "."`. This is because when we created the spreadsheet of mapping between Polity and ISO country names we used "." and not "NA" as the missing value indicator. Can you guess why we did not use "NA"? The reason is that "NA" is the ISO2 code for Namibia. If we had used NA for missing value then Namibia would have been given a missing value for the ISO2 code. The general message here is to be careful when a variable contains entries that might include "NA" as a real value.


## ----eval = FALSE, echo = FALSE-------------------------------------------------------------------
## setdiff(pull(pol_iso_mapping, Country),
##         pull(pol, Country))
## setdiff(pull(pol, Country),
##         pull(pol_iso_mapping, Country))
## # all match


## -------------------------------------------------------------------------------------------------
pol_stand <- left_join(pol, pol_iso_mapping)



## -------------------------------------------------------------------------------------------------
fbs_pol <- inner_join(fbs_div_stand,
                     pol_stand,
                     by = c("ISO2_Code" = "ISO2_Code",
                            "Year" = "Year")) %>%
  select(Country = Country.x,
         Year,
         Richness = richness,
         Diversity = diversity,
         Polity2,
         Official_name_en = official_name_en,
         M49_Code = M49_Code.x,
         ISO2_Code,
         ISO3_Code = ISO3_Code.x) %>%
  ungroup()


## ----eval=FALSE-----------------------------------------------------------------------------------
## saveRDS(fbs_pol, "data/fbs_pol.Rdata")


## ----echo = FALSE---------------------------------------------------------------------------------
rm(list=ls())


## ----message=FALSE, echo=TRUE, warning=FALSE------------------------------------------------------
# Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(vegan)
library(ggbeeswarm)

# load dataset
fbs_pol <- readRDS("data/fbs_pol.Rdata")


## ----fbs-pol2, fig.cap='Frequency distribution of richness of food items across all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
  ggplot() +
  geom_histogram(aes(x = Richness), bins = 10)


## ----fbs-pol3, fig.cap='Frequency distribution of diversity of food items across all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
  ggplot() +
  geom_histogram(aes(x = Diversity), bins = 10)


## ----fbs-pol3x, fig.cap='Frequency distribution of Polity2 all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
  ggplot() +
  geom_histogram(aes(x = Polity2), bins = 10)


## ----fbs-pol3x1, fig.cap='Frequency distribution of Polity2 all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
  select(Richness, Diversity, Polity2) %>%
  pivot_longer(names_to = "Variable", values_to = "Value",
               col = 1:3) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = Value), bins = 10) +
  facet_wrap( ~ Variable, nrow = 3, scales = "free", )



## ----fbs-pol4, fig.cap='Relationship between the two response variables, richness of items and diversity of items, across all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
ggplot() +
  geom_point(aes(x = Richness, y = Diversity),
             alpha = 0.1)


## ----fbs-pol4-1, fig.cap='Relationship between the two response variables, richness of items and diversity of items, across all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
ggplot() +
  geom_hex(aes(x = Richness, y = Diversity))


## ----fbs-pol5, fig.cap='Richness of food items available versus Polity score, across all countries and all available years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
ggplot() +
  geom_point(aes(x = Polity2, y = Richness),
             alpha = 0.05)


## ----fbs-pol6, fig.cap='Diversit of food items available versus Polity score, across all countries and all available years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol %>%
ggplot() +
  geom_point(aes(x = Polity2, y = Diversity),
             alpha = 0.05)


## -------------------------------------------------------------------------------------------------
fbs_pol_year <- fbs_pol %>%
  group_by(Country) %>%
  summarise(Ave_Polity2 = mean(Polity2),
            Ave_Richness = mean(Richness),
            Ave_Diversity = mean(Diversity))


## ----fbs-pol7, fig.cap='Richness of food items available versus Polity score; average value for each country.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol_year %>%
  ggplot() +
  geom_point(aes(x = Ave_Polity2, y = Ave_Richness))


## ----fbs-pol8, fig.cap='Diversity of food items available versus Polity score; average value for each country.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol_year %>%
  ggplot() +
  geom_point(aes(x = Ave_Polity2, y = Ave_Diversity))


## -------------------------------------------------------------------------------------------------
fbs_pol_year <- fbs_pol_year %>%
  mutate(Ave_Polity2_cat = cut(Ave_Polity2,
                           breaks = seq(-11, 11, length = 6),
                           labels = c("Very autocratic",
                                      "Quite autocratic",
                                      "Neutral",
                                      "Quite democratic",
                                      "Very democratic")))


## ----fbs-pol9, fig.cap='Frequency distribution of diversity of food items across all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol_year %>%
  ggplot() +
  geom_boxplot(aes(x = Ave_Polity2_cat, y = Ave_Richness)) +
  geom_beeswarm(aes(x = Ave_Polity2_cat, y = Ave_Richness)) +
  ylab("Dietary richness\n[number of food items]") +
  xlab("Type of political system") +
  coord_flip()


## ----fbs-pol10, fig.cap='Frequency distribution of diversity of food items across all countries and years.', out.width='75%', fig.asp=.75, fig.align='center'----
fbs_pol_year %>%
  ggplot() +
  geom_boxplot(aes(x = Ave_Polity2_cat, y = Ave_Diversity)) +
  geom_beeswarm(aes(x = Ave_Polity2_cat, y = Ave_Diversity)) +
  ylab("Dietary diversity\n[Shannon diversity of food items]") +
  xlab("Type of political system") +
  coord_flip()


## ----eval=FALSE, echo=FALSE-----------------------------------------------------------------------
## fbs_pol_year %>% group_by(Ave_Polity2_cat) %>%
##   summarise(num_countries = n())

