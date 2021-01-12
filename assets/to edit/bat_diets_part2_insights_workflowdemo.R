## ----------------------------------------------------------------------------------------------------------
# how many distinct Bats are there...
# there are multiple observations of each bat
# n_distinct deals with this
bats %>%
  summarise(n_distinct(Bat_ID))


## ----------------------------------------------------------------------------------------------------------
# how many distinct Bats are there...
# there are multiple observations of each bat
# n_distinct deals with this
bats %>%
  summarise(num_bat_IDs = n_distinct(Bat_ID))


## ----------------------------------------------------------------------------------------------------------
# create groups by Sex - Male and Female
# ask for distinct observations in each group
bats %>%
  group_by(Sex) %>%
  summarise(num_bat_IDs = n_distinct(Bat_ID))


## ----------------------------------------------------------------------------------------------------------
# create groups by Age - Juvenile and Adult
# ask for distinct observations in each group
bats %>%
  group_by(Age) %>%
  summarise(num_bat_IDs = n_distinct(Bat_ID))


## ----------------------------------------------------------------------------------------------------------
# summary by two grouping variables
bats %>%
  group_by(Age, Sex) %>%
  summarise(num_bat_IDs = n_distinct(Bat_ID))


## You just experienced two of the most amazing R functions: `group_by` and `summarise`. (You also experienced more of the magical pipe `%>%`). These and other functions in the __dplyr__ package have, in the last few years, revolutionised how people do data analysis in R. They make generating summary tables for insight very easy and literal, we think.  Go back and read the code a few times... for example: start with the bat data, pass it to group by to divide it up by sex and age, then use summarise to calculate the numbers of unique bat individuals in each group.

## 
## **Don't worry** if you don't yet feel 100% comfortable in your understanding of how the `summarise` or the `group_by` work. The point of the chapter is for you to experience and see the general workflow. We go explain again and in more detail in Section \@ref(extra-summarise).


## ----bat-nums1, fig.cap="A first insight! Numbers of male and female bats of each age.", out.width='50%', fig.asp=.75, fig.align='center'----
# make summary data, and give it a name to use
bats_Age_Sex <- bats %>%
  group_by(Age, Sex) %>%
  summarise(num_bat_IDs = n_distinct(Bat_ID))

# basic use of ggplot
bats_Age_Sex %>%
ggplot() +
  geom_col(mapping = aes(x=Sex, y=num_bat_IDs, fill=Age))


## ----prey-sizedist-1, fig.cap="Frequency distribution of the size (wingspan) of the prey.", out.width='50%', fig.asp=.75, fig.align='center'----
ggplot(data = bats,
       mapping = aes(x = Wingspan_mm)) +
    geom_histogram(bins = 30)


## ----------------------------------------------------------------------------------------------------------
# generate in an insightful table of calculations
# the mean, median and standard deviation 
# of the wingspan_mm variable
bats %>%
  summarise(mean_wingspan = mean(Wingspan_mm),
            median_wingspan = median(Wingspan_mm),
            sd_wingspan = sd(Wingspan_mm))


## ----------------------------------------------------------------------------------------------------------
# calculate in an insightful table
# the mean, media and standard deviation 
# of the wingspan_mm variable, dealing with NA values!
bats %>%
  summarise(mean_wingspan = mean(Wingspan_mm, na.rm = TRUE),
            median_wingspan = median(Wingspan_mm, na.rm = TRUE),
            sd_wingspan = sd(Wingspan_mm, na.rm = TRUE))


## We just asked you to make an informed guess of the mean and median and write them down. We suggest you make informed guesses like this whenever you can. Comparison of our guess with what R then tells us is a great *check and balance* between our understanding of the data, and the understanding revealed by our R code.


## ---- eval = FALSE-----------------------------------------------------------------------------------------
## library(praise)
## praise()


## ----------------------------------------------------------------------------------------------------------
prey_stats <- bats %>%
  group_by(Bat_ID) %>%
  summarise(num_prey = n())


## ----------------------------------------------------------------------------------------------------------
prey_stats


## ----------------------------------------------------------------------------------------------------------
prey_stats <- bats %>%
  group_by(Bat_ID) %>%
  summarise(num_prey = n(),
            num_prey1 = n_distinct(Sp._Nr.))


## ----------------------------------------------------------------------------------------------------------
prey_stats <- bats %>%
  group_by(Bat_ID, Sex, Age) %>%
  summarise(num_prey = n(),
            num_prey1 = n_distinct(Sp._Nr.))


## ----------------------------------------------------------------------------------------------------------
prey_stats <- bats %>%
  group_by(Bat_ID, Sex, Age) %>%
  summarise(num_prey = n(),
            num_prey1 = n_distinct(Sp._Nr.),
            mean_wingspan = mean(Wingspan_mm, na.rm = TRUE),
            prop_migratory = sum(Migratory == "yes") / n())


## Logical operators, like `==` are operators just like `+` and `-` but their answer is yes or no (hence their name, logical operators). They include `>` (greater than), `>=` (equal to or greater than), `<` (less than), `<=` (less than or equal to), `!=` (not equal to), `&` (and), and `|` (or). All are used like questions asking something about what appears on their left and right side. We'll see more examples throughout the book and then explain them in more detail.


## Note that several times above we assigned the result to the object `prey_stats`. Hence each time we destroyed the previous version of `prey_stats`. This is not just OK, but actually a good idea. Keeping variables in the same dataframe whenever possible, and not creating multiple dataframes when we need only one is good for safety and efficiency.


## From here on in this chapter we work with the `prey_stats` data, so please make sure you fully understand how it was obtained.


## ----bat-shapes1-1, fig.cap="Frequency distribution of number of prey in bat's diets, with number of bins specified.", out.width='50%', fig.asp=.75, fig.align='center'----
prey_stats %>%
  ggplot() +
  geom_histogram(mapping = aes(x = num_prey),
                 bins = 11)


## ----------------------------------------------------------------------------------------------------------
prey_stats %>%
  # we made prey_stats with grouping variables
  # we need to ignore this meta-information to get the mean and median
  ungroup() %>%
  summarise(mean_num_prey = mean(num_prey, na.rm = TRUE),
            median_num_prey = median(num_prey, na.rm = TRUE))


## ----bat-shapes2, fig.cap="Frequency distribution of mean wingspan of prey in bat's diets.", out.width='50%', fig.asp=.75, fig.align='center', warning=FALSE----
prey_stats %>%
  ggplot() +
    geom_histogram(mapping = aes(x = mean_wingspan),
                   bins = 11)


## ----------------------------------------------------------------------------------------------------------
prey_stats %>%
  ungroup() %>%
  summarise(mean_mean_wingspan = mean(mean_wingspan, na.rm = TRUE),
            median_mean_wingspan = median(mean_wingspan, na.rm = TRUE))



## ----bat-shapes3, fig.cap="Frequency distribution of proportion of migratory prey in bat's diets.", out.width='50%', fig.asp=.75, fig.align='center'----
prey_stats %>%
  ggplot() +
  geom_histogram(mapping = aes(x = prop_migratory),
                 bins = 10)


## ----------------------------------------------------------------------------------------------------------
prey_stats %>%
  ungroup() %>%
  summarise(mean_prop_migratory = mean(prop_migratory, na.rm = TRUE),
            median_prop_migratory = median(prop_migratory, na.rm = TRUE))



## In addition to the insights provided by examining at these histograms, we can again check that the data are what we expect. E.g. are there wildly small or large values, or impossible values (e.g. negative numbers of prey or proportions larger than one)? And as mentioned, looking at the shapes of distributions is a very good prelude to doing statistics.


## We won't say a great deal more about theoretical distributions such as the normal, Poisson, and binomial distributions. You would learn much more about these in a statistics course. It's enough now to see the different types of data they describe, and to be happy that you've experienced three of the most important and common theoretical distributions in statistics!


## ----sex-diet-diffs2, fig.cap='Difference in size of preys in the diets of male and female bats.', out.width='50%', fig.asp=.75, fig.align='center', warning=FALSE----
prey_stats %>%
  ggplot() +
  geom_beeswarm(mapping = aes(x = Sex, y = mean_wingspan))


## ----------------------------------------------------------------------------------------------------------
prey_stats %>%
  group_by(Sex) %>%
  summarise(mean(mean_wingspan, na.rm = TRUE))


## ----echo=FALSE--------------------------------------------------------------------------------------------
set.seed(243)


## ----sex-diet-diffs5, fig.cap='Randomised size of preys in the diets of male and female bats.', out.width='50%', fig.asp=.75, fig.align='center', warning=FALSE----
prey_stats %>%
  ggplot() +
  geom_beeswarm(mapping = aes(x = Sex, y = sample(mean_wingspan)))


## ----sex-diet-diffs3, fig.cap='Difference in migratory nature of prey in the diets of male and female bats.', out.width='50%', fig.asp=.75, fig.align='center'----
prey_stats %>%
  ggplot() +
  geom_beeswarm(mapping = aes(x = Sex, y = prop_migratory))


## ----sex-diet-diffs1, fig.cap='Difference in number of prey in the diets of male and female bats.', out.width='50%', fig.asp=.75, fig.align='center'----
prey_stats %>%
  ggplot() +
  geom_beeswarm(mapping = aes(x = Sex, y = num_prey))


## Notice how we choose to plot the data points, and not bar charts or box and whisker plots. We believe it better to plot the data points rather than summaries of them.


## ----eval=FALSE--------------------------------------------------------------------------------------------
## prey_stats %>%
##   ggplot() +
##   geom_beeswarm(mapping = aes(x = Sex, y = mean_wingspan)) +
##   facet_wrap(~ Age)


## ----two-way-size-1, fig.cap='Difference in prey size in the diets of male and female bats of two ages.', out.width='50%', fig.asp=.75, fig.align='center', echo=FALSE, warning=FALSE----
mean_prey_stats <- prey_stats %>%
  na.omit() %>%
  group_by(Sex, Age) %>%
  summarise(sex_mean_wingspan = mean(mean_wingspan, na.rm = TRUE))
prey_stats %>% 
  ggplot() +
  geom_beeswarm(mapping = aes(x = Sex, y = mean_wingspan)) +
  facet_wrap(~ Age) +
  geom_point(data = mean_prey_stats,
             mapping = aes(x = Sex, y = sex_mean_wingspan),
             size = 4,
             col = "blue",
             alpha = 0.5)


## If at all possible, plot the data, and not summaries of the data. I.e. plot the individual data points. This is well aligned with the *Nature* publishing group instructions: "Present data in a format that shows data distribution (dot-plots or box-and-whisker plots). If using bar graphs, overlay the corresponding dot plots. Confirm that all data presentation meets these requirements and that individual data points are shown."


## ----two-way-size-2, fig.cap='Difference in prey size in the diets of male/female adult/juvenile bats.', out.width='50%', fig.asp=.75, fig.align='center', echo=TRUE, warning=FALSE----
prey_stats %>%
  ggplot() +
  geom_beeswarm(mapping = aes(x = Sex, y = mean_wingspan)) +
  facet_wrap(~ Age) +
  ylab("Mean wingspan of prey items\neaten by individual bats")


## Recall when we changed the entries in the `Sex` variable from `M` to `Male` and `F` to `Female`. And also changed the entries in the `Age` variable to `Adult` and `Juvenile`. This was pretty useful since now we have the words on the graph, rather than their abbreviations. Generally speaking, it's a good idea to use the full words in the data rather than codes, and here is one reason why. The graphs are easier to understand, need less explanatory text, and need less "beautification". This is an example of good preparation making things much much easier quite some distance down the workflow.


## ----eval=FALSE--------------------------------------------------------------------------------------------
## mean_prey_stats <- prey_stats %>%
##   na.omit() %>%
##   group_by(Sex, Age) %>%
##   summarise(sex_mean_wingspan = mean(mean_wingspan, na.rm = TRUE))


## ----eval=FALSE--------------------------------------------------------------------------------------------
## prey_stats %>%
## ggplot() +
##   geom_beeswarm(mapping = aes(x = Sex, y = mean_wingspan)) +
##   facet_wrap(~ Age) +
##   geom_point(data = mean_prey_stats,
##              mapping = aes(y = sex_mean_wingspan),
##              size = 4,
##              col = "blue",
##              alpha = 0.5) +
##   ylab("Mean wingspan of prey")


## What if we don't want the grey gridded background or the box around the graph? One way to deal with this is to change the "theme" by adding `+ theme_classic()` for example, to our `ggplot`. Have a go at this. Have a look on the internet for other themes.


## Note that above we calculated the mean of the prey wingspans consumed by female bats by taking the mean of the mean wingspan of prey consumed by each bat. The same for the mean of the male bats. This is really important as it implies we have properly accounted for non-independence in the data (the multiple prey found in one poop are non-independent). More about this is on the companion website (http://insightsfromdata.io).


## ----echo=FALSE, eval=FALSE--------------------------------------------------------------------------------
## # Version with error bars
## mean_prey_stats <- prey_stats %>%
##   na.omit() %>%
##   group_by(Sex, Age) %>%
##   summarise(gm_mean_wingspan = mean(mean_wingspan, na.rm = TRUE),
##             se_mean_wingspan = sd(mean_wingspan) / sqrt(n()))
## ggplot() +
##   geom_beeswarm(data = prey_stats,
##                 mapping=aes(x = Sex, y = mean_wingspan)) +
##   facet_wrap(~ Age) +
##   geom_point(data = mean_prey_stats,
##              mapping = aes(x = Sex, y = gm_mean_wingspan),
##              size = 4,
##              col = "blue",
##              alpha=0.5) +
##   geom_errorbar(data = mean_prey_stats,
##   	            mapping = aes(x = Sex,
##                               ymin = gm_mean_wingspan - se_mean_wingspan * 2,
##                               ymax = gm_mean_wingspan + se_mean_wingspan * 2),
##                 col = "blue", width = 0.2)


## ----------------------------------------------------------------------------------------------------------
bats %>%
  summarise(num_prey = n_distinct(Species))


## ----------------------------------------------------------------------------------------------------------
num_poops <- bats %>%
  group_by(Sex, Species) %>%
  summarise(num_poops = n())


## ----------------------------------------------------------------------------------------------------------
# Get list of all prey
all_prey <- num_poops %>%
  # force dplyr to ignore the grouping variable Sex in num_poops
  # so that we can get just the Species names
  ungroup() %>% 
  select(Species)# 165 values ( = prey species)

# Get the prey species found in male poops:
prey_in_male_poops <- num_poops %>%
  filter(Sex == "Male") %>%
  # force dplyr to ignore the grouping variable Sex in num_poops
  # so that we can get just the Species names
  ungroup() %>% 
  select(Species) # 93 values ( = prey species)

# and in female poops
prey_in_female_poops <- num_poops %>%
  filter(Sex == "Female") %>%
  # force dplyr to ignore the grouping variable Sex in num_poops
  # so that we can get just the Species names
  ungroup() %>% 
  select(Species) # 72 values ( = prey species)

# Get the number of prey species found
# in either or both males and females...
# should be same as number of unique (i.e. 115 values)
prey_in_either_or_both <- union(prey_in_male_poops,
                                prey_in_female_poops) # 115 values

# Get the prey found in both males and females
prey_in_both <- intersect(prey_in_male_poops,
                          prey_in_female_poops) # 50 values

# Get the prey not found in male poops
prey_not_in_male <- setdiff(all_prey, prey_in_male_poops) # 22 values

# and not found in female poops.
prey_not_in_female <- setdiff(all_prey, prey_in_female_poops) # 43 values


## ----------------------------------------------------------------------------------------------------------
num_poops <- bind_rows(num_poops,
                       tibble(Sex = "Female",
                              # this object is already a tibble with a variable called Species
                              prey_not_in_female,
                              num_poops = 0),
                       tibble(Sex = "Male",
                              # this object is already a tibble with a variable called Species
                              prey_not_in_male,
                              num_poops = 0)
)


## ----------------------------------------------------------------------------------------------------------
total_num_poops <- bats %>%
  select(Sex, Bat_ID) %>%
  distinct() %>%
  group_by(Sex) %>%
  summarise(num_bats = n())


## ----------------------------------------------------------------------------------------------------------
bat_props <- full_join(num_poops, total_num_poops)


## __The four joins__ Merging together two datasets is a quite useful and common task. We will do it again in other Workflow Demonstrations, you will probably do it yourself quite a bit. There are four types of join/merge in **dplyr**. We just used `full_join` which keeps all records in each dataset, even if there is no match; if there is no match we will get some NAs in the new row. `inner_join` keeps only the records for which there is a match. `left_join` gives all the records/observations in the first dataset, regardless of match. `right_join` gives all the observations in the second dataset, regardless of match. There are two other joins: `semi_join` and `anti_join` but we won't cover them in this book.


## When joining/merging datasets, always try to figure out what you expect before you do it. How many rows do you expect in the merged dataset and why that many? R will do its best to join them, but is it doing what you want and what you think it should be doing? Make sure you know. Don't trust R to figure such important things out for you.


## ----------------------------------------------------------------------------------------------------------
bat_props <- mutate(bat_props, props = num_poops / num_bats)


## ----freq-in-diet0, fig.cap='Frequencies with which prey species appeared in the diets of bats, way too small to see anything useful.', out.width='50%', fig.asp=.75, fig.align='center'----
bat_props %>%
  ggplot() +
    geom_col(mapping = aes(x = Species, y = props, fill = Sex),
             position = "dodge")


## ----------------------------------------------------------------------------------------------------------
topten_species <- bat_props %>%
  select(Species, Sex, props) %>%
  pivot_wider(names_from = Sex, values_from = props) %>%
  mutate(diff = abs(Female - Male)) %>%
  arrange(desc(diff)) %>%
  top_n(10)


## ----------------------------------------------------------------------------------------------------------
topten_species


## `semi_join` returns all rows from x with a match in y.  You specify the variable `by` which to join.


## ----freq-in-diet1, fig.cap='Frequencies with which prey species appeared in the diets of bats, only for the ten species with greatest difference between male and female bats.', out.width='50%', fig.asp=.75, fig.align='center'----
semi_join(bat_props, topten_species, by = "Species") %>%
  ggplot() +
  geom_col(mapping = aes(x = Species, y = props, fill = Sex),
           position = "dodge") +
  coord_flip() +
  theme_classic(base_size = 7)


## ----------------------------------------------------------------------------------------------------------
odds_ratios <- bat_props %>%
  mutate(not = num_bats - num_poops,
         odds = num_poops / not) %>%
  select(Sex, Species, odds) %>%
  pivot_wider(names_from = Sex, values_from = odds) %>%
  mutate(Odds_ratio = Female / Male)


## ----------------------------------------------------------------------------------------------------------
glimpse(odds_ratios)


## ----bat-odds-ratio0, fig.cap='Odds ratio of male and female bat frequencies of consuming 11 prey species. Odds ratios greater than zero indicate that females are more likely to consume the prey species than male bats (odds ratios smaller than zero indicate that males are more likely to consume the prey species than females).', out.width='50%', fig.asp=.75, fig.align='center'----
semi_join(odds_ratios, topten_species, by = "Species") %>% 
  ggplot() +
  geom_point(mapping = aes(x = Species, y = log2(Odds_ratio))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_classic(base_size = 7)


## ----bat-odds-ratio1, eval=FALSE, echo=FALSE, fig.cap='Histogram of odds ratio for each prey species. Odds ratios greater than zero indicate that females are more likely to consume the prey species than are male bats.', out.width='50%', fig.asp=.75, fig.align='center'----
## odds_ratios %>%
##   ggplot() +
##   geom_histogram(mapping = aes(x=log2(Odds_ratio)),
##                  binwidth = 1)


## ----eval=FALSE--------------------------------------------------------------------------------------------
## install.packages("devtools")
## devtools::install_github("melissanjohnson/pupR")
## library(pupR)
## pupR()

