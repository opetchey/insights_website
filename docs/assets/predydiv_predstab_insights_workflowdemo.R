

## ----message=FALSE, echo=TRUE, warning=FALSE------------------------------------------------------
# Load the libraries we use
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(forcats)


## Install the `forcats` library if you have not already done so, otherwise `library(forcats)` will not work. Look at the Packages section of the Getting Acquainted chapter if you need help with installation.


## Make sure you opened RStudio by clicking on your project file for this case study (or switch to the RProject in the top right of the RStudio window. This will ensure RStudio is looking at the correct folder, and that the `relative path` used before the file name will be correct:


## -------------------------------------------------------------------------------------------------
stab <- read_csv("data/dileptus_predator_prey_data.csv")


## -------------------------------------------------------------------------------------------------
stab


## -------------------------------------------------------------------------------------------------
stab <- read_csv("data/dileptus_predator_prey_data.csv", na = ".")


## -------------------------------------------------------------------------------------------------
stab <- stab %>%
  mutate(species = recode(species,
                          "1" = "Colpidium",
                          "2" = "Collodictyon",
                          "3" = "Paramecium",
                          "4" = "Dileptus"))


## -------------------------------------------------------------------------------------------------
comm_comp <- stab %>%
  group_by(prey.composition, replicate) %>%
  summarise(community_composition = paste(unique(species), collapse = ", "),
            prey_richness = length(unique(species)) - 1)


## -------------------------------------------------------------------------------------------------
comm_comp %>%
  select(community_composition)


## -------------------------------------------------------------------------------------------------
prey_comp <- stab %>%
  group_by(prey.composition, replicate) %>%
  filter(species != "Dileptus") %>%
  summarise(prey_composition = paste(unique(species), collapse = ", "))


## -------------------------------------------------------------------------------------------------
stab <- full_join(stab, comm_comp) %>%
  full_join(prey_comp) %>%
  select(-prey.composition, -prey.richness) %>%
  ungroup()
rm(comm_comp, prey_comp)


## It's not necessary to remove variables and datasets that we're not interested in using anymore, but is often a good idea to do so. It can prevent us accidentally using them, or just from getting confused about what they are and if they are in fact important when we come back to our work after a gap of a few months, as often happens. If we have a rather large dataset, keeping only the essential variables can usefully reduce the size of the dataset.


## -------------------------------------------------------------------------------------------------
stab %>%
  select(prey_composition)


## -------------------------------------------------------------------------------------------------
stab %>%
  pull(prey_composition) %>%
  levels()


## -------------------------------------------------------------------------------------------------
stab <- stab %>%
  mutate(prey_composition =
           fct_relevel(prey_composition,
                       "Collodictyon",
                       "Colpidium",
                       "Paramecium",
                       "Colpidium, Collodictyon",
                       "Colpidium, Paramecium",
                       "Collodictyon, Paramecium",
                       "Colpidium, Collodictyon, Paramecium")
  )


## -------------------------------------------------------------------------------------------------
stab %>%
  pull(prey_composition) %>%
  levels()


## Do not worry if it's not immediately apparent why we made this variable an ordered factor---we emphasise the point a couple of times below where the benefits of this are very clear.


## -------------------------------------------------------------------------------------------------
stab <- stab %>%
  mutate(dens_per_ml = count * w2 / w1 / w3)


## Actually, the data entry was done so to make this calculation work very easily. In particular, `w2` is very purposefully the sum of the sampled volume and the diluent, and is *not* just the volume of the diluent. Furthermore, if there was no dilution, and the count was made of `w1` we might have entered `NA` for `w2` and `w3` since they did not exist. Instead, the both `w2` and `w3` are set to 1 in this case, which means they have no effect in the calculation (multiplication and division by 1 has no effect). This is an example of designing and planning before and during data entry with downstream calculations in mind.


## -------------------------------------------------------------------------------------------------
temp1 <- stab %>%
  filter(!is.na(count))
nrow(temp1)


## -------------------------------------------------------------------------------------------------
temp2 <- stab %>%
  na.omit()
nrow(temp2)


## -------------------------------------------------------------------------------------------------
stab <- stab %>%
  na.omit()


## -------------------------------------------------------------------------------------------------
stab %>%
  pull(day) %>%
  unique()


## -------------------------------------------------------------------------------------------------
temp1 <-stab %>%
  group_by(prey_composition, jar) %>%
  summarise(num_samples = n(),
            max_day = max(day))
temp1


## Notice how the dataset is arranged in the same order as the levels of the `prey_composition` variable that we specified when we made this variable an ordered factor type variable. The order is as we planned. If we had not made the `prey_composition` variable into an ordered factor, R would have chosen its preferred arrangement/ordering, and often this is not our preference. You can see R's preferred arrangement if you replace `prey_composition` with `as.character(prey_composition)` in the `group_by`.


## -------------------------------------------------------------------------------------------------
temp1 %>%
  group_by(prey_composition) %>%
  summarise(number_of_replicate = n())


## -------------------------------------------------------------------------------------------------
temp2 <- stab %>%
  group_by(prey_composition, jar, species) %>%
  summarise(num_samples = n(),
            max_day = max(day))
temp2


## All this might seem like overkill, particularly if we imagine that we had collected and entered the data into the spreadsheet. Surely then we would know all this, and therefore not have to check it? It's not overkill, however. It's about making efforts to ensure that we and R are in perfect agreement about the nature and contents of the data. Reaching such agreement is absolutely essential, and often involves resolving differences of opinion by highlighting hidden (and potentially dangerous) assumptions made by us and/or by R about the nature and contents of the data. By taking such efforts we are investing in the foundations of our insights. We need solid foundations for reliable, robust, and accurate insights.


## ----microbe1, fig.cap='Population dynamics observed during the experiment.', out.width='100%', fig.align='center', fig.width=8, fig.height=12----
stab %>%
  ggplot(aes(x = day, y = log10(dens_per_ml), col = species)) +
  geom_point() + 
  geom_line() +
  facet_grid(prey_composition ~ replicate)


## Notice that we gave the aesthetic mappings as an argument to the `ggplot` function (we have mostly been giving the aesthetic mappings to the geoms). We give the mappings to ggplot here because we have two geoms... and we would like both to use the same mappings. And because we don't give them any mappings, they are inherited from the mappings giving to ggplot. This "inheritance is explained and illustrated in more detail in the book.


## Notice the order of the rows of facets in the graph---it is from top to bottom the order we specified when we made the `prey_composition` variable an ordered factor. So `ggplot` is using this order. If you like to see what the order would have been if we hadn't specified it, replace `prey_composition` with `as.character(prey_composition)`.


## -------------------------------------------------------------------------------------------------
pred_max_dens <- stab %>%
  filter(species == "Dileptus") %>%
  group_by(prey_richness, prey_composition, replicate) %>%
  summarise(max_dens = max(dens_per_ml),
            all_zero = sum(dens_per_ml > 0) == 0) 


## In the `group_by` we included a redundant grouping variable `prey_richness`. It is redundant in the sense that we would have got the same resulting tibble if we had not included it, the only difference being that the resulting tibble would not have contained the `prey_richness` variable. We will often find ourselves including redundant variables in a `group_by` because we would like to them to appear in the resulting tibble so we can use them later.


## We calculated the maximum of the predator abundance, but could have chosen the mean, median, or the geometric mean, or harmonic mean. It is really important to recognise that the choice of mean here is quite arbitrary, and so introduces an element of subjectivity into our journey from data to insights. Such subjectivity is a rather common feature of this journey.


## We must make sure our insights are robust to subjective decisions about how to reach them. We can do this by looking at whether the insights are sensitive to changes in these decisions. This is quite different, however, from selecting the decision that gives a result we might desire, which would clearly be a totally inappropriate practice.


## -------------------------------------------------------------------------------------------------
pred_CV <- stab %>%
  filter(species == "Dileptus") %>%
  group_by(prey_richness, prey_composition, replicate) %>%
  summarise(CV = sd(dens_per_ml) / mean(dens_per_ml),
            all_zero = sum(dens_per_ml > 0) == 0)


## *Congratulations* if you noticed that we could have calculated maximum abundance and CV of abundance in the same summarise. This would have been more efficient, in the sense that we would have had less code. It would also have been safer since then we could not have accidentally done something different (e.g. a different grouping) for the two.


## -------------------------------------------------------------------------------------------------
temp_persistence_time <- stab %>%
  filter(species == "Dileptus", dens_per_ml != 0) %>%
  group_by(prey_richness, prey_composition, replicate) %>%
  summarise(last_day_observed = max(day))


## -------------------------------------------------------------------------------------------------
pred_last_day <- stab %>%
  filter(species=="Dileptus") %>%
  group_by(prey_richness, prey_composition, replicate) %>%
  summarise(last_sample=max(day))


## -------------------------------------------------------------------------------------------------
pred_persistence_time <- temp_persistence_time %>%
  full_join(pred_last_day) %>%
  mutate(censored=ifelse(last_day_observed==last_sample, T, F))


## **Censored observations in time to event data** Often we do not have the exact time at which the event of interest occurred. In this experiment we sampled only one every two days. If we observed some individuals on day 14, for example, and then none on or after day 16, we know that extinction occurred somewhere in the time interval between day 14 and day 16. Thus we have what is called "interval censored" time to event data. That is, unless extinction was not observed by the last sample, in which case the data are right censored, and we can only say that extinction (since it is at some point inevitable) will happen sometime after the day of the last sample. Censoring of time to event data is very common and is formally accounted for in statistical analyses of time to event type data. It certainly complicates and adds potential for misinterpreting visualisation of the data, so we must then at least differentiate data points by the type of censoring they have.


## -------------------------------------------------------------------------------------------------
wider_rv <- pred_max_dens %>%
  full_join(pred_CV) %>%
  full_join(pred_persistence_time)


## -------------------------------------------------------------------------------------------------
rv <- wider_rv %>%
  pivot_longer(names_to = "Response_type",
               values_to = "Value",
               cols = c(max_dens, CV, last_day_observed))


## -------------------------------------------------------------------------------------------------
summry_rv <- rv %>%
  group_by(prey_richness, prey_composition, Response_type) %>%
  summarise(mean = mean(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            n = sum(!is.na(Value)))


## We counted the number of non-NA values by asking which of the entries in `Value` were not NA using `!is.na`, which gives a series of TRUE and FALSE values. Then we use `sum` to count the number of TRUEs.


## ----microbe2, fig.cap='Histograms of the three response variables.', out.width='100%', fig.align='center', fig.height=3----
rv %>%
  ggplot() +
  geom_histogram(aes(x = Value), bins = 5) +
  facet_wrap(~ Response_type, scales = "free")


## ----microbe3, echo=FALSE, fig.cap='Histogram of log10 maximum abundance. We thought that log transformation would make the distribution more symmetric, but it has not... it just made it asymmetric in the opposite direction!', out.width='70%', fig.align='center'----
rv %>%
  filter(Response_type == "max_dens") %>%
  ggplot() +
  geom_histogram(aes(x = log10(Value)), bins = 5)


## ----microbe4-0, echo=FALSE, fig.cap='A not so useful graph due to the y-axis label being totally uninformative. Value of what?', out.width='100%', fig.align='center', fig.width=8----
rv %>% filter(Response_type == "max_dens") %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness,
                  y = Value,
                  col = prey_composition),
              width = 0.1, height = 0)


## ----eval=FALSE-----------------------------------------------------------------------------------
## rv %>%
##   filter(Response_type == "max_dens") %>%
##   ggplot() +
##   geom_jitter(aes(x = prey_richness, y = Value, col = prey_composition),
##               width = 0.1, height = 0) +
##   ylab("max_dens")


## -------------------------------------------------------------------------------------------------
rv_oi <- "max_dens"


## ----microbe4-1, echo=TRUE, fig.cap='A reasonably nice graph of maximum predator density in a community against the number of prey available to the predator.', out.width='100%', fig.align='center', fig.width=8----
rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness, y = Value, col = prey_composition),
              width = 0.1, height = 0) +
  ylab(rv_oi)


## **Plot the data points** Note that we are plotting the data points, and not bars with error bars, or even box-and-whisker plots. There are few enough data points in this study that we experience no problems when we plot the data itself, rather than some summary of the data (e.g. the mean plus and minus the standard deviation). As a rule, always plot the data.


## ----microbe4-2, echo=FALSE, fig.cap='This time maximum predator density plotted against prey composition, with prey composition on the y-axis as an easy method for having the tick-labels (prey composition) not overlap.', out.width='100%', fig.align='center', fig.width=8----
rv %>% filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_composition, y = Value),
              width = 0.1, height = 0) +
  coord_flip() +
  ylab(rv_oi)


## ----microbe5-1, echo=TRUE, fig.cap='A quite informative graph of predator population variability (measured as the coefficient of variation--CV) in a community against the number of prey available to the predator.', out.width='100%', fig.align='center', fig.width=8----
rv_oi <- "CV"
rv %>% filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness, y = Value, col = prey_composition),
              width = 0.1, height = 0) +
  ylab(rv_oi)
  


## ----microbe5-2, echo=FALSE, fig.cap='Predator population variability (CV) for each of the prey compositions. It would be nice to know from which jar the very low value for one of the *Collodictyon* replicate comes. We show how to do that next.', out.width='100%', fig.align='center', fig.width=8----
rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_composition, y = Value),
              width = 0.1, height = 0) +
  coord_flip() +
  ylab(rv_oi)


## -------------------------------------------------------------------------------------------------
jar_text <- stab %>%
  select(prey_composition, replicate, jar) %>%
  unique()


## ----microbe1-1, fig.cap='Jar numbers added to the plot of population dynamics observed during the experiment.', out.width='100%', fig.align='center', fig.width=8, fig.height=11----
stab %>%
  ggplot(aes(x = day, y = log10(dens_per_ml), col = species)) +
  geom_point() + 
  geom_line() +
  facet_grid(prey_composition ~ replicate) +
  geom_text(data = jar_text, x = 18, y = 3.5, aes(label = jar, col = NULL))


## -------------------------------------------------------------------------------------------------
pred_CV <- stab %>%
  filter(species == "Dileptus") %>%
  group_by(jar, prey_richness, prey_composition, replicate) %>%
  summarise(CV = sd(dens_per_ml) / mean(dens_per_ml),
            all_zero = sum(dens_per_ml > 0) == 0)


## Install the **ggrepel** add-on package. It moves labels on graphs so they don't lie on top of each other, i.e. they repel each other.


## ----microbe5-3, fig.cap='Again the predator population variability data, but this time with jar numbers added as label by the data points.', out.width='100%', fig.align='center', fig.width=8----
library(ggrepel)
pred_CV %>%
  ggplot(aes(x = prey_composition, y = CV)) +
  geom_jitter(width = 0.1, height = 0) +
  coord_flip() +
  geom_text_repel(aes(label = jar))


## ----microbe5-4, echo=FALSE, fig.cap='The same graph twice, but with the axis flipped between them. Do you think pattern is more apparent in one versus the other?', out.width='100%', fig.align='center', fig.width=8----
p1 <- rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness,
                  y = Value,
                  col = prey_composition),
              width = 0.1, height = 0) +
  coord_flip() +
  ylab(rv_oi) +
  scale_color_discrete(guide = FALSE)
p2 <- rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness,
                  y = Value,
                  col = prey_composition),
              width = 0.1, height = 0) +
  ylab(rv_oi)
cowplot::plot_grid(p1, p2, labels = c("A", "B"), rel_widths = c(1, 2))


## ----microbe7-1, echo=FALSE, fig.cap='A reasonably nice graph of predator population persistence time against the number of prey available to the predator.', out.width='100%', fig.align='center', fig.width=8----
rv_oi <- "last_day_observed"
rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness,
                  y = Value,
                  col = prey_composition,
                  shape = censored),
              width = 0.1, height = 0, size = 3) +
  ylab(rv_oi)


## ----microbe7-2, echo=FALSE, warning=FALSE, fig.cap='Predator population persistence time for each of the prey compositions.', out.width='100%', fig.align='center', fig.width=8----
rv_oi <- "last_day_observed"
rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = prey_composition,
                  y = Value,
                  shape = censored),
              width = 0.1, height = 0, size = 3) +
  coord_flip() +
  ylab(rv_oi)


## Look at the figure below, showing what happens if `prey_composition` is just a character variable and not an ordered factor. The ordering of the prey compositions is not quite as desirable.


## ----microbe6-3, echo=FALSE, warning=FALSE, fig.cap='Notice how the prey compositions are ordered when we let R choose the order. This happens when we fail to specify ourselves what the order should be, by making the prey composition variable an ordered factor type variable.', out.width='100%', fig.align='center', fig.width=8----
rv %>%
  filter(Response_type == rv_oi) %>%
  ggplot() +
  geom_jitter(aes(x = as.character(prey_composition),
                  y = Value,
                  shape = censored),
              width = 0.1, height = 0, size = 3) +
  coord_flip() +
  ylab(rv_oi)


## ----microbe8-1, fig.cap='All three reponse variables efficiently made in one call to ggplot with a facet for each variable, and some fiddling with the legend to get it at the top and not too wide.', out.width='100%', fig.align='center', fig.width=8----
rv %>%
  ggplot() +
  geom_jitter(aes(x = prey_richness,
                  y = Value,
                  col = prey_composition),
              width = 0.1, height = 0) +
  facet_wrap(~ Response_type, scales = "free_y")  +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title.position = "top",
                               nrow = 3,
                               position = "top"))


## ----microbe8-2, eval=FALSE, echo=FALSE, fig.cap='   ', out.width='100%', fig.align='center', fig.width=8----
## rv %>%
##   ggplot() +
##   geom_boxplot(aes(x = prey_composition, y=Value)) +
##   coord_flip() +
##   facet_wrap(~ Response_type, scales = "free_x")

