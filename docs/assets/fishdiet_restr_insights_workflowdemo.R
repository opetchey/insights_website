## ----message=FALSE, echo=TRUE, warning=FALSE------------------------------------------------------
# Load the libraries we use
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(skimr)
library(tidyr)
library(ggbeeswarm)
library(taxize)
library(broom)
library(forcats)


## -------------------------------------------------------------------------------------------------
mortality <- read_csv("data/Moatt_et_al_Data_S1.csv")
courtship <- read_csv("data/Moatt_et_al_Data_S5.csv")
eggs <- read_csv("data/Moatt_et_al_Data_S6.csv")
length_weight_condition <- read_csv("data/Moatt_et_al_Data_S15.csv")


## -------------------------------------------------------------------------------------------------
courtship <- courtship %>%
  select(Fish_ID=FID, Family, Shelf_stack, Diet_comp=Diet,
         Prov_level=Level, Fish_size=Size, Trial, Total_court)
eggs <- eggs %>%
  select(Fish_ID=FID, Family, Shelf_stack=Stack_shelf, Diet_comp=Diet,
         Prov_level=Level, Fish_size=Size, Total_egg)
length_weight_condition <- length_weight_condition %>%
  select(Fish_ID=FID, Shelf_stack, Diet_comp=Diet, Sex=Sex,
         Batch,
         Prov_level=Level, Fish_size=Size, Length=Ln, Weigth=Wt, Cond_index=CI)
mortality <- mortality %>%
  select(Fish_ID=FID, Diet_comp=Diet, Sex,
         Prov_level=Level, Fish_size=Size, Week=Week_F, Status)


## -------------------------------------------------------------------------------------------------
courtship <- courtship %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"))

eggs <- eggs %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"))

length_weight_condition <- length_weight_condition %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"),
         Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female"))

mortality <- mortality %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"),
         Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female"),
         Status = case_when(Status == 0 ~ "alive",
                            Status == 1 ~ "dead"))



## -------------------------------------------------------------------------------------------------
courtship %>% select(Fish_ID) %>%
  duplicated() %>%
  sum()

eggs %>% select(Fish_ID) %>%
  duplicated() %>%
  sum()

length_weight_condition %>% select(Fish_ID, Batch) %>%
  duplicated() %>%
  sum()

mortality %>% select(Fish_ID, Week) %>%
  duplicated() %>%
  sum()



dupl <- mortality %>% select(Fish_ID, Week) %>%
  duplicated()

filter(mortality, dupl)
filter(mortality, Fish_ID=="LR504", Week==2)
filter(mortality, Fish_ID=="LR504")


## -------------------------------------------------------------------------------------------------
# here we check if this is the only fish with fewer records than weeks it was observed in
check <- mortality %>%
  group_by(Fish_ID) %>%
  summarise(check = length(unique(Week))==max(Week))
check %>%
  filter(!check)


## -------------------------------------------------------------------------------------------------
courtship %>%
  filter_all(any_vars(is.na(.))) 


## -------------------------------------------------------------------------------------------------
courtship %>%
  pull(Shelf_stack) %>%
  unique()


## -------------------------------------------------------------------------------------------------
courtship %>%
  summarise(mean_total_court = mean(Total_court, na.rm=T),
            media_total_court = median(Total_court, na.rm=T))


## -------------------------------------------------------------------------------------------------
courtship %>%
  pull(Fish_ID) %>%
  unique() %>%
  length()

## -------------------------------------------------------------------------------------------------
eggs %>%
  pull(Fish_ID) %>%
  unique() %>%
  length()


## -------------------------------------------------------------------------------------------------
skim(eggs)
filter(eggs, is.na(Total_egg))


## -------------------------------------------------------------------------------------------------
length_weight_condition %>%
  pull(Fish_ID) %>%
  unique() %>%
  length()


## -------------------------------------------------------------------------------------------------
mortality %>%
  pull(Fish_ID) %>%
  unique() %>%
  length()


## -------------------------------------------------------------------------------------------------
sum(table(pull(courtship, Fish_ID)) > 1)


## -------------------------------------------------------------------------------------------------
sum(table(pull(eggs, Fish_ID)) != 1)


## -------------------------------------------------------------------------------------------------
sum(table(pull(length_weight_condition, Fish_ID)) != 1)


## -------------------------------------------------------------------------------------------------
sum(table(pull(mortality, Fish_ID)) != 1)


## -------------------------------------------------------------------------------------------------
courtship %>%
  group_by(Diet_comp, Prov_level) %>%
  summarise(count=n(),
            n_unique=length(unique(Fish_ID)))


## -------------------------------------------------------------------------------------------------
eggs %>%
  group_by(Diet_comp, Prov_level) %>%
  summarise(n(),
            n_unique=length(unique(Fish_ID)))


## -------------------------------------------------------------------------------------------------
length_weight_condition %>%
  group_by(Diet_comp, Prov_level) %>%
  summarise(n(),
            n_unique=length(unique(Fish_ID)))


## -------------------------------------------------------------------------------------------------
mortality %>%
  group_by(Diet_comp, Prov_level) %>%
  summarise(n(),
            n_unique=length(unique(Fish_ID)))


## -------------------------------------------------------------------------------------------------
# Time of death
temp_mortality2 <- mortality %>%
  filter(Status=="alive") %>%
  group_by(Fish_ID, Diet_comp, Prov_level, Sex) %>%
  summarise(Lifespan=max(Week))
last_obs <- mortality  %>%
  group_by(Fish_ID, Diet_comp, Prov_level) %>%
  summarise(Last_sample=max(Week))
mortality2 <- full_join(temp_mortality2, last_obs) %>%
  mutate(Censored=ifelse(Lifespan==Last_sample, T, F),
         Lifespan=ifelse(is.na(Lifespan), 0, Lifespan),
         Censored=ifelse(Lifespan==0, FALSE, Censored)) %>%
  ungroup() %>%
  select(-Last_sample)


## -------------------------------------------------------------------------------------------------
# Change in length
change1 <- length_weight_condition %>%
  group_by(Fish_ID, Diet_comp, Prov_level) %>%
  do(m1 = tidy(lm(Length ~ Batch, data=.))) %>%
  unnest() %>%
  filter(term=="Batch") %>%
  select(Fish_ID, Diet_comp, Prov_level, Length_change=estimate)


## -------------------------------------------------------------------------------------------------
# Change in condition
change2 <- length_weight_condition %>%
  group_by(Fish_ID, Diet_comp, Prov_level) %>%
  do(m1 = tidy(lm(Cond_index ~ Batch, data=.))) %>%
  unnest() %>%
  filter(term=="Batch") %>%
  select(Fish_ID, Diet_comp, Prov_level, CI_change=estimate)


## -------------------------------------------------------------------------------------------------
dd <- full_join(courtship, eggs) %>%
  full_join(mortality2) %>%
  full_join(change1) %>%
  full_join(change2)


## -------------------------------------------------------------------------------------------------
diet_comp <- read_csv("data/diet_comp_treatments.csv")
dd <- full_join(dd, diet_comp)


## -------------------------------------------------------------------------------------------------
dd <- mutate(dd,
             Diet_comp = fct_relevel(Diet_comp,
                                            "1.6:1",
                                            "2.5:1",
                                            "4.6:1",
                                            "8.5:1",
                                            "10.2:1"),
             Prov_level = fct_relevel(as.character(Prov_level),
                                            "50",
                                            "75",
                                            "100"))



## -------------------------------------------------------------------------------------------------
dd %>%
  filter(Sex == "Female", !is.na(Total_court))


## -------------------------------------------------------------------------------------------------
dd %>%
  filter(Sex == "Male", !is.na(Total_egg))


## ----message = FALSE, warning = FALSE-------------------------------------------------------------
rm(list=ls())
mortality <- read_csv("data/Moatt_et_al_Data_S1_updated.csv")
courtship <- read_csv("data/Moatt_et_al_Data_S5_updated.csv")
eggs <- read_csv("data/Moatt_et_al_Data_S6_updated.csv")
length_weight_condition <- read_csv("data/Moatt_et_al_Data_S15_updated.csv")
courtship <- courtship %>%
  select(Fish_ID=FID, Family, Shelf_stack, Diet_comp=Diet,
         Prov_level=Level, Fish_size=Size, Trial, Total_court)
eggs <- eggs %>%
  select(Fish_ID=FID, Family, Shelf_stack=Stack_shelf, Diet_comp=Diet,
         Prov_level=Level, Fish_size=Size, Total_egg)
length_weight_condition <- length_weight_condition %>%
  select(Fish_ID=FID, Shelf_stack, Diet_comp=Diet, Sex=Sex,
         Batch,
         Prov_level=Level, Fish_size=Size, Length=Ln, Weigth=Wt, Cond_index=CI)
mortality <- mortality %>%
  select(Fish_ID=FID, Diet_comp=Diet, Sex,
         Prov_level=Level, Fish_size=Size, Week=Week_F, Status)
courtship <- courtship %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"))

eggs <- eggs %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"))

length_weight_condition <- length_weight_condition %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"),
         Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female"))

mortality <- mortality %>%
  mutate(Diet_comp = case_when(Diet_comp == 1 ~ "10.2:1",
                               Diet_comp == 2 ~ "4.6:1",
                               Diet_comp == 3 ~ "2.5:1",
                               Diet_comp == 4 ~ "8.5:1",
                               Diet_comp == 5 ~ "1.6:1"),
         Fish_size = case_when(Fish_size == "S" ~ "Small",
                               Fish_size == "L" ~ "Large"),
         Sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female"),
         Status = case_when(Status == 0 ~ "alive",
                            Status == 1 ~ "dead"))

# Check for the odd duplicate
check <- mortality %>%
  group_by(Fish_ID) %>%
  summarise(check = length(unique(Week))==max(Week))
check %>%
  filter(!check)
# no rows... excellent

# Time of death
temp_mortality2 <- mortality %>%
  filter(Status=="alive") %>%
  group_by(Fish_ID, Diet_comp, Prov_level, Sex) %>%
  summarise(Lifespan=max(Week))
last_obs <- mortality  %>%
  group_by(Fish_ID, Diet_comp, Prov_level) %>%
  summarise(Last_sample=max(Week))
mortality2 <- full_join(temp_mortality2, last_obs) %>%
  mutate(Censored=ifelse(Lifespan==Last_sample, T, F),
         Lifespan=ifelse(is.na(Lifespan), 0, Lifespan),
         Censored=ifelse(Lifespan==0, FALSE, Censored)) %>%
  ungroup() %>%
  select(-Last_sample)


# Change in length
change1 <- length_weight_condition %>%
  group_by(Fish_ID, Diet_comp, Prov_level) %>%
  do(m1 = tidy(lm(Length ~ Batch, data=.))) %>%
  unnest() %>%
  filter(term=="Batch") %>%
  select(Fish_ID, Diet_comp, Prov_level, Length_change=estimate)


# Change in condition
change2 <- length_weight_condition %>%
  group_by(Fish_ID, Diet_comp, Prov_level) %>%
  do(m1 = tidy(lm(Cond_index ~ Batch, data=.))) %>%
  unnest() %>%
  filter(term=="Batch") %>%
  select(Fish_ID, Diet_comp, Prov_level, CI_change=estimate)

dd <- full_join(courtship, eggs) %>%
  full_join(mortality2) %>%
  full_join(change1) %>%
  full_join(change2)

diet_comp <- read_csv("data/diet_comp_treatments.csv")
dd <- full_join(dd, diet_comp)

dd <- mutate(dd,
             Diet_comp = fct_relevel(Diet_comp,
                                     "1.6:1",
                                     "2.5:1",
                                     "4.6:1",
                                     "8.5:1",
                                     "10.2:1"),
             Prov_level = fct_relevel(as.character(Prov_level),
                                      "50",
                                      "75",
                                      "100"))

# Check for fish doing what they shouldn't be..
dd %>%
  filter(Sex == "Female", !is.na(Total_court))
dd %>%
  filter(Sex == "Male", !is.na(Total_egg))




## -------------------------------------------------------------------------------------------------
dd %>%
  ggplot() +
  geom_histogram(aes(x=Total_court))
dd %>%
  ggplot() +
  geom_histogram(aes(x=Total_egg))
dd %>%
  ggplot() +
  geom_histogram(aes(x=Lifespan))
dd %>%
  ggplot() +
  geom_histogram(aes(x=Length_change))
dd %>%
  ggplot() +
  geom_histogram(aes(x=CI_change))


## -------------------------------------------------------------------------------------------------
dd <- dd %>%
  mutate(period=cut(Lifespan, breaks=c(-1, 20, 60, 106, 120)))
dd %>%
  filter(Sex=="Male") %>%
  ggplot() +
  geom_point(aes(x=Lipid, y=Lifespan, col=Prov_level),
             position = position_jitterdodge(jitter.width=0.05)) +
  facet_wrap(~period, nrow=3)


## -------------------------------------------------------------------------------------------------
dd %>%
  filter(Sex=="Male") %>%
  ggplot() +
  geom_point(aes(x=Lipid, y=Lifespan, col=Prov_level),
             position = position_jitterdodge(jitter.width=0.05)) +
  facet_wrap(~period, nrow=3)


## -------------------------------------------------------------------------------------------------
dd %>% 
  filter(Sex=="Female") %>%
  ggplot() +
  geom_point(aes(x=Protein, y=Lifespan, col=Prov_level),
             position = position_jitterdodge(jitter.width=0.05)) +
  facet_wrap(~period, nrow=3)


## -------------------------------------------------------------------------------------------------
dd %>%
  filter(Sex=="Male") %>%
  ggplot() +
  geom_point(aes(x=Protein, y=Total_court, col=Prov_level),
             position = position_jitterdodge(jitter.width=0.05))


## -------------------------------------------------------------------------------------------------
dd %>%
  filter(Sex=="Female") %>%
  ggplot() +
  geom_point(aes(x=Protein, y=Total_egg, col=Prov_level), position = position_jitterdodge(jitter.width=0.2, dodge.width = 5))


