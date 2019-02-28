#Replication Task
##attempting to replicate Will's Dataset

#w: load libraries
library(tidyverse)
library(zoo)

#reading, cleaning, filling na cells
rep_dataset <- read_csv("data/vet_participation.csv",
                        skip = 10) %>%
  filter(!is.na(X6)) %>%
  na.locf()

#removing unnecessary variables
rep_dataset <- select(rep_dataset, -"IRSAD Deciles at SA1 Level (Area)", -"RA (UR)")

#renaming variables more intuitively
rep_dataset <- rep_dataset %>%
  rename(age = "AGE5P - Age in Five Year Groups",
         sex = "SEXP Sex",
         institution = "TYPP Type of Educational Institution Attending",
         n = X6)

#identifying unique institutions, defining vectors for (school, university, etc)
(institution_unique <- rep_dataset %>%
    pull(institution) %>% 
    unique())
(university <- institution_unique[9])
(school <- institution_unique[1:7])
(vet <- institution_unique[8])
(other_na <- institution_unique[10:12])

#creating inst_group variable 
rep_dataset <- rep_dataset %>% 
  mutate(inst_group = case_when(
    institution %in% school     ~ "school",
    institution %in% vet        ~ "vet",
    institution %in% university ~ "university",
    institution %in% other_na   ~ "other")
  )
#removing unnecessary column ~ institution 
rep_dataset <- select(rep_dataset, -institution)

#summarising data
rep_dataset <- rep_dataset %>%
  filter(age == "15-19 years" | age == "20-24 years") %>%
  group_by(sex, age, inst_group) %>%
  summarise(n = sum(n))

#view
view(rep_dataset)

