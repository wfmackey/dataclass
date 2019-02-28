#Replication 2
##trying to replicate chart B
library(tidyverse)
library(zoo)

#reading, cleaning, filling na cells
rep_dataset1 <- read_csv("data/vet_participation.csv",
                        skip = 10) %>%
  filter(!is.na(X6)) %>%
  na.locf()

#removing unnecessary variables
rep_dataset1 <- select(rep_dataset1,-"IRSAD Deciles at SA1 Level (Area)",-"RA (UR)")

#renaming variables more intuitively
rep_dataset1 <- rep_dataset1 %>%
  rename(age = "AGE5P - Age in Five Year Groups",
         sex = "SEXP Sex",
         institution = "TYPP Type of Educational Institution Attending",
         n=X6)

#identifying unique institutions, defining vectors for (school, university, etc)
(institution_unique <- rep_dataset1 %>%
    pull(institution)
  %>% unique())
university <- institution_unique[9]
school <- institution_unique[1:7]
vet <- institution_unique[8]
other_na <- institution_unique[10:12]

#creating inst_group variable 
rep_dataset1 <- rep_dataset1 %>% 
  mutate(inst_group = case_when(
    institution %in% school     ~ "school",
    institution %in% vet        ~ "vet",
    institution %in% university ~ "university",
    institution %in% other_na   ~ "other")
  )

#removing unnecessary column ~ institution 
rep_dataset1 <- select(rep_dataset1, -institution)

#finding number of students per inst_group per sex
rep_dataset1 <- rep_dataset1 %>% 
  filter(age == "15-19 years" | age == "20-24 years") %>%
  group_by(inst_group, sex) %>%
  summarise(n = sum(n))

#plotting by inst_group, by sex
rep_dataset1 %>%
  ggplot(aes(x = sex,
             y = n,
             fill = inst_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()

