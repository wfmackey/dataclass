#Replication Task 3
##Part C

library(tidyverse)
library(zoo)


```{r hidden cruft, include = FALSE}
theme_set(theme_minimal())
options(scipen=1000000)
```

#reading, cleaning, filling na cells
rep_dataset <- read_csv("data/vet_participation.csv",
                        skip = 10) %>%
  filter(!is.na(X6)) %>%
  na.locf()
#removing unnecessary variables
rep_dataset <- select(rep_dataset,-"RA (UR)")


#renaming variables more intuitively
rep_dataset <- rep_dataset %>%
  rename(irsad_dec_num = "IRSAD Deciles at SA1 Level (Area)",
         age = "AGE5P - Age in Five Year Groups",
         sex = "SEXP Sex",
         institution = "TYPP Type of Educational Institution Attending",
         n=X6)

#identifying unique institutions, defining vectors for (school, university, etc)
(institution_unique <- rep_dataset %>%
    pull(institution)
  %>% unique())
university <- institution_unique[9]
school <- institution_unique[1:7]
vet <- institution_unique[8]
other_na <- institution_unique[10:12]

#creating inst_group variable 
rep_dataset <- rep_dataset %>% 
  mutate(inst_group = case_when(
    institution %in% school     ~ "school",
    institution %in% vet        ~ "vet",
    institution %in% university ~ "university",
    institution %in% other_na   ~ "other")
  )

#removing unnecessary column ~ institution 
rep_dataset <- select(rep_dataset,-institution)

#summarising data
rep_dataset <- rep_dataset %>%
  filter(age == "15-19 years" | age == "20-24 years") %>%
  filter(inst_group == "university" | inst_group == "vet") %>%
  mutate(irsad = parse_number(irsad_dec_num)) %>%
  group_by(irsad,sex,age, inst_group) %>%
  summarise(n = sum(n))

#plotting chart 
rep_dataset %>%
  ggplot(aes(x = age,
             y = n)) +
  geom_bar(aes(fill = sex, alpha = age), 
           stat = "identity", position = "dodge") + 
  scale_alpha_manual(values = c(0.5,1))+ 
  facet_grid(inst_group ~ irsad, scales = "free_y") + 
  labs(x = "", title = "Number of young people by study type and IRSAD SES decile")+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

#view
#view(rep_dataset)
