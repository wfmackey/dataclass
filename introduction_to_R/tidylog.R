
devtools::install_github("elbersb/tidylog")

library(tidylog)

mtcars %>% 
  filter(vs == 0) %>% 
  select(-carb) %>% 
  mutate(wt2 = 2*wt) %>% 
  group_by(gear, am)
