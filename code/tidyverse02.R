library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# Group Operation ---------------------------------------------------------

## group_by() with summarize()
df_m_sd<-iris_sub %>% 
  group_by(Species) %>%
  summarize(mu_sl=mean(Sepal.Length),(sd=sd(Sepal.Length)))

## group_by() with mutate()
df_eps<-iris_sub %>% 
  group_by(Species) %>% 
  mutate(mean_sl=mean(Sepal.Length)) %>% 
  ungroup() %>% 
  mutate(eps=abs(Sepal.Length - mean_sl))

# Reshape -----------------------------------------------------------------

iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species)