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

## To wide format
iris_w<-iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

## To long format
iris_l<-iris_w %>% 
  pivot_longer(cols=c("setosa","versicolor","virginica"),
               names_to="Species",
               values_to="Sepal.Length")


# Join --------------------------------------------------------------------

# matching by a single column
# left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

df1_2<-left_join(x = df1,
          y = df2,
          by = "Species")


# What if the joining column isn't the same between the data frames?

df2_minus_B<-tibble(Species= c("A","C"), y=c(4,6))

# Where joining df is missing a value,
#   NA is imputed for the value of the missing row
df1_2minusB<-left_join(x = df1,
                 y = df2_minus_B,
                 by = "Species")

# Where baseline df is missing a value, the missing row is just lost
df2minusB_1<-left_join(x=df2_minus_B,
          y=df1,
          by="Species")
