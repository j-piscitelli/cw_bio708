library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# Refresher ---------------------------------------------------------------

# Exercise 1
# Filter iris_sub to those with Sepal.Length greater than 5
# and assign to 'df_g5'

df_g5 <- filter(iris_sub, Sepal.Length>5)
df_g5

# Exercise 2
# Select columns of Sepal.Length and Petal.Width from iris_sub
# and assign to 'df_sp'

df_sp <- select(iris_sub, c("Sepal.Length", "Petal.Width"))
df_sp

# Exercise 3
# Arrange rows by Petal.Width in iris_sub
# and assign to 'df_arrange'

df_arrange <- arrange(iris_sub, Petal.Width)
df_arrange

# Exercise 4
# Do exercises 1-3 at once with pipes
# and assign to 'df_master'

df_master <- iris_sub %>% 
  filter(Sepal.Length>5) %>% 
  select(c("Sepal.Length", "Petal.Width")) %>% 
  arrange (Petal.Width)
df_master

# Extra
# Calculate mean Petal.Width for each Species separately
# use group_by() and summarize()

df_means <- iris_sub %>% 
  group_by(Species) %>% 
  summarize(mean_pw = mean(Petal.Width))
df_means
