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


# ggplot ------------------------------------------------------------------

# without pipe
g_example <- ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width)) +
  geom_point()

# with pipe
g_example <- iris %>% 
  ggplot (mapping = aes(x = Sepal.Length,
                        y =Sepal.Width)) +
  geom_point()

# color

(g_example <- iris %>% 
  ggplot (mapping = aes(x = Sepal.Length,
                        y =Sepal.Width,
                        color = Species)) +
  geom_point())

# 'color' for coloring by some variable goes INSIDE aes(). Below is wrong
(g_example <- iris %>% 
    ggplot (mapping = aes(x = Sepal.Length,
                          y =Sepal.Width,),
            color= Species) +
    geom_point())
# 'color' in geom_* function changes overall color
(g_example <- iris %>% 
    ggplot (mapping = aes(x = Sepal.Length,
                          y =Sepal.Width,)) +
    geom_point(color="salmon"))

## Line plot
# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# basic plot
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line()

## Histogram
iris %>% 
  ggplot(aes(x=Sepal.Length)) +
  geom_histogram()

# colored by species (just outlines!)
colored_histogram <- iris %>% 
  ggplot(aes(x=Sepal.Length,color=Species)) +
  geom_histogram()
colored_histogram

# filled by species
colored_histogram <- iris %>% 
  ggplot(aes(x=Sepal.Length,fill=Species)) +
  geom_histogram()
colored_histogram


## Box plot (for one categorical and one continuous variable)
iris %>% 
  ggplot(aes(y=Sepal.Length, x=Species))+
  geom_boxplot()

#filled by species
iris %>% 
  ggplot(aes(y=Sepal.Length, x=Species, fill=Species))+
  geom_boxplot()

# use multiple layers

#example 1
iris %>% 
  ggplot(aes(y=Sepal.Length, x=Species, fill=Species))+
  geom_boxplot() +
  geom_point()

#example 2 (with jitter)
iris %>% 
  ggplot(aes(y=Sepal.Length, x=Species, fill=Species))+
  geom_boxplot() +
  geom_jitter(alpha=0.5)
