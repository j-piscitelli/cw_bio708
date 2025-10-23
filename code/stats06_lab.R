library(tidyverse)

# Exercise 1: regression analysis of iris dataset by species
# Relationship of sepal width to petal width

# species: setosa
df_setosa <- iris %>% 
  filter(Species == "setosa")

summary(lm(Sepal.Width ~ Petal.Width,
           data = df_setosa))

# species: versicolor
df_versicolor <- iris %>% 
  filter(Species == "versicolor")

summary(lm(Sepal.Width ~ Petal.Width,
           data = df_versicolor))

# species: virginica
df_virginica <- iris %>% 
  filter(Species == "virginica")

summary(lm(Sepal.Width ~ Petal.Width,
           data = df_virginica))


# Exercise 2: multiple explanatory variables:
# Example: sepal width by petal length and petal width in the species setosa

summary(lm(Sepal.Width ~ Petal.Width + Petal.Length,
           data = df_setosa))

# Comparison to single explanatory variable:
# regression estimates of Petal.Width:
    # the estimate for Petal.Width is lower with two explanatory variables
# coefficient of determination:
    # the coefficient of determination is higher with two explanatory variables


# Exercise 3: effect of random normally-distributed data
# as an explanatory variable

v_x <- rnorm(nrow(iris), mean = 0, sd = 1) # random data in normal distribution

iris_x <- iris %>% 
  mutate(x = v_x) # add x column to iris dataframe

df_setosa_x <- iris_x %>% 
  filter(Species == "setosa") # add x column to df_setosa

summary(lm(Sepal.Width ~ Petal.Width + x,
           data = df_setosa_x)) # regression with x as explanatory variable

ggplot() +
  geom_point(data = df_setosa_x, aes(x=x,
                   y=Sepal.Width))

# Comparison:
    # most of the time, x very modestly increases R^2
    # but on the first run, R^2 more than tripled with x included
    # and x had a p value <0.01!
    # Lesson:
    # I should buy a lottery ticket
