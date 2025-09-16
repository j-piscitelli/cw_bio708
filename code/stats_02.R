library(tidyverse)

#Sampling

## first dataset
h1 <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h1, # height
                unit = "cm") # unit

# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)


## second dataset
h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


## full dataset
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2)/nrow(df_h0)

# subsample of 10
df_i <- df_h0 %>% 
  sample_n(size = 10)

# for reproducibility
set.seed(3)

mu_i <- var_i <- var_ub_i <- NULL

for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(size=10)
    mu_i[i] <- mean(df_i$height)
    var_i[i] <- sum((df_i$height - mean(df_i$height))^2)/nrow(df_i)
    var_ub_i[i] <- var(df_i$height)
    }

mu_i
var_i
var_ub_i

# histograms
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i, var_ub = var_ub_i)

df_sample

# histograms for mean and variance
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var <- df_sample %>% 
  ggplot(aes(x=var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_var_ub <- df_sample %>% 
  ggplot(aes(x=var_ub)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_var/g_var_ub

#install.packages("here")
