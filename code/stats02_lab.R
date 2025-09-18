library(tidyverse)
library(patchwork)

# full dataset
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

# Subsets of 50 and 100 ---------------------------------------------------


#true mean and variance
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2)/nrow(df_h0)


# 100 subsets of 50
mu50_i <- var50_i <- NULL

for (i in 1:100) {
  df50_i <- df_h0 %>% 
    sample_n(size=50)
  mu50_i[i] <- mean(df50_i$height)
  var50_i[i] <- var(df50_i$height)
}

# 100 subsets of 100
mu100_i <- var100_i <- NULL

for (i in 1:100) {
  df100_i <- df_h0 %>% 
    sample_n(size=100)
  mu100_i[i] <- mean(df100_i$height)
  var100_i[i] <- var(df100_i$height)
}


# Histograms based on subsets of 50
df_sample_50 <- tibble(mu50_hat = mu50_i,
                       var50_hat = var50_i)


g_mu50 <- df_sample_50 %>% 
  ggplot(aes(x = mu50_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits=(c(18,22))) +
  labs(x = "Means of unbiased samples of 50")

g_var50 <- df_sample_50 %>% 
  ggplot(aes(x=var50_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits=(c(18,36))) +
  labs(x = "Variances of unbiased samples of 50")

g_mu50/g_var50

# Histograms based on subsets of 100
df_sample_100 <- tibble(mu100_hat = mu100_i,
                       var100_hat = var100_i)


g_mu100 <- df_sample_100 %>% 
  ggplot(aes(x = mu100_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits=(c(18,22))) +
  labs(x = "Means of unbiased samples of 100")


g_var100 <- df_sample_100 %>% 
  ggplot(aes(x=var100_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits=(c(18,36))) +
  labs(x = "Variances of unbiased samples of 100")


# Visual comparison of smaller and larger sample sizes
(g_mu100|g_var100)/(g_mu50|g_var50) +
  plot_annotation(title="Comparison of large and small sample sizes",
         caption="Vertical lines show true population parameters")






# Excluding plants under 10 cm in height ----------------------------------


df_h10 <- df_h0 %>% 
  filter(height >=10)

#true mean and variance (including plants under 10 cm)
mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2)/nrow(df_h0)


# 100 subsets of 50
mu50b_i <- var50b_i <- NULL

for (i in 1:100) {
  df50b_i <- df_h10 %>% 
    sample_n(size=50)
  mu50b_i[i] <- mean(df50b_i$height)
  var50b_i[i] <- var(df50b_i$height)
}

# 100 subsets of 100
mu100b_i <- var100b_i <- NULL

for (i in 1:100) {
  df100b_i <- df_h10 %>% 
    sample_n(size=100)
  mu100b_i[i] <- mean(df100b_i$height)
  var100b_i[i] <- var(df100b_i$height)
}


# Histograms based on subsets of 50
df_sample_50b <- tibble(mu50b_hat = mu50b_i,
                       var50b_hat = var50b_i)


g_mu50b <- df_sample_50b %>% 
  ggplot(aes(x = mu50b_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits=(c(18,22))) +
  labs(x = "Means of biased samples of 50")

g_var50b <- df_sample_50b %>% 
  ggplot(aes(x=var50b_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits=(c(18,36))) +
  labs(x = "Variances of biased samples of 50")

g_mu50b/g_var50b

# Histograms based on subsets of 100
df_sample_100b <- tibble(mu100b_hat = mu100b_i,
                        var100b_hat = var100b_i)


g_mu100b <- df_sample_100b %>% 
  ggplot(aes(x = mu100b_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits=(c(18,22))) +
  labs(x = "Means of biased samples of 100")

g_var100b <- df_sample_100b %>% 
  ggplot(aes(x=var100b_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits=(c(18,36))) +
  labs(x = "Variances of biased samples of 100")


g_mu100b/g_var100b


# Visual comparison of biased vs unbiased data 
#(assuming the original dataset is unbiased!)
(g_mu100b | g_var100b)/(g_mu100 | g_var100) +
  plot_annotation(title="Comparison of biased versus unbiased data",
                  caption="Vertical lines show true population parameters")