library(tidyverse)
#Probability distributions

# Distribution of plant heights
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

df_h0 %>% 
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_vline(aes(xintercept = mean(height)))



# Normal Distribution -----------------------------------------------------
#Normal distribution with continuous data (symmetric around mean)

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)


mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x,mean = mu, sd = sigma)

tibble (y = pd, x = x) %>% # data frame
  ggplot(aes(x=x,y=y)) +
  geom_line() + # continuous line between data points
  labs(y = "Probability density",
       x = "Plant height")

# finding the probability between 10 and 20 with pnorm
p10 <- pnorm(q=10, mean = mu, sd = sigma)
p20 <- pnorm(q=20, mean = mu, sd = sigma)
p20_10 <- p20-p10

# repeating the above to get probabilities in bins of 1 cm width
x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

#Overlaying real data and distribution
df_h0 %>% 
  ggplot(aes(x=height)) +
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_point(data = df_prob, #changes source data frame
             aes(y=freq,
                 x=bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")


# Poisson distribution ----------------------------------------------------


# Poisson distribution with a discrete variable
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

df_count %>% 
  ggplot(aes(x=count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0)

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

# getting probability
df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count))

# overlaying Poisson distribution on real data
df_count %>% 
  ggplot(aes(x=count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob,
            aes(x=x,
                y=freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x=x,
                 y=freq))