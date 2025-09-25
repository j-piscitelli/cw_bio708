library(tidyverse)
library(patchwork)

# Normal distribution -----------------------------------------------------
# create vector and df of 50 random samples
v_norm <- rnorm(50, mean = 10, sd = 5)
df_norm <- tibble(width=v_norm)

# create idealized probability distribution
x <- seq(min(v_norm), max(v_norm), length = 100) #equally spaced vector covering
                                                #the range of the random data
mu <- mean(v_norm) #mean of the random data
sigma <- sd(v_norm) #standard deviation of the random data
pd <- dnorm(x, mu, sigma) #probability distribution based on the above quantities

# calculate the probability in discrete bins to compare graphically with random data
x_min <- floor(min(v_norm)) #lower end of range, as integer
x_max <- ceiling(max(v_norm)) #upper end of range, as integer
bin <- seq(x_min, x_max, by = 1) # create bin boundaries

p <- NULL # create object to contain vector of probabilities corresponding to bins
for (i in 1:(length(bin)- 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd =sigma) - pnorm(bin[i], mean = mu, sd = sigma)
} # loop over each bin, adding the cumulative probability within its bounds to
  # the corresponding term of vector p

# scale probability distribution to the data
df_prob <- tibble(p, bin = bin[-length(bin)] +  0.5) %>% 
  mutate (freq = p * length(v_norm))

# plot random samples overlaid by probability distribution
df_norm %>% 
  ggplot(aes(x=width)) +
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(y=freq,
                 x=bin),
             color = "darkgreen") +
  geom_line(data = df_prob,
            aes(y=freq,
                x=bin),
            color = "darkgreen")


# Poisson distribution ----------------------------------------------------
# create vector and df of 1000 random samples

v_pois <- rpois(1000, lambda = 10)

df_pois <- tibble(height = v_pois)

# create probability mass as df
x2 <- seq(0,max(v_pois)+5, by = 1) # equally spaced vector covering range of random sample

lambda_hat <- mean(v_pois) # lambda of random data
pm <- dpois(x2, lambda = lambda_hat) # probability mass over the range of the data
                                      # based on given lambda_hat

df_prob2 <- tibble(x2=x2,y=pm) %>%  # scale probability to data
  mutate(freq = y * length(v_pois))

# plot random samples overlaid by probability distribution
df_pois %>% 
  ggplot(aes(x=height))+
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data=df_prob2,
            aes(x=x2,
                y=freq),
            linetype = "dashed",
            color = "darkgreen") +
  geom_point(data = df_prob2,
             aes(x=x2,
                 y=freq),
             color="darkgreen")
