pacman::p_load(tidyverse,
               here,
               patchwork)


set.seed(1)

# Least Squares method with Normal distribution ---------------------------


# hypothetical sample size
n <- 100

# true parameters
b <- c(0.1,0.5)

# simulated predictor
x1 <- rnorm(n = n, mean = 0, sd = 1)

# design matrix
X <- model.matrix(~ x1)

#get simulated y
y_hat <- X %*% b # %*% is the matrix multiplication operator

plot(y_hat ~ x1)

# adding error in simulated data
y <- rnorm (n = n, mean = y_hat, sd = 0.5)

df0 <- tibble(y = y,
             x1 = x1)

ggplot(df0,
       aes(y = y,
           x = x1)) +
  geom_point()


# fit model to simulated data
lm(y ~ x1,
   data = df0)


# Maximum Likelihood method with other distributions ----------------------

# dpois() calculates likelihood of getting a given data point value
# with a given lambda

dpois(3, lambda = 3.5) # probability is higher near lambda

dpois(1, lambda = 3.5) # and lower further from it

# compared different lambdas
lambda <- seq(0, 10, by = 0.1)
pr <- dpois(3, lambda = lambda)

df_pois <- tibble(y = 3,
       lambda = lambda,
       pr = pr)

# maximum probability of observing 3, which is our only data point,
# is the x value where y is maximized in this plot
df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_point() +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(y = 3")

# we can also see this by arranging df_pois
df_pois %>% 
  arrange(desc(pr))

# What about multiple data points?
# We can find the probability of each data point at any lambda,
# and then multiply those probabilities together to get the
# overall probability of the dataset appearing with that lambda

pr <- dpois(c(3, 2, 5), lambda = 3)
prod(pr)

y <- c(3, 2, 5)

lambda <- seq(0, 10, by = .01)

# sapply repeats the task in FUN
# each element in X will be sequentially substituted in "z"

pr <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))

df_pois <- tibble(lambda = lambda,
                  pr = pr)

# highest lambda values around 3.33--the mean of 3, 2, and 5
df_pois %>% 
  arrange(desc(pr))

mean(c(3,2,5))


# logLik() function applied to model
df_count <- read_csv(here("data_raw/data_garden_count.csv"))

m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

logLik(m_pois)
exp(logLik(m_pois))
