pacman::p_load(tidyverse,
               here,
               patchwork)

set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- X %*% b

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()

# correct model used to generate the data
m1 <- lm(y ~ x1, data = df0)
summary(m1)

df0 <- df0 %>% 
  mutate(x2 = rnorm(nrow(.)))

df0 %>% 
  ggplot(aes(y = y,
             x = x2)) + 
  geom_point()

m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)

summary(m1)
summary(m2)
m3 <- lm(y ~ x2, data = df0)
summary(m3)

# Log likelihood
logLik(m1)
logLik(m2)

# Deviance
dev_m1 <- -2 * logLik(m1)
dev_m2 <- -2 * logLik(m2)

log_lr <- dev_m1 - dev_m2

anova(m1, m2, test = "Chisq")

# AIC
AIC(m1)
AIC(m2)
AIC(m3)

# Multi-model inference with AIC
# install.packages("MuMIn")
library(MuMIn)

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

# Full model
m_full <- glm(n_sp ~ distance + cat_area + hull_area,
    data = df_fish,
    family = "poisson")

# magic spell for dredge()
options(na.action = "na.fail")
# dredge
mst <- dredge(m_full,
       rank = "AIC")
