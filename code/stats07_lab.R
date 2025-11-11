pacman::p_load(tidyverse,
               here)


# Exercise 1: Normality Assumption ----------------------------------------

# Linear model of tooth length by supplement type,
# dose level, and their interaction, for ToothGrowth dataset
m_tooth <- lm(len ~ supp * dose,
              data = ToothGrowth)

# Testing normality assumption:
# do the residuals of m_tooth follow a normal distribution

shapiro.test(resid(m_tooth))
# the distribution of residuals does not significantly differ from normal


# Exercise 2: Model Interpretation -------------------------------------------------------------

df_pred <- ToothGrowth %>% 
  group_by(supp) %>% 
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                      newdata = df_pred)

df_pred <- mutate(df_pred, y = y_pred)

m_tooth %>% 
  ggplot(aes(x = dose,
             y = len,
             color = supp)) +
  geom_jitter(alpha = 0.5) +
  geom_line(data = df_pred,
             aes(y = y_pred))



# Exercise 3: Multicollinearity -------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

# Figures to examine
df_y %>% 
  ggplot() +
  geom_point(aes(x = x1,
             y = y))
# It looks like x1 has some positive effect on y

df_y %>% 
  ggplot() +
  geom_point(aes(x = x2,
                 y = y))
# x2 appears to have a more definite positive effect on y

# Linear model (without interaction)
summary(lm(y ~ x1 + x2,
   data = df_y))
# According to the model, x2 is significant, but x1 is not 


# Figure of relationship between predictors
df_y %>% 
  ggplot() +
  geom_point(aes(x = x1,
            y = x2))
# They are tightly correlated!

with(df_y,
     cor(x1, x2))

