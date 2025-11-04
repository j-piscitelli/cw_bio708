pacman::p_load(tidyverse,
               patchwork,
               here)


# Count data --------------------------------------------------------------

df_count <- read_csv(here("data_raw/data_garden_count.csv"))
print(df_count)

# We _can_ run a normal lm()...
m_normal <- lm(count ~ nitrate,
               df_count)
summary(m_normal)

alpha <- coef(m_normal)[1] # intercept
beta <- coef(m_normal)[2] # slope

# ...but the regression line implies negative count values
# which is impossible
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)

# A Poisson distribution is preferable
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

summary(m_pois)

# This isn't how to do it.
alpha_p <- coef(m_pois)[1]
beta_p <- coef(m_pois)[2]

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha_p,
              slope = beta_p)

# visualization of Poisson distribution
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                     max(df_count$nitrate),
                     length = 100))

y_pred <- predict(m_pois,
        newdata = df_pred) %>% 
  exp()

df_pred <- df_pred %>% 
  mutate(y = y_pred)

ggplot(df_count,
       aes(x = nitrate,
           y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y))

# compare summary of m_normal and m_pois
summary(m_normal)
summary(m_pois)

# random numbers in Poisson distribution
rpois(n = 10,
      lambda = 2)


# Proportional data -------------------------------------------------------

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))

# Looking at relationship of rate of fertilization to density
df_mussel <- df_mussel %>% # create col for proportion fertilized
  mutate(prop_fert = n_fertilized/n_examined)

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

# coding binomial distribution

cbind_mussel <- cbind(df_mussel$n_fertilized,
      df_mussel$n_examined - df_mussel$n_fertilized)

m_binom <- glm(cbind_mussel ~ density,
    data = df_mussel,
    family = "binomial")
summary(m_binom)

# logit function
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
       x = exp(logit_x)/(1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "x",
       x = "logit(x)")
