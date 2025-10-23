pacman::p_load(tidyverse,
               patchwork,
               here)

# Linear regression -------------------------------------------------------


df_algae <- read_csv(here("data_raw/data_algae.csv"))

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

# linear regression
m <- lm(biomass ~ conductivity,
   data = df_algae)
summary(m)

coef(m)
alpha <- coef(m)[1]
beta <- coef(m)[2]

# plot with data points and regression line
df_algae %>% 
ggplot(aes(x = conductivity,
           y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)


# t-value calculation (using beta)
se <- sqrt(diag(vcov(m))) # standard error

t_value <- beta/se[2] # t value for slope


p_beta <- (1 - pt(t_value, df = 48)) +
  pt(-t_value, df = 48) # p value for slope
  # we add the probability mass more extreme than
  # the observed t-value or its negative equivalent.
  # 48 is the number of samples (here, 50) minus
  # the number of parameters (here, alpha and beta)


# Coefficient of determination (R^2) --------------------------------------


eps <- resid(m) # extract residuals
df_algae <- df_algae %>%  
  mutate(eps = eps) # add residual column to df_algae

# visualizing errors
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps),
               linetype = "dashed")

ss <- sum(eps^2)
ss0 <- sum((df_algae$biomass - mean(df_algae$biomass))^2)
r_sq <- 1 - (ss/ss0) # coefficent of determination
