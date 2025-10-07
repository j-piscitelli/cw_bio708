# ANOVA

pacman::p_load(tidyverse,
              patchwork,
              here)

# read data
df_anova <- read_csv("data_raw/data_fish_length_anova.csv")

# there are three lakes this time, not two
distinct(df_anova, lake)

#visualization
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2,
              fill = "tomato") +
  geom_jitter(alpha = 0.2) +
  theme_bw()



# ANOVA -------------------------------------------------------------------
# use function aov()
# formula: y ~ x. That is, [response variable ~ predictor variable]

m <- aov(length ~ lake, data = df_anova)
summary(m)

# Step by step

# Between-group variability
mu <- mean (df_anova$length) # global mean of fish length

s_b <- df_anova %>% 
  group_by(lake) %>% # group by lake
  summarize(mu_g = mean(length),  # take mean of each
            n =  n()) %>%  # and count
  mutate(dev_g = (mu_g - mu)^2, # squared difference from global mean
         ss_g = dev_g * n) %>%  # summed dev_g for each group
  pull(ss_g) %>% # select the ss_g column
  sum() # sum ss_g across groups

# Within-group variability
s_w <- df_anova %>% 
  group_by(lake) %>%
  mutate(mu_g = mean(length)) %>% # puts group mean next to each individual fish
  ungroup() %>% # no need for groups once we have the mean
  mutate(dev_i = (length - mu_g)^2) %>%  # deviation of individuals from group
  pull(dev_i) %>% # select the dev_i 
  sum()

# Summed squares to variance
s2_b <- s_b / (n_distinct(df_anova$lake) - 1) # standardize by number of lakes
s2_w <- s_w / (nrow(df_anova) - n_distinct(df_anova$lake)) # and number of fish

# Test statistic
f_value <- s2_b / s2_w

# Null hypothesis test
f_null <- seq(0, 10, by = 0.1)
y <- df(x = f_null, # df() creates F-distribution
   df1 = 2, # degrees of freedom at group level
   df2 = 147) # degrees of freedom at individual level

tibble(x = f_null, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value,
             color = "red")

p_value <- 1 - pf(q = f_value, # p value is area under curve right of f value
       df1 = 2,
       df2 = 147)