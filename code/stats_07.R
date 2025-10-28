pacman::p_load(tidyverse,
               here)


df_fl <- read_csv(here("data_raw/data_fish_length.csv"))

m <- lm(length ~ lake,
        data = df_fl)

# coefficients: Intercept = 13.350, lakeb = 2.056

v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull (mu)

# mean length for lake a = Intercept
v_mu[1]

# difference of mean lengths between lakes = lakeb
v_mu[2] - v_mu[1]

# sum of coefficients = mean of length b
sum(coef(m))

summary(m)

# compare with t-test
a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = a,
       y = b,
       var.equal = TRUE)


# ANOVA vs linear model ---------------------------------------------------

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
print(df_anova)

m1 <- lm(length ~ lake,
   data = df_anova)

v_mu_anova <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length)) %>% 
  pull(mu_l)

summary(m1)

v_mu_anova[3] - v_mu_anova[1] # = Coefficient "lakec"
v_mu_anova[2] - v_mu_anova[1] # = Coefficient "lakeb"
v_mu_anova[1] # = Intercept

summary(aov(data = df_anova,
    length ~ lake))


# ANCOVA ------------------------------------------------------------------
# ancova mixes different types of explanatory variable

m2 <- lm(Sepal.Length ~ Sepal.Width + Species,
   data = iris)

summary(m2)

# Visualization
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

# predictor data frame  
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                             max(iris$Petal.Width),
                             length = 100),
                         times = 3),
       Species = rep(unique(iris$Species),
                     each = 100))

y_pred <- predict(m_iris,
        newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))
# All predicted lines have same slope, with intercept
# differing by species


# Interactive effects -----------------------------------------------------

# how to include interaction terms:
# with *
m_int <- lm(Petal.Length ~ Petal.Width * Species,
   data = iris)

# writing out term for interaction explicitly
lm(Petal.Length ~ Petal.Width + Species + Petal.Width:Species,
   data = iris)

summary(m_int)
