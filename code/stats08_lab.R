pacman::p_load(tidyverse,
               here,
               patchwork,
               boot)


# Exercise 1: Choice of model family --------------------------------------------------------------

# Develop model for species count

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
    data = df_fish,
    family = "poisson")

# Develop model for manual vs automatic transmission
m_am_bino <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
    data = mtcars,
    family = "binomial") # cbind() is not strictly necessary
                          # with a 0/1 variable

# Using a normal distribution where it is not appropriate
m_am_gaus <- glm(am ~ mpg + hp + wt,
    data = mtcars,
    family = "gaussian")

# Comparison of analyses with correct (binomial)
# and incorrect (normal) distributions
summary(m_am_bino)
summary(m_am_gaus)


# Exercise 2: Effect sizes--------------------------------------------------------------

# Scaling with scale() to standardize variables with different units

df_fish <- df_fish %>% 
  mutate(std_dist = scale(distance),
         std_cat = scale(cat_area),
         std_hull = scale(hull_area))
# scale() moves the y axis (i.e. x = 0) to the mean of x
# and then divides by x's standard deviation to standardize units

# This allows comparison of effect sizes between variables
m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
    data = df_fish,
    family = "poisson")

coef(m_fish)
coef(m_fish_std)


# Exercise 3: Offset term -------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)

# plotting variables against each other
ggplot(data = df_offset,
       aes(x = nitrate,
           y = count)) +
  geom_point() # count by nitrate (no correlation)

ggplot(data = df_offset,
       aes(x = area,
           y = count)) +
  geom_point() # count by area (strong correlation)

ggplot(data = df_offset,
       aes(x = nitrate,
           y = count/area)) +
  geom_point() # density by nitrate (correlation)

df_offset <- df_offset %>% 
  mutate(density = count/area)

# Density is continuous, so Poisson distribution
# will no longer work, but there is still a floor of 0
glm(density ~ nitrate,
    data = df_offset,
    family = "poisson") # 50+ warnings!

# Ignoring the effect of area
m_no_offset <- glm(count ~ nitrate,
    data = df_offset,
    family = "poisson") ## This keeps things poissony, but
                        # doesn't capture the real effect
                        # because difference in sampling
                        # effort (area) is not accounted for
                        ## This gets a significant _negative_
                        # effect of nitrate!

# Using an offset term instead of converting to density
## We use log(area) because log() is the linking function
# for a Poisson distribution
m_offset <- glm(count ~ nitrate + offset(log(area)),
    data = df_offset,
    family = "poisson") # This produces a correlation that
                        # reflects a plausible

summary(m_no_offset)
summary(m_offset)


# Exercise 4: Overdispersion ----------------------------------------------
# When variation is much greater than the mean,
# data is overdispersed. The Poisson distribution
# assumes these quantities are roughly equal, so
# using it in a GLM will return inaccurate results.

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

# Variance is much higher than mean (~ 10 times)
mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

# Plotting tadpole counts by aquatic veg cover and pond permanence
g_aqveg <- df_tadpole %>% 
  ggplot() +
  geom_point(aes(x = aqveg,
                 y = tadpole))

g_perm <- df_tadpole %>% 
  ggplot() +
  geom_point(aes(x = permanence,
                 y = tadpole))

g_aqveg + g_perm

# GLM of tadpole count by aqveg and permanence
# using Poisson distribution (not appropriately)
m_tadpole_pois <- glm(tadpole ~ aqveg + permanence,
    data = df_tadpole,
    family = "poisson")

summary(m_tadpole_pois)

# Using negative binomial distribution
m_tadpole_nb <- MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole)

summary(m_tadpole_nb)
