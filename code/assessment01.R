# To complete the following exercises, you must use either base R or tidyverse functions.  
# Manual searching or hard-coding answers will not be graded.

# base R ------------------------------------------------------------------

# 1: Create a vector with three elements. Assign it to `v_three`.
v_three <- c(1,2,3)

# 2: Create a vector containing 20 "a", 30 "b", and 50 "c" (total length = 100).  
# Assign it to `v_abc100`.
v_abc100 <- c(rep("a",20),rep("b",30),rep("c",50))

# 3: The script below creates a vector `v_x` with 100 random numbers from a normal distribution.  
# Select only the positive numbers (> 0) from `v_x`, calculate their mean, and assign it to `mu_x_plus`.

set.seed(100)
v_x <- rnorm(100)
mu_x_plus <- mean(v_x>0)


# 4: Create a numeric matrix with the numbers 1 through 9 arranged in 3 rows × 3 columns.  
# Assign it to `m_num`.
m_num <- matrix(data=1:9,nrow=3,ncol=3)


# 5: Create a base R data frame (`data.frame()` function) using `v_x` and `v_abc100`.  
# Name the columns `"x"` for `v_x` and `"group"` for `v_abc`, and assign it to `df_sample`.
df_sample <- data.frame("x"=v_x,"group"=v_abc100)


# tidyverse ---------------------------------------------------------------

# 6: Load the `tidyverse` package.
library(tidyverse)

# 7: The `mtcars` dataset is a built-in base R data frame.  
# Convert it to a tibble using `as_tibble()` and assign it to `df_mtcars`.  
# Use `?as_tibble()` to read the documentation before doing so.
?as_tibble
df_mtcars <- as_tibble(mtcars)


# 8: `mtcars` has the following columns:
#
# mpg   - Miles per gallon  
# cyl   - Number of cylinders  
# disp  - Engine displacement (cu. in.)  
# hp    - Gross horsepower  
# drat  - Rear axle ratio  
# wt    - Weight (1,000 lbs)  
# qsec  - 1/4 mile time (seconds)  
# vs    - Engine shape (0 = V-shaped, 1 = straight)  
# am    - Transmission (0 = automatic, 1 = manual)  
# gear  - Number of forward gears  
# carb  - Number of carburetors  
#
# Display the column names of `df_mtcars` using `colnames()`.  
# Do NOT assign the result to a new object.
colnames(mtcars)


# 9: Extract the row names of the `mtcars` dataset using `rownames()`.  
# Assign the result to `v_make`.
v_make <- rownames(mtcars)

# 10: Add `v_make` as a new column to `df_mtcars` and name the column `"make"`.
mutate(df_mtcars,"make"=v_make)

# 11: Filter `df_mtcars` to include only rows where:  
# - `mpg` is less than 20 AND  
# - `disp` is greater than 200  
# Assign the result to `df_subset`.
df_subset <- filter(df_mtcars, mpg<20 & disp>200)

# 12: Count how many car makes meet the above conditions (Q11).
# Apply `nrow()` to `df_subset`.
nrow(df_subset)

# 13: Repeat Q11 and Q12 in a single pipeline (with %>%), and assign the result to `n_make`.
n_make <- df_mtcars %>% 
  filter(mpg<20 & disp>200) %>% 
  nrow()

# 14: Convert the `cyl` column from numeric to factor using `factor()`.  
# Add it to `df_mtcars` as a new column named `f_cyl` using `mutate()` function.
mutate(df_mtcars,f_cyl=factor(cyl))

# 15: Draw a box plot showing car weight (`wt`) for each number of cylinders (`f_cyl`).
df_mtcars %>% 
  mutate(f_cyl=factor(cyl)) %>% 
  ggplot(aes(y=wt, x=f_cyl))+
  geom_boxplot()
## Jonah's note: I have included the step of creating the f_cyl column inside
# the pipeline because otherwise the code doesn't recognize 'f_cyl',
# even though I just created 'f_cyl' in Q14. I don't remember this kind of thing
# being necessary in the past, and I am confused about why it is necessary here.


# 16: Calculate the average car weight (`wt`) separately for each number of cylinders (`cyl`).
df_mtcars %>% 
  group_by(cyl) %>% 
  summarize(mean(wt)) %>% 
  ungroup()


# 17: Identify the heaviest car make (`wt`) among cars with 6 cylinders (`cyl`).
df_mtcars %>%
  filter(cyl==6) %>% 
  filter(wt==max(wt)) %>% 
  summarize(make)

# 18: Create a histogram showing the distribution of 1/4 mile time (`qsec`).
df_mtcars %>% 
  ggplot(aes(x=qsec))+
  geom_histogram()

# 19: The following script creates two tibbles:  
# `df_length` (body length) and `df_weight` (body weight),  
# each with a species code (`sp_code` column).  
# Combine these two data frames based on `sp_code` and assign the result to `df_fish`.

set.seed(123)

v_l <- runif(150, 60, 150)
v_w <- rnorm(n = length(v_l),
             mean = 0.1 * v_l^1.5, sd = 10)

v_sp <- sample(c("bhc", "rbs", "gsf"),
       size = length(v_l),
       replace = TRUE)

df_length <- tibble(length = v_l,
                    sp_code = v_sp)

df_weight <- tibble(weight = v_w,
                    sp_code = v_sp)

df_fish <- left_join(x=df_length,
                     y=df_weight,
                     by=sp_code)

# 20: Draw a scatter plot (point plot) of `length` vs. `weight` from `df_fish`,  
# coloring the points by species code (`sp_code`).

df_fish %>% 
  ggplot(aes(y=length,
             x=weight,
             color=sp_code))+
  geom_point()
