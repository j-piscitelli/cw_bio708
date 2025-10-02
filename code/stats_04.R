pacman::p_load(tidyverse,
               patchwork,
               here)

# Looking at the data -----------------------------------------------------


#read data
df_fl <- read_csv(here("data_raw/data_fish_length.csv"))

#check number of unique lakes
unique(df_fl$lake) # returns vector

distinct(df_fl, lake) # returns tibble

# group by mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean (length),
            sd_l = sd(length))

# plot
df_fl %>% 
  ggplot(aes(x=lake,
             y=length)) +
  geom_jitter(width = 0.1,
              height = 0,
              alpha = 0.25) +
  geom_segment(data = df_fl_mu,
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data=df_fl_mu,
             aes(x=lake,
                 y=mu_l),
             size = 3) +
  labs(x="Lake",
       y="Fish body length")


# T-tests -----------------------------------------------------------------

# create vectors to run t-test on
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
         pull(length)

#t-test
t.test(x,y,var.equal=TRUE) # usually we use var.equal=FALSE

#The t-statistic

mu_x <- mean(x)
mu_y <- mean(y)
mu_x-mu_y # difference of average length in each lake

df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            var_l = var(length),
            n = n()) #counts number of rows per group

v_mu <- pull(df_t,mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_a <- ((v_n[1] - 1)/((sum(v_n)-2))) * v_var[1]
var_b <- ((v_n[2] - 1)/(sum((v_n)-2))) * v_var[2]
var_p <- var_a + var_b

t_value <- (v_mu[1]-v_mu[2])/sqrt(var_p * ((1/v_n[1]) + (1/v_n[2])))


# Drawing the null distribution:
# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = 98) #dt() calculates probability density

# draw figure
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability density",
       x = "t-statistic") +
  geom_vline(xintercept = t_value) +
  geom_vline(xintercept = -1*t_value) # location of found t-stat on null distribution

# p-values ----------------------------------------------------------------

#probability on null distribution under t-statistic
#and over its mirror on the other side of 0
p_lower <- (pt(q = t_value, df=98))
p_higher <- 1 - pt(q = -1*t_value, df=98)

#sum of these is the p-value
p_lower + p_higher
