pacman::p_load(tidyverse,
               patchwork)

## 10.5.1
#Influence of sample size

xs <- rnorm(10, mean = 10, sd = 5)
ys <- rnorm(10, mean = 12, sd = 5)
xl <- rnorm(100, mean = 10, sd = 5)
yl <- rnorm(100, mean = 12, sd = 5)

t.test(xs, ys, var.equal = TRUE)
t.test(xl, yl, var.equal = TRUE)



## 10.5.2
#Difference and uncertainty

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

# create tibble of with group and value columns
df_ab12 <- tibble(a1,a2,b1,b2) %>% 
  pivot_longer(cols = c(a1,a2,b1,b2),
              names_to="group",
              values_to="value")
  
df_ab12_mus <- df_ab12 %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sd = sd(value))


# figure showing a1 and a2
df_a12 <- filter(df_ab12, group %in% c("a1","a2"))
df_a12_mus <- filter(df_ab12_mus, group %in% c("a1","a2"))

df_a12 %>% 
  ggplot(aes(x = group, y = value)) +
  geom_jitter(width = 0.3,
              height = 0,
              alpha = 0.3) +
  geom_segment(data = df_a12_mus,
               aes(x = group,
                   xend = group,
                   y = mu - sd,
                   yend = mu + sd)) +
  geom_point(data = df_a12_mus,
             aes(x = group,
                 y = mu),
             size = 2) +
  labs(x = "Group",
       y = "Value")

# t-tests
t.test(a1,a2)
t.test(a1,b1)
t.test(a1,b2)
t.test(a2,b1)
t.test(a2,b2)
t.test(b1,b2)


## 10.5.3
# t-test without t.test()

data = (c(a1,b1))

# t-statistic
t_a1_b1 <- (mean(data[1])-mean(data[2]))/
  sqrt((var(data[1])/length(data[1]))+
         (var(data[2])/length(data[2])))
# degrees of freedom
dof_a1_b1 <- length(data[1]) + length(data[2]) - 2
# p-value
p_a1_b1 <- pt(q = t_a1_b1, df = dof_a1_b1) + 1 -
  pt(q = -1*t_a1_b1, df = dof_a1_b1)

# print report
(ttest_report <- tibble("t-stat" = t_a1_b1,
                       "deg. of fr." = dof_a1_b1,
                       "p-value" = p_a1_b1))
