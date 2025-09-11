library(tidyverse)

# Comparing central tendency measures -------------------------------------


#Create a new vector z with length 1000
#as exp(rnorm(n = 1000, mean = 0, sd = 0.1)),
#and calculate the arithmetic mean, geometric mean, and median.

z <- exp(rnorm(n=1000, mean = 0, sd = 0.1))
#mean
mu_z <- mean(z)
#median
median_z <- median(z)
#geometric mean
gmu_z <- prod(z)^(1/length(z))

#Draw a histogram of z using functions tibble(),
#ggplot(), and geom_histogram().
z_hist <- z %>% 
  tibble() %>% 
  ggplot(aes(x=z)) +
  geom_histogram()

#Draw vertical lines of arithmetic mean, geometric mean,
#and median on the histogram with different colors using a function geom_vline() .

z %>% 
  tibble() %>% 
  ggplot(aes(x=z)) +
  geom_histogram() +
  geom_vline(xintercept=mu_z, colour = "red") +
  geom_vline(xintercept=median_z, colour = "blue") +
  geom_vline(xintercept=gmu_z, colour = "yellow")

#Create a new vector z_rev as -z + max(z) + 0.1, and repeat step 1 – 4.

z_rev <- -z + max(z) + 0.1
mu_z_rev <- mean(z_rev)
median_z_rev <- median(z_rev)             
gmu_z_rev <- prod(z_rev)^(1/length(z_rev))
z_rev_hist <- z_rev %>% 
  tibble() %>% 
  ggplot(aes(x=z_rev)) +
  geom_histogram() +
  geom_vline(xintercept=mu_z_rev, colour = "red") +
  geom_vline(xintercept=median_z_rev, colour = "blue") +
  geom_vline(xintercept=gmu_z_rev, colour = "yellow")
z_rev_hist

# install.packages("patchwork")
library(patchwork)
z_hist/z_rev_hist



# Comparing variation measures --------------------------------------------

#fish weights in grams
w <- rnorm(100, mean = 10, sd = 1)
head(w)
# show first 10 elements in w

#Convert the unit of w to “milligram” and create a new vector m.
m <- w*1000

#Calculate SD and MAD for w and m.
var_w <- (sum((w-mean(w))^2))/length(w)
sd_w <- sqrt(var_w)
var_m <- (sum((m-mean(m))^2))/length(m)
sd_m <- sqrt(var_m)

(MAD_w <- median(abs(w-median(w))))
(MAD_m <- median(abs(m-median(m))))

#Calculate CV and MAD/Median for w and m.
(CV_w <- sd_w/mean(w))
(CV_m <- sd_m/mean(m))

(MADmed_w <- MAD_w/median(w))
(MADmed_m <- MAD_m/median(m))
