# Descriptive Stats 1
library(tidyverse)

# Central tendency --------------------------------------------------------

#arithmetic mean
#calculate arithmetic mean using length() and sum()
v_x <- rnorm(10)

mean_v_x <- sum(v_x)/length(v_x)

#geometric mean
# calculate geometric mean using prod() and length ()
v_y <- runif(10, min = 10, max = 20)
geom_mean_y <- prod(v_y)^(1/length(v_y))
geom_mean_y
exp(mean(log(v_y)))

#median
v_z <- runif(9, min = 10, max = 20)
sort(v_z)[(length(v_z) + 1)/2]
median(v_z)


# Variation ---------------------------------------------------------------

#Variance
# Use sum(), mean(), and length() to define variance
v_a <- rnorm(100)

s2 <- sum((v_a-mean(v_a))^2)/length(v_a)


#Standard deviation
s <- sqrt(s2)


#Interquartile range
a_l <- quantile(v_a, probs = 0.25)
a_h <- quantile(v_a, probs = 0.75)
iqr <- abs(a_h-a_l)

#MAD
median(abs(v_a - median(v_a)))

#Coefficient of variation

v_b <- runif(10, min = 10, max = 20)
s2 <- sum((v_b-mean(v_b))^2)/length(v_b)
s <- sqrt(s2)

CV_b <- s/mean(v_b)
CV_b

# MAD/median
MAD_b <- median(abs(v_b - median(v_b)))
MADmed_b <- MAD_b/median(v_b)
MADmed_b
