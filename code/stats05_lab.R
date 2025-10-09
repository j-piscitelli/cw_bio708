pacman::p_load(tidyverse, patchwork, here, pwr)

# Create violin plot for plant growth dataset
PlantGrowth %>% 
  ggplot(aes(x=group,
             y=weight)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2) +
  geom_jitter(alpha = 0.2)

# ANOVA to examine differences in weight by group
summary(aov(weight ~ group,
            PlantGrowth))

# The most important figures to report are the p-value (Pr),
# degrees of freedom (Df), and the F-value

# Power analysis to determine sample size to achieve 80% power
# at 0.05 significance, for an effect size of Cohen's f = 0.5
# comparing 3 groups.
pwr.anova.test(k = 30,
               f = 0.5,
               sig.level = 0.05,
               power = 0.8)
# n = 13.89521

# leaving power blank
pwr.anova.test(k = 5,
               n = 100,
               f = 0.2,
               sig.level = 0.05)
