# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

pacman::p_load(tidyverse,
               patchwork,
               janitor,
               MuMIn,
               glmmTMB)
# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

df_co2 <- as_tibble(CO2)

# Q2
# Convert column names to lowercase and reassign to `df_co2`

df_co2 <- df_co2 %>%
  rename(plant = Plant,
         type = Type,
         treatment = Treatment)

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

## JP: Creating separate dataframes for the two sites
df_qc <- df_co2 %>% 
  filter(type == "Quebec")

df_ms <- df_co2 %>% 
  filter(type == "Mississippi")

## JP: Plotting
gg_qc <- df_qc %>% 
  ggplot() +
  geom_point(aes(x = conc,
                 y = uptake,
                 color = treatment)) +
  labs(title = "Quebec",
       x = "CO2 concentration",
       y = "CO2 assimilation rate") +
  ylim(0,50) # Making the y-axis the same on both graphs, for ease of comparison

gg_ms <- df_ms %>% 
  ggplot() +
  geom_point(aes(x = conc,
                 y = uptake,
                 color = treatment)) +
  labs(title = "Mississippi",
       x = "CO2 concentration",
       y = "CO2 assimilation rate") +
  ylim(0,50) # Making the y-axis the same on both graphs, for ease of comparison


gg_qc + gg_ms + guide_area() +
  plot_layout(guides = "collect")

# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop suitable statistical models to examine:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

## JP: Models for Quebec
m_conc_qc <- lm(uptake ~ conc,
                data = df_qc)

m_treat_qc <- lm(uptake ~ treatment,
                 data = df_qc)

m_qc <- lm(uptake ~ conc * treatment,
               data = df_qc)

## JP: Models for Mississippi
m_conc_ms <- lm(uptake ~ conc,
           data = df_ms)

m_treat_ms <- lm(uptake ~ treatment,
                 data = df_ms)

m_ms <- lm(uptake ~ conc * treatment,
               data = df_ms)


# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:

## JP: For the plants from Quebec, there was a highly significant correlation between
## CO2 concentration and assimilation rate (with a p value around .00008), and no
## connection between treatment and CO2 assimilation rate. For the plants from
## Mississippi, there was a similar effect of CO2 concentration. When I looked at
## the effect of treatment on CO2 assimilation rate in Mississippi, it initially
## appeared highly significant (and negative), but after including the interaction
## of concentration and treatment that effect became only modestly significant
## (p = .023).

# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`

df_env <- df_env %>% janitor::clean_names()

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.


## JP: Create duplicate of df_env to edit
df_env_sub <- df_env

## JP: Remove columns with only one unique value
for (col in names(df_env_sub)) {
  if (length(unique(df_env_sub[[col]])) == 1) {
    df_env_sub <- select(df_env_sub, -col)
  }
}


# Q8
# Calculate summary statistics for each plot using `df_bci`.
# For each plot, compute:
# - n_sum: total count of all individuals across species
# - n1: count of the most dominant species (maximum count among species)
# - p: proportion of the most abundant species (n1 / n_sum)
# Assign the resulting dataframe to `df_n`.

df_n <- df_bci %>% 
  group_by(plot) %>% 
  mutate(n_sum = sum(count)) %>% 
  mutate(n1 = max(count)) %>% 
  mutate(p = n1/n_sum) %>% 
  ungroup() %>% 
  select(c(plot, n_sum, n1, p)) %>% 
  distinct()


# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- left_join(df_n,df_env_sub)

# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species in each plot. Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# Fit a suitable statistical model to the data.
# Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

## JP: The proportion of the dominant species is bounded by 0 and 1, but it is
## continuous, not discrete, so I think I should use a Beta distribution.
m_p <- glmmTMB(p ~ env_het + stream + habitat,
               data = df_m,
               family = "beta_family")

options(na.action = "na.fail")
m_set <- dredge(m_p, rank = "AIC")

## JP: The best predictive model uses habitat alone, though the model with habitat &
## environmental heterogeneity is almost as good (with a delta of 0.28)
