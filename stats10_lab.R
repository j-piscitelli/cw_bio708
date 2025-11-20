pacman::p_load(tidyverse,
               here,
               janitor,
               palmerpenguins,
               patchwork,
               MuMIn)

# Data Cleaning and Manipulation -------------------------------------------------------

# clean column names
colnames(penguins_raw) # lots of spaces and parentheses

penguins_clean <- penguins_raw %>%
  clean_names()

colnames(penguins_clean) # that's better

# Change clutch_completion from "Yes"/"No" to numeric values
unique(penguins_clean$clutch_completion)

penguins_clean <- penguins_clean %>% 
  mutate(clutch_completion = ifelse(clutch_completion == "Yes", 
                                    1,
                                    0))

# Change species names to single words
unique(penguins_clean$species)

penguins_clean <- penguins_clean %>% 
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))

# Remove rows missing values
# in columns that will be used for analysis
penguins_clean <- penguins_clean %>% 
  drop_na(c("culmen_length_mm",
            "culmen_depth_mm",
            "flipper_length_mm",
            "body_mass_g",
            "sex"))

# Data analysis -----------------------------------------------------------

# GLM explaining clutch completion by individual penguin traits
m_clutch <- glm(clutch_completion ~ culmen_length_mm + culmen_depth_mm +
                  flipper_length_mm + body_mass_g + sex + species,
         data = penguins_clean,
         family = "binomial")

summary(m_clutch)

# AIC-based model selection
options(na.action = "na.fail") # magic spell for dredge()
m_set <- dredge(m_clutch, rank = "AIC")
subset(m_set, delta < 2)

# Note that this does not account for multicollinearity

