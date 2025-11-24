# Ctrl (Command) + Shift + N is a hotkey for creating a new script file
# Ctrl + I is to fix indent
# data cleaning tips in R

pacman::p_load(tidyverse,
               patchwork,
               janitor,
               stringdist,
               here)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_messy.csv"
df_messy <- read_csv(url)

# check data entries before you analyze data ------------------------------

## check class of each column
## - sapply & class

sapply(df_messy,
       FUN = class)

## check unique elements in each column
## - sapply & unique

sapply(df_messy,
       FUN = unique)

## check possible type
## - stringdistmatrix(unique())

stringdistmatrix(unique(df_messy$collector))

# text cleaning in R ------------------------------------------------------

## remove white space
## str_squish()

a <- c(" a ", "a  b", "b ", "a", "b")
str_squish(a) # multiple consecutive whitespaces are reduced to one, not removed
  
## align text case
## str_to_lower() & str_to_upper()

b <- c("A", "a", "bB", "BB")
str_to_lower(b)
str_to_upper(b)

## replace text
## str_replace() & str_replace_all()

v <- c("a b", "a.b", "a b.c")

    # str_replace() replaces the _first_ instance of the target character(s)
str_replace(v, "\\s", "_") %>%  # \\s is regex for whitespace
  str_replace("\\.", "_") # \\. is regex for a period
str_replace(v, "\\s|\\.", "_") # replace multiple characters with the same thing

    # str_replace_all() replaces _every_ instance
str_replace_all(v, "\\s|\\.", "_")

## remove text
## str_remove() & str_remove_all()

x <- c("abc", "dd", "abd")

str_remove_all(x, "ab")

## extract text
## str_extract()
z <- c("abc","dd","abd")

str_extract(z, "a.") # . is regex for any character

## detect text
## str_detect()

  # can be useful with filter() or ifelse()

str_detect(z, "a") # returns TRUE or FALSE depending on presence of "a"

ifelse(str_detect(z, "a"),
       yes = 1,
       no = 0)

y <- c("ab","Ab")
str_detect(y, "[Aa]") # brackets mean that either "A" or "a" will be detected

# date objects ------------------------------------------------------------

d <- c("2024/06/01", "June 4 2024", "2024.06.07")

lubridate::parse_date_time(d,
                           tz = "EST",
                           orders = c("Y/m/d", # Y is 4-digit year, y is 2-digit
                                      "B d Y", # B is month as string  
                                      "Y.m.d"))

# clean data --------------------------------------------------------------

df_messy %>% 
  mutate(collector = str_to_lower(collector),
         species = str_remove(str_to_lower(str_replace_all(str_squish(species),
                                                "\\s|\\.",
                                                "_")),
                              "_$"), # $ is regex for end of a string,
                                      # ^ is the start of a string
         length_mm = str_replace_all(length_mm,",",".") %>% 
           str_remove_all("\\smm") %>%  # length_mm is still character
           as.numeric(), # now it is numeric
         sample_date = parse_date_time(sample_date,
                                       orders = c("Y/m/d",
                                                  "B d y",
                                                  "Y.m.d",
                                                  "d B Y")) %>% 
           as.Date(),
         recaptured = ifelse(str_detect(recaptured, "[Yy]"),
                  yes = 1,
                  no = 0))


# column manipulation based on column type --------------------------------
## mutate(), across(), where()




# example code ------------------------------------------------------------

# chr_clean <- function(x) {
#   x %>%
#     str_squish() %>% 
#     str_to_lower() %>% 
#     str_replace_all("\\.|\\s", "_") %>% 
#     str_remove_all("^_|_$")
# }
# 
# df_messy %>% 
#   mutate(collector = chr_clean(collector),
#          species = chr_clean(species),
#          length_mm = str_squish(length_mm) %>% 
#            str_replace(",", "\\.") %>% 
#            str_extract("\\d{1,}") %>% 
#            as.numeric(),
#          sample_date = parse_date_time(sample_date,
#                                        tz = "EST",
#                                        order = c("Y/m/d",
#                                                  "B d Y",
#                                                  "d B Y")),
#          recaptured = str_to_lower(recaptured) %>% 
#            str_sub(start = 1L,
#                    end = 1L)
#   )