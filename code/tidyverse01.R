library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

filter(iris_sub,Species=="virginica")

filter(iris_sub,Species %in% c("virginica","versicolor"))

filter(iris_sub, Species != "virginica")

filter(iris_sub, !(Species %in% c("virginica","versicolor")))

filter(iris_sub, Sepal.Length>5)

filter(iris_sub,Sepal.Length>=5)

filter(iris_sub, Sepal.Length<5)

filter(iris_sub,Sepal.Length<=5)

#Sepal.Length is less than 5 AND Species is "setosa"
filter(iris_sub,Sepal.Length<5 & Species == "setosa")

#Same, but different syntax
filter(iris_sub,Sepal.Length<5, Species=="setosa")

#Same, but OR, not AND
filter(iris_sub,Sepal.Length<5|Species=="setosa")

#Arrange rows
arrange(iris_sub,Sepal.Length)

#arrange descending
arrange(iris_sub, desc(Sepal.Length))

#Exercise 1
iris_3<-filter(iris_sub,Sepal.Width>3.0)
iris_3

#Exercise 2
iris_setosa<-filter(iris_sub,Species=="setosa")
iris_setosa

#Exercise 3
iris_3_setosa<-filter(iris_sub,Species=="setosa" & Sepal.Width>3)
iris_3_setosa

#Column Manipulation
select(iris_sub,Sepal.Length)

#multiple columns
select(iris_sub, c(Sepal.Length, Sepal.Width))

#remove column(s)

select(iris_sub, -Sepal.Length)

select(iris_sub, -c(Sepal.Length,Sepal.Width))

#using starts_with() and ends_with()
select(iris_sub, starts_with("Sepal"))

select(iris_sub, -starts_with("Sepal"))

select(iris_sub, ends_with("Width"))

select(iris_sub, -ends_with("Width"))


#From scratch

#mutate()

#A new column (row ID)
#make vector of desired length and contents
x_max<-nrow(iris_sub)
x<-c(1:x_max)
x_max
x
#Add as column
mutate(iris_sub,row_id=x)

#From an existing column

#new column equal to twice 'Sepal.Length'
mutate(iris_sub,twice_sepal=2 * Sepal.Length)



#Piping
#the slow way--creates superfluous intermediate object (dfsl)
dfsl<-select(iris_sub, Sepal.Length)
dfsl_2times<-mutate(dfsl, twice= 2*Sepal.Length)
dfsl_2times

#the piped way [ctrl+shift+m write a pipe]
dftw<-iris_sub %>%
  select(Sepal.Length) %>% 
  mutate(twice=2*Sepal.Length)
