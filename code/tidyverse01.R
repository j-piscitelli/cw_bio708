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

#Exercise 1
iris_pw<-select(iris_sub,c(Petal.Width,Species))
iris_pw
#Exercise 2
iris_petal<-select(iris_sub,starts_with("Petal"))
iris_petal
#Exercise 3
iris_pw_two<-mutate(iris_sub, pw_two_times=2*Petal.Width)
iris_pw_two

#Piping
#the slow way--creates superfluous intermediate object (dfsl)
dfsl<-select(iris_sub, Sepal.Length)
dfsl_2times<-mutate(dfsl, twice= 2*Sepal.Length)
dfsl_2times

#the piped way [ctrl+shift+m write a pipe]
dftw<-iris_sub %>%
  select(Sepal.Length) %>% 
  mutate(twice=2*Sepal.Length)

#Exercise 1
iris_pipe<-iris_sub %>% 
  filter(Species=="setosa") %>% 
  mutate(pw_two_times=2*Petal.Width)
iris_pipe

#Grouping with group_by()
iris_sub %>% 
  group_by(Species)

#Summarize with summarize()
iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_sl=mean(Sepal.Length))
#multiple stats
iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_sl=mean(Sepal.Length),
            sum_sl=sum(Sepal.Length))

#Summarize with mutate()
#Shows original dataframe with new summary column
#not just a summary table
iris_sub %>% 
  group_by(Species) %>% 
  mutate(mu_sl=mean(Sepal.Length)) %>% 
  ungroup()
iris_sub

#Reshape
#pivot_wider() to reshape a dataframe to a wide format
iris_w<-iris %>% 
  mutate(id=rep(1:50,3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols="id",values_from="Sepal.Length",names_from="Species")
print(iris_w)

#pivot_longer()
iris_l<-iris_w %>% 
  pivot_longer(cols=c("setosa","versicolor", "virginica"),
               names_to="Species",
               values_to="Sepal.Length")
iris_l

#Join
#left_join() to merge dataframes based on shared columns
#one-to-one
df1<-tibble(Species=c("A","B","C"),x=c(1,2,3))
df2<-tibble(Species=c("A","B","C"), x=c(4,5,6))
left_join(x=df1,y=df2, by="Species")
#one-to-many
df6<-tibble(Species=c("A","A","B","C"),
            x=c(1,1,2,3),
            z=c("cool","cool","awesome","magical"))
left_join(x=df1, y=df6, by=c("Species","x"))
#one-to-missing
df6<-tibble(Species=c('A','B','C'),
            x=c(1,2,4),
            z=c('cool','awesome','magical'))
left_join(x=df1, y=df6, by = c("Species","x"))

