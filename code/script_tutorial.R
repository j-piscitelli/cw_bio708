# Example code ------------------------------------------------------------
x <- c(1, 2)
x
y <- c(3, 4)
y

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

## quick coding
z <- c(1,2,3)


# Exercises Aug 26 2025 ----------------------------------------------------------------

#create a vector with 10 elements
z<-c(1:10)
zprime<-seq(1,10,length=10)
zprimeprime<-letters[1:10]

#create a matrix with 2 rows and 2 columns

m<-matrix(1:4,nrow=2,ncol=2)
m
n<-cbind(c(1,2),c(3,4))
n
o<-rbind(c("a","b"),c("c","d"))
o

#data frame
df0<-data.frame(name= c("Smith","John","Kate","Akira"),height=c(154, 170, 156, 175))
df0
