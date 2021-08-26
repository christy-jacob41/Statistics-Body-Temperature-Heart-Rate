require(tidyverse)

data <- read_csv("bodytemp-heartrate.csv")

# getting male and female data
male <- data[which(data$gender==1),]
females <- data[which(data$gender==2),]

# body temperature side by side boxplot and qq plot
boxplot(male$body_temperature, females$body_temperature, names = c("Male", "Female"), ylab = "Body Temperature")
qqnorm(male$body_temperature, main = "Male")
qqline(male$body_temperature)
qqnorm(females$body_temperature, main="Female")
qqline(females$body_temperature)

# getting confidence interval for difference between body temperature between males and females
t.test(male$body_temperature, females$body_temperature, alternative="two.sided", var.equal = F)

# heart rate side by side boxplot and qq plot
boxplot(male$heart_rate, females$heart_rate, names = c("Male", "Female"), ylab = "Heart Rate")
qqnorm(male$heart_rate, main = "Male")
qqline(male$heart_rate)
qqnorm(females$heart_rate, main="Female")
qqline(females$heart_rate)

# getting confidence interval for difference between heart rate between males and females
t.test(male$heart_rate, females$heart_rate, alternative="two.sided", var.equal = F)

# looking at correlation between body temperature and heart rate
cor(male$body_temperature, male$heart_rate)
cor(females$body_temperature, females$heart_rate)

# getting graphical view for males to see if any correlation between heart rate and body temperature
plot(male$heart_rate, male$body_temperature, main="Male")
abline(lm(male$body_temperature~male$heart_rate))

# getting graphical view for females to see if any correlation between heart rate and body temperature
plot(females$heart_rate, females$body_temperature, main="Female")
abline(lm(females$body_temperature~females$heart_rate))

#############################################################################################################

#functions to help get large sample z-interval
# see if true mean is in large sample z-interval after simulating a sample
large.sample <- function(n, lambda) {
  S <- rexp(n,lambda)
  lower <- mean(S) - qnorm(0.975) * sd(S)/sqrt(n)
  upper <- mean(S) + qnorm(0.975) * sd(S)/sqrt(n)
  true.mean <- 1/lambda
  if(true.mean<upper & true.mean>lower)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

# replicating large sample z-interval 5000 times to get coverage probabilities
large.sample.replicate <- function(n,lambda) {
  in.interval <- replicate(5000, large.sample(n,lambda))
  return(length(in.interval[which(in.interval==1)])/5000)
}

# functions to help get bootstrap percentile method interval

#gets mean of sample
means <- function(n, lambda){
  S <- rexp(n, lambda)
  return(mean(S))
}

# see if true mean is in bootstrap percentile method interval after simulating a sample
bootstrap.interval <- function(n, lambda){
  S <- rexp(n, lambda)
  true.mean <- 1/lambda
  B <- replicate(1000,means(n,1/mean(S)))
  bounds <- sort(B)[c(25,975)]
  if(true.mean>bounds[1] & true.mean <bounds[2])
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

# replicating bootstrap percentile method interval 5000 times to get coverage probabilities
bootstrap.interval.replicate <- function(n, lambda){
  in.interval <- replicate(5000, bootstrap.interval(n, lambda))
  yes.in.interval <-in.interval[which(in.interval==1)]
  return(length(yes.in.interval)/5000)
}

# getting coverage probability for large sample z-interval for n=5 and lambda = 0.01
large.sample.replicate(5,0.01)
# getting coverage probability for bootstrap interval for n=5 and lambda = 0.01
bootstrap.interval.replicate(5,0.01)

# getting large sample z-interval coverage probability for all combinations of n and lambda that we are working with and putting in matrix to make it easier to graph
large.sample.matrix <-matrix(c(large.sample.replicate(5, 0.01), large.sample.replicate(10, 0.01), large.sample.replicate(30, 0.01), large.sample.replicate(100, 0.01), 
                               large.sample.replicate(5, 0.1), large.sample.replicate(10, 0.1), large.sample.replicate(30, 0.1), large.sample.replicate(100, 0.1), 
                               large.sample.replicate(5, 1), large.sample.replicate(10, 1), large.sample.replicate(30, 1), large.sample.replicate(100, 1), 
                               large.sample.replicate(5, 10), large.sample.replicate(10, 10), large.sample.replicate(30, 10), large.sample.replicate(100, 10)))

# getting boostrap interval coverage probability for all combinations of n and lambda that we are working with and putting in matrix to make it easier to graph
bootstrap.interval.matrix <-matrix(c(bootstrap.interval.replicate(5, 0.01), bootstrap.interval.replicate(10, 0.01), bootstrap.interval.replicate(30, 0.01), bootstrap.interval.replicate(100, 0.01), 
                               bootstrap.interval.replicate(5, 0.1), bootstrap.interval.replicate(10, 0.1), bootstrap.interval.replicate(30, 0.1), bootstrap.interval.replicate(100, 0.1), 
                               bootstrap.interval.replicate(5, 1), bootstrap.interval.replicate(10, 1), bootstrap.interval.replicate(30, 1), bootstrap.interval.replicate(100, 1), 
                               bootstrap.interval.replicate(5, 10), bootstrap.interval.replicate(10, 10), bootstrap.interval.replicate(30, 10), bootstrap.interval.replicate(100, 10)))

# graphing based on lambda
plot(c(5,10,30,100), large.sample.matrix[c(1,2,3,4)], main="lambda = 0.01", col = "blue", type = "b", xlab = "n", ylab="Probabilites",ylim=c(0,1))
lines(c(5,10,30,100), bootstrap.interval.matrix[c(1,2,3,4)], col="red", type="b")

plot(c(5,10,30,100), large.sample.matrix[c(5,6,7,8)], main="lambda = 0.1", col = "blue", type = "b", xlab = "n",ylab="Probabilites", ylim=c(0,1))
lines(c(5,10,30,100), bootstrap.interval.matrix[c(5,6,7,8)], col="red", type="b")

plot(c(5,10,30,100), large.sample.matrix[c(9,10,11,12)], main="lambda = 1", col = "blue", type = "b", xlab = "n",ylab="Probabilites", ylim=c(0,1))
lines(c(5,10,30,100), bootstrap.interval.matrix[c(9,10,11,12)], col="red", type="b")

plot(c(5,10,30,100), large.sample.matrix[c(13,14,15,16)], main="lambda = 10", col = "blue", type = "b", xlab = "n",ylab="Probabilites", ylim=c(0,1))
lines(c(5,10,30,100), bootstrap.interval.matrix[c(13,14,15,16)], col="red", type="b")

# graphing based on n
plot(c(0.01,0.1,1,10), large.sample.matrix[c(1,5,9,13)], main="n=5", col = "blue", type = "b", xlab = "lambda",ylab="Probabilites", ylim=c(0,1))
lines(c(0.01,0.1,1,10), bootstrap.interval.matrix[c(1,5,9,13)], col="red", type="b")

plot(c(0.01,0.1,1,10), large.sample.matrix[c(2,6,10,14)], main="n=10", col = "blue", type = "b", xlab = "lambda",ylab="Probabilites", ylim=c(0,1))
lines(c(0.01,0.1,1,10), bootstrap.interval.matrix[c(2,6,10,14)], col="red", type="b")

plot(c(0.01,0.1,1,10), large.sample.matrix[c(3,7,11,15)], main="n=30", col = "blue", type = "b", xlab = "lambda", ylab="Probabilites",ylim=c(0,1))
lines(c(0.01,0.1,1,10), bootstrap.interval.matrix[c(3,7,11,15)], col="red", type="b")

plot(c(0.01,0.1,1,10), large.sample.matrix[c(4,8,12,16)], main="n = 100", col = "blue", type = "b", xlab = "lambda", ylab="Probabilites", ylim=c(0,1))
lines(c(0.01,0.1,1,10), bootstrap.interval.matrix[c(4,8,12,16)], col="red", type="b")
