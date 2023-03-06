
## PART 1: Project Instructions

#In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations. :


library(ggplot2)

# set simulation
lambda<- 0.2
n <- 40 
n_simulations <- 1000 

# setting seed to reproduce in future
set.seed(2)

# run matrix
ex_dis <- matrix(data=rexp(n * n_simulations, lambda), nrow=n_simulations)
ex_dis_m <- data.frame(means=apply(ex_dis, 1, mean))




## Sample mean compared to the theoretical mean of the distribution

#The expected mean of a exponential distribution of rate 
#lambda is 1/lambda.Therefore, the theoretical mean is as follows:
  
theoretical_m <- 1/lambda
theoretical_m



#The average sample mean of the 1000 simulations of 40 randomly sampled exponential distributions is as follows:


#meansofmeans
m_m <- mean(ex_dis_m$means)
m_m


#As demonstrated above, one can see that both means are very close to each other. 

## Sample Variance compared to Theoretical Variance of distribution:

#The theoretical variance is as follows:

variance_t <- 1/lambda/sqrt(n)
variance_t


#The sample variance of distribution is as follows:
        


variance_S <- sd(ex_dis_m$means)
variance_S


#Similar to the means, the variance of distribution between both the sample and the theoritical are very similar. 

##Distribution is Approximately Normal.

# plot the means
ggplot(data = ex_dis_m, aes(x = means)) + 
        geom_histogram(binwidth=0.1, aes(y=..density..), alpha=0.2) + 
        geom_density(colour="blue", size=1) +
        geom_vline(xintercept = m_m, size=1, colour="#0000CC") 



#As shown in the graph above, the distribution of the sample mean shows a normal distribution. Note the line shows the exact mean of the sample data.


## PART 2: Project Instructions:

#Now in the second portion of the project, we're going to analyze the ToothGrowth data in the R datasets package.
#1.Load the ToothGrowth data and perform some basic exploratory data analyses
#2.Provide a basic summary of the data.
#3.Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
#4.State your conclusions and the assumptions needed for your conclusions.


library(datasets)

data(ToothGrowth)
toothGrowth <- ToothGrowth 


## Summary of the data

#Data Description:
#        The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC).



str(toothGrowth)
summary(toothGrowth)
head(toothGrowth)
table(toothGrowth$supp, toothGrowth$dose)


anova.out <- aov(len ~ supp * dose, data=toothGrowth)
summary(anova.out)

ggplot(toothGrowth, aes(x = dose, y = len, colour = supp)) +
        geom_line()




#The anova results indicate that there is a statistical significant association in the interaction betweeen orange juiCe (OJ) and ascorbic acid (VC). The linear graph showing the relationship between dosage amount and the response to leghth of odontoblasts also further clarifies the relationship between OJ and VC. As shown in the graph we can see they are very similar while OJ being slightly more effective on guinea pigs.
