########
# Author: Andrew Derbak
# *Statistical Rethinking* Notes
# from the book *Statistical Rethinking* by Roger McElreath
#########


# Install rethinking packages
install.packages('devtools', type = "win.binary") # Use this to install devtools on windows
install.packages(c("coda","mvtnorm","loo","daggity"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")

2
#######################

# Chapter 2

### Plausabilities of blue v white marbles
ways <- c(0,3,8,9,0) # how many ways to produce blue marbles when pulling from differnt bags
ways/sum(ways) # the probabilites across all options (sum of all combined will equal 1!)
sum(ways/sum(ways))

## Catching a globe and thumb lands on land or water

# density binomial probability. There is also a random binomial (rbinom) and cumulative binomial (pbinom)
dbinom(6, size = 9, prob = 0.5) # realtive number of ways to get of getting 6 waters (W) in a row, where there is 50% prior chance

# Binomaial distribution
# Pr(w|n,p) = (n!/(w!*(n-w)!))*p^w*(1-p)^(n-w)
# where w = water, n = size, and p = initial probability
(factorial(9)/(factorial(6)*factorial(9-6)))*0.5^6*(1-0.5)^(9-6)

# dynamic way to read program this
w <- 6
n <- 9
p <- 0.5

(factorial(n)/(factorial(w)*factorial(n-w)))*p^w*(1-p)^(n-w)

# Important!
#[] Understand the difference between Probablity, Plausibility, and Likelihood!

########################

# 2.4 Making the Model Go

##2.4.1 Grid approximation

# define grid
p_grid <- seq(from=0, to=1, length.out = 20)

# define prior -- different examples yield different charts
prior <- rep(1, 20) # prior example 1
prior <- ifelse(p_grid < 0.5,0,1) # prior example 2
prior <- exp(-5*abs(p_grid-0.5)) # prior example 3

# compute likelihood at each value in grid
likelihood <-  dbinom(6, size = 9, prob=p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <-unstd.posterior / sum(unstd.posterior)

## Display the posterior distribution

plot(p_grid, posterior, type="b", 
     xlab = "Probability of water",
     ylab = "Posterior probability")
mtext("20 points")


##2.4.2 Quadratic Approximation

library(rethinking)
