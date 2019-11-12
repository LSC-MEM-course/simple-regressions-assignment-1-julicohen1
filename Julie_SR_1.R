library(tidyverse)
#Sample data

#normal distribution
x <- rnorm(10, mean = 2, sd = 3)
mean(x)
sd(x)
print(x)

#to standardize the "seed" for randomization.
set.seed(42)
rnorm(10)

#data sim - fake data parameters
beta_0 <- 2
beta_1 <- 3
sigma <- 1
n_obs <- 1000
x_1 <- as.numeric(scale(rnorm(n_obs)))

# use linear model to get y values
y <- beta_0 + beta_1*x_1 + rnorm(n_obs, mean = 0, sd = sigma)

#plot the data - scatter of x_1 and predicted y
ggplot(data = NULL, aes(x_1, y)) + geom_point()

#fit model - parameter estimates are close to what we defined above (B0 = 2 vs 1.98; B1 = 3 vs 3.009)
fit1 <- lm(y ~ x_1)
fit1$coefficients
sd(fit1$residuals)

#Practice 1
## ## Practice Exercise #1

## 1. Replicate what we did above, adding two more (continuous) predictors, and using a data frame.  That is:
##     a. Create variables for all of the parameters needed to generate data from the linear model, including a slope parameter for each of three predictors, as well as the number of observations.  Choose different parameter values than those given above.

n_obs <- 100
beta_0 <- 1
beta_1 <- 2
beta_2 <- 7
beta_3 <- 4
sigma <- 2

##     b. Generate the predictor data.  For now, generate (theoretically) uncorrelated predictors by generating their samples separately (and independently), and putting them in a data frame.

x_1 <- as.numeric(scale(rnorm(n_obs)))
x_2 <- as.numeric(scale(rnorm(n_obs)))
x_3 <- as.numeric(scale(rnorm(n_obs)))

df <- cbind(x_1, x_2, x_3)
df <- data.frame(df)

##     c. Generate the response data in the same data frame, using the linear model equation.

df$y <- beta_0 + beta_1*df$x_1 + beta_2*df$x_2 + beta_3*df$x_3 + rnorm(n_obs, sd = sigma)

##    d. Fit a model using lm(), and pull out the coefficients from the fitted model, to compare with your “ground truth” values

fit2 <- lm(y ~ x_1 + x_2 + x_3, data = df)
fit2$coefficients
sd(fit2$residuals)


## 2. Convert your code from #1 into a function, so that you can pass the parameter values as arguments and get back the estimated parameter values in a list or data frame with appropriate names


sim_lm <- function(n_obs, beta_0, beta_1, beta_2, beta_3, sigma) {
  #predictor values
  x_1 <- as.numeric(scale(rnorm(n_obs)))
  x_2 <- as.numeric(scale(rnorm(n_obs)))
  x_3 <- as.numeric(scale(rnorm(n_obs)))
  
  df <- cbind(x_1, x_2, x_3)
  df <- data.frame(df)
  
  #generate y response values
  df$y <- beta_0 + beta_1*df$x_1 + beta_2*df$x_2 + beta_3*df$x_3 + rnorm(n_obs, sd = sigma)
  
  #fit the model
  fit <- lm(y ~ x_1 + x_2 + x_3, data = df)
 
  output <- data.frame(beta_0, beta_0_hat = NA,
                       beta_1, beta_1_hat = NA,
                       beta_2, beta_2_hat = NA,
                       beta_3, beta_3_hat = NA,
                       sigma, sigma_hat = NA)
  output[, c("beta_0", "beta_1", "beta_2", "beta_3")] <- c(beta_0, beta_1, beta_2, beta_3)
  output[, c("beta_0_hat", "beta_1_hat", "beta_2_hat", "beta_3_hat")] <- fit$coef
  output$sigma_hat <- sd(fit$resid)
  output
  
}

sim_lm(1000, 10, 5, 7, 12, 1)
sim_lm(1000, 10, 5, 7, 12, 10)
sim_lm(100000, 10, 5, 7, 12, 100)


##Repeated Simulation

sim_simple_lm <- function(n_obs, beta_0, beta_1, sigma) {
  #predictor values
  x_1 <- as.numeric(scale(rnorm(n_obs)))
  errors <- rnorm(n_obs, 0, sigma)
  
  #generate y response values
  y <- beta_0 + beta_1*x_1 + errors
  
  #fit the model
  fit <- lm(y ~ x_1)
  
  output <- list(n_obs = n_obs,
                 beta_0 = beta_0,
                 beta_0_hat = fit$coefficients["(Intercept)"],
                 beta_1 = beta_1,
                 beta_1_hat = fit$coefficients["x_1"],
                 sigma = sigma,
                 sigma_hat = sd(fit$residuals))
  return(output)
 
}

set.seed(854)
sim_simple_lm(10, 1, 2, 10)


n_sims <- 1e4
sim_results <- data.frame(sim = 1:n_sims,
                          beta_0_hat = NA,
                          beta_1_hat = NA,
                          sigma_hat = NA)

for(this_sim in 1:n_sims) {
  if(this_sim %% 1000 == 0) {cat("starting simulation", this_sim, "\n")}
  this_fit <- sim_simple_lm(10, 1, 2, 10)
  sim_results[this_sim, 2:4] <- c(this_fit$beta_0_hat,
                                  this_fit$beta_1_hat,
                                  this_fit$sigma_hat)
}

summary(sim_results[, 2:4])


#Practice 2
#1 Practice your data wrangling by plotting histograms of the parameter values in `sim_results`.  That is:
##     a. Reshape the data so that all of the parameter values are in a single column, with a new column to   distinguish the different parameters (`beta_0_hat`, etc.).

sim_results_long <- sim_results %>%
  pivot_longer(cols = 2:4,
               names_to = "parameter",
               values_to = "value")

##     b - c. Use `ggplot` to plot histograms of the parameters, using `facet_wrap(scales = "free")` to separate the parameters into different sub-plots.

ggplot(sim_results_long, aes(value, fill = parameter)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(~ parameter, scales = "free") +
  scale_fill_brewer()


## 2. Take the loop code chunk above and convert the whole thing to a function, also including your plotting code.  The function should:
##     - take the model parameters plus the number of simulations as arguments
##     - should print out the summary of the simulated parameters as a side effect
##     - should generate your plot from #1 as a side effect
##     - should return the full data frame of `sim_results` as the return value


full_simple_lm <- function(n_sims, n_obs, beta_0, beta_1, sigma) {
 
  sim_results <- data.frame(sim = 1:n_sims,
                            beta_0_hat = NA,
                            beta_1_hat = NA,
                            sigma_hat = NA)
  
  for(this_sim in 1:n_sims) {
    if(this_sim %% 1000 == 0) {cat("starting simulation", this_sim, "\n")}
    this_fit <- sim_simple_lm(10, 1, 2, 10)
    sim_results[this_sim, 2:4] <- c(this_fit$beta_0_hat,
                                    this_fit$beta_1_hat,
                                    this_fit$sigma_hat)
  }
  cat("\nSummary of simulated parameter estimates:\n\n")
  print(summary(sim_results[, 2:4]))
    sim_results_long <- sim_results %>%
    pivot_longer(cols = 2:4,
                 names_to = "parameter",
                 values_to = "value")
  print(ggplot(sim_results_long, aes(value)) +
          geom_histogram(aes(fill = parameter), binwidth = .1) +
          facet_wrap(~ parameter, scales = "free") +
          scale_fill_brewer() +
          theme_bw())
    return(sim_results)

    }
simresults_1 <- full_simple_lm(10000, 100, 5, 2, 3)
simresults_2 <- full_simple_lm(10000, 10, 5, 2, 3)
simresults_3 <- full_simple_lm(10000, 5, 5, 2, 3)


#Categorical predictors

mysleep <- sleep
levels(mysleep$group) <- c("pre", "post")
head(mysleep)

summary(lm(extra ~ group, data = mysleep))


mysleep %>% group_by(group) %>%
  summarize(mean = mean(extra))
mysleep <- mysleep %>% mutate(group.num = as.numeric(group) - 1)
summary(lm(extra ~ group.num, mysleep))

#Practice 3

mysleep$group.c <- scale(as.numeric(mysleep$group) - 1.5) 

fit.groupc <- lm(extra ~ group.c, mysleep)
summary(fit.groupc)

## 3. What does the new intercept correspond to?  Calculate that value with another method to demonstrate this interpretation of the coefficient.

mysleep %>% summarize(grand.mean = mean(extra))

#more factor coding

contrasts(iris$Species)

#Practice 4

my.iris <- iris

#1 predict sepal width with species

summary(lm(Sepal.Width ~ Species, my.iris))

#dummy code
my.iris <- my.iris %>% mutate(vers = ifelse(Species %in% "versicolor", 1, 0),
                            virg = ifelse(Species %in% "virginica", 1, 0))

summary(lm(Sepal.Width ~ vers + virg, my.iris))

my.iris <- my.iris %>% mutate(Species2 = relevel(Species, ref = "versicolor"))

summary(lm(Sepal.Width ~ Species2, my.iris))

my.iris <- my.iris %>% mutate(seto = ifelse(Species %in% "setosa", 1, 0))

summary(lm(Sepal.Width ~ seto + virg, my.iris))
