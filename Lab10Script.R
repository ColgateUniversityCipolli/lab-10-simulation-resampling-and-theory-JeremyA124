library(tidyverse)
set.seed(7272)

samples1 <- tibble(
  observations = rbinom(10000, 1004, 0.39),     #Take Samples using rbinom
  observations2 = rbinom(10000, 2008, 0.39)
) %>%
  mutate(observations = observations/1004,
         observations2 = observations2/2008)

range = c(quantile(samples1$observations, 0.975), quantile(samples1$observations, 0.025))
range2 = c(quantile(samples1$observations2, 0.975), quantile(samples1$observations2, 0.025))
error = ((quantile(samples1$observations, 0.975) - quantile(samples1$observations, 0.025))/2)*100
error2 = ((quantile(samples1$observations2, 0.975) - quantile(samples1$observations2, 0.025))/2)*100

ggplot(data = samples1) +
  geom_histogram(aes(x=observations, y = after_stat(density)), fill = "black") +
  geom_density(aes(x=observations), color = "red", size = 1) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "Porportions")

ggplot(data = samples1) +
  geom_histogram(aes(x=observations2, y = after_stat(density)), fill = "black") +
  geom_density(aes(x=observations2), color = "red", size = 1) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "Porportions")

gallup.samples <- tibble(
  observations = c(rep(1, times = 392), rep(0, times = 592), rep(-1, times = 20))
)

sampled = c()

for(i in 1:1000){
  curr.sample = sample(gallup.samples$observations, size = 1004, replace = TRUE)
  
  sampled[i] = length(which(curr.sample == "1"))/length(curr.sample)
}

summary <- tibble(
  prop.yes = sampled
)

range = c(quantile(summary$prop.yes, 0.975), quantile(summary$prop.yes, 0.025))
error = ((quantile(summary$prop.yes, 0.975) - quantile(summary$prop.yes, 0.025))/2)*100

ggplot(data = summary) +
  geom_histogram(aes(x = prop.yes, y = after_stat(density)), fill = "black") +
  geom_density(aes(x=prop.yes), color = "red", size = 1) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "Porportions of Responses with Yes")

n.p.sim <- tibble(
  n = numeric(),
  p = numeric(),
  range = numeric()
)

for(n in seq(100, 3000, 10)){
  for(p in seq(0.01, 0.99, 0.01)){
    data = rbinom(10000, n, p) / n
    range = (quantile(data, 0.975) - quantile(data, 0.025))/2
    range.data <- tibble(
      n = n,
      p = p,
      error = range
    )
    n.p.sim <- rbind(n.p.sim,
                     range.data)
  }
}

ggplot(data = n.p.sim, aes(x = n, y = p, fill = error)) +
  geom_raster() +
  theme_minimal() +
  scale_fill_viridis_c() +
  theme_bw()

errors = tibble(
  n = numeric(),
  p = numeric(),
  error = numeric()
)

for(n in seq(100, 3000, 10)){
  for(p in seq(0.01, 0.99, 0.01)){
    curr = 1.96*((sqrt(n*p*(1-p)+1.96/4))/(n+1.96**2))
    curr.tibble = tibble(
      n = n,
      p = p,
      error = curr
    )
    errors = rbind(errors,
                   curr.tibble)
  }
}

ggplot(data = errors, aes(x = n, y = p, fill = error)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_bw()

################################################################################
# OPTIONAL CHALLENGE
################################################################################
n.p.sim.avg <- tibble(
  n = numeric(),
  p = numeric(),
  average.range = numeric()
)

for(n in seq(100, 3000, 10)){ #For n in the sequence {100, 200, ..., 30000}
  for(p in seq(0.01, 0.99, 0.01)){ #For p in the sequence {0.01, 0.02, ..., 0.99}
    range.data = c()    #Where ranges of the 100 simulations will be stored
    for(i in 1:100){ #With 100 simulations
      data = rbinom(1004, n, p) / n #Simulate the data
      resample = sample(data, size = 1004, replace = TRUE)
      range = (quantile(resample, 0.975) - quantile(resample, 0.025))/2 #Take the middle 95% of the data (range)
      range.data = c(range.data, range) #stores the ranges of the 100 simulations
    }
    avg.range = mean(range.data) #Take the average of the 100 simulations (in terms of range)
    n.p.sim.avg = rbind(n.p.sim.avg, #Place the results as a column in our final data frame
                        tibble (n=n, p=p, average.range = avg.range))
  }
}

ggplot(data = n.p.sim.avg, aes(x = n, y = p, fill = average.range)) + #plot our results using a raster plot
  geom_raster() + #raster plot command
  scale_fill_viridis_c() + #use color-blind friendly coloring
  theme_bw() #Makes the background white
