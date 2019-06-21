library(dslabs)
library(tidyverse)
library(haven)

galton_heights <- read_dta('galton-stata11.dta')
str(galton_heights)

#Getting the average of sons with whose fathers' height is around 72 inches
conditional_avg <- galton_heights %>% filter(gender == "M", round(father) == 72) %>% summarize(avg = mean(height)) %>% .$avg
conditional_avg

#plot(father, son)
galton_heights %>% mutate(father_strata = factor(round(father))) %>%  filter(gender == "M") %>% ggplot(aes(father_strata, height)) + geom_boxplot() + geom_point()

#plotting conditional sons' height due to fathers' height
galton_heights %>% mutate(father = round(father)) %>% group_by(father) %>% filter(gender == "M") %>% summarise(son_conditional_avg = mean(height)) %>% ggplot(aes(father, son_conditional_avg)) + geom_point()

#correlation
r <- galton_heights %>% filter(gender == "M") %>% summarise(r = cor(father, height)) %>% .$r

#plottoing the relation between heights and the correlation regression
galton_heights %>% mutate(father = round(father)) %>% group_by(father) %>% filter(gender == "M") %>% summarise(son = mean(height)) %>% mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) + geom_point() + geom_abline(intercept = 0, slope = r)

#parameters of linear regression y = b + m*x
male <- galton_heights %>% filter(gender == "M")
mu_x <- mean(galton_heights$father)
mu_y <- mean(male$height)
s_x <- sd(galton_heights$father)
s_y <- sd(male$height)
r <- cor(galton_heights$father, male$height)
m <- r*s_y / s_x
b <- mu_y - m*mu_x

#plot
galton_heights %>% filter(gender == "M") %>% ggplot(aes(father, height)) + geom_point(alpha = 0.5) + geom_abline(intercept = b, slope = m)


#Stratifying sons' height by the standardized fathers' height
galton_heights %>% filter(gender == "M") %>% mutate(z_father = round((father - mean(father))/sd(father))) %>% filter(z_father %in% -2:2) %>%
  ggplot() + stat_qq(aes(sample = height)) + facet_wrap(~z_father)

#Variance 
#When two variables follow a bivariate normal distribution, the variation 
#explained can be calculated as  𝜌2×100 
vari <- r^2
vari

#linear regression
lm(height ~ father, data = male)

#least squares estimate
rss <- function(beta0, beta1, data) {
  resid <- male$height - (beta0 + beta1 * male$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + geom_line(aes(beta1, rss), col=2)

#Monte Carlo simulation
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(male, N, replace = TRUE) %>% 
    lm(height ~ father, data = .) %>% .$coef 
})

sample_n(male, N, replace = TRUE) %>% 
  lm(male ~ father, data = .) %>% summary

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse %>% summarize(cor(beta_0, beta_1))

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(bins = 20, binwidth = 1, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(bins = 20, binwidth = 0.01, color = "black")  
grid.arrange(p1, p2, ncol = 2)  

#If we standardize the father heights
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(male, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(height ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,])

#Drawing graphs with confidence interval and different ways to get the predictions
male %>% ggplot(aes(father, height)) + geom_point() +
  geom_smooth(method = "lm")

male %>% mutate(Y_hat = predict(lm(height ~ father, data =  .))) %>%
  ggplot(aes(father, Y_hat)) + geom_line()

fit <- male %>% lm(height ~ father, data =  .)
Y_hat = predict(fit, se.fit = TRUE)
names(Y_hat)

model <- lm(height ~ father, data = male)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = male$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = male, aes(x = father, y = height))

  