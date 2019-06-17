library(dslabs)
library(tidyverse)

galton_heights <- read_dta('galton-stata11.dta')
str(galton_heights)

#Getting the average of sons with whose fathers' height is around 72 inches
conditional_avg <- galton_heights %>% filter(round(father) == 72) %>% summarize(avg = mean(height)) %>% .$avg
conditional_avg

#plot(father, son)
galton_heights %>% mutate(father_strata = factor(round(father))) %>% ggplot(aes(father_strata, height)) + geom_boxplot() + geom_point()

#plotting conditional sons' height due to fathers' height
galton_heights %>% mutate(father = round(father)) %>% group_by(father) %>% summarise(son_conditional_avg = mean(height)) %>% ggplot(aes(father, son_conditional_avg)) + geom_point()

#correlation
r <- galton_heights %>% summarise(r = cor(father, height)) %>% .$r

#plottoing the relation between heights and the correlation regression
galton_heights %>% mutate(father = round(father)) %>% group_by(father) %>% summarise(son = mean(height)) %>% mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) + geom_point() + geom_abline(intercept = 0, slope = r)

#parameters of linear regression y = b + m*x
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$height)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$height)
r <- cor(galton_heights$father, galton_heights$height)
m <- r*s_y / s_x
b <- mu_y - m*mu_x

#plot
galton_heights %>% ggplot(aes(father, height)) + geom_point(alpha = 0.5) + geom_abline(intercept = b, slope = m)


#Stratifying sons' height by the standardized fathers' height
galton_heights %>% mutate(z_father = round((father - mean(father))/sd(father))) %>% filter(z_father %in% -2:2) %>%
  ggplot() + stat_qq(aes(sample = height)) + facet_wrap(~z_father)

#Variance 
#When two variables follow a bivariate normal distribution, the variation 
#explained can be calculated as  𝜌2×100 
vari <- r^2*100
vari
