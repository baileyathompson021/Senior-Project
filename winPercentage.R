library(tidyverse)

X2019_Everything$RALLOWPG = with(X2019_Everything, RALLOW / G)

## RELATIONSHIP BETWEEN RUNS & WINS - 2019
 # AVG Runs Scored in a Game vs Winning PCT 2019
plot(X2019_Everything$PCT, X2019_Everything$RUNSPG, xlab = "Winning PCT", ylab = "AVG Runs Scored PG", main = "Runs Per Game vs. Winning PCT")

 # Run Differential vs Winning PCT 2019
X2019_Everything$RD = with(X2019_Everything, R - RALLOW)
plot(X2019_Everything$RD, X2019_Everything$PCT, xlab = "Run Differential", ylab = "Winning PCT", main = "Winning PCT vs Run Differential")
  #strong, positive linear association/correlation between Run Differential & Winning PCT
  #Win more games if you limit amount of runs allowed
 #linear regression

X2018_Everything$RDPG = with(X2018_Everything, RUNSPG - RALLOWPG)

linfit = lm(PCT~RD, data=X2019_Everything)
linfit
 # WIN PCT = 0.48738 + 0.00144*RD
   # 0.487 -> if team has RD of 0, on avg they win 48.7% of games
   # 0.0014 -> for every 1 additional run in RD, 0.0014 increase in Win PCT on avg
 # Look at residuals
X2019_Everything$linearResiduals = residuals(linfit)
plot(X2019_Everything$RD, X2019_Everything$linearResiduals, xlab = "Run Differential", ylab = "Residual", main = "Run Differential vs. Residual")
abline (h=0, lty = 3)
  # residuals randomly dispersed around 0, linear model is appropriate

## CREATE LINFIT FOR RUNSPG AND RUNS ALLOWED PG!!!

## Estimated WIN PCT for all teams in 2019 based on Bill James Pythagorean Win PCT

X2019_Everything$ESTWP = with(X2019_Everything, (R^2)/(R^2+RALLOW^2))

  # Estimated percentage and actual percentage
plot(x2019_Everything$ESTWP, X2019_Everything$RD)

ggplot() +geom_point(data=X2019_Everything, aes(RD, ESTWP), color = 'purple', alpha = 0.3) +
   geom_smooth(data=X2019_Everything, aes(RD, ESTWP), color = 'purple', alpha= 0.3,color = 'purple',se= F)+
   labs(x= 'ff', y = 'ff', title = 'gdfd', subtitle = 'dssss', caption = 'bailey') +
   geom_point(data=X2019_Everything, aes(RD, PCT), color = 'olivedrab', alpha = 0.3)+
   geom_smooth(data=X2019_Everything, aes(RD, PCT), color = 'olivedrab', alpha = 0.3, color = 'blue', se = F)

## Estimated Win PCT using Pythagorean WIN PCT vs Actual Win PCT for 2019
ggplot(data = X2019_Everything, aes(PCT, ESTWP)) + geom_point() + geom_smooth(se = F, size = 2)


## predicting WIN PCT with above line vs Pythagorean Win PCT
X2019_Everything$fitted <- predict(linfit)
ggplot(data = X2019_Everything, aes(fitted, ESTWP)) + geom_smooth()

  # orange line is est. WP vs actual WP via Pythagorean
  # green line is Est. WP vs actual WP via linear regression
ggplot() +geom_point(data=X2019_Everything, aes(PCT, ESTWP), color = 'darkorange', alpha = 0.3) +
   geom_smooth(data=X2019_Everything, aes(PCT, ESTWP), color = 'darkorange', alpha= 0.3,color = 'darkorange',
               se= F) +
   labs(x= 'Actual WP 2019', y = 'Estimate WP 2019', title = 'Pythagorean WP vs Linear Fit WP', 
        caption = 'Figure 238') +
   geom_point(data=X2019_Everything, aes(PCT, fitted), color = 'olivedrab', alpha = 0.3) +
   geom_smooth(data=X2019_Everything, aes(PCT, fitted), color = 'olivedrab', alpha = 0.3, color = 'blue', se = F)


## X2018_Everything$fitted <- predict(linfit, X2018_Everything)


ggplot(data= X2019_Everything, aes(x= RUNSPG)) +geom_density(binwidth = 0.25, size =2)

## Show normal-ness of runs pg
ggplot( data= X2019_Everything, aes(x = RUNSPG)) +
   geom_histogram(aes( y= ..density..),binwidth = .5, size =1, color= 'white', fill = 'skyblue')+
   stat_function(fun = dnorm,size=1.5, args = list(mean = mean(X2019_Everything$RUNSPG), 
                                                   sd = sd(X2019_Everything$RUNSPG)), color = 'red') + 
   annotate(geom = 'text', x = 6.8, y = 0.33, label = 'mean = 4.321') + annotate(geom = 'text', x = 6.8, y = .30, 
                                                                                 label = 'sd = 1.047') + 
   geom_vline(xintercept = mean(X2019_Everything$RUNSPG), color = 'red', linetype = 'dashed', size = 1)
   
mean(X2019_Everything$RUNSPG)
sd(X2019_Everything$RUNSPG)


## Show normal-ness of runs allowed pg
ggplot( data= X2019_Everything, aes(x = RALLOWPG)) +
   geom_histogram(aes( y= ..density..),binwidth = .5, size =1, color= 'white', fill = 'skyblue')+
   stat_function(fun = dnorm,size=1.5, args = list(mean = mean(X2019_Everything$RALLOWPG), 
                                                   sd = sd(X2019_Everything$RALLOWPG)), color = 'red') + 
   annotate(geom = 'text', x = 7.8, y = 0.33, label = 'mean = 4.458') + annotate(geom = 'text', x = 7.8, y = .30, 
                                                                                 label = 'sd = 1.381') + 
   geom_vline(xintercept = mean(X2019_Everything$RALLOWPG), color = 'red', linetype = 'dashed', size = 1)

mean(X2019_Everything$RALLOWPG)
sd(X2019_Everything$RALLOWPG)


X2019_Everything$RDPG = with(X2019_Everything, RUNSPG - RALLOWPG)
