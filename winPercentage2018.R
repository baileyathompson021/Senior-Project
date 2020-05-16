library(tidyverse)

X2018_Everything$RALLOWPG = with(X2018_Everything, RALLOW / G)

## RELATIONSHIP BETWEEN RUNS & WINS - 2018
# AVG Runs Scored in a Game vs Winning PCT 2018
plot(X2018_Everything$PCT, X2018_Everything$RUNSPG, xlab = "Winning PCT", ylab = "AVG Runs Scored PG", main = "Runs Per Game vs. Winning PCT")

# Run Differential vs Winning PCT 2018
X2018_Everything$RD = with(X2018_Everything, R - RALLOW)
plot(X2018_Everything$RD, X2018_Everything$PCT, xlab = "Run Differential", ylab = "Winning PCT", main = "Winning PCT vs Run Differential")
#strong, positive linear association/correlation between Run Differential & Winning PCT
#Win more games if you limit amount of runs allowed
#linear regression


## linear regression with RD PG

X2018_Everything$RDPG = with(X2018_Everything, RUNSPG - RALLOWPG)
linfit2 = lm(PCT~RDPG, data = X2018_Everything)
linfit2
   ## .49915 + .08006*RDPG
X2018_Everything$ESTWPRDPG = with(X2018_Everything, 0.49915+(.08006* (RDPG))) # I think this one is better

####
# IGNORE
linfit = lm(PCT~RD, data=X2018_Everything)
linfit
# WIN PCT = 0.48765 + 0.001524*RD
# 0.48765 -> if team has RD of 0, on avg they win 48.7% of games
# 0.001524 -> for every 1 additional run in RD, 0.001524 increase in Win PCT on avg
# Look at residuals
X2018_Everything$linearResiduals = residuals(linfit)
plot(X2018_Everything$RD, X2018_Everything$linearResiduals, xlab = "Run Differential", ylab = "Residual", main = "Run Differential vs. Residual")
abline (h=0, lty = 3)
#END IGNORE (LINFIT -> USE RESIDUAL)
# residuals randomly dispersed around 0, linear model is appropriate

## Estimated WIN PCT for all teams in 2018 based on my linfit model -> IGNORE
X2018_Everything$ESTWPLIN <- predict(linfit)
ggplot(data = X2018_Everything, aes(ESTWPLIN, PCT)) + geom_smooth()


## Estimated WIN PCT for all teams in 2018 based on Bill James Pythagorean Win PCT

X2018_Everything$ESTWP = with(X2018_Everything, (R^2)/(R^2+RALLOW^2))

# Estimated percentage and actual percentage (Bill James)
plot(x2018_Everything$ESTWP, X2018_Everything$PCT)

## Estimated Win PCT using Pythagorean WIN PCT vs Actual Win PCT for 2019
ggplot(data = X2018_Everything, aes(PCT, ESTWP)) + geom_point() + geom_smooth(se = F, size = 2)


## predicting WIN PCT with above line vs Pythagorean Win PCT
ggplot(data = X2018_Everything, aes(ESTWPRDPG, ESTWP)) + geom_smooth()

# orange line is Est. WP (Pythagorean) vs actual WP
# green line is Est. WP (Linear Regression) vs actual WP
ggplot() +geom_point(data=X2018_Everything, aes(PCT, ESTWP), color = 'darkorange', alpha = 0.3) +
  geom_smooth(data=X2018_Everything, aes(PCT, ESTWP), color = 'darkorange', alpha= 0.3,color = 'darkorange',
              se= F) +
  labs(x= 'Actual WP 2019', y = 'Estimate WP 2019', title = 'Pythagorean WP vs Linear Fit WP Fit', 
       caption = 'Figure 238') +
  geom_point(data=X2018_Everything, aes(PCT, ESTWPRDPG), color = 'olivedrab', alpha = 0.3) +
  geom_smooth(data=X2018_Everything, aes(PCT, ESTWPRDPG), color = 'olivedrab', alpha = 0.3, color = 'blue', se = F) + 
  annotate(geom = 'text', x = .8, y = 0.33, label = 'Pythagorean WP', color = 'darkorange') + 
  annotate(geom = 'text', x = .8, y = 0.38, label = 'Linear Regression', color = 'olivedrab')


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

