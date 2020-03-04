library(tidyverse)

## RELATIONSHIP BETWEEN RUNS & WINS - Most recent 2019 season
 # AVG Runs Scored in a Game vs Winning PCT 2019
plot(X2019_Everything$PCT, X2019_Everything$RUNSPG, xlab = "Winning PCT", ylab = "AVG Runs Scored PG", main = "Runs Per Game vs. Winning PCT")

 # Run Differential vs Winning PCT 2019
X2019_Everything$RD = with(X2019_Everything, R - RALLOW)
plot(X2019_Everything$RD, X2019_Everything$PCT, xlab = "Run Differential", ylab = "Winning PCT", main = "Winning PCT vs Run Differential")
  #strong, positive linear association/correlation between Run Differential & Winning PCT
  #Win more games if you limit amount of runs allowed
 #linear regression

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