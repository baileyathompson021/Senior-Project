library(tidyverse)
library(plotly)
library(ggplot2)

X2019_10G <- X2019_10G %>%
  rename(Team = 'X1')

## EST WIN PCT CALCULATED BASED ON STEIN ESTIMATES FOR RPG AND RUNS ALLOW PG

  #USING PYTHAGOREAN WIN PCT
X2019_10G$PYTHWINPCT = with(X2019_10G, (R^2)/(R^2+RALLOW^2))
X2019_10G$PYTHWINPCTPG = with(X2019_10G, (RPG^2)/(RPG^2+RALLOWPG^2))
  ## nearly same


  # USING LINFIT FROM 2019 ACTUAL SEASON
X2019_10G$RD = with (X2019_10G, STEINRPG - STEINRALLOWPG)   # Estimated Run Differential using Stein Estimator 2019

#### linfit rdpg

X2019_10G$ESTWPRD = with(X2019_10G, 0.49915+(.08006* (RD)))


#######
linfit1 = lm(PCT~RD, data=X2018_Everything)                 # linfit from 2018
linfit1
## .48765 + .001524*RD
# 0.48765 -> if team has RD of 0, on avg they win 48.7% of games
# 0.001524 -> for every 1 additional run in RD, 0.001524 increase in Win PCT on avg

X2019_10G$ESTWPLIN = with(X2019_10G, .487650 + (.001524 * RD))

######
write.csv(X2019_Everything, "everything.csv", row.names = FALSE)
write.csv(X2019_10G, "tengames.csv", row.names = FALSE)
#######

## RUNS PG Estimate vs Actual RUNS PG for Season (2019)

tryme <-data.frame(X2019_Everything$Team,X2019_Everything$RUNSPG,pleasework$STEINRPG)

gg1 <- ggplot(data=tryme, aes(X2019_Everything.RUNSPG, pleasework.STEINRPG, 
                            text = paste("<br>STE: ", round(pleasework.STEINRPG, 2), "<br>ACT: ",
                                         round(X2019_Everything.RUNSPG, 2), "<br>", 
                                         X2019_Everything.Team))) + geom_point()
ggplotly(gg1, tooptip = "text")  


## RUNS ALLOW PG Estimate vs Actual RUNS ALLOW PG for Season(2019)

try2 <-data.frame(X2019_Everything$Team,X2019_Everything$RALLOWPG,pleasework$STEINRALLOWPG)

g1 <- ggplot(data=try2, aes(X2019_Everything.RALLOWPG, pleasework.STEINRALLOWPG, 
                            text = paste("<br>STE: ", round(pleasework.STEINRALLOWPG, 2), "<br>ACT: ",
                                         round(X2019_Everything.RALLOWPG, 2), "<br>", 
                                         X2019_Everything.Team))) + geom_point() + labs(x= 'Actual RALLOWPG', 
                                                                                        y = 'Stein RALLOWPG', 
                                                                                        title = 'Stein Est. v Actual RALLOWPG', 
                                                                                        caption = 'Figure 238')
ggplotly(g1, tooltip = "text")


## Estimated WP after first 10 games vs actual Win PCT for season

try3 <-data.frame(X2019_Everything$Team, pleasework$Team,X2019_Everything$PCT, pleasework$PYTHWINPCT)

g2 <- ggplot(data=try3, aes(X2019_Everything.PCT, pleasework.PYTHWINPCT,
                            text = paste("<br>PYT: ", round(pleasework.PYTHWINPCT, 3), "<br>ACT: ",
                                         X2019_Everything.PCT, "<br>", 
                                         pleasework.Team))) + geom_point()
ggplotly(g2, tooltip = "text")
