

## James-Stein Estimator for Runs Scored and Runs allowed
   # Then used to find a predicted win percentage of NCAA teams after first ~10 games of season. 


k = 295 # teams
n = 10  # first games

## RUNS PG
m = mean(X2019_10G$RPG)   # mean
s = sd(X2019_10G$RPG)     # standard deviation
V = (s^2)/n               # variance
  
max(X2019_10G$RPG)        # max RPG value
min(X2019_10G$RPG)        # min RPG value


X2019_10G <- X2019_10G %>%
  mutate(residual = RPG - mean(X2019_10G$RPG),
         square = residual**2)

sum(X2019_10G$square)

B = ((k-3)*V)/sum(X2019_10G$square)    # B = .0993
((295-3)*.318)/935.767

(V*(1-B))/B                            # A = 2.886 -> variance of team theta i's

sqrt((V*(1-B))/B)                      # standard deviation of team theta i's

## STEIN ESTIMATE for RPG!!
X2019_10G$STEINRPG = with(X2019_10G, (B*m)+(1-B)*RPG)


## RUNS ALLOWED PG
m1 = mean(X2019_10G$RALLOWPG)   # mean
s1 = sd(X2019_10G$RALLOWPG)     # standard deviation
V1 = (s1^2)/n                   # variance

max(X2019_10G$RALLOWPG)        # max RALLOW PG value
min(X2019_10G$RALLOWPG)        # min RALLOW PG value


X2019_10G <- X2019_10G %>%
  mutate(residual1 = RALLOWPG - mean(X2019_10G$RALLOWPG),
         square1 = residual1**2)

sum(X2019_10G$square1)

B1 = ((k-3)*V1)/sum(X2019_10G$square1)    # B = .0993
((295-3)*.404)/1186.812

(V1*(1-B1))/B1                            # A = 3.661 -> variance of team theta i's

sqrt((V1*(1-B1))/B1)                      # standard deviation of team theta i's

## STEIN ESTIMATE for RALLOW PG!!
X2019_10G$STEINRALLOWPG = with(X2019_10G, (B1*m1)+(1-B1)*RALLOWPG)

