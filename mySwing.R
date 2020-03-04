library(tidyverse)
library(plotly)

## MY HITTING
# Exit Velocity vs Horizonal Distance in Hitting
plot(soft$Dist, soft$Velo, xlab = "Distance", ylab = "Exit Velocity", main = "Exit Velocity vs. Distance")
# Pretty linear relationship, doesn't seem to be a relationship in this case.

# Let's add Launch Angle
# Adding Launch Angle




#3D Plotting of where I hit each ball
soft$Hit[which(soft$Hit == 1)] <- 'Hit'
soft$Hit[which(soft$Hit == 0)] <- 'Out'
soft$Hit <- as.factor(soft$Hit)

fig <- plot_ly(soft, x = ~POIX, y = ~POIY, z = ~POIZ, color = ~Hit, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig<- fig %>% layout(scene = list(xaxis = list(title = 'POI X'), yaxis = list(title = 'POI Y'), zaxis = list(title = 'POI Z')))
fig



fig <- plot_ly(soft, x = ~POIX, y = ~POIY, z = ~POIZ, marker = list(color = ~Velo, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'POI X'),
                                   yaxis = list(title = 'POI Y'),
                                   zaxis = list(title = 'POI Z')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Exit Velocity',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig
