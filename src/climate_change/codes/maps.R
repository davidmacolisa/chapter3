#======================================================================================================================#
### Map of US border counties
#======================================================================================================================#
### WD and Source file
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
source(file = "./Thesis/chapter3/src/climate_change/codes/did_design_onsite.R", echo = T)
#======================================================================================================================#
# Load necessary libraries
library(usmap)

# Plot all counties of the U.S.
p <- plot_usmap(regions = "counties") +
  labs(title = "U.S. counties",
       subtitle = "This is a blank map of the United States.") +
  theme(panel.background = element_blank())

print(p)

ggplot() +
  geom_polygon(data = triQc, aes(x = long, y = lat, group = treated, fill = treated.match), color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = "right")
