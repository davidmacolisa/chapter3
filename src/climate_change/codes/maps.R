#======================================================================================================================#
### Map of US border counties
#======================================================================================================================#
library(tidyverse)
library(usmap)
#======================================================================================================================#
### WD and Source file
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
# Get state data from usmap package
#======================================================================================================================#
# Map of border states
#======================================================================================================================#
dat_states <- triQc %>%
  select(c(facility.state, treated)) %>%
  rename(state = facility.state) %>%
  distinct()
dat_states <- dat_states[order(dat_states$state),]

border_state_map <- plot_usmap(
  data = dat_states,
  values = "treated",
  exclude = c("AK", "HI", "DC"),
  labels = T
) +
  scale_fill_gradient(low = "azure", high = "pink") +
  labs(title = "Treated vs. Control Border States") +
  theme(
    legend.position = c(x = 0.75, y = 0.08),
    legend.key.width = unit(x = 0.3, units = "in"),
    legend.key.height = unit(x = 0.3, units = "in"),
    legend.text = element_text(size = 10),
  ) +
  theme_bw()
#======================================================================================================================#
# Map of border counties
#======================================================================================================================#
treated_county <- triQc %>%
  select(treated.match, treated.cluster.id) %>%
  rename(state = treated.match, fips = treated.cluster.id) %>%
  distinct()
control_county <- triQc %>%
  select(control.match, control.cluster.id) %>%
  rename(state = control.match, fips = control.cluster.id) %>%
  distinct()
county_bd <- bind_rows(treated_county, control_county) %>%
  mutate(treated = case_when(fips %in% treated_county$fips ~ 1, TRUE ~ 0))

county_bd <- county_bd[order(county_bd$fips),]

border_county_map <- plot_usmap(
  data = county_bd,
  values = "treated",
  exclude = c("AK", "HI", "DC"),
  # labels = T
) +
  scale_fill_gradient(low = "azure", high = "pink") +
  labs(title = "Treated vs. Control Counties") +
  theme(
    legend.position = c(x = 0.75, y = 0.08),
    legend.key.width = unit(x = 0.3, units = "in"),
    legend.key.height = unit(x = 0.3, units = "in"),
    legend.text = element_text(size = 10),
  ) +
  theme_bw()

pdf(file = "./Thesis/chapter3/src/climate_change/latex/border_state_map.pdf", width = 17, height = 10)
border_state_map
dev.off()

pdf(file = "./Thesis/chapter3/src/climate_change/latex/border_county_map.pdf", width = 17, height = 10)
border_county_map
dev.off()
#======================================================================================================================#