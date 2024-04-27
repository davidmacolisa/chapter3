### Function for rounding data.frames
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(length = 1))
  df[, nums] <- round(df[, nums], digits = digits)
  (df)
}

#======================================================================================================================#
### librarys
#======================================================================================================================#
library(patchwork)
#======================================================================================================================#
### Plots
#======================================================================================================================#
triQc %>%
  group_by(year, treated) %>%
  summarise(air = mean(total.air.emissions.onsite.intensity, na.rm = T) %>% unique()) %>%
  print(n = nrow(.))

triQc %>%
  group_by(year, treated) %>%
  summarise(water = mean(total.surface.water.discharge.onsite.intensity, na.rm = T) %>% unique()) %>%
  print(n = nrow(.))

triQc %>%
  group_by(year, treated) %>%
  summarise(water = mean(total.land.releases.onsite.intensity, na.rm = T) %>% unique()) %>%
  print(n = nrow(.))


#======================================================================================================================#
plot.df <- data.frame(
  year = 2011:2017,
  air.emiss.onsite.treated = c(0.199, 0.118, 0.373, 0.457, 0.388, 0.408, 0.579),
  air.emiss.onsite.ntreated = c(0.222, 0.223, 0.294, 0.175, 0.184, 0.172, 0.206),
  surface.water.discharge.onsite.treated = c(0.333, 0.020, 0.051, 0.064, 0.056, 0.056, 0.172),
  surface.water.discharge.onsite.ntreated = c(0.014, 0.017, 0.024, 0.013, 0.008, 0.010, 0.017),
  total.land.releases.onsite.treated = c(0.001, 0.004, 0.006, 0.030, 0.012, 0.006, 0.015),
  total.land.releases.onsite.ntreated = c(0.038, 0.025, 0.036, 0.023, 0.024, 0.048, 0.029)
)

# air emissions onsite intensity between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
air.emiss.onsite.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = air.emiss.onsite.treated, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.treated, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = air.emiss.onsite.ntreated, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.ntreated, colour = "never-treated"), size = 1) +
  labs(y = "total air emission intensity (%)", title = "Total Air Emissions Intensity by Treatment Status (Onsite)") +
  geom_vline(xintercept = c(2014, 2015, 2017), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.4),  # Adjust x and y position
  )

# air emissions onsite between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
surface.water.discharge.onsite.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = surface.water.discharge.onsite.treated, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = surface.water.discharge.onsite.treated, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = surface.water.discharge.onsite.ntreated, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = surface.water.discharge.onsite.ntreated, colour = "never-treated"), size = 1) +
  labs(y = "total surface water discharge intensity (%)", title = "Total Surface Water Discharge Intensity by Treatment Status (Onsite)") +
  geom_vline(xintercept = c(2014, 2015, 2017), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.9),  # Adjust x and y position
  )

# air emissions onsite between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
total.land.releases.onsite.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = total.land.releases.onsite.treated, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = total.land.releases.onsite.treated, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = total.land.releases.onsite.ntreated, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = total.land.releases.onsite.ntreated, colour = "never-treated"), size = 1) +
  labs(y = "total land releases intensity (%)", title = "Total Land Releases Intensity by Treatment Status (Onsite)") +
  geom_vline(xintercept = c(2014, 2015, 2017), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.9),  # Adjust x and y position
  )

air.emiss.onsite.plot +
  surface.water.discharge.onsite.plot +
  total.land.releases.onsite.plot

#======================================================================================================================#
### Plots---Offsite
#======================================================================================================================#
triQc_off %>%
  group_by(year, treated) %>%
  summarise(water = mean(l.total.land.releases.offsite.intensity, na.rm = T) %>% unique()) %>%
  print(n = nrow(.))

triQc_off %>%
  group_by(year, treated) %>%
  summarise(water = mean(l.total.releases.offsite.intensity, na.rm = T) %>% unique()) %>%
  print(n = nrow(.))
#======================================================================================================================#
plot.df <- data.frame(
  year = 2011:2017,
  # air.emiss.offsite.treated = c(0.199, 0.118, 0.373, 0.457, 0.388, 0.408, 0.579),
  # air.emiss.offsite.ntreated = c(0.222, 0.223, 0.294, 0.175, 0.184, 0.172, 0.206),
  # surface.water.discharge.offsite.treated = c(0.333, 0.020, 0.051, 0.064, 0.056, 0.056, 0.172),
  # surface.water.discharge.offsite.ntreated = c(0.014, 0.017, 0.024, 0.013, 0.008, 0.010, 0.017),
  total.land.releases.offsite.treated = c(0.175, 0.049, 0.086, 0.097, 0.097, 0.077, 0.101),
  total.land.releases.offsite.ntreated = c(0.090, 0.057, 0.060, 0.066, 0.076, 0.114, 0.100)
)

# Land releases offsite intensity between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
total.land.releases.offsite.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = total.land.releases.offsite.treated, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = total.land.releases.offsite.treated, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = total.land.releases.offsite.ntreated, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = total.land.releases.offsite.ntreated, colour = "never-treated"), size = 1) +
  labs(y = "total land releases intensity", title = "Total Land Releases Intensity by Treatment Status (Offsite)") +
  geom_vline(xintercept = c(2014, 2015, 2017), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.4),  # Adjust x and y position
  )

# air emissions offsite between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
surface.water.discharge.offsite.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = surface.water.discharge.offsite.treated, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = surface.water.discharge.offsite.treated, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = surface.water.discharge.offsite.ntreated, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = surface.water.discharge.offsite.ntreated, colour = "never-treated"), size = 1) +
  labs(y = "total surface water discharge intensity (%)", title = "Total Surface Water Discharge Intensity by Treatment Status (Offsite)") +
  geom_vline(xintercept = c(2014, 2015, 2017), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.9),  # Adjust x and y position
  )

# air emissions offsite between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
total.land.releases.offsite.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = total.land.releases.offsite.treated, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = total.land.releases.offsite.treated, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = total.land.releases.offsite.ntreated, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = total.land.releases.offsite.ntreated, colour = "never-treated"), size = 1) +
  labs(y = "total land releases intensity (%)", title = "Total Land Releases Intensity by Treatment Status (Offsite)") +
  geom_vline(xintercept = c(2014, 2015, 2017), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.9),  # Adjust x and y position
  )

air.emiss.offsite.plot +
  surface.water.discharge.offsite.plot +
  total.land.releases.offsite.plot

