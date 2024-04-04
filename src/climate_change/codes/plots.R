### Function for rounding data.frames
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(length = 1))
  df[, nums] <- round(df[, nums], digits = digits)
  (df)
}

#======================================================================================================================#
### Plots
#======================================================================================================================#
triQ.on %>%
  filter(mw.year != Inf) %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year == Inf) %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year != Inf & chemical.classification == "TRI") %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year == Inf & chemical.classification == "TRI") %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year != Inf & chemical.classification == "PBT") %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year == Inf & chemical.classification == "PBT") %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year != Inf & chemical.classification == "DIOXIN") %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

triQ.on %>%
  filter(mw.year == Inf & chemical.classification == "DIOXIN") %>%
  group_by(year) %>%
  summarise(mean(total.air.emissions.onsite.intensity, na.rm = TRUE))

#======================================================================================================================#
plot.df <- data.frame(
  year = 2011:2017,
  air.emiss.onsite.treated.tri = c(0.102, 0.067, 0.122, 0.226, 0.192, 0.151, 0.117),
  air.emiss.onsite.ntreated.tri = c(0.164, 0.130, 0.112, 0.074, 0.068, 0.076, 0.096),
  # air.emiss.onsite.treated.tri = c(20.5, 14.6, 12.9, 16.5, 12.8, 10.7, 11.7),
  # air.emiss.onsite.ntreated.tri = c(23.1, 25.0, 24.4, 14.9, 13.1, 14.4, 15.2),
  air.emiss.onsite.treated.pbt = c(0.23, 0.26, 0.26, 0.13, 0.12, 0.10, 0.09),
  air.emiss.onsite.ntreated.pbt = c(0.58, 0.49, 0.37, 0.28, 0.23, 0.28, 0.19),
  air.emiss.onsite.treated.dio = c(0.02, 0.11, 0.02, 0.01, 0.01, 0.01, 0.06),
  air.emiss.onsite.ntreated.dio = c(0.03, 0.05, 0.03, 0.03, 0.01, 0.01, 0.00)
)

# air emissions onsite between treated and nevertreated group
colours_grid <- c("treated" = "blue2", "never-treated" = "red2")
air.emiss.onsite.tri.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = air.emiss.onsite.treated.tri, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.treated.tri, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = air.emiss.onsite.ntreated.tri, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.ntreated.tri, colour = "never-treated"), size = 1) +
  labs(y = "total air emission intensity (%)", title = "Total Air Emissions Intensity by Treatment Status (Onsite)") +
  geom_vline(xintercept = c(2014, 2015), linetype = "dashed", colour = "black") +
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
air.emiss.onsite.pbt.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = air.emiss.onsite.treated.pbt, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.treated.pbt, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = air.emiss.onsite.ntreated.pbt, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.ntreated.pbt, colour = "never-treated"), size = 1) +
  labs(y = "total air emission intensity (%)", title = "Total Air Emissions Intensity by Treatment Status (Onsite)") +
  geom_vline(xintercept = c(2014, 2015), linetype = "dashed", colour = "black") +
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
air.emiss.onsite.dio.plot <- ggplot(data = plot.df, aes(x = year)) +
  geom_line(mapping = aes(y = air.emiss.onsite.treated.dio, colour = "treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.treated.dio, colour = "treated"), size = 1) +
  geom_line(mapping = aes(y = air.emiss.onsite.ntreated.dio, colour = "never-treated"), size = 1) +
  geom_point(mapping = aes(y = air.emiss.onsite.ntreated.dio, colour = "never-treated"), size = 1) +
  labs(y = "total air emission intensity (%)", title = "Total Air Emissions Intensity by Treatment Status (Onsite)") +
  geom_vline(xintercept = c(2014, 2015), linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = as.character(plot.df$year), breaks = plot.df$year) +
  scale_colour_manual(values = colours_grid) +
  theme(
    legend.key.size = unit(x = 0.2, units = "in"),
    legend.key.width = unit(x = 0.35, units = "in"),
    legend.justification = c("right", "top"),  # Adjust legend position
    legend.position = c(x = 0.9, y = 0.9),  # Adjust x and y position
  )
air.emiss.onsite.tri.plot +
  air.emiss.onsite.pbt.plot +
  air.emiss.onsite.dio.plot

