#======================================================================================================================#
### PhD Chapter 3
### Unforeseen Minimum Wage Consequences
### 30 October 2023
### Use Regression Discontinuity Analysis
#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(fixest)
library(did)
library(TwoWayFEWeights)
library(ggplot2)
library(ggtext)
library(patchwork)
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### Not yet treated group
#======================================================================================================================#
triQc_nyt <- triQc %>% filter(ch.year != Inf)
table(triQc_nyt$year, triQc_nyt$ch.year)
table(triQc$year, triQc$ch.year)
#======================================================================================================================#
### Controls
#======================================================================================================================#
controls <- ~
  gdppc.1 +
    #   annual.avg.estabs.1 +
    #   cpi.1 +
    # federal.facility +
    produced.chem.facility +
    # imported.chem.facility +
    chemical.formulation.component +
    chemical.article.component +
    chemical.manufacturing.aid +
    chemical.ancilliary.use +
    # production.ratio.activity.index +
    maxnum.chem.onsite +
    clean.air.act.chems +
    hap.chems +
    pbt.chems +
    facility.id.fe +
    border.county.fe +
    chemical.id.fe
    # chemical.year.fe
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_total_releases <- att_gt(
  yname = "l.total.releases.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_total_releases)
sdid_total_releases_es <- aggte(
  sdid_total_releases,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_total_releases_es)
ggdid(sdid_total_releases_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_total_releases_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_total_releases_es$egt, ")"),
  event.time = sdid_total_releases_es$egt,
  estimate = sdid_total_releases_es$att.egt,
  std.error = sdid_total_releases_es$se.egt,
  conf.low = sdid_total_releases_es$att.egt - sdid_total_releases_es$crit.val.egt * sdid_total_releases_es$se.egt,
  conf.high = sdid_total_releases_es$att.egt + sdid_total_releases_es$crit.val.egt * sdid_total_releases_es$se.egt,
  point.conf.low = sdid_total_releases_es$att.egt - stats::qnorm(1 - sdid_total_releases_es$DIDparams$alp / 2) *
    sdid_total_releases_es$se.egt,
  point.conf.high = sdid_total_releases_es$att.egt + stats::qnorm(1 - sdid_total_releases_es$DIDparams$alp / 2) *
    sdid_total_releases_es$se.egt
)

# First option
es_total_releases_plot <- ggplot(data = es_total_releases_nyt,
                                 mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_total_releases_nyt$conf.low,
    es_total_releases_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:3) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-1.5, 1.5) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: 0.2911*** (0.1289)", color = "black") +
  ggtitle(label = "Total onsite releases intensity") +
  theme_bw()

es_total_releases_plot

summary(aggte(sdid_total_releases, type = 'group'))
sdid_total_releases_grp <- ggdid(aggte(sdid_total_releases, type = 'group'))
#======================================================================================================================#
### Onsite: Total air emissions intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_air <- att_gt(
  yname = "l.total.air.emissions.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_air)
aggte(sdid_air, type = "simple")
sdid_air_es <- aggte(
  sdid_air,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_air_es)
ggdid(sdid_air_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_air_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_air_es$egt, ")"),
  event.time = sdid_air_es$egt,
  estimate = sdid_air_es$att.egt,
  std.error = sdid_air_es$se.egt,
  conf.low = sdid_air_es$att.egt - sdid_air_es$crit.val.egt * sdid_air_es$se.egt,
  conf.high = sdid_air_es$att.egt + sdid_air_es$crit.val.egt * sdid_air_es$se.egt,
  point.conf.low = sdid_air_es$att.egt - stats::qnorm(1 - sdid_air_es$DIDparams$alp / 2) *
    sdid_air_es$se.egt,
  point.conf.high = sdid_air_es$att.egt + stats::qnorm(1 - sdid_air_es$DIDparams$alp / 2) *
    sdid_air_es$se.egt
)

# First option
es_air_plot <- ggplot(data = es_air_nyt,
                      mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-0.7, 1.7) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: 0.5237*** (0.1490)", color = "black") +
  ggtitle(label = "Total onsite air emissions intensity") +
  theme_bw()

es_air_plot

summary(aggte(sdid_air, type = 'group'))
sdid_air_grp <- ggdid(aggte(sdid_air, type = 'group'))
#======================================================================================================================#
### Onsite: Total point air emissions intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_point_air <- att_gt(
  yname = "l.total.point.air.emissions.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_point_air)
sdid_point_air_es <- aggte(
  sdid_point_air,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_point_air_es)
ggdid(sdid_point_air_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_point_air_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_point_air_es$egt, ")"),
  event.time = sdid_point_air_es$egt,
  estimate = sdid_point_air_es$att.egt,
  std.error = sdid_point_air_es$se.egt,
  conf.low = sdid_point_air_es$att.egt - sdid_point_air_es$crit.val.egt * sdid_point_air_es$se.egt,
  conf.high = sdid_point_air_es$att.egt + sdid_total_releases_es$crit.val.egt * sdid_point_air_es$se.egt,
  point.conf.low = sdid_point_air_es$att.egt - stats::qnorm(1 - sdid_point_air_es$DIDparams$alp / 2) *
    sdid_point_air_es$se.egt,
  point.conf.high = sdid_point_air_es$att.egt + stats::qnorm(1 - sdid_point_air_es$DIDparams$alp / 2) *
    sdid_point_air_es$se.egt
)

# First option
es_point_air_plot <- ggplot(data = es_point_air_nyt,
                            mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_point_air_nyt$conf.low,
    es_point_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-1, 1.2) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: 0.4436*** (0.1223)", color = "black") +
  ggtitle(label = "Total onsite point air emissions intensity") +
  theme_bw()

es_point_air_plot

summary(aggte(sdid_point_air, type = 'group'))
sdid_point_air_grp <- ggdid(aggte(sdid_point_air, type = 'group'))
sdid_point_air_grp
#======================================================================================================================#
### Onsite: Total fugitive air emissions intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_fug_air <- att_gt(
  yname = "l.total.fug.air.emissions.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_fug_air)
sdid_fug_air_es <- aggte(
  sdid_fug_air,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_fug_air_es)
ggdid(sdid_fug_air_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_fug_air_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_fug_air_es$egt, ")"),
  event.time = sdid_fug_air_es$egt,
  estimate = sdid_fug_air_es$att.egt,
  std.error = sdid_fug_air_es$se.egt,
  conf.low = sdid_fug_air_es$att.egt - sdid_fug_air_es$crit.val.egt * sdid_fug_air_es$se.egt,
  conf.high = sdid_fug_air_es$att.egt + sdid_fug_air_es$crit.val.egt * sdid_fug_air_es$se.egt,
  point.conf.low = sdid_fug_air_es$att.egt - stats::qnorm(1 - sdid_fug_air_es$DIDparams$alp / 2) *
    sdid_fug_air_es$se.egt,
  point.conf.high = sdid_fug_air_es$att.egt + stats::qnorm(1 - sdid_fug_air_es$DIDparams$alp / 2) *
    sdid_fug_air_es$se.egt
)

# First option
es_fug_air_plot <- ggplot(data = es_fug_air_nyt,
                          mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-0.7, 1.2) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.3, label = "ATT: 0.0937 (0.2023)", color = "black") +
  ggtitle(label = "Total onsite fugitive air emissions intensity") +
  theme_bw()

es_fug_air_plot

summary(aggte(sdid_fug_air, type = 'group'))
sdid_fug_air_grp <- ggdid(aggte(sdid_fug_air, type = 'group'))
#======================================================================================================================#
### Onsite: Total surface water discharge intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_water_disc <- att_gt(
  yname = "l.total.surface.water.discharge.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_water_disc)
sdid_water_disc_es <- aggte(
  sdid_water_disc,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_water_disc_es)
ggdid(sdid_water_disc_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_water_disc_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_water_disc_es$egt, ")"),
  event.time = sdid_water_disc_es$egt,
  estimate = sdid_water_disc_es$att.egt,
  std.error = sdid_water_disc_es$se.egt,
  conf.low = sdid_water_disc_es$att.egt - sdid_water_disc_es$crit.val.egt * sdid_water_disc_es$se.egt,
  conf.high = sdid_water_disc_es$att.egt + sdid_water_disc_es$crit.val.egt * sdid_water_disc_es$se.egt,
  point.conf.low = sdid_water_disc_es$att.egt - stats::qnorm(1 - sdid_water_disc_es$DIDparams$alp / 2) *
    sdid_water_disc_es$se.egt,
  point.conf.high = sdid_water_disc_es$att.egt + stats::qnorm(1 - sdid_water_disc_es$DIDparams$alp / 2) *
    sdid_water_disc_es$se.egt
)

# First option
es_water_disc_plot <- ggplot(data = es_water_disc_nyt,
                             mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-0.9, 0.7) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = 0.2, label = "ATT: -0.2514 (0.1377)", color = "black") +
  ggtitle(label = "Total onsite surface water discharge intensity") +
  theme_bw()

es_water_disc_plot

summary(aggte(sdid_water_disc, type = 'group'))
sdid_water_disc_grp <- ggdid(aggte(sdid_water_disc, type = 'group'))
#======================================================================================================================#
### Onsite: Number of receiving streams
#======================================================================================================================#
start_time <- Sys.time()
sdid_receiving_streams <- att_gt(
  yname = "total.num.receiving.streams.onsite",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_receiving_streams)
sdid_receiving_streams_es <- aggte(
  sdid_receiving_streams,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_receiving_streams_es)
ggdid(sdid_receiving_streams_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_receiver_streams_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_receiving_streams_es$egt, ")"),
  event.time = sdid_receiving_streams_es$egt,
  estimate = sdid_receiving_streams_es$att.egt,
  std.error = sdid_receiving_streams_es$se.egt,
  conf.low = sdid_receiving_streams_es$att.egt - sdid_receiving_streams_es$crit.val.egt * sdid_receiving_streams_es$se.egt,
  conf.high = sdid_receiving_streams_es$att.egt + sdid_receiving_streams_es$crit.val.egt * sdid_receiving_streams_es$se.egt,
  point.conf.low = sdid_receiving_streams_es$att.egt - stats::qnorm(1 - sdid_receiving_streams_es$DIDparams$alp / 2) *
    sdid_receiving_streams_es$se.egt,
  point.conf.high = sdid_receiving_streams_es$att.egt + stats::qnorm(1 - sdid_receiving_streams_es$DIDparams$alp / 2) *
    sdid_receiving_streams_es$se.egt
)

# First option
es_receiver_streams_plot <- ggplot(data = es_receiver_streams_nyt,
                                   mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-1.5, 0.3) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: -0.7135*** (0.0863)", color = "black") +
  ggtitle(label = "Total number of receiving streams, onsite") +
  theme_bw()

es_receiver_streams_plot

summary(aggte(sdid_receiving_streams, type = 'group'))
sdid_receiving_streams_grp <- ggdid(aggte(sdid_receiving_streams, type = 'group'))
#======================================================================================================================#
### Onsite: Total land releases intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_land_releases <- att_gt(
  yname = "l.total.land.releases.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_land_releases)
sdid_land_releases_es <- aggte(
  sdid_land_releases,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_land_releases_es)
ggdid(sdid_land_releases_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_land_releases_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_land_releases_es$egt, ")"),
  event.time = sdid_land_releases_es$egt,
  estimate = sdid_land_releases_es$att.egt,
  std.error = sdid_land_releases_es$se.egt,
  conf.low = sdid_land_releases_es$att.egt - sdid_land_releases_es$crit.val.egt * sdid_land_releases_es$se.egt,
  conf.high = sdid_land_releases_es$att.egt + sdid_land_releases_es$crit.val.egt * sdid_land_releases_es$se.egt,
  point.conf.low = sdid_land_releases_es$att.egt - stats::qnorm(1 - sdid_land_releases_es$DIDparams$alp / 2) *
    sdid_land_releases_es$se.egt,
  point.conf.high = sdid_land_releases_es$att.egt + stats::qnorm(1 - sdid_land_releases_es$DIDparams$alp / 2) *
    sdid_land_releases_es$se.egt
)

# First option
es_land_releases_plot <- ggplot(data = es_land_releases_nyt,
                                mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-1.5, 0.4) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: -0.0374 (0.0331)", color = "black") +
  ggtitle(label = "Total onsite land releases intensity") +
  theme_bw()

es_land_releases_plot

summary(aggte(sdid_land_releases, type = 'group'))
sdid_land_releases_grp <- ggdid(aggte(sdid_land_releases, type = 'group'))
#======================================================================================================================#
### Onsite: Total landfills intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_landfills <- att_gt(
  yname = "l.total.landfills.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_landfills)
sdid_landfills_es <- aggte(
  sdid_landfills,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_landfills_es)
ggdid(sdid_landfills_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_landfills_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_landfills_es$egt, ")"),
  event.time = sdid_landfills_es$egt,
  estimate = sdid_landfills_es$att.egt,
  std.error = sdid_landfills_es$se.egt,
  conf.low = sdid_landfills_es$att.egt - sdid_landfills_es$crit.val.egt * sdid_landfills_es$se.egt,
  conf.high = sdid_landfills_es$att.egt + sdid_landfills_es$crit.val.egt * sdid_landfills_es$se.egt,
  point.conf.low = sdid_landfills_es$att.egt - stats::qnorm(1 - sdid_landfills_es$DIDparams$alp / 2) *
    sdid_landfills_es$se.egt,
  point.conf.high = sdid_landfills_es$att.egt + stats::qnorm(1 - sdid_landfills_es$DIDparams$alp / 2) *
    sdid_landfills_es$se.egt
)

# First option
es_landfills_plot <- ggplot(data = es_landfills_nyt,
                            mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-1.4, 0.4) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: -0.0072 (0.0151)", color = "black") +
  ggtitle(label = "Total onsite landfills intensity") +
  theme_bw()

es_landfills_plot

summary(aggte(sdid_landfills, type = 'group'))
sdid_landfills_grp <- ggdid(aggte(sdid_landfills, type = 'group'))
#======================================================================================================================#
### Onsite: Total surface impoundment intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_surface_impound <- att_gt(
  yname = "l.total.surface.impoundment.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_surface_impound)
sdid_surface_impound_es <- aggte(
  sdid_surface_impound,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_surface_impound_es)
ggdid(sdid_surface_impound_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_surface_impound_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_surface_impound_es$egt, ")"),
  event.time = sdid_surface_impound_es$egt,
  estimate = sdid_surface_impound_es$att.egt,
  std.error = sdid_surface_impound_es$se.egt,
  conf.low = sdid_surface_impound_es$att.egt - sdid_surface_impound_es$crit.val.egt * sdid_surface_impound_es$se.egt,
  conf.high = sdid_surface_impound_es$att.egt + sdid_surface_impound_es$crit.val.egt * sdid_surface_impound_es$se.egt,
  point.conf.low = sdid_surface_impound_es$att.egt - stats::qnorm(1 - sdid_surface_impound_es$DIDparams$alp / 2) *
    sdid_surface_impound_es$se.egt,
  point.conf.high = sdid_surface_impound_es$att.egt + stats::qnorm(1 - sdid_surface_impound_es$DIDparams$alp / 2) *
    sdid_surface_impound_es$se.egt
)

# First option
es_surface_impound_plot <- ggplot(data = es_surface_impound_nyt,
                                  mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-0.025, 0.025) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.02, label = "ATT: 0.0013 (0.0024)", color = "black") +
  ggtitle(label = "Total onsite surface impoundment intensity") +
  theme_bw()

es_surface_impound_plot

summary(aggte(sdid_surface_impound, type = 'group'))
sdid_surface_impound_grp <- ggdid(aggte(sdid_surface_impound, type = 'group'))
#======================================================================================================================#
### Onsite: Total land releases intensity, Others
#======================================================================================================================#
start_time <- Sys.time()
sdid_land_releases_other <- att_gt(
  yname = "l.total.land.releases.other.onsite.intensity",
  tname = "year",
  idname = "facility.zipcode",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.zipcode", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_land_releases_other)
sdid_land_releases_other_es <- aggte(
  sdid_land_releases_other,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_land_releases_other_es)
ggdid(sdid_land_releases_other_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_land_releases_other_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_land_releases_other_es$egt, ")"),
  event.time = sdid_land_releases_other_es$egt,
  estimate = sdid_land_releases_other_es$att.egt,
  std.error = sdid_land_releases_other_es$se.egt,
  conf.low = sdid_land_releases_other_es$att.egt - sdid_land_releases_other_es$crit.val.egt * sdid_land_releases_other_es$se.egt,
  conf.high = sdid_land_releases_other_es$att.egt + sdid_land_releases_other_es$crit.val.egt * sdid_land_releases_other_es$se.egt,
  point.conf.low = sdid_land_releases_other_es$att.egt - stats::qnorm(1 - sdid_land_releases_other_es$DIDparams$alp / 2) *
    sdid_land_releases_other_es$se.egt,
  point.conf.high = sdid_land_releases_other_es$att.egt + stats::qnorm(1 - sdid_land_releases_other_es$DIDparams$alp / 2) *
    sdid_land_releases_other_es$se.egt
)

# First option
es_land_releases_other_plot <- ggplot(data = es_land_releases_other_nyt,
                                      mapping = aes(x = event.time, y = estimate)) +
  geom_line(size = 0.5, alpha = 2, colour = "black") +
  geom_vline(xintercept = -1, color = "red", size = 1.2, linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype = 1, size = 1.1,
                  color = "red") +
  geom_pointrange(aes(ymin = point.conf.low, ymax = point.conf.high), show.legend = FALSE, size = 1.1) +
  xlab(label = "Event time") +
  ylab(label = "Event Study Estimate") +
  ylim(range(
    es_air_nyt$conf.low,
    es_air_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-0.15, 0.07) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.035, label = "ATT: -0.0390 (0.0278)", color = "black") +
  ggtitle(label = "Total onsite land releases intensity, others") +
  theme_bw()

es_land_releases_other_plot

summary(aggte(sdid_land_releases_other, type = 'group'))
sdid_land_releases_other_grp <- ggdid(aggte(sdid_land_releases_other, type = 'group'))
#======================================================================================================================#
es_total_releases_plot +
  es_air_plot +
  es_point_air_plot +
  es_fug_air_plot +
  es_water_disc_plot +
  es_land_releases_plot +
  es_landfills_plot +
  es_surface_impound_plot +
  es_land_releases_other_plot
#======================================================================================================================#