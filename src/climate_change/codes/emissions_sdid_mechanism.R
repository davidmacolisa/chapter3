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
#======================================================================================================================#
### Controls
#======================================================================================================================#
controls <- ~
  facility.id.fe +
    border.county.fe +
    chemical.year.fe
#======================================================================================================================#
### Onsite: Total releases intensity
#======================================================================================================================#
start_time <- Sys.time()
sdid_source_reduce <- att_gt(
  yname = "source.reduction",
  tname = "year",
  idname = "facility.id",
  gname = "ch.year",
  xformla = controls,
  panel = T,
  allow_unbalanced_panel = T,
  control_group = "notyettreated",
  clustervars = c("facility.id", "facility.state"),
  est_method = "dr",
  pl = T,
  base_period = "universal",
  data = triQc_nyt,
)
end_time <- Sys.time()
end_time - start_time

summary(sdid_source_reduce)
aggte(sdid_source_reduce, type = "simple")
sdid_source_reduce_es <- aggte(
  sdid_source_reduce,
  type = "dynamic",
  bstrap = T,
  cband = T,
  min_e = -3,
  max_e = 3
)
summary(sdid_source_reduce_es)
ggdid(sdid_source_reduce_es)

#----------------------------------------------------------------------------------------------------------------------#
### Event study plot
#----------------------------------------------------------------------------------------------------------------------#
es_source_reduce_nyt <- data.frame(
  type = "dynamic",
  term = paste0('ATT(', sdid_source_reduce_es$egt, ")"),
  event.time = sdid_source_reduce_es$egt,
  estimate = sdid_source_reduce_es$att.egt,
  std.error = sdid_source_reduce_es$se.egt,
  conf.low = sdid_source_reduce_es$att.egt - sdid_source_reduce_es$crit.val.egt * sdid_source_reduce_es$se.egt,
  conf.high = sdid_source_reduce_es$att.egt + sdid_source_reduce_es$crit.val.egt * sdid_source_reduce_es$se.egt,
  point.conf.low = sdid_source_reduce_es$att.egt - stats::qnorm(1 - sdid_source_reduce_es$DIDparams$alp / 2) *
    sdid_source_reduce_es$se.egt,
  point.conf.high = sdid_source_reduce_es$att.egt + stats::qnorm(1 - sdid_source_reduce_es$DIDparams$alp / 2) *
    sdid_source_reduce_es$se.egt
)

# First option
es_source_reduce_plot <- ggplot(data = es_source_reduce_nyt,
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
    es_source_reduce_nyt$conf.low,
    es_source_reduce_nyt$conf.high
  )) +
  scale_x_continuous(breaks = -3:2) +
  #scale_y_continuous(breaks = seq(-600, 200, 200), limits = c(-700,200))+
  ylim(-0.5, 0.5) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title = element_text(color = "black", size = 12)) +
  theme(plot.title = ggtext::element_markdown(size = 12,
                                              #face = "bold",
                                              color = "black",
                                              hjust = 0,
                                              lineheight = 1.2)
  ) +
  annotate(geom = "text", x = 1, y = -0.1, label = "ATT: 0.1195 (0.0697)", color = "black") +
  ggtitle(label = "Onsite source reduction activities") +
  theme_bw()

es_source_reduce_plot

summary(aggte(sdid_total_releases, type = 'group'))
sdid_total_releases_grp <- ggdid(aggte(sdid_total_releases, type = 'group'))
