#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(kableExtra)
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
file <- "./Data_PhD/US/BLS/offsite/triQc_off.rds"
triQc_off <- read_rds(file = file)
file <- "./Data_PhD/US/BLS/offsite/triQc_potw.rds"
triQc_potw <- read_rds(file = file)

file <- "./Data_PhD/US/BLS/onsite/triQs_on.rds"
triQs <- read_rds(file = file)
file <- "./Data_PhD/US/BLS/offsite/triQs_off.rds"
triQs_off <- read_rds(file = file)
file <- "./Data_PhD/US/BLS/offsite/triQs_potw.rds"
triQs_potw <- read_rds(file = file)
#======================================================================================================================#
### The samples
#======================================================================================================================#
# Onsite
nrow(triQc)
n_distinct(triQc$facility.id)
n_distinct(triQc$facility.zipcode)
n_distinct(triQc$facility.city)
n_distinct(triQc$facility.county)
n_distinct(triQc[triQc$treated == 0,]$facility.state)
n_distinct(triQc[triQc$treated == 1,]$facility.state)
sort(unique((triQc[triQc$treated == 0,]$facility.state)))
sort(unique((triQc[triQc$treated == 1,]$facility.state)))
n_distinct(triQc[triQc$treated == 0,]$control.match)
n_distinct(triQc[triQc$treated == 1,]$treated.match)
sort(unique((triQc[triQc$treated == 0,]$control.match)))
sort(unique((triQc[triQc$treated == 1,]$treated.match)))
n_distinct(triQc$facility.state)
n_distinct(triQc$chemical.name)
n_distinct(triQc$naics.code)
n_distinct(triQc$industry.name)
sort(unique(triQc$industry.name))

# Offsite
nrow(triQc_off)
n_distinct(triQc_off$facility.id)
n_distinct(triQc_off$facility.zipcode)
n_distinct(triQc_off$facility.city)
n_distinct(triQc_off$facility.county)
n_distinct(triQc_off[triQc_off$treated == 0,]$facility.state)
n_distinct(triQc_off[triQc_off$treated == 1,]$facility.state)
sort(unique((triQc_off[triQc_off$treated == 0,]$facility.state)))
sort(unique((triQc_off[triQc_off$treated == 1,]$facility.state)))
n_distinct(triQc_off[triQc_off$treated == 0,]$control.match)
n_distinct(triQc_off[triQc_off$treated == 1,]$treated.match)
sort(unique((triQc_off[triQc_off$treated == 0,]$control.match)))
sort(unique((triQc_off[triQc_off$treated == 1,]$treated.match)))
n_distinct(triQc_off$facility.state)
n_distinct(triQc_off$chemical.name)
n_distinct(triQc_off$naics.code)
n_distinct(triQc_off$industry.name)
sort(unique(triQc_off$industry.name))

n_distinct(triQc_off$offsite.id)
n_distinct(triQc_off$offsite.facility.id)
n_distinct(triQc_off$offsite.zipcode)
n_distinct(triQc_off$offsite.city)
n_distinct(triQc_off$offsite.county)
n_distinct(triQc_off$offsite.state)
sort(unique((triQc_off$offsite.state)))

# POTW
nrow(triQc_potw)
n_distinct(triQc_potw$facility.id)
n_distinct(triQc_potw$facility.zipcode)
n_distinct(triQc_potw$facility.city)
n_distinct(triQc_potw$facility.county)
n_distinct(triQc_potw[triQc_potw$treated == 0,]$facility.state)
n_distinct(triQc_potw[triQc_potw$treated == 1,]$facility.state)
sort(unique((triQc_potw[triQc_potw$treated == 0,]$facility.state)))
sort(unique((triQc_potw[triQc_potw$treated == 1,]$facility.state)))
n_distinct(triQc_potw[triQc_potw$treated == 0,]$control.match)
n_distinct(triQc_potw[triQc_potw$treated == 1,]$treated.match)
sort(unique((triQc_potw[triQc_potw$treated == 0,]$control.match)))
sort(unique((triQc_potw[triQc_potw$treated == 1,]$treated.match)))
n_distinct(triQc_potw$facility.state)
n_distinct(triQc_potw$chemical.name)
n_distinct(triQc_potw$naics.code)
n_distinct(triQc_potw$industry.name)
sort(unique(triQc_potw$industry.name))

n_distinct(triQc_potw$potw.id)
n_distinct(triQc_potw$potw.zipcode)
n_distinct(triQc_potw$potw.city)
n_distinct(triQc_potw$potw.county)
n_distinct(triQc_potw$potw.state)
sort(unique((triQc_potw$potw.state)))
#======================================================================================================================#
### List of the chemicals---Table
#======================================================================================================================#
chemicals_onsite <- triQc %>%
  select(
    chemical.id, chemical.name, chemical.classification, carcinogenic.chems,
    clean.air.act.chems, metal.restrict.tri, chemical.formulation.component, chemical.article.component,
    chemical.manufacturing.aid, chemical.ancilliary.use, trade.secret, sanitised
  ) %>%
  mutate(
    tri = ifelse(chemical.classification == "TRI", yes = 1, no = 0),
    pbt = ifelse(chemical.classification == "PBT", yes = 1, no = 0),
    dioxin = ifelse(chemical.classification == "DIOXIN", yes = 1, no = 0),
  ) %>%
  group_by(chemical.id, chemical.name) %>%
  summarise(
    tri.chem.class = sum(tri, na.rm = TRUE),
    pbt.chem.class = sum(pbt, na.rm = TRUE),
    dioxin.chem.class = sum(dioxin, na.rm = TRUE),
    n.carcinogen = sum(carcinogenic.chems, na.rm = TRUE),
    n.caa = sum(clean.air.act.chems, na.rm = TRUE),
    n.met.restr.tri = sum(metal.restrict.tri, na.rm = TRUE),
    n.chem.form.comp = sum(chemical.formulation.component, na.rm = TRUE),
    n.chem.art.comp = sum(chemical.article.component, na.rm = TRUE),
    n.chem.manu.aid = sum(chemical.manufacturing.aid, na.rm = TRUE),
    n.chem.anci.use = sum(chemical.ancilliary.use, na.rm = TRUE),
    n.chem.trade.secret = sum(trade.secret, na.rm = TRUE),
    n.chem.sanitised = sum(sanitised, na.rm = TRUE),
  ) %>%
  mutate(onsite = "yes") %>%
  print(n = nrow(.)) %>%
  data.frame()

chemicals.offsite <- triQc_off %>%
  select(chemical.id, chemical.name, chemical.classification, carcinogenic.chems, clean.air.act.chems,
         metal.restrict.tri, chemical.formulation.component, chemical.article.component,
         chemical.manufacturing.aid, chemical.ancilliary.use) %>%
  mutate(
    tri = ifelse(chemical.classification == "TRI", yes = 1, no = 0),
    pbt = ifelse(chemical.classification == "PBT", yes = 1, no = 0),
    dioxin = ifelse(chemical.classification == "DIOXIN", yes = 1, no = 0),
  ) %>%
  group_by(chemical.id, chemical.name) %>%
  summarise(
    tri.chem.class = sum(tri, na.rm = TRUE),
    pbt.chem.class = sum(pbt, na.rm = TRUE),
    dioxin.chem.class = sum(dioxin, na.rm = TRUE),
    n.carcinogen = sum(carcinogenic.chems, na.rm = TRUE),
    n.caa = sum(clean.air.act.chems, na.rm = TRUE),
    n.met.restr.tri = sum(metal.restrict.tri, na.rm = TRUE),
    n.chem.form.comp = sum(chemical.formulation.component, na.rm = TRUE),
    n.chem.art.comp = sum(chemical.article.component, na.rm = TRUE),
    n.chem.manu.aid = sum(chemical.manufacturing.aid, na.rm = TRUE),
    n.chem.anci.use = sum(chemical.ancilliary.use, na.rm = TRUE),
  ) %>%
  mutate(offsite = "yes") %>%
  print(n = nrow(.)) %>%
  data.frame()

chemicals_potw <- triQc_potw %>%
  select(chemical.id, chemical.name, chemical.classification, carcinogenic.chems, clean.air.act.chems,
         metal.restrict.tri, chemical.formulation.component, chemical.article.component,
         chemical.manufacturing.aid, chemical.ancilliary.use) %>%
  mutate(
    tri = ifelse(chemical.classification == "TRI", yes = 1, no = 0),
    pbt = ifelse(chemical.classification == "PBT", yes = 1, no = 0),
    dioxin = ifelse(chemical.classification == "DIOXIN", yes = 1, no = 0),
  ) %>%
  group_by(chemical.id, chemical.name) %>%
  summarise(
    tri.chem.class = sum(tri, na.rm = TRUE),
    pbt.chem.class = sum(pbt, na.rm = TRUE),
    dioxin.chem.class = sum(dioxin, na.rm = TRUE),
    n.carcinogen = sum(carcinogenic.chems, na.rm = TRUE),
    n.caa = sum(clean.air.act.chems, na.rm = TRUE),
    n.met.restr.tri = sum(metal.restrict.tri, na.rm = TRUE),
    n.chem.form.comp = sum(chemical.formulation.component, na.rm = TRUE),
    n.chem.art.comp = sum(chemical.article.component, na.rm = TRUE),
    n.chem.manu.aid = sum(chemical.manufacturing.aid, na.rm = TRUE),
    n.chem.anci.use = sum(chemical.ancilliary.use, na.rm = TRUE),
  ) %>%
  mutate(potw = "yes") %>%
  print(n = nrow(.)) %>%
  data.frame()

chemicals <- chemicals_onsite %>%
  left_join(
    y = chemicals.offsite %>% select(chemical.id, chemical.name, offsite)
  ) %>%
  left_join(
    y = chemicals_potw %>% select(chemical.id, chemical.name, potw)
  ) %>%
  mutate(
    classification = case_when(
      tri.chem.class > 0 ~ "TRI",
      pbt.chem.class > 0 ~ "PBT",
      dioxin.chem.class > 0 ~ "DIOXIN",
    ),
    attribute = case_when(
      n.carcinogen > 0 ~ "carcinogenic",
      n.caa > 0 ~ "clean air act",
      # n.pfas > 0 ~ "polyfluoroalkyl",
      n.met.restr.tri > 0 ~ "metal restricted",
      # n.met.incl.tri > 0 ~ "elemental metal included",
      # n.chem.intm.uses > 0 ~ "intermediate uses",
      n.chem.form.comp > 0 ~ "formulation component",
      n.chem.art.comp > 0 ~ "article component",
      n.chem.manu.aid > 0 ~ "manufacturing aid",
      n.chem.anci.use > 0 ~ "ancillary use",
      n.carcinogen == 0 |
        n.caa == 0 |
        n.met.restr.tri == 0 |
        # n.met.incl.tri == 0 |
        n.chem.form.comp == 0 |
        n.chem.art.comp == 0 |
        n.chem.manu.aid == 0 |
        n.chem.anci.use == 0 ~ "others",
    ),
    location = case_when(
      onsite == "yes" ~ "onsite",
      offsite == "yes" ~ "offsite",
      potw == "yes" ~ "POTW",
    ),
  ) %>%
  select(c(chemical.name, classification, attribute, onsite, offsite, potw))

n_distinct(chemicals$chemical.name)
chemicals <- chemicals[order(chemicals$chemical.name),]
chemicals84 <- slice(chemicals, 1:84)
chemicals167 <- slice(chemicals, 84:167)
nrow(chemicals84)
nrow(chemicals167)
chems <- cbind(chemicals84, chemicals167) %>% data.frame()

# Convert data frame to LaTeX table
chemicals_tex <- chems %>%
  kable(., format = "latex", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(chemicals_tex)
writeLines(chemicals_tex, con = "./Thesis/chapter3/src/climate_change/latex/tbl_chemicals1.tex")
#======================================================================================================================#
### Variable Definitions
#======================================================================================================================#
var_def <- triQc %>% select(c(facility.id, treated, ind.output:l.tfp5))
var_def1 <- triQc_off %>% select(c(facility.id, treated, ind.output:l.tfp5))
var_def2 <- triQc_potw %>% select(c(facility.id, treated, ind.output:l.tfp5))
#======================================================================================================================#
### Descriptive Statistics
#======================================================================================================================#
# Distribution of NAICS industries
naics_distribution <- triQc %>%
  select(industry.name, naics.code) %>%
  group_by(industry.name) %>%
  summarise(naics.code = n()) %>%
  ggplot(aes(x = industry.name, y = naics.code)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Distribution of NAICS Manufacturing Industries", x = "", y = "counts") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
        axis.title.x = element_text(size = 18))
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_naics_distribution.pdf", width = 12, height = 10)
naics_distribution
dev.off()
#======================================================================================================================#
# Distribution of total releases intensity onsite by NAICS industries
releases_distribution_naics <- triQc %>%
  select(industry.name, naics.code, total.releases.onsite.intensity) %>%
  group_by(industry.name) %>%
  summarise(naics.code = n(),
            total.releases.onsite.intensity = sum(total.releases.onsite.intensity, na.rm = TRUE)) %>%
  ggplot(aes(x = industry.name, y = total.releases.onsite.intensity)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total Releases Intensity (Onsite)",
    x = "",
    y = "total releases Intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
        axis.title.x = element_text(size = 20))
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_releases_distribution.pdf", width = 12, height = 10)
releases_distribution_naics
dev.off()
#======================================================================================================================#
# Distribution of total releases intensity onsite between the Treated and Control States
releases_distribution_states <- triQc %>%
  select(facility.state, industry.name, naics.code, treated, total.releases.onsite.intensity) %>%
  group_by(facility.state) %>%
  summarise(
    naics.code = n(),
    total.releases.onsite.intensity = mean(total.releases.onsite.intensity, na.rm = TRUE),
    treated = treated %>% unique()
  ) %>%
  ggplot(aes(x = facility.state, y = total.releases.onsite.intensity, fill = treated)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Releases Intensity (Onsite)",
    x = "",
    y = "total releases intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_releases_distribution_states.pdf", width = 15, height = 10)
releases_distribution_states
dev.off()
#======================================================================================================================#
# Distribution of total releases intensity onsite between the Treated and Control States
air_emissions_distribution <- triQc %>%
  select(facility.state, industry.name, naics.code, treated, total.air.emissions.onsite.intensity) %>%
  group_by(facility.state) %>%
  summarise(
    naics.code = n(),
    total.air.emissions.onsite.intensity = sum(total.air.emissions.onsite.intensity, na.rm = TRUE),
    treated = treated %>% unique()
  ) %>%
  ggplot(aes(x = facility.state, y = total.air.emissions.onsite.intensity, fill = treated)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Air Emissions Intensity (Onsite)",
    x = "",
    y = "total air emissions intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_air_emissions_distribution.pdf", width = 15, height = 10)
air_emissions_distribution
dev.off()
#======================================================================================================================#
# Distribution of total land releases intensity onsite between the Treated and Control States
land_releases_distribution <- triQc %>%
  select(facility.state, industry.name, naics.code, treated, total.land.releases.onsite.intensity) %>%
  group_by(facility.state) %>%
  summarise(
    naics.code = n(),
    total.land.releases.onsite.intensity = sum(total.land.releases.onsite.intensity, na.rm = TRUE),
    treated = treated %>% unique()
  ) %>%
  ggplot(aes(x = facility.state, y = total.land.releases.onsite.intensity, fill = treated)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Land Releases Intensity (Onsite)",
    x = "",
    y = "total land releases intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_land_releases_distribution.pdf", width = 15, height = 10)
land_releases_distribution
dev.off()
#======================================================================================================================#
# Distribution of total surface water discharge intensity onsite between the Treated and Control States
water_discharge_distribution <- triQc %>%
  select(facility.state, industry.name, naics.code, treated, total.surface.water.discharge.onsite.intensity) %>%
  group_by(facility.state) %>%
  summarise(
    naics.code = n(),
    total.surface.water.discharge.onsite.intensity = sum(total.surface.water.discharge.onsite.intensity, na.rm = TRUE),
    treated = treated %>% unique()
  ) %>%
  ggplot(aes(x = facility.state, y = total.surface.water.discharge.onsite.intensity, fill = treated)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Surface Water Discharge Intensity (Onsite)",
    x = "",
    y = "total surface water discharge intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_water_discharge_distribution.pdf", width = 15, height = 10)
water_discharge_distribution
dev.off()
#======================================================================================================================#
# Distribution of industries by ownership
triQc %>%
  select(
    facility.state, industry.name, naics.code, own_code, federal.facility,
    govt.owned.facility
  ) %>%
  mutate(
    ownership = case_when(
      federal.facility == 1 ~ "federal",
      govt.owned.facility == 1 ~ "state",
      own_code == 5 ~ "private"
    )
  ) %>%
  filter(!is.na(ownership)) %>%  # Filter out rows where ownership is NA
  group_by(ownership) %>%
  summarise(
    naics.code = n(),
    own.code = own_code %>% unique()
  ) %>%
  ggplot(aes(x = ownership, y = naics.code)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Distribution of Industries by Ownership Type",
    x = "Ownership Types",
    y = "counts"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
        axis.title.x = element_text(size = 20))
#======================================================================================================================#
### Descriptive Statistics
### Balance Test: Using the year immediately preceding the first initial treatment date, which is 2013.
#======================================================================================================================#
# Facility and County level
#----------------------------------------------------------------------------------------------------------------------#
library(mosaic)
pre <- sum_up(df = triQc %>% filter(year == 2013),
              c(gdp.pc, annual_avg_estabs, emp, entire.facility, private.facility,
                produced.chem.facility, imported.chem.facility, chemical.formulation.component,
                chemical.manufacturing.aid, chemical.ancilliary.use, production.ratio.activity.index,
                maxnum.chem.onsite),
              d = F) %>%
  select(-c(Obs, Missing, Min, Max)) %>%
  rename(SD = StdDev) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

pre_summ <- sum_up(df = triQc %>%
  filter(year == 2013) %>%
  group_by(treated),
                   c(gdp.pc, annual_avg_estabs, emp, entire.facility, private.facility,
                     produced.chem.facility, imported.chem.facility, chemical.formulation.component,
                     chemical.manufacturing.aid, chemical.ancilliary.use, production.ratio.activity.index,
                     maxnum.chem.onsite),
                   d = F) %>%
  select(-c(Obs, Missing, StdDev, Min, Max)) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

treat <- pre_summ %>%
  filter(treated == 1) %>%
  rename(T = Mean) %>%
  select(treated, Variable, T) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

control <- pre_summ %>%
  filter(treated == 0) %>%
  rename(C = Mean) %>%
  select(treated, Variable, C) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

bal_test <- cbind(pre, T = treat$T, C = control$C)

# Convert data frame to LaTeX table
bal_test_tex <- bal_test %>%
  kable(format = "latex", label = "Descriptive Statistics: Treated vs. Control Border Counties", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(bal_test_tex)
writeLines(bal_test_tex, con = "./Thesis/chapter3/src/climate_change/latex/tbl_bal_test_tex1.tex")
#----------------------------------------------------------------------------------------------------------------------#
# State level
#----------------------------------------------------------------------------------------------------------------------#
pre <- sum_up(df = triQs %>% filter(year == 2013),
              c(gdp.pc, annual_avg_estabs, emp, entire.facility, private.facility,
                produced.chem.facility, imported.chem.facility, chemical.formulation.component,
                chemical.manufacturing.aid, chemical.ancilliary.use, production.ratio.activity.index,
                maxnum.chem.onsite),
              d = F) %>%
  select(-c(Obs, Missing, Min, Max)) %>%
  rename(SD = StdDev) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

pre_summ <- sum_up(df = triQs %>%
  filter(year == 2013) %>%
  group_by(treated),
                   c(gdp.pc, annual_avg_estabs, emp, entire.facility, private.facility,
                     produced.chem.facility, imported.chem.facility, chemical.formulation.component,
                     chemical.manufacturing.aid, chemical.ancilliary.use, production.ratio.activity.index,
                     maxnum.chem.onsite),
                   d = F) %>%
  select(-c(Obs, Missing, StdDev, Min, Max)) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

treat <- pre_summ %>%
  filter(treated == 1) %>%
  rename(T = Mean) %>%
  select(treated, Variable, T) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

control <- pre_summ %>%
  filter(treated == 0) %>%
  rename(C = Mean) %>%
  select(treated, Variable, C) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

bal_test <- cbind(pre, T = treat$T, C = control$C)

# Convert data frame to LaTeX table
bal_test_tex <- bal_test %>%
  kable(format = "latex", label = "Descriptive Statistics: Treated vs. Control Border States", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(bal_test_tex)
writeLines(bal_test_tex, con = "./Thesis/chapter3/src/climate_change/latex/tbl_bal_test_state_tex1.tex")
#======================================================================================================================#
### Summary Statistics (Onsite)
#======================================================================================================================#
summ <- sum_up(df = triQc,
               c(total.releases.onsite.intensity, total.air.emissions.onsite.intensity,
                 total.fug.air.emissions.onsite.intensity, total.point.air.emissions.onsite.intensity,
                 total.surface.water.discharge.onsite.intensity, total.num.receiving.streams.onsite.intensity,
                 total.underground.injection.onsite.intensity, total.landfills.onsite.intensity,
                 total.releases.toland.treatment.onsite.intensity, total.surface.impoundment.onsite.intensity,
                 total.land.releases.onsite.intensity, gdp.pc, annual_avg_estabs, emp, entire.facility,
                 private.facility, produced.chem.facility, imported.chem.facility, chemical.formulation.component,
                 chemical.manufacturing.aid, chemical.ancilliary.use, production.ratio.activity.index,
                 maxnum.chem.onsite),
               d = F) %>%
  select(-Missing) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

# Convert data frame to LaTeX table
summ_tex <- summ %>%
  kable(format = "latex", label = "Descriptive Statistics", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(summ_tex)
writeLines(summ_tex, con = "./Thesis/chapter3/src/climate_change/latex/tbl_summ_on_tex1.tex")
#======================================================================================================================#
### Summary Statistics (Offsite)
#======================================================================================================================#
summ_off <- sum_up(df = triQc_off,
                   c(
                     total.releases.offsite.intensity, total.releases.unknown.offsite.intensity,
                     total.releases.wastebroker.offsite.intensity, total.releases.other.mgt.offsite.intensity,
                     total.releases.metalsolidify.offsite.intensity, total.releases.storage.offsite.intensity,
                     total.wastewater.releases.offsite.intensity, total.land.releases.offsite.intensity,
                     total.land.releases.other.offsite.intensity, total.surface.impoundment.offsite.intensity,
                     total.releases.toland.treatment.offsite.intensity, total.landfills.offsite.intensity,
                     total.underground.injection.offsite.intensity
                   ),
                   d = F) %>%
  select(-Missing) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

# Convert data frame to LaTeX table
summ_off_tex <- summ_off %>%
  kable(format = "latex", label = "Descriptive Statistics (Offsite)", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(summ_off_tex)
writeLines(summ_off_tex, con = "./Thesis/chapter3/src/climate_change/latex/tbl_summ_off_tex1.tex")
#======================================================================================================================#
### Summary Statistics (POTWs)
#======================================================================================================================#
summ_potw <- sum_up(df = triQc_potw,
                    c(total.potw.releases.offsite.intensity, potw.releases.underground.other.offsite.intensity,
                      potw.releases.underground.Iwells.offsite.intensity),
                    d = F) %>%
  select(-Missing) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

# Convert data frame to LaTeX table
summ_potw_tex <- summ_potw %>%
  kable(format = "latex", label = "Descriptive Statistics (Offsite)", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(summ_potw_tex)
writeLines(summ_potw_tex, con = "./Thesis/chapter3/src/climate_change/latex/tbl_summ_potw_tex1.tex")
#======================================================================================================================#
### Pre-period regressions --- County
#======================================================================================================================#
# GDP per capita
#----------------------------------------------------------------------------------------------------------------------#
did_gdppc <- fixest::feols(
  log(gdp.pc) ~
    i(treated * year)
      |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_gdppc, digits = 4, digits.stats = 4)

coefficients <- did_gdppc$coefficients
conf_intervals <- confint(did_gdppc)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
gdppc_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
gdppc_pre <- ggplot(gdppc_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "GDP per Capita"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.6, 0.6) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# GDP
#----------------------------------------------------------------------------------------------------------------------#
did_gdp <- fixest::feols(
  log(gdp) ~
    i(treated * year)
      |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_gdp, digits = 4, digits.stats = 4)

coefficients <- did_gdp$coefficients
conf_intervals <- confint(did_gdp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
gdp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
gdp_pre <- ggplot(gdp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "GDP"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Population
#----------------------------------------------------------------------------------------------------------------------#
did_pop <- fixest::feols(
  log(population) ~
    i(treated * year)
      |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_pop, digits = 4, digits.stats = 4)

coefficients <- did_pop$coefficients
conf_intervals <- confint(did_pop)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
pop_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
pop_pre <- ggplot(pop_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "County Population"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# industry employment
#----------------------------------------------------------------------------------------------------------------------#
did_emp <- fixest::feols(
  log(emp) ~
    i(treated * year)
      |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_emp, digits = 4, digits.stats = 4)

coefficients <- did_emp$coefficients
conf_intervals <- confint(did_emp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
emp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
emp_pre <- ggplot(emp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Industry Employment"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.6, 0.6) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Annual average establishments (county)
#----------------------------------------------------------------------------------------------------------------------#
did_estabs <- fixest::feols(
  log(annual_avg_estabs) ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_estabs, digits = 4, digits.stats = 4)

coefficients <- did_estabs$coefficients
conf_intervals <- confint(did_estabs)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
estabs_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
estabs_pre <- ggplot(estabs_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Annual average number of establishments (county)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.6, 0.6) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Inflation (city)
#----------------------------------------------------------------------------------------------------------------------#
did_cpi <- fixest::feols(
  log(cpi) ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_cpi, digits = 4, digits.stats = 4)

coefficients <- did_cpi$coefficients
conf_intervals <- confint(did_cpi)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
cpi_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
cpi_pre <- ggplot(cpi_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "City Region Average Consumer Price Inflation"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Produced chemical at facility
#----------------------------------------------------------------------------------------------------------------------#
did_prod_chem_fac <- fixest::feols(
  produced.chem.facility ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_prod_chem_fac, digits = 4, digits.stats = 4)

coefficients <- did_prod_chem_fac$coefficients
conf_intervals <- confint(did_prod_chem_fac)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
prod_chem_fac_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
prod_chem_fac_pre <- ggplot(prod_chem_fac_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Produced chemical at facility"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Imported chemical at facility
#----------------------------------------------------------------------------------------------------------------------#
did_imp_chem_fac <- fixest::feols(
  imported.chem.facility ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_imp_chem_fac, digits = 4, digits.stats = 4)

coefficients <- did_imp_chem_fac$coefficients
conf_intervals <- confint(did_imp_chem_fac)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
imp_chem_fac_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
imp_chem_fac_pre <- ggplot(imp_chem_fac_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Imported chemical at facility"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical formulation component
#----------------------------------------------------------------------------------------------------------------------#
did_chem_form_comp <- fixest::feols(
  chemical.formulation.component ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_form_comp, digits = 4, digits.stats = 4)

coefficients <- did_chem_form_comp$coefficients
conf_intervals <- confint(did_chem_form_comp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_form_comp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_form_comp_pre <- ggplot(did_chem_form_comp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical formulation component"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical article component
#----------------------------------------------------------------------------------------------------------------------#
did_chem_art_comp <- fixest::feols(
  chemical.article.component ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_art_comp, digits = 4, digits.stats = 4)

coefficients <- did_chem_art_comp$coefficients
conf_intervals <- confint(did_chem_art_comp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_art_comp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_art_comp_pre <- ggplot(did_chem_art_comp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical article component"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical manufacturing aid
#----------------------------------------------------------------------------------------------------------------------#
did_chem_manu_aid <- fixest::feols(
  chemical.manufacturing.aid ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_manu_aid, digits = 4, digits.stats = 4)

coefficients <- did_chem_manu_aid$coefficients
conf_intervals <- confint(did_chem_manu_aid)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_manu_aid_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_manu_aid_pre <- ggplot(did_chem_manu_aid_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical manufacturing aid"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical ancilliary use
#----------------------------------------------------------------------------------------------------------------------#
did_chem_anc_use <- fixest::feols(
  chemical.ancilliary.use ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_anc_use, digits = 4, digits.stats = 4)

coefficients <- did_chem_anc_use$coefficients
conf_intervals <- confint(did_chem_anc_use)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_anc_use_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_anc_use_pre <- ggplot(did_chem_anc_use_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical ancilliary use"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Production or activity index
#----------------------------------------------------------------------------------------------------------------------#
did_prod_activity <- fixest::feols(
  log(production.ratio.activity.index) ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_prod_activity, digits = 4, digits.stats = 4)

coefficients <- did_prod_activity$coefficients
conf_intervals <- confint(did_prod_activity)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_prod_activity_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
prod_activity_pre <- ggplot(did_prod_activity_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Production or activity index"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Maximum number of chemical onsite
#----------------------------------------------------------------------------------------------------------------------#
did_max_num_chem <- fixest::feols(
  log(maxnum.chem.onsite) ~
    i(treated * year) |
      year +
        treated.cluster.id +
        treated.cluster.year.fe,
  data = triQc %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_max_num_chem, digits = 4, digits.stats = 4)

coefficients <- did_max_num_chem$coefficients
conf_intervals <- confint(did_max_num_chem)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_max_num_chem_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
max_num_chem_pre <- ggplot(did_max_num_chem_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Maximum number of chemicals onsite"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_pre_evolution.pdf", width = 23, height = 15)
gdppc_pre +
  gdp_pre +
  pop_pre +
  emp_pre +
  estabs_pre +
  cpi_pre +
  prod_chem_fac_pre +
  imp_chem_fac_pre +
  chem_form_comp_pre +
  chem_art_comp_pre +
  chem_manu_aid_pre +
  chem_anc_use_pre +
  prod_activity_pre +
  max_num_chem_pre +
  plot_layout(nrow = 3, ncol = 5, axes = "collect")
dev.off()
#======================================================================================================================#
### Pre-period regressions --- State
#======================================================================================================================#
# GDP per capita
#----------------------------------------------------------------------------------------------------------------------#
did_gdppc <- fixest::feols(
  log(gdp.pc) ~
    treated:factor(year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_gdppc, digits = 4, digits.stats = 4)

coefficients <- did_gdppc$coefficients
conf_intervals <- confint(did_gdppc)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQs$year[triQs$year <= 2013])

# Create dataframe for plotting
gdppc_data_state <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
gdppc_pre_state <- ggplot(gdppc_data_state, aes(x = year, y = coefficients, color = "red", size = 1)) +
  geom_point(size = 8) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 3
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "GDP per Capita"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.6, 0.6) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# GDP
#----------------------------------------------------------------------------------------------------------------------#
did_gdp <- fixest::feols(
  log(gdp) ~
    i(treated * year)
      |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_gdp, digits = 4, digits.stats = 4)

coefficients <- did_gdp$coefficients
conf_intervals <- confint(did_gdp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
gdp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
gdp_pre_state <- ggplot(gdp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "GDP"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-2, 2) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Population
#----------------------------------------------------------------------------------------------------------------------#
did_pop <- fixest::feols(
  log(population) ~
    i(treated * year)
      |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_pop, digits = 4, digits.stats = 4)

coefficients <- did_pop$coefficients
conf_intervals <- confint(did_pop)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
pop_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
pop_pre_state <- ggplot(pop_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "State Population"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-2, 2) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# industry employment
#----------------------------------------------------------------------------------------------------------------------#
did_emp <- fixest::feols(
  log(emp) ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_emp, digits = 4, digits.stats = 4)

coefficients <- did_emp$coefficients
conf_intervals <- confint(did_emp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
emp_data_state <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
emp_pre_state <- ggplot(emp_data_state, aes(x = year, y = coefficients, color = "red", size = 1)) +
  geom_point(size = 8) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 3
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Industry Employment"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.6, 0.6) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Annual average establishments (state)
#----------------------------------------------------------------------------------------------------------------------#
did_estabs <- fixest::feols(
  log(annual_avg_estabs) ~
    treated:factor(year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_estabs, digits = 4, digits.stats = 4)

coefficients <- did_estabs$coefficients
conf_intervals <- confint(did_estabs)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
estabs_data_state <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
estabs_pre_state <- ggplot(estabs_data_state, aes(x = year, y = coefficients, color = "red", size = 1)) +
  geom_point(size = 8) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 3
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Annual average number of establishments (state)"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Inflation (city)
#----------------------------------------------------------------------------------------------------------------------#
did_cpi <- fixest::feols(
  log(cpi) ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_cpi, digits = 4, digits.stats = 4)

coefficients <- did_cpi$coefficients
conf_intervals <- confint(did_cpi)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
cpi_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
cpi_pre_state <- ggplot(cpi_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "City Region Average Consumer Price Inflation"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Produced chemical at facility
#----------------------------------------------------------------------------------------------------------------------#
did_prod_chem_fac <- fixest::feols(
  produced.chem.facility ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_prod_chem_fac, digits = 4, digits.stats = 4)

coefficients <- did_prod_chem_fac$coefficients
conf_intervals <- confint(did_prod_chem_fac)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
prod_chem_fac_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
prod_chem_fac_pre_state <- ggplot(prod_chem_fac_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Produced chemical at facility"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.9, 0.9) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Imported chemical at facility
#----------------------------------------------------------------------------------------------------------------------#
did_imp_chem_fac <- fixest::feols(
  imported.chem.facility ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_imp_chem_fac, digits = 4, digits.stats = 4)

coefficients <- did_imp_chem_fac$coefficients
conf_intervals <- confint(did_imp_chem_fac)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
imp_chem_fac_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
imp_chem_fac_pre_state <- ggplot(imp_chem_fac_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Imported chemical at facility"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical formulation component
#----------------------------------------------------------------------------------------------------------------------#
did_chem_form_comp <- fixest::feols(
  chemical.formulation.component ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_form_comp, digits = 4, digits.stats = 4)

coefficients <- did_chem_form_comp$coefficients
conf_intervals <- confint(did_chem_form_comp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_form_comp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_form_comp_pre_state <- ggplot(did_chem_form_comp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical formulation component"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical article component
#----------------------------------------------------------------------------------------------------------------------#
did_chem_art_comp <- fixest::feols(
  chemical.article.component ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_art_comp, digits = 4, digits.stats = 4)

coefficients <- did_chem_art_comp$coefficients
conf_intervals <- confint(did_chem_art_comp)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_art_comp_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_art_comp_pre_state <- ggplot(did_chem_art_comp_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical article component"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical manufacturing aid
#----------------------------------------------------------------------------------------------------------------------#
did_chem_manu_aid <- fixest::feols(
  chemical.manufacturing.aid ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_manu_aid, digits = 4, digits.stats = 4)

coefficients <- did_chem_manu_aid$coefficients
conf_intervals <- confint(did_chem_manu_aid)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_manu_aid_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_manu_aid_pre_state <- ggplot(did_chem_manu_aid_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical manufacturing aid"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.2, 0.2) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Chemical ancilliary use
#----------------------------------------------------------------------------------------------------------------------#
did_chem_anc_use <- fixest::feols(
  chemical.ancilliary.use ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_chem_anc_use, digits = 4, digits.stats = 4)

coefficients <- did_chem_anc_use$coefficients
conf_intervals <- confint(did_chem_anc_use)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_chem_anc_use_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
chem_anc_use_pre_state <- ggplot(did_chem_anc_use_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Chemical ancilliary use"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Production or activity index
#----------------------------------------------------------------------------------------------------------------------#
did_prod_activity <- fixest::feols(
  log(production.ratio.activity.index) ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_prod_activity, digits = 4, digits.stats = 4)

coefficients <- did_prod_activity$coefficients
conf_intervals <- confint(did_prod_activity)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_prod_activity_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
prod_activity_pre_state <- ggplot(did_prod_activity_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Production or activity index"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

#----------------------------------------------------------------------------------------------------------------------#
# Maximum number of chemical onsite
#----------------------------------------------------------------------------------------------------------------------#
did_max_num_chem <- fixest::feols(
  log(maxnum.chem.onsite) ~
    i(treated * year) |
      year +
        treated.match.fe +
        treated.match.year.fe,
  data = triQs %>% filter(year <= 2013),
  cluster = ~facility.state.id,
)
fixest::etable(did_max_num_chem, digits = 4, digits.stats = 4)

coefficients <- did_max_num_chem$coefficients
conf_intervals <- confint(did_max_num_chem)
conf_intervals <- lapply(conf_intervals, as.character) %>% data.frame()
conf_intervals <- conf_intervals %>% rename(ci_min = "X2.5..", ci_max = "X97.5..")
conf_intervals <- lapply(conf_intervals, as.numeric) %>% data.frame()

# Extract years
year <- unique(triQc$year[triQc$year <= 2013])

# Create dataframe for plotting
did_max_num_chem_data <- data.frame(year, coefficients, conf_intervals)

# Plot coefficients against years with confidence intervals
max_num_chem_pre_state <- ggplot(did_max_num_chem_data, aes(x = year, y = coefficients, color = "red", size = 0.8)) +
  geom_point(size = 7) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(ymin = ci_min, ymax = ci_max),
    width = 0.2,
    color = "blue",
    size = 2.5
  ) +
  labs(
    x = "Year",
    y = "Estimates & Confidence Intervals",
    title = "Maximum number of chemicals onsite"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(-0.4, 0.4) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 1))

pdf(file = "./Thesis/chapter3/src/climate_change/latex/fig_pre_evolution_state.pdf", width = 23, height = 15)
gdppc_pre_state +
  gdp_pre_state +
  pop_pre_state +
  emp_pre_state +
  estabs_pre_state +
  cpi_pre_state +
  prod_chem_fac_pre_state +
  imp_chem_fac_pre_state +
  chem_form_comp_pre_state +
  chem_art_comp_pre_state +
  chem_manu_aid_pre_state +
  chem_anc_use_pre_state +
  prod_activity_pre_state +
  max_num_chem_pre_state +
  plot_layout(nrow = 3, ncol = 5, axes = "collect")
dev.off()
#======================================================================================================================#