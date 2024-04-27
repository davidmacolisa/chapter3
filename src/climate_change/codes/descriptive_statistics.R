#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(kableExtra)
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
#======================================================================================================================#
### The samples
#======================================================================================================================#
# Onsite
n_distinct(triQc$facility.id)
n_distinct(triQc$facility.zipcode)
n_distinct(triQc$facility.city)
n_distinct(triQc$facility.county)
n_distinct(triQc[triQc$treated == 0,]$facility.state)
n_distinct(triQc[triQc$treated == 1,]$facility.state)
sort(unique((triQc[triQc$treated == 0,]$facility.state)))
sort(unique((triQc[triQc$treated == 1,]$facility.state)))
n_distinct(triQc$facility.state)
n_distinct(triQc$chemical.name)
n_distinct(triQc$naics.code)

# Offsite
n_distinct(triQc_off$offsite.id)
n_distinct(triQc_off$offsite.facility.id)
n_distinct(triQc_off$offsite.zipcode)
n_distinct(triQc_off$offsite.city)
n_distinct(triQc_off$offsite.county)
n_distinct(triQc_off$offsite.state)
n_distinct(triQc_off$chemical.name)
n_distinct(triQc_off$naics.code)
n_distinct(triQc_off[triQc_off$treated == 0,]$facility.state)
n_distinct(triQc_off[triQc_off$treated == 1,]$facility.state)
sort(unique((triQc_off$offsite.state)))

# POTW
n_distinct(triQc_potw$potw.id)
n_distinct(triQc_potw$potw.zipcode)
n_distinct(triQc_potw$potw.city)
n_distinct(triQc_potw$potw.county)
n_distinct(triQc_potw$potw.state)
n_distinct(triQc_potw$chemical.name)
n_distinct(triQc_potw$naics.code)
n_distinct(triQc_potw[triQc_potw$treated == 0,]$facility.state)
n_distinct(triQc_potw[triQc_potw$treated == 1,]$facility.state)
sort(unique((triQc_potw[triQc_potw$treated == 0,]$facility.state)))
sort(unique((triQc_potw$potw.state)))
#======================================================================================================================#
### List of the chemicals---Table
#======================================================================================================================#
chemicals_onsite <- triQc %>%
  select(chemical.id, chemical.name, chemical.classification, carcinogenic.chems,
         clean.air.act.chems, pfas.chems, metal.restrict.tri, elemental.metal.included,
         chemical.intermediate.uses, chemical.formulation.component, chemical.article.component,
         chemical.manufacturing.aid, chemical.ancilliary.use, trade.secret, sanitised) %>%
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
    n.pfas = sum(pfas.chems, na.rm = TRUE),
    n.met.restr.tri = sum(metal.restrict.tri, na.rm = TRUE),
    n.met.incl.tri = sum(elemental.metal.included, na.rm = TRUE),
    n.chem.intm.uses = sum(chemical.intermediate.uses, na.rm = TRUE),
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
      n.pfas > 0 ~ "polyfluoroalkyl",
      n.met.restr.tri > 0 ~ "metal restricted",
      n.met.incl.tri > 0 ~ "elemental metal included",
      n.chem.intm.uses > 0 ~ "intermediate uses",
      n.chem.form.comp > 0 ~ "formulation component",
      n.chem.art.comp > 0 ~ "article component",
      n.chem.manu.aid > 0 ~ "manufacturing aid",
      n.chem.anci.use > 0 ~ "ancillary use",
      n.carcinogen == 0 |
        n.caa == 0 |
        n.pfas == 0 |
        n.met.restr.tri == 0 |
        n.met.incl.tri == 0 |
        n.chem.intm.uses == 0 |
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
chemicals83 <- slice(chemicals, 1:83)
chemicals167 <- slice(chemicals, 84:167)
nrow(chemicals83)
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
triQc %>%
  select(industry.name, naics.code) %>%
  group_by(industry.name) %>%
  summarise(naics.code = n()) %>%
  ggplot(aes(x = industry.name, y = naics.code)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Distribution of NAICS Industries", x = "NAICS Industries", y = "counts") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
        axis.title.x = element_text(size = 18))
#======================================================================================================================#
# Distribution of total releases onsite by NAICS industries
triQc %>%
  select(industry.name, naics.code, total.releases.onsite.intensity) %>%
  group_by(industry.name) %>%
  summarise(naics.code = n(),
            total.releases.onsite.intensity = sum(total.releases.onsite.intensity, na.rm = TRUE)) %>%
  ggplot(aes(x = industry.name, y = total.releases.onsite.intensity)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total Releases Onsite",
    x = "NAICS Industries",
    y = "total releases Intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
        axis.title.x = element_text(size = 20))
#======================================================================================================================#
triQc %>%
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
    x = "facility states",
    y = "total releases (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
#======================================================================================================================#
triQc %>%
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
    x = "facility states",
    y = "total air emissions (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
#======================================================================================================================#
triQc %>%
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
    title = "Total land releases intensity (onsite)",
    x = "facility states",
    y = "total land releases intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
#======================================================================================================================#
triQc %>%
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
    title = "Total Surface Water Discharge Intensity Onsite",
    x = "facility states",
    y = "total surface water discharge intensity (onsite)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.0, hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 12))
#======================================================================================================================#
triQc %>%
  select(
    facility.state, industry.name, naics.code, own_code, federal.facility,
    govt.owned.facility, total.releases.onsite,
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
### Balance Test
#======================================================================================================================#
library(tableone)

b_test <- CreateTableOne(
  vars = c("treated", "trade.secret"),
  strata = "treated",
  factorVars = c("treated, trade.secret"),
  test = T,
  data = triQc
)

print(b_test, smd = T)
# Define treatment variable and covariates

library(vtable)
triQc %>%
  select(treated, trade.secret) %>%
  sumtable(group = "treated", group.test = T)

library(cobalt)
triQc$p.score <- glm(treated ~ wage.perhr, data = triQc,
                     family = "binomial")$fitted.values
covariates <- subset(triQc, select = c(wage.perhr))

## Propensity score weighting using IPTW
triQc$iptw.weights <- ifelse(test = triQc$treated == 1,
                             yes = 1 / triQc$p.score,
                             no = 1 / (1 - triQc$p.score))

bal.tab(x = covariates, treat = "treated", data = triQc,
        weights = "iptw.weights", s.d.denom = "pooled")

