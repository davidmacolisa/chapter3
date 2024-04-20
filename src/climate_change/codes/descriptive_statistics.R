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
file <- "./Data_PhD/US/BLS/onsite/triQc.rds"
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
      dioxin.chem.class > 0 ~ "TRI",
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

chemicals <- chemicals[order(chemicals$chemical.name),]
chemicals108 <- slice(chemicals, 1:108)
chemicals216 <- slice(chemicals, 109:216)

chems <- cbind(chemicals108, chemicals216) %>% data.frame()

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
