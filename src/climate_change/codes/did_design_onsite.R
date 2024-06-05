#======================================================================================================================#
### PhD Chapter 3
### Indirect Consequences of a Raising Minimum Wage
### 30 November 2023
### Using Border-County Designs
#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(statar)
library(usgeogr)
# install.packages("remotes")
# remotes::install_github("davidsovich/usgeogr")
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data---Onsite
#======================================================================================================================#
start_time <- Sys.time()
triQ <- read_rds(file = "./Data_PhD/US/BLS/triQ.rds") %>%
  group_by(fips_code, year) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    tot.ch.percent = (tot.ch.amt / start.mw) * 100,
    end.mw = start.mw + tot.ch.amt,
    lat = as.character(lat),
    long = as.character(long)
  ) %>%
  select(
    c(
      year, facility.id, facility.zipcode, zip.length, facility.city, fips_code, facility.county, facility.state, state,
      lat, long, zip.length, naics.code, industry.name, chemical.id, chemical.name, chemical.classification,
      unit.of.measure,
      # air pollution emissions
      total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
      # water pollution
      total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
      # land pollution
      total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
      total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
      total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite,
      # all releases onsite
      total.releases.onsite,

      # onsite waste management
      energy.recovery.onsite, industrial.kiln.onsite, industrial.furnace.onsite, industrial.boiler.onsite,
      # recycling
      recycling.onsite, metal.recovery.onsite, solvent.recovery.onsite, reuse.onsite,
      # reuse
      reuse.onsite, biological.treatment.onsite, chemical.treatment.onsite,
      incineration.thermal.treatment.onsite, physical.treatment.onsite, material.subandmod,
      # onsite chemical production
      produced.chem.facility, imported.chem.facility, pi.chem.facility,
      # treatment onsite
      treatment.onsite, air.emissions.treatment.onsite, biological.treatment.onsite, chemical.treatment.onsite,
      incineration.thermal.treatment.onsite, physical.treatment.onsite,
      # overall onsite treatment: sum of energy reovery, recycling and treatment.onsite
      total.waste.management.onsite,
      # Other onsite mechanisms - onsite Source or Pollution Reduction Activities
      material.subandmod:intro.inline.productquality.process.analysis.opt,
      # placebo---when included, keep only complete cases in the dataset. This would reduce sample size
      total.release.onsite.catastrophicevents,
      # control variables
      maxnum.chem.onsite:chemical.ancilliary.use,
      # macro controls (including state-level regional price parity and population), county level wages and contributions,
      # establishments, employment, and industry-level variables (total employment, pay, production workers,
      # production worker hours, production workers wages, value of shipments, material cost, value added,
      # investments, inventories, energy cost, capital, equipment, plant, and total factor productivity growth rate)
      cpi:tfp5, population, treated:sum2.sub.mw.ch, tot.ch.amt, tot.ch.percent, start.mw, end.mw, match.ch.amt,
      match.ch.year, dist.to.border
    )) %>%
  mutate(private.facility = ifelse(test = own_code == 5, yes = 1, no = 0)) %>%
  data.frame()
end_time <- Sys.time()
end_time - start_time
gc()

triQc <- triQ
#======================================================================================================================#
### Generating mehcanism variables
#======================================================================================================================#
zero_cols <- names(triQc)[colSums(triQc == 0) == nrow(triQc)]
sort(unique(triQc$comment.type.description))
sort(unique(triQc$classification))

sorted_comments <- triQc %>%
  # filter(classification == "Source Reduction") %>%
  group_by(classification, comment.type, comment.type.description) %>%
  summarize(
    comment.type = sort(unique(comment.type.description)),
    comment.text = sort(unique(comment.text))
  ) %>%
  data.frame()
sort(unique(sorted_comments$comment.type.description))

triQc <- triQc %>%
  mutate(
    ### Source Reduction Activities
    source.reduction = case_when(classification == "Source Reduction" ~ 1, T ~ 0),

    ## Raw Materials Modifications
    material.subandmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W42 - Substituted raw materials" ~ 1, T ~ 0
    ),
    sub.rawm.feedstock.reactchem.matsubmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W43 - Substituted a feedstock or reagent chemical with a different chemical" ~ 1, T ~ 0
    ),
    sub.fuel.matsubmod = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "fuel", x = comment.text, ignore.case = T) |
        grepl(pattern = "natural gas", x = comment.text, ignore.case = T) |
        comment.text == "Have switched from coal to gas as an energy source for steam production in 2014." |
        comment.text == "Transitioned from use of two fossil fuel (#6 oil) boilers to a biomass gasification unit" |
        comment.text == "Replaced Coal boiler with natural gas boiler." |
        comment.text == "Replaced Coal fired boiler with natural gas boiler" |
        comment.text == "Switched from #6 Fuel Oil to Natural Gas fired boilerInitiated work to eventually substitute less toxic colorant for lead containing colorant" |
        comment.text == "Switched from burning coal in winter to only natural gas.  Coal combustion was responsible for the coincidental manufacturing of chromium compounds." |
        comment.text == "Switched over from coal burning (winter) to only natural gas." |
        comment.text == "Switched to natural gas in 2011.  Did not use #6 fuel oil in RY 2013." ~ 1, T ~ 0
    ),
    sub.organic.solvent.matsubmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W56 - Reduced or eliminated use of an organic solvent" ~ 1, T ~ 0
    ),
    sub.manu.proccess.ancilliary.chems.matsubmod = case_when(
      classification == "Source Reduction" &
        comment.text == "Reduction of on hand chemicals aided in avoiding material expiration contributing to waste.  Continue to tighten operational controls and extend chemical usage life to reduce generation of waste." |
        comment.text == "In 2009 we replaced our thermal oxidizer to a more efficient unit.  It has both increased our capture and detruction efficiencies as well as reduced our consumption of natural gas in our manufacturing processes.  Though our processes require the use of chemical processing aids, we are truly committed to source reduction and minimizing our emissions and waste wherever possible via employee suggestion and support from our vendor base." |
        comment.text == "Reduce the amount of Chemical as a processing aid in the formulations of the product"
        ~ 1, T ~ 0
    ),
    mod.content.grade.purity.chems.matsubmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W41 - Increased purity of raw materials" ~ 1, T ~ 0
    ),
    other.matmods.matsubmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W49 - Other raw material modifications made" ~ 1, T ~ 0
    ),
    r.and.d = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "research", x = comment.text, ignore.case = T) |
        grepl(pattern = "development", x = comment.text, ignore.case = T) |
        grepl(pattern = "academic", x = comment.text, ignore.case = T) |
        grepl(pattern = "experiment", x = comment.text, ignore.case = T) |
        grepl(pattern = "LVP", x = comment.text, ignore.case = T) ~ 1, T ~ 0),

    # product modification
    product.modification = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W81 - Changed product specifications" |
        (comment.type.description == "Source Reduction" &
          grepl(pattern = "mod", x = comment.text, ignore.case = T)) ~ 1, T ~ 0
    ),
    devd.newproductline.pmod = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "reformulate", x = comment.text, ignore.case = T) |
        grepl(pattern = "new product", x = comment.text, ignore.case = T) |
        comment.type.description == "W84 - Developed a new chemical product to replace previous chemical product" ~ 1, T ~ 0
    ),
    alt.dim.comp.design.pmod = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "All new design", x = comment.text, ignore.case = T) |
        comment.text == "Primarily used cleaner components." ~ 1, T ~ 0
    ),
    mod.packaging.pmod = case_when(
      classification == "Source Reduction" &
        (grepl(pattern = "J&J", x = comment.text, ignore.case = T) |
          grepl(pattern = "packaging", x = comment.text, ignore.case = T)) ~ 1, T ~ 0
    ),
    other.pmods.pmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W89 - Other product modifications" ~ 1, T ~ 0
    ),

    # process modification
    optimised.process.efficiency.pequipmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "Source Reduction" &
        (grepl(pattern = "efficiency", x = comment.text, ignore.case = T) |
          grepl(pattern = "optimized", x = comment.text, ignore.case = T)) ~ 1, T ~ 0
    ),
    newtech.technique.process.pequipmod = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "technology", x = comment.text, ignore.case = T) |
        grepl(pattern = "technique", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    recirculationinprocess.pequipmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W51 - Instituted recirculation within a process" ~ 1, T ~ 0
    ),
    process.equip.modification = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "equipment", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    equipment.upgrade.update.pequipmod = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "equipment upgrade", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    other.pequipmods.pequipmod = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W58 - Other process modifications" ~ 1, T ~ 0
    ),

    # Operation
    improved.schdule.operation.procedures.opt = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W13 - Improved maintenance scheduling, record keeping, or procedures" ~ 1, T ~ 0
    ),
    intro.inline.productquality.process.analysis.opt = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W15 - Introduced an in-line product quality monitoring or other process analysis system" ~ 1, T ~ 0
    ),
    changed.production.schedule.opt = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W14 - Changed production schedule to minimize equipment and feedstock changeovers" ~ 1, T ~ 0
    ),
    operating.practices.training = case_when(
      classification == "Source Reduction" &
        (comment.type.description == "W32 - Improved procedures for loading, unloading, and transfer operations" |
          grepl(pattern = "operating practice", x = comment.text, ignore.case = T)) ~ 1, T ~ 0
    ),

    # Inventory and material management
    inventory.material.mgt = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "inventory management", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    containers.sizechange.immgt = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "containers", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    improved.monitoring.immgt = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "monitoring", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    better.labelling.testing.immgt = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "label", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    improved.materialhandling.operations.immgt = case_when(
      classification == "Source Reduction" &
        grepl(pattern = "handling", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    other.immgts.immgt = case_when(
      classification == "Source Reduction" &
        comment.type.description == "W19 - Other changes in operating practices Inventory Control" |
        comment.type.description == "W29 - Other changes in inventory control" ~ 1, T ~ 0
    ),

    # Other mechanisms
    waste.water.treatment = case_when(
      grepl(pattern = "waste water treatment", x = comment.text, ignore.case = T) ~ 1, T ~ 0
    ),
    recycling.dummy = case_when(comment.type.description == "Recycling" ~ 1, T ~ 0),
  ) %>%
  select(-c(pfas.chems, elemental.metal.included, chemical.intermediate.uses))

zero_cols <- names(triQc)[colSums(triQc == 0) == nrow(triQc)]
sort(unique(triQc$comment.type.description))
sort(unique(triQc$classification))

sum_up(triQc, c(energy.recovery.onsite, industrial.kiln.onsite, industrial.furnace.onsite, industrial.boiler.onsite,
                recycling.onsite, metal.recovery.onsite, solvent.recovery.onsite, reuse.onsite,
                biological.treatment.onsite, chemical.treatment.onsite, incineration.thermal.treatment.onsite,
                physical.treatment.onsite, material.subandmod, treatment.onsite, air.emissions.treatment.onsite,
                total.waste.management.onsite, sub.fuel.matsubmod, sub.organic.solvent.matsubmod,
                sub.rawm.feedstock.reactchem.matsubmod, sub.manu.proccess.ancilliary.chems.matsubmod,
                mod.content.grade.purity.chems.matsubmod, other.matmods.matsubmod, product.modification,
                devd.newproductline.pmod, alt.dim.comp.design.pmod, mod.packaging.pmod, other.pmods.pmod,
                process.equip.modification, optimised.process.efficiency.pequipmod,
                recirculationinprocess.pequipmod, newtech.technique.process.pequipmod, equipment.upgrade.update.pequipmod,
                other.pequipmods.pequipmod, inventory.material.mgt, better.labelling.testing.immgt, containers.sizechange.immgt,
                improved.materialhandling.operations.immgt, improved.monitoring.immgt, other.immgts.immgt,
                operating.practices.training, improved.schdule.operation.procedures.opt, waste.water.treatment, recycling.dummy,
                changed.production.schedule.opt, intro.inline.productquality.process.analysis.opt))
#======================================================================================================================#
### Keeping only complete cases
#======================================================================================================================#
triQc <- triQc %>%
  select(
    -c(comment.type, comment.text, comment.type.description, classification, production.or.activity,
       alt.dim.comp.design.pmod, equipment.upgrade.update.pequipmod)
  )

# Winsorise the total onsite releases for catastrophe events: replace with the median == 0
triQc$total.release.onsite.catastrophicevents <- ifelse(
  test = is.na(triQc$total.release.onsite.catastrophicevents),
  yes = median(triQc$total.release.onsite.catastrophicevents, na.rm = T),
  no = triQc$total.release.onsite.catastrophicevents
)
table(is.na(triQc$total.release.onsite.catastrophicevents))
sum_up(triQc, c(total.release.onsite.catastrophicevents))

sum(is.na(triQc))
na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
triQc <- triQc[complete.cases(triQc),]
sort(unique(triQc$year))
sum(is.na(triQc))

sum_up(triQc, c(energy.recovery.onsite, industrial.kiln.onsite, industrial.furnace.onsite, industrial.boiler.onsite,
                recycling.onsite, metal.recovery.onsite, solvent.recovery.onsite, reuse.onsite,
                biological.treatment.onsite, chemical.treatment.onsite, incineration.thermal.treatment.onsite,
                physical.treatment.onsite, material.subandmod, treatment.onsite, air.emissions.treatment.onsite,
                total.waste.management.onsite, sub.fuel.matsubmod, sub.organic.solvent.matsubmod,
                sub.rawm.feedstock.reactchem.matsubmod, sub.manu.proccess.ancilliary.chems.matsubmod,
                mod.content.grade.purity.chems.matsubmod, other.matmods.matsubmod, product.modification,
                devd.newproductline.pmod, mod.packaging.pmod, other.pmods.pmod,
                process.equip.modification, optimised.process.efficiency.pequipmod,
                recirculationinprocess.pequipmod, newtech.technique.process.pequipmod,
                other.pequipmods.pequipmod, inventory.material.mgt, better.labelling.testing.immgt, containers.sizechange.immgt,
                improved.materialhandling.operations.immgt, improved.monitoring.immgt, other.immgts.immgt,
                operating.practices.training, improved.schdule.operation.procedures.opt, waste.water.treatment, recycling.dummy,
                changed.production.schedule.opt, intro.inline.productquality.process.analysis.opt))

# sum(is.na(triQc$facility.id))
# triQc_na <- triQc[triQc$facility.id == "NA",]
# fill_columns <- colnames(triQc_na)[colSums(!is.na(triQc_na)) > 0]
# triQc <- triQc[complete.cases(triQc$facility.id),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$production.ratio.activity.index))
# triQc_na <- triQc[triQc$production.ratio.activity.index == "NA",]
# fill_columns <- colnames(triQc_na)[colSums(!is.na(triQc_na)) > 0]
# triQc <- triQc[complete.cases(triQc$production.ratio.activity.index),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$maxnum.chem.onsite))
# triQc_na <- triQc[triQc$maxnum.chem.onsite == "NA",]
# fill_columns <- colnames(triQc_na)[colSums(!is.na(triQc_na)) > 0]
# triQc <- triQc[complete.cases(triQc$maxnum.chem.onsite),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$personal_income))
# triQc_na <- triQc[triQc$personal_income == "NA",]
# fill_columns <- colnames(triQc_na)[colSums(!is.na(triQc_na)) > 0]
# triQc <- triQc[complete.cases(triQc$personal_income),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$own_code))
# triQc_na <- triQc[triQc$own_code == "NA",]
# fill_columns <- colnames(triQc_na)[colSums(!is.na(triQc_na)) > 0]
# triQc <- triQc[complete.cases(triQc$own_code),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
#
# sum(is.na(triQc$emp))
# triQc_na <- triQc[triQc$emp == "NA",]
# fill_columns <- colnames(triQc_na)[colSums(!is.na(triQc_na)) > 0]
# triQc <- triQc[complete.cases(triQc$emp),]
# na_columns <- colnames(triQc)[colSums(is.na(triQc)) > 0]
# sort(unique(triQc$year))
# sum(is.na(triQc))
#======================================================================================================================#
### Functions to panelize the IDs
#======================================================================================================================#
# Function to get unique IDs across years
get_unique_ids <- function(df, id_var, year_var) {
  # order the df by year
  df <- df[order(df[[year_var]]),]

  # Get unique IDs for each year
  unique_ids_by_year <- split(df[[id_var]], df[[year_var]])
  unique_ids <- lapply(unique_ids_by_year, FUN = unique)

  # Get the IDs from the first year
  ids_year1 <- unique_ids[[1]]

  # Check if these IDs are in all other years
  ids_in_all_years <- Reduce(f = intersect, x = unique_ids)

  # Subset the data frame to keep only these IDs
  df_subset <- df[df[[id_var]] %in% ids_in_all_years,]

  return(df_subset)
}

# Function to check if IDs in the first year are the same across all years
check_ids <- function(df, id_var, year_var) {
  # order the df by year
  df <- df[order(df[[year_var]]),]

  # Get unique IDs for each year
  unique_ids <- split(df[[id_var]], df[[year_var]])
  unique_ids <- lapply(unique_ids, FUN = unique)

  # Get the IDs from the first year
  ids_year1 <- unique_ids[[1]]

  # Check if these IDs are in all other years
  ids_in_all_years <- Reduce(f = intersect, x = unique_ids)

  # Check if the IDs from the first year are the same as the IDs in all years
  same_ids <- all(ids_year1 %in% ids_in_all_years)

  # Print a statement saying whether or not the IDs are the same across years
  if (same_ids) {
    print("The IDs in the first year are the same across all other years.")
  } else {
    print("The IDs in the first year are not the same across all other years.")
  }
}

#======================================================================================================================#
### Keeping only common facility.id across years---Panelize the facility.ids
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "facility.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.id", year_var = "year")
n_distinct(triQc$facility.id)
#======================================================================================================================#
### Keeping only common facility.zipcode across years---Panelize the facility.zipcodes
#======================================================================================================================#
# triQc <- get_unique_ids(df = triQc, id_var = "facility.zipcode", year_var = "year")
# check_ids(df = triQc, id_var = "facility.zipcode", year_var = "year")
# n_distinct(triQc$facility.zipcode)
#======================================================================================================================#
### Keeping only common facility.city across years---Panelize the facility.city
#======================================================================================================================#
# triQc <- get_unique_ids(df = triQc, id_var = "facility.city", year_var = "year")
# check_ids(df = triQc, id_var = "facility.city", year_var = "year")
# n_distinct(triQc$facility.city)
#======================================================================================================================#
### Keeping only common naics.code across years---Panelize the naics.code
#======================================================================================================================#
# triQc <- get_unique_ids(df = triQc, id_var = "naics.code", year_var = "year")
# check_ids(df = triQc, id_var = "naics.code", year_var = "year")
# n_distinct(triQc$naics.code)
#======================================================================================================================#
### Keeping only common chemical across years---Panelize the chemicals
#======================================================================================================================#
triQc <- get_unique_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
n_distinct(triQc$chemical.id)
#======================================================================================================================#
### Keeping only common facility states across years---Panelize the facility.state
#======================================================================================================================#
#triQc <- get_unique_ids(df = triQc, id_var = "facility.state", year_var = "year")
# check_ids(df = triQc, id_var = "facility.state", year_var = "year")
#======================================================================================================================#
check_ids(df = triQc, id_var = "facility.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.zipcode", year_var = "year")
check_ids(df = triQc, id_var = "facility.city", year_var = "year")
check_ids(df = triQc, id_var = "fips_code", year_var = "year")
check_ids(df = triQc, id_var = "facility.county", year_var = "year")
check_ids(df = triQc, id_var = "chemical.id", year_var = "year")
check_ids(df = triQc, id_var = "facility.state", year_var = "year")
check_ids(df = triQc, id_var = "treated.match", year_var = "year")
check_ids(df = triQc, id_var = "treated.cluster.id", year_var = "year")
#======================================================================================================================#
sum_up(triQc %>% filter(treated == 1),
       c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
         total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
         total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
         total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
         total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite))

sum_up(triQc %>% filter(treated == 0),
       c(total.fug.air.emissions.onsite, total.point.air.emissions.onsite, total.air.emissions.onsite,
         total.num.receiving.streams.onsite, total.surface.water.discharge.onsite,
         total.underground.injection.I.wells.onsite, total.underground.injection.I.IV.wells.onsite,
         total.underground.injection.onsite, total.landfills.onsite, total.releases.toland.treatment.onsite,
         total.surface.impoundment.onsite, total.land.releases.other.onsite, total.land.releases.onsite))

sort(unique(triQc$year))
sort(unique(triQc$state))
sort(unique(triQc$chemical.name))
n_distinct(triQc$state)
n_distinct(triQc$facility.id)
n_distinct(triQc$facility.zipcode)
n_distinct(triQc$facility.city)
n_distinct(triQc$fips_code)
n_distinct(triQc$facility.county)
n_distinct(triQc$chemical.name)
n_distinct(triQc$naics.code)
n_distinct(triQc$industry.name)
#======================================================================================================================#
### Converting variables in triQc to numeric
#======================================================================================================================#
triQc <- triQc %>%
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long),
    industrial.kiln.onsite = as.numeric(industrial.kiln.onsite),
    industrial.furnace.onsite = as.numeric(industrial.furnace.onsite),
    industrial.boiler.onsite = as.numeric(industrial.boiler.onsite),
    metal.recovery.onsite = as.numeric(metal.recovery.onsite),
    solvent.recovery.onsite = as.numeric(solvent.recovery.onsite),
    reuse.onsite = as.numeric(reuse.onsite),
    biological.treatment.onsite = as.numeric(biological.treatment.onsite),
    chemical.treatment.onsite = as.numeric(chemical.treatment.onsite),
    incineration.thermal.treatment.onsite = as.numeric(incineration.thermal.treatment.onsite),
    physical.treatment.onsite = as.numeric(physical.treatment.onsite),
    material.subandmod = as.numeric(material.subandmod),
    sub.fuel.matsubmod = as.numeric(sub.fuel.matsubmod),
    sub.organic.solvent.matsubmod = as.numeric(sub.organic.solvent.matsubmod),
    sub.rawm.feedstock.reactchem.matsubmod = as.numeric(sub.rawm.feedstock.reactchem.matsubmod),
    sub.manu.proccess.ancilliary.chems.matsubmod = as.numeric(sub.manu.proccess.ancilliary.chems.matsubmod),
    mod.content.grade.purity.chems.matsubmod = as.numeric(mod.content.grade.purity.chems.matsubmod),
    other.matmods.matsubmod = as.numeric(other.matmods.matsubmod),
    product.modification = as.numeric(product.modification),
    devd.newproductline.pmod = as.numeric(devd.newproductline.pmod),
    # alt.dim.comp.design.pmod = as.numeric(alt.dim.comp.design.pmod),
    mod.packaging.pmod = as.numeric(mod.packaging.pmod),
    other.pmods.pmod = as.numeric(other.pmods.pmod),
    process.equip.modification = as.numeric(process.equip.modification),
    optimised.process.efficiency.pequipmod = as.numeric(optimised.process.efficiency.pequipmod),
    recirculationinprocess.pequipmod = as.numeric(recirculationinprocess.pequipmod),
    newtech.technique.process.pequipmod = as.numeric(newtech.technique.process.pequipmod),
    # equipment.upgrade.update.pequipmod = as.numeric(equipment.upgrade.update.pequipmod),
    other.pequipmods.pequipmod = as.numeric(other.pequipmods.pequipmod),
    inventory.material.mgt = as.numeric(inventory.material.mgt),
    better.labelling.testing.immgt = as.numeric(better.labelling.testing.immgt),
    containers.sizechange.immgt = as.numeric(containers.sizechange.immgt),
    improved.materialhandling.operations.immgt = as.numeric(improved.materialhandling.operations.immgt),
    improved.monitoring.immgt = as.numeric(improved.monitoring.immgt),
    other.immgts.immgt = as.numeric(other.immgts.immgt),
    operating.practices.training = as.numeric(operating.practices.training),
    improved.schdule.operation.procedures.opt = as.numeric(improved.schdule.operation.procedures.opt),
    changed.production.schedule.opt = as.numeric(changed.production.schedule.opt),
    intro.inline.productquality.process.analysis.opt = as.numeric(intro.inline.productquality.process.analysis.opt),
    trade.secret = as.numeric(trade.secret),
    sanitised = as.numeric(sanitised),
    entire.facility = as.numeric(entire.facility),
    federal.facility = as.numeric(federal.facility),
    govt.owned.facility = as.numeric(govt.owned.facility),
    # elemental.metal.included = as.numeric(elemental.metal.included),
    clean.air.act.chems = as.numeric(clean.air.act.chems),
    carcinogenic.chems = as.numeric(carcinogenic.chems),
    metal.restrict.tri = as.numeric(metal.restrict.tri),
    produced.chem.facility = as.numeric(produced.chem.facility),
    imported.chem.facility = as.numeric(imported.chem.facility),
    pi.chem.facility = as.numeric(pi.chem.facility),
    # chemical.intermediate.uses = as.numeric(chemical.intermediate.uses),
    chemical.formulation.component = as.numeric(chemical.formulation.component),
    chemical.article.component = as.numeric(chemical.article.component),
    chemical.manufacturing.aid = as.numeric(chemical.manufacturing.aid),
    chemical.ancilliary.use = as.numeric(chemical.ancilliary.use),
    source.reduction = as.numeric(source.reduction),
    r.and.d = as.numeric(r.and.d),
    recycling.dummy = as.numeric(recycling.dummy),
    waste.water.treatment = as.numeric(waste.water.treatment),
  )
glimpse(triQc)
#======================================================================================================================#
### Facility-level aggregation---Onsite
#======================================================================================================================#
# library(collapse)
# triQf <- triQc %>%
#   collap(
#     X = .,
#     by = ~
#       year +
#         # facility.id +
#         facility.zipcode +
#         facility.city +
#         fips_code +
#         facility.county +
#         facility.state +
#         state +
#         lat +
#         long +
#         zip.length +
#         naics.code +
#         industry.name +
#         chemical.id +
#         chemical.name +
#         chemical.classification +
#         unit.of.measure +
#         total.fug.air.emissions.onsite +
#         total.point.air.emissions.onsite +
#         total.air.emissions.onsite +
#         total.num.receiving.streams.onsite +
#         total.surface.water.discharge.onsite +
#         total.underground.injection.I.wells.onsite +
#         total.underground.injection.I.IV.wells.onsite +
#         total.underground.injection.onsite +
#         total.landfills.onsite +
#         total.releases.toland.treatment.onsite +
#         total.surface.impoundment.onsite +
#         total.land.releases.other.onsite +
#         total.land.releases.onsite +
#         total.releases.onsite +
#         energy.recovery.onsite +
#         industrial.kiln.onsite +
#         industrial.furnace.onsite +
#         industrial.boiler.onsite +
#         recycling.onsite +
#         metal.recovery.onsite +
#         solvent.recovery.onsite +
#         reuse.onsite +
#         biological.treatment.onsite +
#         chemical.treatment.onsite +
#         incineration.thermal.treatment.onsite +
#         physical.treatment.onsite +
#         material.subandmod +
#         produced.chem.facility +
#         imported.chem.facility +
#         pi.chem.facility +
#         treatment.onsite +
#         air.emissions.treatment.onsite +
#         total.waste.management.onsite +
#         sub.fuel.matsubmod +
#         sub.organic.solvent.matsubmod +
#         sub.rawm.feedstock.reactchem.matsubmod +
#         sub.manu.proccess.ancilliary.chems.matsubmod +
#         mod.content.grade.purity.chems.matsubmod +
#         other.matmods.matsubmod +
#         product.modification +
#         devd.newproductline.pmod +
#         alt.dim.comp.design.pmod +
#         mod.packaging.pmod +
#         other.pmods.pmod +
#         process.equip.modification +
#         optimised.process.efficiency.pequipmod +
#         recirculationinprocess.pequipmod +
#         newtech.technique.process.pequipmod +
#         equipment.upgrade.update.pequipmod +
#         other.pequipmods.pequipmod +
#         inventory.material.mgt +
#         better.labelling.testing.immgt +
#         containers.sizechange.immgt +
#         improved.materialhandling.operations.immgt +
#         improved.monitoring.immgt +
#         other.immgts.immgt +
#         operating.practices.training +
#         improved.schdule.operation.procedures.opt +
#         changed.production.schedule.opt +
#         intro.inline.productquality.process.analysis.opt +
#         maxnum.chem.onsite +
#         trade.secret +
#         sanitised +
#         entire.facility +
#         federal.facility +
#         govt.owned.facility +
#         elemental.metal.included +
#         clean.air.act.chems +
#         carcinogenic.chems +
#         metal.restrict.tri +
#         production.ratio.activity.index +
#         chemical.intermediate.uses +
#         chemical.formulation.component +
#         chemical.article.component +
#         chemical.manufacturing.aid +
#         chemical.ancilliary.use +
#         cpi +
#         personal_income +
#         gdp +
#         compensation_to_employees +
#         bea_unit +
#         regional_price_parity +
#         bea_rpp_unit +
#         own_code +
#         annual_avg_estabs +
#         annual_avg_emplvl +
#         total_annual_wages +
#         taxable_annual_wages +
#         annual_contributions +
#         annual_avg_wkly_wage +
#         avg_annual_pay +
#         emp +
#         pay +
#         prode +
#         prodh +
#         prodw +
#         vship +
#         matcost +
#         vadd +
#         invest +
#         invent +
#         energy +
#         cap +
#         equip +
#         plant +
#         tfp4 +
#         tfp5 +
#         population +
#         treated +
#         treated.match +
#         control.match +
#         overlap +
#         state.border.id +
#         treated.cluster.name +
#         control.cluster.name +
#         treated.cluster.id +
#         control.cluster.id +
#         cbcp.id +
#         treated.cluster.population +
#         control.cluster.population +
#         treated.cluster.lat +
#         treated.cluster.long +
#         control.cluster.lat +
#         control.cluster.long +
#         ch.year +
#         ch.amt +
#         sum2.sub.mw.ch +
#         start.mw +
#         end.mw +
#         match.ch.amt +
#         match.ch.year +
#         dist.to.border
#     ,
#     na.rm = T,
#     FUN = fsum,
#     catFUN = fmode,
#     keep.col.order = T,
#     return = "long"
#   ) %>%
#   select(-c(Function, facility.id))
#
# triQf <- triQf[complete.cases(triQf),]
# sum(is.na(triQf))
# sort(unique(triQf$year))
# sort(unique(triQf$state))
# sort(unique(triQf$chemical.name))
# n_distinct(triQf$state)
# n_distinct(triQf$facility.id)
# n_distinct(triQf$facility.zipcode)
# n_distinct(triQf$facility.city)
# n_distinct(triQf$fips_code)
# n_distinct(triQf$facility.county)
# n_distinct(triQf$chemical.name)
# n_distinct(triQf$naics.code)
# n_distinct(triQf$industry.name)
#======================================================================================================================#
### For the state-level analysis---Onsite
### Collapse triQc to state level
#======================================================================================================================#
# library(collapse)
# triQs <- triQc %>%
#   collap(
#     X = .,
#     by = ~
#       year +
#         facility.id +
#         facility.zipcode +
#         zip.length +
#         fips_code +
#         facility.state +
#         state +
#         naics.code +
#         industry.name +
#         chemical.id +
#         chemical.name +
#         chemical.classification +
#         unit.of.measure +
#         total.fug.air.emissions.onsite +
#         total.point.air.emissions.onsite +
#         total.air.emissions.onsite +
#         total.num.receiving.streams.onsite +
#         total.surface.water.discharge.onsite +
#         total.underground.injection.I.wells.onsite +
#         total.underground.injection.I.IV.wells.onsite +
#         total.underground.injection.onsite +
#         total.landfills.onsite +
#         total.releases.toland.treatment.onsite +
#         total.surface.impoundment.onsite +
#         total.land.releases.other.onsite +
#         total.land.releases.onsite +
#         total.releases.onsite +
#         energy.recovery.onsite +
#         industrial.kiln.onsite +
#         industrial.furnace.onsite +
#         industrial.boiler.onsite +
#         recycling.onsite +
#         metal.recovery.onsite +
#         solvent.recovery.onsite +
#         reuse.onsite +
#         biological.treatment.onsite +
#         chemical.treatment.onsite +
#         incineration.thermal.treatment.onsite +
#         physical.treatment.onsite +
#         material.subandmod +
#         produced.chem.facility +
#         imported.chem.facility +
#         pi.chem.facility +
#         treatment.onsite +
#         air.emissions.treatment.onsite +
#         total.waste.management.onsite +
#         sub.fuel.matsubmod +
#         sub.organic.solvent.matsubmod +
#         sub.rawm.feedstock.reactchem.matsubmod +
#         sub.manu.proccess.ancilliary.chems.matsubmod +
#         mod.content.grade.purity.chems.matsubmod +
#         other.matmods.matsubmod +
#         product.modification +
#         devd.newproductline.pmod +
#         # alt.dim.comp.design.pmod +
#         mod.packaging.pmod +
#         other.pmods.pmod +
#         process.equip.modification +
#         optimised.process.efficiency.pequipmod +
#         recirculationinprocess.pequipmod +
#         newtech.technique.process.pequipmod +
#         # equipment.upgrade.update.pequipmod +
#         other.pequipmods.pequipmod +
#         inventory.material.mgt +
#         better.labelling.testing.immgt +
#         containers.sizechange.immgt +
#         improved.materialhandling.operations.immgt +
#         improved.monitoring.immgt +
#         other.immgts.immgt +
#         operating.practices.training +
#         improved.schdule.operation.procedures.opt +
#         changed.production.schedule.opt +
#         intro.inline.productquality.process.analysis.opt +
#         maxnum.chem.onsite +
#         trade.secret +
#         sanitised +
#         entire.facility +
#         private.facility +
#         federal.facility +
#         govt.owned.facility +
#         # elemental.metal.included +
#         clean.air.act.chems +
#         carcinogenic.chems +
#         metal.restrict.tri +
#         production.ratio.activity.index +
#         # chemical.intermediate.uses +
#         chemical.formulation.component +
#         chemical.article.component +
#         chemical.manufacturing.aid +
#         chemical.ancilliary.use +
#         source.reduction +
#         r.and.d +
#         waste.water.treatment +
#         recycling.dummy +
#         cpi +
#         personal_income +
#         gdp +
#         compensation_to_employees +
#         bea_unit +
#         regional_price_parity +
#         bea_rpp_unit +
#         own_code +
#         annual_avg_estabs +
#         annual_avg_emplvl +
#         total_annual_wages +
#         taxable_annual_wages +
#         annual_contributions +
#         annual_avg_wkly_wage +
#         avg_annual_pay +
#         emp +
#         pay +
#         prode +
#         prodh +
#         prodw +
#         vship +
#         matcost +
#         vadd +
#         invest +
#         invent +
#         energy +
#         cap +
#         equip +
#         plant +
#         tfp4 +
#         tfp5 +
#         population +
#         treated +
#         treated.match +
#         control.match +
#         # treated.cluster.name +
#         # treated.cluster.id +
#         # control.cluster.name +
#         # control.cluster.id +
#         # cbcp.id +
#         # treated.cluster.population +
#         # control.cluster.population +
#         # treated.cluster.lat +
#         # treated.cluster.long +
#         # control.cluster.lat +
#         # control.cluster.long +
#         overlap +
#         state.border.id +
#         ch.year +
#         ch.amt +
#         sum2.sub.mw.ch +
#         tot.ch.amt +
#         start.mw +
#         end.mw +
#         match.ch.amt +
#         match.ch.year +
#         dist.to.border
#     ,
#     na.rm = T,
#     FUN = fsum,
#     catFUN = fmode,
#     keep.col.order = T,
#     return = "long"
#   ) %>%
#   select(
#     -c(Function, facility.city:facility.county, lat, long, treated.cluster.name:cbcp.id,
#        treated.cluster.lat:control.cluster.long, treated.cluster.population, control.cluster.population
#     )
#   ) %>%
#   mutate(
#     industrial.kiln.onsite = as.numeric(industrial.kiln.onsite),
#     industrial.furnace.onsite = as.numeric(industrial.furnace.onsite),
#     industrial.boiler.onsite = as.numeric(industrial.boiler.onsite),
#     metal.recovery.onsite = as.numeric(metal.recovery.onsite),
#     solvent.recovery.onsite = as.numeric(solvent.recovery.onsite),
#     reuse.onsite = as.numeric(reuse.onsite),
#     biological.treatment.onsite = as.numeric(biological.treatment.onsite),
#     chemical.treatment.onsite = as.numeric(chemical.treatment.onsite),
#     incineration.thermal.treatment.onsite = as.numeric(incineration.thermal.treatment.onsite),
#     physical.treatment.onsite = as.numeric(physical.treatment.onsite),
#     material.subandmod = as.numeric(material.subandmod),
#     sub.fuel.matsubmod = as.numeric(sub.fuel.matsubmod),
#     sub.organic.solvent.matsubmod = as.numeric(sub.organic.solvent.matsubmod),
#     sub.rawm.feedstock.reactchem.matsubmod = as.numeric(sub.rawm.feedstock.reactchem.matsubmod),
#     sub.manu.proccess.ancilliary.chems.matsubmod = as.numeric(sub.manu.proccess.ancilliary.chems.matsubmod),
#     mod.content.grade.purity.chems.matsubmod = as.numeric(mod.content.grade.purity.chems.matsubmod),
#     other.matmods.matsubmod = as.numeric(other.matmods.matsubmod),
#     product.modification = as.numeric(product.modification),
#     devd.newproductline.pmod = as.numeric(devd.newproductline.pmod),
#     # alt.dim.comp.design.pmod = as.numeric(alt.dim.comp.design.pmod),
#     mod.packaging.pmod = as.numeric(mod.packaging.pmod),
#     other.pmods.pmod = as.numeric(other.pmods.pmod),
#     process.equip.modification = as.numeric(process.equip.modification),
#     optimised.process.efficiency.pequipmod = as.numeric(optimised.process.efficiency.pequipmod),
#     recirculationinprocess.pequipmod = as.numeric(recirculationinprocess.pequipmod),
#     newtech.technique.process.pequipmod = as.numeric(newtech.technique.process.pequipmod),
#     # equipment.upgrade.update.pequipmod = as.numeric(equipment.upgrade.update.pequipmod),
#     other.pequipmods.pequipmod = as.numeric(other.pequipmods.pequipmod),
#     inventory.material.mgt = as.numeric(inventory.material.mgt),
#     better.labelling.testing.immgt = as.numeric(better.labelling.testing.immgt),
#     containers.sizechange.immgt = as.numeric(containers.sizechange.immgt),
#     improved.materialhandling.operations.immgt = as.numeric(improved.materialhandling.operations.immgt),
#     improved.monitoring.immgt = as.numeric(improved.monitoring.immgt),
#     other.immgts.immgt = as.numeric(other.immgts.immgt),
#     operating.practices.training = as.numeric(operating.practices.training),
#     improved.schdule.operation.procedures.opt = as.numeric(improved.schdule.operation.procedures.opt),
#     changed.production.schedule.opt = as.numeric(changed.production.schedule.opt),
#     intro.inline.productquality.process.analysis.opt = as.numeric(intro.inline.productquality.process.analysis.opt),
#     trade.secret = as.numeric(trade.secret),
#     sanitised = as.numeric(sanitised),
#     entire.facility = as.numeric(entire.facility),
#     federal.facility = as.numeric(federal.facility),
#     govt.owned.facility = as.numeric(govt.owned.facility),
#     # elemental.metal.included = as.numeric(elemental.metal.included),
#     clean.air.act.chems = as.numeric(clean.air.act.chems),
#     carcinogenic.chems = as.numeric(carcinogenic.chems),
#     metal.restrict.tri = as.numeric(metal.restrict.tri),
#     produced.chem.facility = as.numeric(produced.chem.facility),
#     imported.chem.facility = as.numeric(imported.chem.facility),
#     pi.chem.facility = as.numeric(pi.chem.facility),
#     # chemical.intermediate.uses = as.numeric(chemical.intermediate.uses),
#     chemical.formulation.component = as.numeric(chemical.formulation.component),
#     chemical.article.component = as.numeric(chemical.article.component),
#     chemical.manufacturing.aid = as.numeric(chemical.manufacturing.aid),
#     chemical.ancilliary.use = as.numeric(chemical.ancilliary.use),
#     r.and.d = as.numeric(r.and.d),
#     waste.water.treatment = as.numeric(waste.water.treatment),
#     recycling.dummy = as.numeric(recycling.dummy),
#   )
#
# sum(is.na(triQs))
# # na_columns <- colnames(triQs)[colSums(is.na(triQs)) > 0]
# sort(unique(triQs$year))
#======================================================================================================================#
### TRI for researchers: source - https://shorturl.at/kqvy7
### CAUSING CANCER: source- https://ntp.niehs.nih.gov/whatwestudy/assessments/cancer/roc#toc1
#======================================================================================================================#
# Outdoor air pollutants
# Arsenic, arsenic compounds,Chromium, Chromium compounds (except for chromite ore mined in the Transvaal Region),
# Nickel, Nickel compounds, Vanadium (except when contained in an alloy), Vanadium compounds.

# Indoor air pollutants
# Asbestos (friable),

# Both indoor and outdoor air pollutants
# Lead, Lead compounds, Mercury, Mercury compounds, Ammonia,
# hazardous air pollutants (benzene, 1,3-Butadiene, Hexachloro-1,3-butadiene)
#======================================================================================================================#
### Standardising variable formats
#======================================================================================================================#
# triQc <- triQc %>% mutate(ind.output.lb = vadd * 1000)
#======================================================================================================================#
### Variables Creation
### Log Transformations with x + 1 to correct for 0
#======================================================================================================================#
# If $1 bill weighs 1 gram; and 454 grams = 1lb. Then, $454 bills = 1lb. Thus, ind.output.lb = ind.output / 454
triQc <- triQc %>%
  mutate(
    vadd = vadd / 100,
    l.vadd = log(x = vadd),
    output.perworker = vadd / emp,
    l.output.perworker = log(x = output.perworker),
    output.perhr = vadd / prodh,
    l.output.perhr = log(x = output.perhr),
    wage.perworker = prodw / prode,
    l.wage.perworker = log(x = wage.perworker),
    wage.perhr = prodw / prodh,
    l.wage.perhr = log(x = wage.perhr),
    energy.intensity = energy / vadd,
    l.energy.intensity = log(x = energy.intensity),
    total.air.emissions.onsite.intensity = total.air.emissions.onsite / vadd,
    l.total.air.emissions.onsite.intensity = log(x = total.air.emissions.onsite.intensity + 1),
    total.fug.air.emissions.onsite.intensity = total.fug.air.emissions.onsite / vadd,
    l.total.fug.air.emissions.onsite.intensity = log(x = total.fug.air.emissions.onsite.intensity + 1),
    total.point.air.emissions.onsite.intensity = total.point.air.emissions.onsite / vadd,
    l.total.point.air.emissions.onsite.intensity = log(x = total.point.air.emissions.onsite.intensity + 1),
    total.surface.water.discharge.onsite.intensity = total.surface.water.discharge.onsite / vadd,
    l.total.surface.water.discharge.onsite.intensity = log(x = total.surface.water.discharge.onsite.intensity + 1),
    total.underground.injection.I.wells.onsite.intensity = total.underground.injection.I.wells.onsite / vadd,
    l.total.underground.injection.I.wells.onsite.intensity = log(x = total.underground.injection.I.wells.onsite.intensity + 1),
    total.underground.injection.I.IV.wells.onsite.intensity = total.underground.injection.I.IV.wells.onsite / vadd,
    l.total.underground.injection.I.IV.wells.onsite.intensity = log(x = total.underground.injection.I.IV.wells.onsite.intensity + 1),
    total.underground.injection.onsite.intensity = total.underground.injection.onsite / vadd,
    l.total.underground.injection.onsite.intensity = log(x = total.underground.injection.onsite.intensity + 1),
    total.landfills.onsite.intensity = total.landfills.onsite / vadd,
    l.total.landfills.onsite.intensity = log(x = total.landfills.onsite.intensity + 1),
    total.releases.toland.treatment.onsite.intensity = total.releases.toland.treatment.onsite / vadd,
    l.total.releases.toland.treatment.onsite.intensity = log(x = total.releases.toland.treatment.onsite.intensity + 1),
    total.surface.impoundment.onsite.intensity = total.surface.impoundment.onsite / vadd,
    l.total.surface.impoundment.onsite.intensity = log(x = total.surface.impoundment.onsite.intensity + 1),
    total.land.releases.other.onsite.intensity = total.land.releases.other.onsite / vadd,
    l.total.land.releases.other.onsite.intensity = log(x = total.land.releases.other.onsite.intensity + 1),
    total.land.releases.onsite.intensity = total.land.releases.onsite / vadd,
    l.total.land.releases.onsite.intensity = log(x = total.land.releases.onsite.intensity + 1),
    total.releases.onsite.intensity = total.releases.onsite / vadd,
    l.total.releases.onsite.intensity = log(x = total.releases.onsite.intensity + 1),
    total.release.onsite.catastrophicevents.intensity = total.release.onsite.catastrophicevents / vadd,
    l.total.release.onsite.catastrophicevents.intensity = log(x = total.release.onsite.catastrophicevents.intensity + 1),
    l.industrial.kiln.onsite = log(x = (industrial.kiln.onsite + 1)),
    l.industrial.boiler.onsite = log(x = (industrial.boiler.onsite + 1)),
    l.industrial.furnace.onsite = log(x = (industrial.furnace.onsite + 1)),
    l.industrial.boiler.onsite = log(x = (industrial.boiler.onsite + 1)),
    l.recycling.onsite = log(x = (recycling.onsite + 1)),
    l.reuse.onsite = log(x = (reuse.onsite + 1)),
    l.energy.recovery.onsite = log(x = (energy.recovery.onsite + 1)),
    l.metal.recovery.onsite = log(x = (metal.recovery.onsite + 1)),
    l.solvent.recovery.onsite = log(x = (solvent.recovery.onsite + 1)),
    l.treatment.onsite = log(x = (treatment.onsite + 1)),
    l.biological.treatment.onsite = log(x = (biological.treatment.onsite + 1)),
    l.chemical.treatment.onsite = log(x = (chemical.treatment.onsite + 1)),
    l.physical.treatment.onsite = log(x = (physical.treatment.onsite + 1)),
    l.incineration.thermal.treatment.onsite = log(x = (incineration.thermal.treatment.onsite + 1)),
    l.air.emissions.treatment.onsite = log(x = (air.emissions.treatment.onsite + 1)),
    l.total.waste.management.onsite = log(x = (total.waste.management.onsite + 1)),
    l.annual.avg.emplvl = log(x = (annual_avg_emplvl + 1)),
    l.total.annual.wages = log(x = (total_annual_wages + 1)),
    l.taxable.annual.wages = log(x = (taxable_annual_wages + 1)),
    l.annual.contributions = log(x = (annual_contributions + 1)),
    l.annual.avg.wkly.wages = log(x = (annual_avg_wkly_wage + 1)),
    l.avg.annual.pay = log(x = (avg_annual_pay + 1)),
    l.cpi = log(x = (cpi)),
    annual.avg.emplvl.1 = stats::lag(annual_avg_emplvl, k = 1),
    total.annual.wages.1 = stats::lag(total_annual_wages, k = 1),
    taxable.annual.wages.1 = stats::lag(taxable_annual_wages, k = 1),
    annual.contributions.1 = stats::lag(annual_contributions, k = 1),
    annual.avg.wkly.wages.1 = stats::lag(annual_avg_wkly_wage, k = 1),
    avg.annual.pay.1 = stats::lag(avg_annual_pay, k = 1),
    private.naics = ifelse(test = own_code == 5, yes = 1, no = 0),
    cpi.1 = stats::lag(cpi, k = 1),
    emp.1 = stats::lag(emp, k = 1),
    l.emp = log(emp),
    l.pay = log(pay),
    l.prode = log(prode),
    l.prodh = log(prodh),
    l.prodw = log(prodw),
    l.revenue = log(vship),
    l.matcost = log(matcost),
    l.invest = log(invest),
    l.invent = log(invent),
    l.vadd = log(vadd),
    l.revenue = log(vship),
    l.cap = log(cap),
    l.equip = log(equip),
    l.plant = log(plant),
    l.tfp4 = log(tfp4),
    l.tfp5 = log(tfp5),
    gdp.pc = gdp / population,
    gdppc.1 = stats::lag(gdp.pc, k = 1),
    gdp.1 = stats::lag(gdp, k = 1),
    pinc.1 = stats::lag(personal_income, k = 1),
    annual.avg.estabs.1 = stats::lag(annual_avg_estabs, k = 1),
    population.1 = stats::lag(population, k = 1),
    cap.1 = stats::lag(cap, k = 1),
    equip.1 = stats::lag(equip, k = 1),
    plant.1 = stats::lag(plant, k = 1),
    vadd.1 = stats::lag(vadd, k = 1),
    revenue.1 = stats::lag(vship, k = 1),
    invest.1 = stats::lag(invest, k = 1),
    invent.1 = stats::lag(invent, k = 1),
  )
#======================================================================================================================#
### Experiment Design
#======================================================================================================================#
triQc <- triQc %>%
  rename(fips.code = fips_code) %>%
  mutate(
    hap.chems = case_when(chemical.classification == "TRI" ~ 1, T ~ 0),
    dioxin.chems = case_when(chemical.classification == "Dioxin" ~ 1, T ~ 0),
    pbt.chems = case_when(chemical.classification == "PBT" ~ 1, T ~ 0),
  ) %>%
  mutate(
    e.treated = case_when(year >= ch.year ~ 1, T ~ 0), #states e-years away from the initial treatment year
    rel.year = year - ch.year,
    post = case_when(year == 2014 | year == 2015 | year == 2017 ~ 1, T ~ 0),
    rel_year = year - 2014,
    naics.code = as.numeric(naics.code),
    facility.id = as.numeric(facility.id),
    facility.zipcode = as.numeric(facility.zipcode),
    facility.id.fe = as.numeric(as.factor(facility.id)),
    chemical.id.fe = as.numeric(as.factor(chemical.id)),
    facility.state.fe = as.numeric(as.factor(facility.state)),
    fips.code.fe = as.numeric(as.factor(fips.code)),
    fips.code = as.numeric(fips.code),
    facility.county.fe = as.numeric(as.factor(facility.county)),
    border.county = as.numeric(treated.cluster.id) * as.numeric(control.cluster.id),
    border.county.fe = as.numeric(as.factor(border.county)),
    border.state = as.numeric(as.factor(treated.match)) * as.numeric(as.factor(control.match)),
    border.state.fe = as.numeric(as.factor(border.state)),
    fac.chem.fe = facility.id.fe * chemical.id.fe,
    fips.state.fe = fips.code.fe * facility.state.fe,
    chem.ind.state = chemical.id.fe *
      as.numeric(as.factor(naics.code)) *
      facility.state.fe,
    fips.year.fe = fips.code.fe * year,
    facility.year.fe = facility.id.fe * year,
    chemical.year.fe = chemical.id.fe * year,
    border.county.year.fe = border.county.fe * year,
    border.state.year.fe = border.state.fe * year,
    state.year.fe = facility.state.fe * year,
    fac.chem.year.fe = facility.id.fe * chemical.id.fe * year,
  )
#======================================================================================================================#
### check for zero columns
#======================================================================================================================#
zero_cols <- names(triQc)[colSums(triQc == 0) == nrow(triQc)]
triQc <- triQc[, !names(triQc) %in% zero_cols]
# zero_cols <- names(triQs)[colSums(triQs == 0) == nrow(triQs)]
# triQs <- triQs[, !names(triQs) %in% zero_cols]
#======================================================================================================================#
### Save data
#======================================================================================================================#
start_time <- Sys.time()
write_rds(triQc, file = "./Data_PhD/US/BLS/onsite/triQc_on.rds", compress = "xz")
# write_rds(triQs, file = "./Data_PhD/US/BLS/onsite/triQs_on.rds", compress = "xz")
end_time <- Sys.time()
end_time - start_time
#======================================================================================================================#