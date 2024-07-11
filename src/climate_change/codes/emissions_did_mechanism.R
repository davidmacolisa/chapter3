#======================================================================================================================#
library(tidyverse)
library(statar)
library(fixest)
library(did)
library(car)
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
### Financial Constraints
### I proxy financial constraints by the ratio of industries profit margin. That is, the ratio of industry revenue to
# profit, which measures the proportion of industry revenue allocated to profit. Industries with profit margin lower
# than the median profit margin are more financially constrained, and those above are less.
#======================================================================================================================#
triQc <- triQc %>%
  group_by(year, naics.code) %>%
  rename(revenue = vship) %>%
  mutate(
	total.cost = matcost + prodw + energy,
	profit = revenue - total.cost,
	revenue.to.profit = (revenue / profit) * 100,
	# Operational efficiency ratio
	payroll.to.revenue = pay / revenue,
	wages.to.revenue = prodw / revenue,
  ) %>%
  ungroup() %>%
  mutate(
	high.profit.margin = case_when(revenue.to.profit > mean(revenue.to.profit) ~ 1, TRUE ~ 0),
	high.payroll.revenue = case_when(payroll.to.revenue > mean(payroll.to.revenue) ~ 1, TRUE ~ 0),
	high.wages.revenue = case_when(wages.to.revenue > mean(wages.to.revenue) ~ 1, TRUE ~ 0),
  )

triQc %>% sum_up(
  c(high.payroll.revenue, high.wages.revenue, high.profit.margin)
)
#======================================================================================================================#
### Waste Management Activities - Onsite
#======================================================================================================================#
### Total waste management onsite---high profit margin
#======================================================================================================================#
sdid_waste_mgt_onsite_hrpr <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_waste_mgt_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Biological treatment onsite---high profit margin
#======================================================================================================================#
sdid_bio_treatment_hrpr <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_bio_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Physical treatment onsite---high profit margin
#======================================================================================================================#
sdid_physical_treatment_hrpr <- fixest::feols(
  l.physical.treatment.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_physical_treatment_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high profit margin
#======================================================================================================================#
sdid_recycle_onsite_hrpr <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recycle_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Reuse onsite---high profit margin
#======================================================================================================================#
sdid_reuse_onsite_hrpr <- fixest::feols(
  l.reuse.onsite ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_reuse_onsite_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high profit margin (revenue to profit ratio)
#======================================================================================================================#
sdid_source_reduction_hrpr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_source_reduction_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution---high profit margin (revenue to profit ratio)
#======================================================================================================================#
sdid_mat_submod_hrpr <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_submod_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Organic Solvent Substitution---high profit margin (revenue to profit ratio)
#======================================================================================================================#
sdid_organic_solvent_hrpr <- fixest::feols(
  sub.organic.solvent.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_organic_solvent_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high profit margin
#======================================================================================================================#
sdid_clean_fuel_hrpr <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_clean_fuel_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Energy Cost Intensity---high profit margin
#======================================================================================================================#
sdid_energy_cost_intensity_hrpr <- fixest::feols(
  l.energy.cost.intensity ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_cost_intensity_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recirculation in Process---high profit margin
#======================================================================================================================#
sdid_recirculate_inprocess_hrpr <- fixest::feols(
  recirculationinprocess.pequipmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recirculate_inprocess_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high profit margin
#======================================================================================================================#
sdid_new_tech_hrpr <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_tech_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high profit margin
#======================================================================================================================#
sdid_recylce_dummy_hrpr <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recylce_dummy_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Research & Development---high profit margin
#======================================================================================================================#
sdid_research_hrpr <- fixest::feols(
  r.and.d ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_research_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Total factor productivity---high profit margin
#======================================================================================================================#
sdid_tfp_hrpr <- fixest::feols(
  l.tfp5 ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_tfp_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations---high profit margin
#======================================================================================================================#
sdid_mat_handling_hrpr <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_handling_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training---high profit margin
#======================================================================================================================#
sdid_operating_practices_training_hrpr <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year):high.profit.margin +
	e.treated +
	treated:high.profit.margin +
	post:high.profit.margin +
	treated +
	high.profit.margin +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_operating_practices_training_hrpr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Type of Production Technology
# Labour intensive industries---High payroll or wages to revenue ratio
# Capacity intensive industries---Low payroll or wages to revenue ratio
#======================================================================================================================#
### Total waste management onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_waste_mgt_onsite_hprr <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_waste_mgt_onsite_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Biological treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_bio_treatment_hprr <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_bio_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Incineration thermal treatment onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_incineration_treatment_hprr <- fixest::feols(
  l.incineration.thermal.treatment.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_incineration_treatment_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial boiler onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_ind_boiler_hprr <- fixest::feols(
  l.industrial.boiler.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_boiler_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_recycle_onsite_hprr <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recycle_onsite_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high payroll to revenue ratio (labour intensive)
#======================================================================================================================#
sdid_source_reduction_hprr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_source_reduction_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution---high payroll to revenue (payroll to revenue ratio)
#======================================================================================================================#
sdid_mat_submod_hprr <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_submod_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Increase Purity of Chemicals---high payroll to revenue ratio
#======================================================================================================================#
sdid_mat_purity_hprr <- fixest::feols(
  mod.content.grade.purity.chems.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_purity_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high payroll to revenue ratio
#======================================================================================================================#
sdid_clean_fuel_hprr <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_clean_fuel_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material modification: Energy Cost Intensity---high payroll to revenue ratio
#======================================================================================================================#
sdid_energy_cost_intensity_hprr <- fixest::feols(
  l.energy.cost.intensity ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_cost_intensity_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high wages to revenue ratio
#======================================================================================================================#
sdid_new_tech_hprr <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_tech_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high payroll to revenue ratio
#======================================================================================================================#
sdid_recylce_dummy_hprr <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recylce_dummy_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Containers size change---high payroll to revenue ratio
#======================================================================================================================#
sdid_containers_sizechange_hprr <- fixest::feols(
  containers.sizechange.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_containers_sizechange_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations---high payroll to revenue ratio
#======================================================================================================================#
sdid_mat_handling_hprr <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_handling_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training---high payroll to revenue ratio
#======================================================================================================================#
sdid_operating_practices_training_hprr <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_operating_practices_training_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis---high payroll to revenue ratio
#======================================================================================================================#
sdid_inline_prod_quality_analysis_hprr <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inline_prod_quality_analysis_hprr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Type of Production Technology
# Labour intensive industries---High payroll or wages to revenue ratio
# Capacity intensive industries---Low payroll or wages to revenue ratio
#======================================================================================================================#
### Total waste management onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_waste_mgt_onsite_hwrr <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_waste_mgt_onsite_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Biological treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_bio_treatment_hwrr <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_bio_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Incineration thermal treatment onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_incineration_treatment_hwrr <- fixest::feols(
  l.incineration.thermal.treatment.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_incineration_treatment_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial boiler onsite---high wages to revenue ratio
#======================================================================================================================#
sdid_ind_boiler_hwrr <- fixest::feols(
  l.industrial.boiler.onsite ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_ind_boiler_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high payroll to revenue ratio
#======================================================================================================================#
sdid_recycle_onsite_hwrr <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year):high.payroll.revenue +
	e.treated +
	treated:high.payroll.revenue +
	post:high.payroll.revenue +
	treated +
	high.payroll.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recycle_onsite_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high wages to revenue ratio (labour intensive)
#======================================================================================================================#
sdid_source_reduction_hwrr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_source_reduction_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution---high wages to revenue (wages to revenue ratio)
#======================================================================================================================#
sdid_mat_submod_hwrr <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_submod_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Increase Purity of Chemicals---high wages to revenue ratio
#======================================================================================================================#
sdid_mat_purity_hwrr <- fixest::feols(
  mod.content.grade.purity.chems.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_purity_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high wages to revenue ratio
#======================================================================================================================#
sdid_clean_fuel_hwrr <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_clean_fuel_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material modification: Energy Cost Intensity---high wages to revenue ratio
#======================================================================================================================#
sdid_energy_cost_intensity_hwrr <- fixest::feols(
  l.energy.cost.intensity ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_energy_cost_intensity_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high wages to revenue ratio
#======================================================================================================================#
sdid_new_tech_hwrr <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_new_tech_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high wages to revenue ratio
#======================================================================================================================#
sdid_recylce_dummy_hwrr <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_recylce_dummy_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Containers size change---high wages to revenue ratio
#======================================================================================================================#
sdid_containers_sizechange_hwrr <- fixest::feols(
  containers.sizechange.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_containers_sizechange_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations---high wages to revenue ratio
#======================================================================================================================#
sdid_mat_handling_hwrr <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_mat_handling_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training---high wages to revenue ratio
#======================================================================================================================#
sdid_operating_practices_training_hwrr <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_operating_practices_training_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis---high wages to revenue ratio
#======================================================================================================================#
sdid_inline_prod_quality_analysis_hwrr <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year):high.wages.revenue +
	e.treated +
	treated:high.wages.revenue +
	post:high.wages.revenue +
	treated +
	high.wages.revenue +
	post +
	gdppc.1 +
	annual.avg.estabs.1 +
	cpi.1 +
	federal.facility +
	produced.chem.facility +
	imported.chem.facility +
	chemical.formulation.component +
	chemical.article.component +
	chemical.manufacturing.aid +
	chemical.ancilliary.use +
	production.ratio.activity.index +
	maxnum.chem.onsite +
	clean.air.act.chems +
	hap.chems +
	pbt.chems
	|
	year +
	  facility.id.fe +
	  border.county.fe +
	  chemical.id.fe +
	  chemical.year.fe +
	  border.county.year
  ,
  data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_inline_prod_quality_analysis_hwrr, agg = "ATT", digits = 3, digits.stats = 3)
#======================================================================================================================#