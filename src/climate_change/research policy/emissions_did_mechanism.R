#======================================================================================================================#
library(tidyverse)
library(statar)
library(fixest)
library(did)
library(car)
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "your_path")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
source(file = "your_path/ej/functions.R", echo = T)
file <- "your_path/ej/triQ_onsite_econj.rds"
triQc <- read_rds(file = file)
#======================================================================================================================#
### High profit and labour intensive vs high profit and capital intensive
#======================================================================================================================#
### I proxy financial constraints by the first pre-treatment period ratio of industries profit margin. That is, the
# ratio of industry revenue to profit, which measures the proportion of industry revenue allocated to profit.
# Industries with profit margin lower than the median profit margin are more financially constrained, and those above
# are less.
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
  ungroup()

rtp_50th_percentile <- quantile(triQc[triQc$year == 2011,]$revenue.to.profit, probs = 0.5)
ptr_50th_percentile <- quantile(triQc[triQc$year == 2011,]$payroll.to.revenue, probs = 0.5)
wtr_50th_percentile <- quantile(triQc[triQc$year == 2011,]$wages.to.revenue, probs = 0.5)
profit_50th_percentile <- quantile(triQc[triQc$year == 2011,]$profit, probs = 0.5)

triQc <- triQc %>%
  mutate(
	high.profit.margin = case_when(revenue.to.profit > rtp_50th_percentile ~ 1, TRUE ~ 0),
	high.payroll.revenue = case_when(payroll.to.revenue > ptr_50th_percentile ~ 1, TRUE ~ 0),
	high.wages.revenue = case_when(wages.to.revenue > wtr_50th_percentile ~ 1, TRUE ~ 0),
	high.profit = case_when(profit > profit_50th_percentile ~ 1, TRUE ~ 0),
	labour.intensive = case_when(high.payroll.revenue == 1 & high.wages.revenue == 1 ~ 1, TRUE ~ 0),
	high.profit.labour = case_when(high.profit == 1 & labour.intensive == 1 ~ 1,
								   high.profit == 1 & labour.intensive == 0 ~ 0),
	low.profit.labour = case_when(high.profit == 0 & labour.intensive == 1 ~ 1,
								  high.profit == 0 & labour.intensive == 0 ~ 0),
	high.profit.capital = case_when(high.profit == 1 & labour.intensive == 0 ~ 1,
									high.profit == 1 & labour.intensive == 1 ~ 0),
	low.profit.capital = case_when(high.profit == 0 & labour.intensive == 0 ~ 1,
								   high.profit == 0 & labour.intensive == 1 ~ 0),
  )

triQc %>% sum_up(c(high.payroll.revenue, high.wages.revenue, high.profit, labour.intensive,
				   high.profit.labour, low.profit.labour, high.profit.capital, low.profit.capital))
#======================================================================================================================#
### Waste Management Activities - Onsite
#======================================================================================================================#
### High profit and labour intensive vs high profit and capital intensive
#======================================================================================================================#
### Treatment method: Chemical treatment onsite---high profit and production technology
#======================================================================================================================#
sdid_chemical_treatment_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.chemical.treatment.onsite",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_chemical_treatment_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_chemical_treatment_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Biological treatment onsite---high profit and production technology
#======================================================================================================================#
sdid_bio_treatment_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.biological.treatment.onsite",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_bio_treatment_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_bio_treatment_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Thermal treatment onsite---high profit and production technology
#======================================================================================================================#
sdid_thermal_treatment_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.incineration.thermal.treatment.onsite",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_thermal_treatment_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_thermal_treatment_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment method: Physical treatment onsite---high profit and production technology
#======================================================================================================================#
sdid_physical_treatment_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.physical.treatment.onsite",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_physical_treatment_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_physical_treatment_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite---high profit and production technology
#======================================================================================================================#
sdid_recycle_onsite_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.recycling.onsite",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_recycle_onsite_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_recycle_onsite_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---high profit and production technology
#======================================================================================================================#
sdid_source_reduction_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "source.reduction",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_source_reduction_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_source_reduction_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Organic Solvent Substitution---high profit and production technology
#======================================================================================================================#
sdid_organic_solvent_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "sub.organic.solvent.matsubmod",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_organic_solvent_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_organic_solvent_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel---high profit and production technology
#======================================================================================================================#
sdid_clean_fuel_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "sub.fuel.matsubmod",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_clean_fuel_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_clean_fuel_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Energy Cost Intensity---high profit and production technology
#======================================================================================================================#
sdid_energy_cost_intensity_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "l.energy.cost.intensity",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_energy_cost_intensity_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_energy_cost_intensity_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process---high profit and production technology
#======================================================================================================================#
sdid_new_tech_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "newtech.technique.process.pequipmod",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_new_tech_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_new_tech_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse---high profit and production technology
#======================================================================================================================#
sdid_recylce_dummy_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "recycling.dummy",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_recylce_dummy_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_recylce_dummy_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Modified spray equipment---high profit and production technology
#======================================================================================================================#
sdid_modified_spray_equipment_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "modified.spray.equipment.pequipmod",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_modified_spray_equipment_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_modified_spray_equipment_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Changes in inventory control---high profit and production technology
#======================================================================================================================#
sdid_changes_inventory_control_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "changes.inventory.control.immgt",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_changes_inventory_control_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_changes_inventory_control_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Implemented inspection and monitoring---high profit and production technology
#======================================================================================================================#
sdid_impl_inspection_monitoring_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "impl.inspection.monitoring.leak.spill.immgt",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_impl_inspection_monitoring_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_impl_inspection_monitoring_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Changed Production Schedule---high profit and production technology
#======================================================================================================================#
sdid_changed_prod_schedule_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "changed.production.schedule.opt",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_changed_prod_schedule_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_changed_prod_schedule_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Changes in Operating Practices---high profit and production technology
#======================================================================================================================#
sdid_changes_operating_practices_hpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "changes.operating.practices.inventory.control.opt",
  interact_var = "high.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_changes_operating_practices_hpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_changes_operating_practices_hpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Onsite: Source Reduction Activities---low profit and production technology
#======================================================================================================================#
### Material: Clean Fuel---low profit and production technology
#======================================================================================================================#
sdid_clean_fuel_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "sub.fuel.matsubmod",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_clean_fuel_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_clean_fuel_lpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recirculation in Process---low profit and production technology
#======================================================================================================================#
sdid_recirculate_inprocess_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "recirculationinprocess.pequipmod",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_recirculate_inprocess_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_recirculate_inprocess_lpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Modified spray equipment---high profit and production technology
#======================================================================================================================#
sdid_modified_spray_equipment_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "modified.spray.equipment.pequipmod",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_modified_spray_equipment_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_modified_spray_equipment_lpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Changes in inventory control---low profit and production technology
#======================================================================================================================#
sdid_changes_inventory_control_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "changes.inventory.control.immgt",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_changes_inventory_control_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_changes_inventory_control_lpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Improved procedure loading and transfer---low profit and production technology
#======================================================================================================================#
sdid_impr_procedure_loading_transfer_lpli <- sdid_releases_heter_1(
  data = triQc,
  depvar = "impr.procedures.loading.transfer.opt",
  interact_var = "low.profit.labour",
  fes = tri_fes(),
  cluster = ~c(chemical.id, naics.code, facility.state)
)
etable(sdid_impr_procedure_loading_transfer_lpli, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_impr_procedure_loading_transfer_lpli, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#