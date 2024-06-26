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
### Summary
#======================================================================================================================#
sum_up(triQc,
	   c(total.waste.management.onsite, treatment.onsite, air.emissions.treatment.onsite,
		 biological.treatment.onsite, chemical.treatment.onsite, incineration.thermal.treatment.onsite,
		 physical.treatment.onsite, energy.recovery.onsite, industrial.kiln.onsite, industrial.furnace.onsite,
		 industrial.boiler.onsite, recycling.onsite, metal.recovery.onsite, solvent.recovery.onsite, reuse.onsite,
		 source.reduction, material.subandmod, sub.fuel.matsubmod, sub.organic.solvent.matsubmod,
		 sub.rawm.feedstock.reactchem.matsubmod, sub.manu.proccess.ancilliary.chems.matsubmod,
		 mod.content.grade.purity.chems.matsubmod, other.matmods.matsubmod, product.modification,
		 devd.newproductline.pmod, mod.packaging.pmod, other.pmods.pmod, process.equip.modification,
		 optimised.process.efficiency.pequipmod, recirculationinprocess.pequipmod,
		 newtech.technique.process.pequipmod, other.pequipmods.pequipmod, inventory.material.mgt,
		 better.labelling.testing.immgt, containers.sizechange.immgt, improved.materialhandling.operations.immgt,
		 improved.monitoring.immgt, other.immgts.immgt, operating.practices.training,
		 improved.schdule.operation.procedures.opt, changed.production.schedule.opt,
		 intro.inline.productquality.process.analysis.opt, r.and.d, waste.water.treatment, recycling.dummy))
#======================================================================================================================#
### Waste Management Activities - Onsite
#======================================================================================================================#
### Total waste management onsite
#======================================================================================================================#
sdid_waste_mgt_onsite <- fixest::feols(
  l.total.waste.management.onsite ~ sunab(ch.year, year) +
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
etable(sdid_waste_mgt_onsite, agg = "ATT", digits = 3, digits.stats = 3)
etable(sdid_waste_mgt_onsite, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment onsite
#======================================================================================================================#
sdid_treatment_onsite <- fixest::feols(
  l.treatment.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_treatment_onsite, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_treatment_onsite, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Air emissions treatment onsite
#======================================================================================================================#
sdid_air_emissions_treatment <- fixest::feols(
  l.air.emissions.treatment.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_air_emissions_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_air_emissions_treatment, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Biological treatment onsite
#======================================================================================================================#
sdid_bio_treatment <- fixest::feols(
  l.biological.treatment.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_bio_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_bio_treatment, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Chemical treatment onsite
#======================================================================================================================#
sdid_chem_treatment <- fixest::feols(
  l.chemical.treatment.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_chem_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_chem_treatment, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Physical treatment onsite
#======================================================================================================================#
sdid_physical_treatment <- fixest::feols(
  l.physical.treatment.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_physical_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_physical_treatment, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Treatment menthod: Incineration thermal treatment onsite
#======================================================================================================================#
sdid_incineration_treatment <- fixest::feols(
  l.incineration.thermal.treatment.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_incineration_treatment, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_incineration_treatment, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery onsite
#======================================================================================================================#
sdid_energy_recovery <- fixest::feols(
  l.energy.recovery.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_energy_recovery, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_recovery, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial kiln onsite
#======================================================================================================================#
sdid_ind_klin <- fixest::feols(
  l.industrial.kiln.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_ind_klin, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_klin, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial boiler onsite
#======================================================================================================================#
sdid_ind_boiler <- fixest::feols(
  l.industrial.boiler.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_ind_boiler, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_boiler, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Energy recovery method: Industrial furnace onsite
#======================================================================================================================#
sdid_ind_furnace <- fixest::feols(
  l.industrial.furnace.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_ind_furnace, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_ind_furnace, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling onsite
#======================================================================================================================#
sdid_recycle_onsite <- fixest::feols(
  l.recycling.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_recycle_onsite, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recycle_onsite, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Reuse onsite
#======================================================================================================================#
sdid_reuse_onsite <- fixest::feols(
  l.reuse.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_reuse_onsite, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_reuse_onsite, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Metal recovery onsite
#======================================================================================================================#
sdid_metal_recovery <- fixest::feols(
  l.metal.recovery.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_metal_recovery, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_metal_recovery, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Recycling process: Solvent recovery onsite
#======================================================================================================================#
sdid_solvent_recovery <- fixest::feols(
  l.solvent.recovery.onsite ~ sunab(ch.year, year) +
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
fixest::etable(sdid_solvent_recovery, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_solvent_recovery, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Source Reduction Activities - Onsite
#TODO: Check the effect on chemical manufacturing aid, formulation component, article component and ancilliary use
#======================================================================================================================#
sdid_source_reduction <- fixest::feols(
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
fixest::etable(sdid_source_reduction, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_source_reduction, agg = "cohort", digits = 3, digits.stats = 3)
fixest::iplot(sdid_source_reduction, xlim = c(-3, 3), ylim = c(-0.3, 0.5), col = "blue",
			  main = "Source Reduction Activities, HRPR", xlab = "relative year",
			  lwd = 1, cex = 4, pt.cex = 3, pt.col = "red", pt.join = T, ci.lwd = 5, ci.lty = 1) %>%
  abline(v = -1, col = "red", lty = 2, lwd = 2)

sdid_source_reduction_lrpr <- fixest::feols(
  source.reduction ~ sunab(ch.year, year) +
	e.treated:high.profit.margin +
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
fixest::etable(sdid_source_reduction_lrpr, digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material Substitution
#======================================================================================================================#
sdid_mat_submod <- fixest::feols(
  material.subandmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_mat_submod, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_submod, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Organic Solvent Substitution
#======================================================================================================================#
sdid_organic_solvent <- fixest::feols(
  sub.organic.solvent.matsubmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_organic_solvent, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_organic_solvent, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Feedstock and Reactive Chemical Substitution
#======================================================================================================================#
sdid_feedstock_reactchems <- fixest::feols(
  sub.rawm.feedstock.reactchem.matsubmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_feedstock_reactchems, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_feedstock_reactchems, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: manufacturing aid, processing aid, and ancillary chemical Substitution
#======================================================================================================================#
sdid_manu_aid <- fixest::feols(
  sub.manu.proccess.ancilliary.chems.matsubmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_manu_aid, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_manu_aid, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Increase Purity of Chemicals
#======================================================================================================================#
sdid_mat_purity <- fixest::feols(
  mod.content.grade.purity.chems.matsubmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_mat_purity, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_purity, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Clean Fuel
#======================================================================================================================#
sdid_clean_fuel <- fixest::feols(
  sub.fuel.matsubmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_clean_fuel, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_clean_fuel, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Material: Other material modification
#======================================================================================================================#
sdid_mat_others <- fixest::feols(
  other.matmods.matsubmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_mat_others, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_others, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification
#======================================================================================================================#
sdid_prod_mod <- fixest::feols(
  product.modification ~ sunab(ch.year, year) +
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
  , data = triQc,
  cluster = ~c(chemical.id, naics.code, facility.state)
)
fixest::etable(sdid_prod_mod, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: New Product Line
#======================================================================================================================#
sdid_new_prod_line <- fixest::feols(
  devd.newproductline.pmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_new_prod_line, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_prod_line, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Modified Packaging
#======================================================================================================================#
sdid_mod_packaging <- fixest::feols(
  mod.packaging.pmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_mod_packaging, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mod_packaging, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Energy Intensity
#======================================================================================================================#
sdid_energy_intensity <- fixest::feols(
  l.energy.intensity ~ sunab(ch.year, year) +
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
fixest::etable(sdid_energy_intensity, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_energy_intensity, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Product modification: Other Product Modification
#======================================================================================================================#
sdid_prod_mod_others <- fixest::feols(
  other.pmods.pmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_prod_mod_others, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_prod_mod_others, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process and Equipment Modification
#======================================================================================================================#
sdid_process_equip_mod <- fixest::feols(
  process.equip.modification ~ sunab(ch.year, year) +
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
fixest::etable(sdid_process_equip_mod, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_equip_mod, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Optimised Process Efficiency
#======================================================================================================================#
sdid_optimised_process_efficiency <- fixest::feols(
  optimised.process.efficiency.pequipmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_optimised_process_efficiency, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_optimised_process_efficiency, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recirculation in Process
#======================================================================================================================#
sdid_recirculate_inprocess <- fixest::feols(
  recirculationinprocess.pequipmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_recirculate_inprocess, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recirculate_inprocess, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: New Technology and Technique in manufacturing process
#======================================================================================================================#
sdid_new_tech <- fixest::feols(
  newtech.technique.process.pequipmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_new_tech, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_new_tech, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Recycling to reuse
#======================================================================================================================#
sdid_recylce_dummy <- fixest::feols(
  recycling.dummy ~ sunab(ch.year, year) +
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
fixest::etable(sdid_recylce_dummy, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_recylce_dummy, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Research & Development
#======================================================================================================================#
sdid_research <- fixest::feols(
  r.and.d ~ sunab(ch.year, year) +
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
fixest::etable(sdid_research, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_research, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Process modification: Process Modification, Others
#======================================================================================================================#
sdid_process_mod_others <- fixest::feols(
  other.pequipmods.pequipmod ~ sunab(ch.year, year) +
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
fixest::etable(sdid_process_mod_others, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_process_mod_others, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Material Management
#======================================================================================================================#
sdid_inventory_mat_mgt <- fixest::feols(
  inventory.material.mgt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_inventory_mat_mgt, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mat_mgt, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Better Labelling and Testing
#======================================================================================================================#
sdid_better_labelling_testing <- fixest::feols(
  better.labelling.testing.immgt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_better_labelling_testing, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_better_labelling_testing, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Containers size change
#======================================================================================================================#
sdid_containers_sizechange <- fixest::feols(
  containers.sizechange.immgt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_containers_sizechange, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_containers_sizechange, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved material handling operations
#======================================================================================================================#
sdid_mat_handling <- fixest::feols(
  improved.materialhandling.operations.immgt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_mat_handling, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_mat_handling, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Improved monitoring
#======================================================================================================================#
sdid_improved_monitoring <- fixest::feols(
  improved.monitoring.immgt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_improved_monitoring, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_monitoring, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Inventory management: Inventory Management, Others
#======================================================================================================================#
sdid_inventory_mgt_others <- fixest::feols(
  other.immgts.immgt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_inventory_mgt_others, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inventory_mgt_others, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Operating Practices & Training
#======================================================================================================================#
sdid_operating_practices_training <- fixest::feols(
  operating.practices.training ~ sunab(ch.year, year) +
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
fixest::etable(sdid_operating_practices_training, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_operating_practices_training, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Improved Scheduling Operation
#======================================================================================================================#
sdid_improved_schedule_operation <- fixest::feols(
  improved.schdule.operation.procedures.opt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_improved_schedule_operation, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_improved_schedule_operation, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Changed Production Schedule
#======================================================================================================================#
sdid_changed_prod_schedule <- fixest::feols(
  changed.production.schedule.opt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_changed_prod_schedule, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_changed_prod_schedule, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#
### Operations: Inline Product Quality and Process Analysis
#======================================================================================================================#
sdid_inline_prod_quality_analysis <- fixest::feols(
  intro.inline.productquality.process.analysis.opt ~ sunab(ch.year, year) +
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
fixest::etable(sdid_inline_prod_quality_analysis, agg = "ATT", digits = 3, digits.stats = 3)
fixest::etable(sdid_inline_prod_quality_analysis, agg = "cohort", digits = 3, digits.stats = 3)
#======================================================================================================================#