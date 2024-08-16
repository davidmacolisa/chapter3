#======================================================================================================================#
### County level Fixed Effect Functions
#======================================================================================================================#
county_fes <- function() {
  fes <- paste(
	"year +",
	"fips.code.fe +",
	"border.county.fe +",
	"border.county.year.fe"
  )
  return(fes)
}

did_county_fes <- function() {
  fes <- paste(
	"csw(",
	"year",
	"fips.code.fe",
	"border.county.fe",
	"border.county.year.fe)",
	sep = ","
  )
  return(fes)
}

#======================================================================================================================#
### DID Preliminary Functions
#======================================================================================================================#
did_preliminary <- function(data, depvar, ATT, cluster = NULL, did_county_fes) {
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "entire.facility", "private.facility", "federal.facility")

  formula <- as.formula(
	paste0(
	  depvar, " ~ ", ATT, " + ",
	  "sw0(",
	  paste(vars, collapse = "+", sep = " "),
	  ")", "|",
	  as.character(did_county_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

dynamic_did_preliminary <- function(data, depvar, relative_year, cluster = NULL, county_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "entire.facility", "private.facility", "federal.facility")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", "i(", relative_year, ", ref = c(-1, Inf)", ") +",
	  paste(vars, collapse = "+", sep = " "), "|",
	  as.character(county_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

#======================================================================================================================#
### SDID Preliminary Functions
#======================================================================================================================#
sdid_preliminary <- function(data, depvar, ATT, cluster = NULL, did_county_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "entire.facility", "private.facility", "federal.facility")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", ATT, " + ",
	  "sw0(",
	  paste(vars, collapse = "+", sep = " "),
	  ")", "|",
	  as.character(did_county_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

dynamic_sdid_preliminary <- function(data, depvar, ATT, cluster = NULL, county_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "entire.facility", "private.facility", "federal.facility")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", ATT, " + ",
	  paste(vars, collapse = "+", sep = " "), "|",
	  as.character(county_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

#======================================================================================================================#
# SDID preliminary heterogeneous functions
#======================================================================================================================#
sdid_preliminary_heter_1 <- function(data, depvar, interact_var, cluster = NULL, county_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "entire.facility", "private.facility", "federal.facility")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", "sunab(ch.year, year):", interact_var, " + ",
	  "e.treated +",
	  "treated:", interact_var, " + ",
	  "post:", interact_var, " + ",
	  "treated +",
	  interact_var, "+",
	  "post +",
	  paste(vars, collapse = "+", sep = " "), "|",
	  as.character(county_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	cluster = cluster,
	data = data
  )

  return(model)
}

sdid_preliminary_heter_0 <- function(data, depvar, interact_var, cluster = NULL, county_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "entire.facility", "private.facility", "federal.facility")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", "sunab(ch.year, year) + ",
	  "e.treated:", interact_var, " + ",
	  "treated:", interact_var, " + ",
	  "post:", interact_var, " + ",
	  "treated +",
	  interact_var, "+",
	  "post +",
	  paste(vars, collapse = "+", sep = " "), "|",
	  as.character(county_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	cluster = cluster,
	data = data
  )

  return(model)
}

#======================================================================================================================#
### TRI Fixed Effect Functions
#======================================================================================================================#
tri_fes <- function() {
  fes <- paste(
	"year +",
	"facility.id.fe +",
	"border.county.fe +",
	"chemical.id.fe +",
	"chemical.year.fe +",
	"border.county.year"
  )
  return(fes)
}

did_tri_fes <- function() {
  fes <- paste(
	"csw(",
	"year",
	"facility.id.fe",
	"border.county.fe",
	"chemical.id.fe",
	"chemical.year.fe",
	"border.county.year)",
	sep = ","
  )
  return(fes)
}

#======================================================================================================================#
### DID Releases Functions
#======================================================================================================================#
did_releases <- function(data, depvar, ATT, cluster = NULL, did_tri_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite", "clean.air.act.chems", "hap.chems", "pbt.chems")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", ATT, " + ",
	  "sw0(",
	  paste(vars, collapse = "+", sep = " "),
	  ")", "|",
	  as.character(did_tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

dynamic_did_releases <- function(data, depvar, relative_year, cluster = NULL, tri_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite", "clean.air.act.chems", "hap.chems", "pbt.chems")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", "i(", relative_year, ", ref = c(-1, Inf)", ") +",
	  paste(vars, collapse = "+", sep = " "), "|",
	  as.character(tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

#======================================================================================================================#
### SDID Releases Functions
#======================================================================================================================#
sdid_releases <- function(data, depvar, ATT, cluster = NULL, did_tri_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite", "clean.air.act.chems", "hap.chems", "pbt.chems")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", ATT, " + ",
	  "sw0(",
	  paste(vars, collapse = "+", sep = " "),
	  ")", "|",
	  as.character(did_tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

dynamic_sdid_releases <- function(data, depvar, ATT, cluster = NULL, tri_fes) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite", "clean.air.act.chems", "hap.chems", "pbt.chems")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ ", ATT, " + ",
	  paste(vars, collapse = "+", sep = " "), "|",
	  as.character(tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

#======================================================================================================================#
### SDID Heterogeneous Functions
#======================================================================================================================#
sdid_releases_heter_1 <- function(data, depvar, interact_var, tri_fes, cluster = NULL) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite", "clean.air.act.chems", "hap.chems", "pbt.chems")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ sunab(ch.year, year):", interact_var, " + ",
	  "e.treated + ",
	  "treated:", interact_var, " + ",
	  "post:", interact_var, " + ",
	  "treated + ",
	  interact_var, " + ",
	  "post + ",
	  paste(vars, collapse = "+", sep = " "),
	  "|",
	  as.character(tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

sdid_releases_heter_0 <- function(data, depvar, interact_var, tri_fes, cluster = NULL) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite", "clean.air.act.chems", "hap.chems", "pbt.chems")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ sunab(ch.year, year)", " + ",
	  "e.treated:", interact_var, " + ",
	  "treated:", interact_var, " + ",
	  "post:", interact_var, " + ",
	  "treated + ",
	  interact_var, " + ",
	  "post + ",
	  paste(vars, collapse = "+", sep = " "),
	  "|",
	  as.character(tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )
}

#======================================================================================================================#
### SDID Heterogeneous Functions---EPA: CAA, HAPs and PBT chemicals
#======================================================================================================================#
sdid_releases_heter_epa_1 <- function(data, depvar, interact_var, tri_fes, cluster = NULL) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ sunab(ch.year, year):", interact_var, " + ",
	  "e.treated + ",
	  "treated:", interact_var, " + ",
	  "post:", interact_var, " + ",
	  "treated + ",
	  interact_var, " + ",
	  "post + ",
	  paste(vars, collapse = "+", sep = " "),
	  "|",
	  as.character(tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )

  return(model)
}

sdid_releases_heter_epa_0 <- function(data, depvar, interact_var, tri_fes, cluster = NULL) {
  # Define the regressors
  vars <- c("gdppc.1", "annual.avg.estabs.1", "cpi.1", "federal.facility", "produced.chem.facility",
			"imported.chem.facility", "chemical.formulation.component", "chemical.article.component",
			"chemical.manufacturing.aid", "chemical.ancilliary.use", "production.ratio.activity.index",
			"maxnum.chem.onsite")

  # Construct the formula dynamically
  formula <- as.formula(
	paste0(
	  depvar, " ~ sunab(ch.year, year)", " + ",
	  "e.treated:", interact_var, " + ",
	  "treated:", interact_var, " + ",
	  "post:", interact_var, " + ",
	  "treated + ",
	  interact_var, " + ",
	  "post + ",
	  paste(vars, collapse = "+", sep = " "),
	  "|",
	  as.character(tri_fes())
	)
  )

  # Fit the model using feols
  model <- feols(
	formula,
	data = data,
	cluster = cluster
  )
}

#======================================================================================================================#