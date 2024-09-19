# ---- Description --------------------------------------------------------------------------------

# Global constants for minimum wage analysis.

# ---- Individual fixed effects, clusters, and controls -------------------------------------------

ind_fe1 = function() {
  c(
    "indiv_fe",
    "border_county_cluster_time_fe"
  )
}

ind_fe2 = function() {
  c(
    "indiv_fe",
    "border_county_cluster_time_fe",
    "firm_time_fe", 
    "cohort_time_fe", 
    "tenure_time_fe" 
  )
}

ind_fe3 = function() {
  c(
    "indiv_fe",
    "bin_border_county_cluster_time_fe",
    "bin_firm_time_fe", 
    "bin_cohort_time_fe",
    "bin_tenure_time_fe"
  )
}

ind_clusters = function() {
  c(
    "fips_code"
  )
}

ind_ctrls = function() {
  c(
    "l3_state_gdp_pc",
    "l3_state_hpi_purch_sa_gr"
  )
}

ind_ref_month = function() {
  -1
}

# ---- Hiring fixed effects, clusters, and controls -----------------------------------------------

hir_fe1 = function() {
  c(
    "county_bin_fe",
    "border_county_cluster_bin_time_fe"
  )
}

hir_fe2 = function() { 
  c(
    "county_firm_title_bin_fe",
    "border_county_cluster_firm_title_bin_time_fe"
  )
}

hir_fe4 = function() {
  c(
    "firm_county_fe",
    "border_county_cluster_time_fe",
    "firm_time_fe"
  )
}

hir_clusters = function() { 
  c(
    "county_bin_fe"
  )
}

hir_ctrls = function() { 
  c(
    "l3_state_gdp_pc",
    "l3_state_hpi_purch_sa_gr"
  )
}

# ---- Establishment fixed effects, clusters, and controls ----------------------------------------

# Fixed effects 
est_fe1 = function() {
  c(
    "firm_county_fe",
    "border_county_cluster_time_fe"
  )
}

est_fe2 = function() {
  c(
    "firm_county_fe",
    "border_county_cluster_time_fe",
    "firm_time_fe"
  )
}

est_fe3 = function() {
  c(
    "firm_county_bin_fe",
    "border_county_cluster_bin_time_fe",
    "firm_match_bin_time_fe" 
  )
}

est_clusters = function() {
  c(
    "fips_code"
  )
}

est_ctrls = function() {
  c(
    "l3_state_gdp_pc",
    "l3_state_hpi_purch_sa_gr"
  )
}

est_ref_month = function() {
  -1
}

