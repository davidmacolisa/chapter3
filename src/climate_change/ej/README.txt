README File for Unforeseen MW Consequences
All scripts are written in R.

The data for onsite facilities are in "./data/triQ_onsite_econj.rds"
The data for offsite facilities are in "./data/triQ_offsite_econj.rds"
The data for POTWs facilities are in "./data/triQ_potws_econj.rds"

# Scripts
The cross-border quasi-experiment design is in "border_state_county.R"

The formulas are in "functions.R"

The maps are in "maps.R"

To draw Table 1: Exogenous State-Level MW adjustments, use "mw_changes_tbl.R"

The descriptive statistics are in "descriptive_stats.R"

# Preliminary regressions
For Wages, Wage per hour, Total Payroll, and Material Cost, use "wage_did_onsite.R"

For Employment, Production workers, and Hours, use "wage_did_onsite.R"

For Output, output per hour, output per worker or labour productivity, use "wage_did_offsite.R"

For Profits and profit margin, use "wage_did_offsite.R"

Preliminary heterogeneity, use "wage_did_heterogeneous.R"

Preliminary robustness checks, use "wage_did_robustness.R"

The state-level results are in all "*_state.R" scripts.



# Main regressions: Environmental effects
# Onsite release intensities
Total intensity, air emissions intensity, surface water discharge and land releases, use "emissions_did_onsite.R"

Heterogeneous effects, use "emissions_did_onsite_heterogeneous.R"

Robustness checks, use "emissions_did_onsite_robustness.R"

Mechanisms, use "emissions_did_mechanisms.R"

# Offsite and POTWs release intensities
Offsite Total release intensities, use "emissions_did_offsite.R"
POTWs Total release intensities, use "emissions_did_potw.R"