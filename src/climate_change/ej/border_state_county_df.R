#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(usgeogr)
# devtools::install_github(repo = "haozhu233/kableExtra")
library(kableExtra)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
# for the state-level design
data(state_df, package = "usgeogr")
state_df <- state_df %>% data.frame()

names(state_df$state) <- tolower(state_df$state)
state_df$state <- stringi::stri_trans_totitle(state_df$state)

# for border-county design
data(adjacent_county_df, package = "usgeogr")
adjacent_county_df <- adjacent_county_df %>% data.frame()

data(county_df, package = "usgeogr")
county_df <- county_df %>% data.frame()

data(cbcp_df, package = "usgeogr")
cbcp_df <- cbcp_df %>% data.frame()
#======================================================================================================================#
### Experiment Design: State-level MW >= $0.5 for Border States
#======================================================================================================================#
fac_states_df <- state_df %>%
  rename(state.code = state_code) %>%
  filter(
	state.code %in% c(
	  #treated states
	  "AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NJ", "NY", "SD", "WV",
	  #control states
	  "GA", "IA", "ID", "IL", "IN", "KS", "KY", "NH", "NM", "NV", "NC", "ND", "OK",
	  "PA", "TX", "UT", "VA", "WI", "WY"
	)
  ) %>%
  mutate(
	# Treated states
	treated = case_when(
	  state.code %in% c("AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NJ", "NY", "SD", "WV") ~ 1, T ~ 0
	),
	# Year of first MW raise >= $0.45
	ch.year = case_when(
	  state.code == "AR" ~ 2015,
	  state.code == "CA" ~ 2014,
	  state.code == "DE" ~ 2014,
	  state.code == "ME" ~ 2017,
	  state.code == "MA" ~ 2015,
	  state.code == "MD" ~ 2015,
	  state.code == "MI" ~ 2014,
	  state.code == "MN" ~ 2014,
	  state.code == "NE" ~ 2015,
	  state.code == "NJ" ~ 2014,
	  state.code == "NY" ~ 2014,
	  state.code == "SD" ~ 2015,
	  state.code == "WV" ~ 2015, T ~ Inf
	),
	# Exogenous first MW raise
	ch.amt = case_when(
	  state.code == "AR" ~ 1.25,
	  state.code == "CA" ~ 1,
	  state.code == "DE" ~ 0.5,
	  state.code == "ME" ~ 1.5,
	  state.code == "MA" ~ 1,
	  state.code == "MD" ~ 1,
	  state.code == "MI" ~ 0.75,
	  state.code == "MN" ~ 1.85,
	  state.code == "NE" ~ 0.75,
	  state.code == "NJ" ~ 1,
	  state.code == "NY" ~ 0.75,
	  state.code == "SD" ~ 1.25,
	  state.code == "WV" ~ 0.75, T ~ 0
	),
	# Exogenous subsequent MW raise
	sum2.sub.mw.ch = case_when(
	  state.code == "AR" ~ 1,
	  state.code == "CA" ~ 1.5,
	  state.code == "DE" ~ 0.5,
	  state.code == "ME" ~ 0,
	  state.code == "MA" ~ 2,
	  state.code == "MD" ~ 1,
	  state.code == "MI" ~ 0.75,
	  state.code == "MN" ~ 1.5,
	  state.code == "NE" ~ 1,
	  state.code == "NJ" ~ 0.19,
	  state.code == "NY" ~ 1.7,
	  state.code == "SD" ~ 0.15,
	  state.code == "WV" ~ 0.75, T ~ 0
	),
	# Starting MW
	start.mw = case_when(
	  state.code == "AR" ~ 6.25,
	  state.code == "CA" ~ 8,
	  state.code == "DE" ~ 7.25,
	  state.code == "ME" ~ 7.5,
	  state.code == "MA" ~ 8,
	  state.code == "MD" ~ 7.25,
	  state.code == "MI" ~ 7.4,
	  state.code == "MN" ~ 6.16,
	  state.code == "NE" ~ 7.25,
	  state.code == "NJ" ~ 7.25,
	  state.code == "NY" ~ 7.25,
	  state.code == "SD" ~ 7.25,
	  state.code == "WV" ~ 7.25,
	  state.code == "IA" ~ 7.25,
	  state.code == "IL" ~ 8.25,
	  state.code == "IN" ~ 7.25,
	  state.code == "KS" ~ 7.25,
	  state.code == "KY" ~ 7.25,
	  state.code == "NH" ~ 7.25,
	  state.code == "NV" ~ 8.25,
	  state.code == "ND" ~ 7.25,
	  state.code == "OK" ~ 7.25,
	  state.code == "PA" ~ 7.25,
	  state.code == "TX" ~ 7.25,
	  state.code == "VA" ~ 7.25,
	  state.code == "WI" ~ 7.25,
	  state.code == "WY" ~ 7.25),
  ) %>%
  # Match with adjacent neighbor states
  left_join(
	y = adjacent_county_df %>%
	  select(c(county_state, neighbor_state)) %>%
	  rename(match.state = neighbor_state) %>%
	  filter(match.state %in% c("AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NJ", "NY", "SD", "WV")),
	by = c("state.code" = "county_state")
  ) %>%
  # Removing GA, ID, NM, NC and UT for they didn't match to any treated states
  filter(!is.na(match.state)) %>%
  # Removing a treated state matched to another treated state
  mutate(overlap = paste(state.code, match.state, sep = "-")) %>%
  filter(!overlap %in% c("DE-MD", "DE-NJ", "MA-NY", "MD-DE", "MD-WV", "MI-MN", "MN-MI", "MN-SD",
						 "NE-SD", "NJ-DE", "NJ-NY", "NY-MA", "NY-NJ", "SD-MN", "SD-NE", "WV-MD")) %>%
  mutate(
	# Exogenous first MW raise
	match.ch.amt = case_when(
	  match.state == "AR" ~ 1.25,
	  match.state == "CA" ~ 1,
	  match.state == "DE" ~ 0.5,
	  match.state == "ME" ~ 1.5,
	  match.state == "MA" ~ 1,
	  match.state == "MD" ~ 1,
	  match.state == "MI" ~ 0.75,
	  match.state == "MN" ~ 1.85,
	  match.state == "NE" ~ 0.75,
	  match.state == "NJ" ~ 1,
	  match.state == "NY" ~ 0.75,
	  match.state == "SD" ~ 1.25,
	  match.state == "WV" ~ 0.75
	),
	match.ch.year = case_when(
	  match.state == "AR" ~ 2015,
	  match.state == "CA" ~ 2014,
	  match.state == "DE" ~ 2014,
	  match.state == "ME" ~ 2017,
	  match.state == "MA" ~ 2015,
	  match.state == "MD" ~ 2015,
	  match.state == "MI" ~ 2014,
	  match.state == "MN" ~ 2014,
	  match.state == "NE" ~ 2015,
	  match.state == "NJ" ~ 2014,
	  match.state == "NY" ~ 2014,
	  match.state == "SD" ~ 2015,
	  match.state == "WV" ~ 2015
	),
  ) %>%
  distinct()
#======================================================================================================================#
### Experiment Design: Border-county MW changes
#======================================================================================================================#
fac_county_df <- fac_states_df %>%
  left_join(
	y = adjacent_county_df %>%
	  rename(county.name = county_name) %>%
	  filter(
		neighbor_state %in% c("AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NJ", "NY", "SD", "WV")
	  ),
	by = c("state.code" = "county_state")
  ) %>%
  filter(
	!is.na(neighbor_name) |
	  !is.na(neighbor_state) |
	  !is.na(neighbor_fips_code) |
	  !is.na(neighbor_population) |
	  !is.na(neighbor_lat) |
	  !is.na(neighbor_long)
  ) %>%
  rename(
	treated.match = neighbor_state, treated.cluster.name = neighbor_name,
	treated.cluster.id = neighbor_fips_code, treated.cluster.population = neighbor_population,
	treated.cluster.lat = neighbor_lat, treated.cluster.long = neighbor_long
  ) %>%
  left_join(
	# merging the cross-border counties
	y = adjacent_county_df %>%
	  select(c(fips_code, neighbor_name:neighbor_long)) %>%
	  filter(
		neighbor_state %in% c("IA", "IL", "IN", "KS", "KY", "NH", "NV", "ND", "OK", "PA", "TX", "VA", "WI", "WY")
	  ),
	by = c("fips_code" = "fips_code")
  ) %>%
  filter(
	!is.na(neighbor_name) |
	  !is.na(neighbor_state) |
	  !is.na(neighbor_fips_code) |
	  !is.na(neighbor_population) |
	  !is.na(neighbor_lat) |
	  !is.na(neighbor_long)
  ) %>%
  rename(
	control.match = neighbor_state, control.cluster.name = neighbor_name,
	control.cluster.id = neighbor_fips_code, control.cluster.population = neighbor_population,
	control.cluster.lat = neighbor_lat, control.cluster.long = neighbor_long
  ) %>%
  mutate(
	# county distance to state border in kilometres
	dist.to.border = county_to_state_border(fips_code = fips_code, return_state_border_id = FALSE, miles = F)
  ) %>%
  mutate(
	cbcp.id = paste(treated.cluster.id, control.cluster.id, sep = "-"),
	state.border.id = paste(treated.match, control.match, sep = "-")
  ) %>%
  select(
	c(fips_code, county.name, state, state.code, population, lat, long, treated, treated.match, control.match,
	  overlap, state.border.id, treated.cluster.name, control.cluster.name, treated.cluster.id, control.cluster.id,
	  cbcp.id, treated.cluster.population, control.cluster.population, treated.cluster.lat, treated.cluster.long,
	  control.cluster.lat, control.cluster.long, ch.year, ch.amt, sum2.sub.mw.ch, start.mw, match.ch.amt,
	  match.ch.year, dist.to.border
	)
  ) %>%
  distinct()

# fac_county_df <- fac_county_df[complete.cases(fac_county_df$dist.bt.centers),]
sum(is.na(fac_county_df))
n_distinct(fac_county_df$state.code)
n_distinct(fac_county_df$treated.match)
n_distinct(fac_county_df$control.match)
sort(unique(fac_county_df$treated.match))
sort(unique(fac_county_df$control.match))
sort(unique(fac_county_df$state.border.id))
sort(unique(fac_county_df$state.code))
#======================================================================================================================#
### Label the counties
#======================================================================================================================#
# Add state codes to treated cluster names/labels
fac_county_df[fac_county_df$treated.match == "AR",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " AR",
	   fac_county_df[fac_county_df$treated.match == "AR",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "CA",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " CA",
	   fac_county_df[fac_county_df$treated.match == "CA",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "DE",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " DE",
	   fac_county_df[fac_county_df$treated.match == "DE",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "ME",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " ME",
	   fac_county_df[fac_county_df$treated.match == "ME",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "MA",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MA",
	   fac_county_df[fac_county_df$treated.match == "MA",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "MD",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MD",
	   fac_county_df[fac_county_df$treated.match == "MD",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "MI",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MI",
	   fac_county_df[fac_county_df$treated.match == "MI",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "MN",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MN",
	   fac_county_df[fac_county_df$treated.match == "MN",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "NE",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NE",
	   fac_county_df[fac_county_df$treated.match == "NE",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "NJ",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NJ",
	   fac_county_df[fac_county_df$treated.match == "NJ",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "NY",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NY",
	   fac_county_df[fac_county_df$treated.match == "NY",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "SD",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " SD",
	   fac_county_df[fac_county_df$treated.match == "SD",]$treated.cluster.name)

fac_county_df[fac_county_df$treated.match == "WV",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " WV",
	   fac_county_df[fac_county_df$treated.match == "WV",]$treated.cluster.name)

# Remove "County" at the back county_name labels, for the state codes
fac_county_df$county.name <- gsub(pattern = "\\b(\\w+)$", replacement = "", fac_county_df$county.name)

# Add state codes to control cluster names/labels
fac_county_df[fac_county_df$control.match == "IA",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " IA",
	   fac_county_df[fac_county_df$control.match == "IA",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "IL",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " IL",
	   fac_county_df[fac_county_df$control.match == "IL",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "IN",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " IN",
	   fac_county_df[fac_county_df$control.match == "IN",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "KS",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " KS",
	   fac_county_df[fac_county_df$control.match == "KS",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "KY",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " KY",
	   fac_county_df[fac_county_df$control.match == "KY",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "NH",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NH",
	   fac_county_df[fac_county_df$control.match == "NH",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "NV",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NV",
	   fac_county_df[fac_county_df$control.match == "NV",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "ND",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " ND",
	   fac_county_df[fac_county_df$control.match == "ND",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "OK",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " OK",
	   fac_county_df[fac_county_df$control.match == "OK",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "PA",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " PA",
	   fac_county_df[fac_county_df$control.match == "PA",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "TX",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " TX",
	   fac_county_df[fac_county_df$control.match == "TX",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "VA",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " VA",
	   fac_county_df[fac_county_df$control.match == "VA",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "WI",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " WI",
	   fac_county_df[fac_county_df$control.match == "WI",]$control.cluster.name)

fac_county_df[fac_county_df$control.match == "WY",]$control.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " WY",
	   fac_county_df[fac_county_df$control.match == "WY",]$control.cluster.name)
#======================================================================================================================#