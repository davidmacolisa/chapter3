#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(usgeogr)
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
# data(adjacent_county_df, package = "usgeogr")
# adjacent_county_df <- adjacent_county_df %>% data.frame()

# for border-county design
data(cbcp_df, package = "usgeogr")
cbcp_df <- cbcp_df %>% data.frame()
#======================================================================================================================#
### Experiment Design: State-level MW >= $0.5 for Border States
#======================================================================================================================#
fac_states <- state_df %>%
  rename(state.code = state_code) %>%
  filter(
	state.code %in% c(
	  #treated states
	  "AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NY", "WV",
	  #control states
	  "GA", "IA", "ID", "IL", "IN", "KS", "KY", "NH", "NM", "NV", "NC",
	  "ND", "OK", "PA", "TX", "UT", "VA", "WI", "WY"
	)
  ) %>%
  mutate(
	# Year of first MW raise >= $0.5
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
	  state.code == "NY" ~ 2014,
	  state.code == "WV" ~ 2015, T ~ Inf
	),
	# Treated states
	treated = case_when(ch.year != Inf ~ 1, T ~ 0),
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
	  state.code == "NY" ~ 0.75,
	  state.code == "WV" ~ 0.75, T ~ 0
	),
	# Exogenous subsequent MW raise
	sum2.sub.mw.ch = case_when(
	  state.code == "AR" ~ 1,
	  state.code == "CA" ~ 1.5,
	  state.code == "DE" ~ 0,
	  state.code == "ME" ~ 0,
	  state.code == "MA" ~ 2,
	  state.code == "MD" ~ 1,
	  state.code == "MI" ~ 0.7,
	  state.code == "MN" ~ 1.5,
	  state.code == "NE" ~ 1,
	  state.code == "NY" ~ 1.7,
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
	  state.code == "NY" ~ 7.25,
	  state.code == "WV" ~ 7.25, T ~ 0
	),
  ) %>%
  # Match with adjacent neighbor states
  left_join(
	y = adjacent_county_df %>%
	  select(c(county_state, neighbor_state)) %>%
	  rename(match.state = neighbor_state) %>%
	  filter(match.state %in% c("AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NY", "WV")),
	by = c("state.code" = "county_state")
  ) %>%
  # Removing GA, ID, NM, NC and UT are removed for they didn't match to any treated states
  filter(!is.na(match.state)) %>%
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
	  match.state == "NY" ~ 0.75,
	  match.state == "WV" ~ 0.75, T ~ 0
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
	  match.state == "NY" ~ 2014,
	  match.state == "WV" ~ 2015
	),
  ) %>%
  distinct()
#======================================================================================================================#
### Experiment Design: Border-county MW changes
#======================================================================================================================#
fac_county <- fac_states %>%
  rename(treated.match = match.state) %>%
  left_join(
	y = adjacent_county_df %>%
	  filter(neighbor_state %in% c("AR", "CA", "DE", "ME", "MA", "MD", "MI", "MN", "NE", "NY", "WV")),
	by = c("state.code" = "county_state", "treated.match" = "neighbor_state")
  ) %>%
  rename(treated.cluster.name = neighbor_name, treated.cluster.id = neighbor_fips_code,
		 treated.cluster.population = neighbor_population, treated.cluster.lat = neighbor_lat,
		 treated.cluster.long = neighbor_long) %>%
  select(
	c(fips_code, county_name, state, state.code, population, lat, long, treated, treated.match, treated.cluster.name,
	  treated.cluster.id, ch.year:start.mw, match.ch.amt, match.ch.year, treated.cluster.population,
	  treated.cluster.lat, treated.cluster.long)
  ) %>%
  distinct()

# Add state codes to treated cluster names/labels
fac_county[fac_county$treated.match == "AR",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " AR",
	   fac_county[fac_county$treated.match == "AR",]$treated.cluster.name)

fac_county[fac_county$treated.match == "CA",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " CA",
	   fac_county[fac_county$treated.match == "CA",]$treated.cluster.name)

fac_county[fac_county$treated.match == "DE",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " DE",
	   fac_county[fac_county$treated.match == "DE",]$treated.cluster.name)

fac_county[fac_county$treated.match == "ME",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " ME",
	   fac_county[fac_county$treated.match == "ME",]$treated.cluster.name)

fac_county[fac_county$treated.match == "MA",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MA",
	   fac_county[fac_county$treated.match == "MA",]$treated.cluster.name)

fac_county[fac_county$treated.match == "MD",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MD",
	   fac_county[fac_county$treated.match == "MD",]$treated.cluster.name)

fac_county[fac_county$treated.match == "MI",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MI",
	   fac_county[fac_county$treated.match == "MI",]$treated.cluster.name)

fac_county[fac_county$treated.match == "MN",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " MN",
	   fac_county[fac_county$treated.match == "MN",]$treated.cluster.name)

fac_county[fac_county$treated.match == "NE",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NE",
	   fac_county[fac_county$treated.match == "NE",]$treated.cluster.name)

fac_county[fac_county$treated.match == "NY",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " NY",
	   fac_county[fac_county$treated.match == "NY",]$treated.cluster.name)

fac_county[fac_county$treated.match == "WV",]$treated.cluster.name <-
  gsub(pattern = "\\b(\\w+)$", replacement = " WV",
	   fac_county[fac_county$treated.match == "WV",]$treated.cluster.name)

fac_counties <- fac_county %>%
  left_join(
	# merging the cross-border counties
	y = cbcp_df %>%
	  rename(control.match = neighbor_state, control.cluster.id = neighbor_fips_code) %>%
	  filter(control.match %in% c("IA", "IL", "IN", "KS", "KY", "NH", "NV",
								  "ND", "OK", "PA", "TX", "VA", "WI", "WY")),
	by = c("fips_code" = "fips_code", "state.code" = "county_state")
  ) %>%
  filter(!is.na(cbcp_id) |
		   !is.na(state_border_id) |
		   !is.na(control.cluster.id) |
		   !is.na(control.match) |
		   !is.na(dist_bt_centers)) %>%
  select(
	c(fips_code, county_name, state, state.code, population, lat, long, treated, treated.match, control.match,
	  state_border_id, treated.cluster.name, treated.cluster.id, control.cluster.id, cbcp_id, ch.year:start.mw,
	  match.ch.amt, match.ch.year, treated.cluster.population, treated.cluster.lat, treated.cluster.long,
	  dist_bt_centers)
  ) %>%
  distinct()