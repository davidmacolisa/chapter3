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
data(state_df, package = "usgeogr")
state_df <- state_df %>% data.frame()
names(state_df$state) <- tolower(state_df$state)
state_df$state <- stringi::stri_trans_totitle(state_df$state)

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

data(adjacent_county_df, package = "usgeogr")
adjacent_county_df <- adjacent_county_df %>% data.frame()


data(cbcp_df, package = "usgeogr")
cbcp_df <- cbcp_df %>% data.frame()

