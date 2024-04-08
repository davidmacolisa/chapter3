#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Importing shapefiles
#======================================================================================================================#
source(file = "./Thesis/chapter3/src/climate_change/data/border_state_county_df.R", echo = T)
#======================================================================================================================#
### Summarize the number of treated and control states
#======================================================================================================================#
# Treated state information
border_mw_ch_tbl <- fac_states_df %>%
  filter(treated == 1) %>%
  mutate(
    tot.ch.amt = ch.amt + sum2.sub.mw.ch,
    end.mw = start.mw + tot.ch.amt
  ) %>%
  left_join(
    y = fac_county_df %>%
      filter(treated == 1) %>%
      group_by(state.code) %>%
      summarise(
        treated.border.counties = sum(!is.na(unique(treated.cluster.name)))
        # treated.border.counties = n_distinct(treated.cluster.name)
      ),
    by = c("state.code" = "state.code")
  )

# Control state information
border_mw_ch_tbl <- border_mw_ch_tbl %>%
  left_join(
    y = fac_states_df %>%
      filter(treated == 0) %>%
      group_by(match.state) %>%
      summarise(
        control.state.codes = paste0("(", paste0(
          unique(state.code),
          sep = "",
          collapse = ", "
        ), ")")
      ),
    by = c("state.code" = "match.state")
  ) %>%
  left_join(
    y = fac_county_df %>%
      filter(treated == 0) %>%
      group_by(treated.match) %>%
      summarise(
        control.border.counties = sum(!is.na(unique(control.cluster.name)))
        # control.border.counties = n_distinct(control.cluster.name)
      ),
    by = c("state.code" = "treated.match")
  ) %>%
  select(
    c(state.code, match.ch.year, match.ch.amt, sum2.sub.mw.ch, tot.ch.amt, start.mw, end.mw, control.state.codes,
      treated.border.counties, control.border.counties)
  ) %>%
  arrange(desc(tot.ch.amt))
sum(border_mw_ch_tbl$treated.border.counties, na.rm = TRUE)
sum(border_mw_ch_tbl$control.border.counties, na.rm = TRUE)

# Convert data frame to LaTeX table
border_mw_ch_tbl <- border_mw_ch_tbl %>%
  kable(., format = "latex", booktabs = TRUE) %>%
  kable_styling()

# Print or save the LaTeX table
cat(border_mw_ch_tbl)
# writeLines(border_mw_ch_tbl, con = "./Thesis/chapter3/src/climate_change/latex/treated_control_counties1.tex")