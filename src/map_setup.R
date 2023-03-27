# TODO: create files with map data that can be leveraged.

# loads with name combined_summary
# gives the fungicide treated/untreated acres by state
load('combined_summary.rda')
# source map polygons. Surely there is a better way to do this.
# loads with name counties_shp
load('map_polygons.rda')

# added some needed variables defined in shiny_setup.R
combined_summary$rebate_rate <- initial_rebate_bu
combined_summary$max_rebate <- initial_max_rebate

combined_summary$max_payout <- 6
combined_summary$ppv <- 0.7 # NPV for Cycle 3 was 0.58
combined_summary[, percent_portfolio_acres := available_acres / sum(available_acres)]

combined_summary[, percent_portfolio_acres := round(percent_portfolio_acres, 2)]
combined_summary[, perc_treated_acres := round(perc_treated_acres, 2)]

# variable choices for map
variable_choices <- c('-', names(which(sapply(combined_summary, is.numeric))))
