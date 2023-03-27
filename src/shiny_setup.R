# Initiate simulation

# set seed in order to always get identical draws
set.seed(42)

### best file that matches data (as of last update) is:
# .../CloudStorage/OneDrive-Bayer/Delaro Data Request September 2021/corn_pre_spray_model_03_23_2023.csv"

#juan_data_location = #### deprecated

# Set initial acreage parameters ------------------------------------------
max_program_acres <- 480000
init_program_acres <- 260000 #260000 including west
threshold_baseline = .45
end_year = 2022

program_acre_list <- list(`CS Central Plains` = 15000,
                          `CS Eastern Corn Belt` = 10000,
                          `CS IL` = 70000,
                          `CS Midwest` = 45000,
                          `CS MN/WI` = 40000,
                          `CS Northern Plains` = 55000,
                          `CS South & East Coast` = 5000,
                          `CS West` = 20000)

program_acre_weights <- c(`CS Central Plains` = 15000,
                          `CS Eastern Corn Belt` = 10000,
                          `CS IL` = 70000,
                          `CS Midwest` = 45000,
                          `CS MN/WI` = 40000,
                          `CS Northern Plains` = 55000,
                          `CS South & East Coast` = 5000,
                          `CS West` = 20000)/init_program_acres

program_acre_weights <- program_acre_weights # exclude west
# setup files from which to work
#preds <- load_baseline_data('regionalShape/regional_shapefile.shp', path_trial = juan_data_location)
# preds = fread('preds_file.csv')


# Plot settings -----------------------------------------------------------
# set theme for plots
theme_set(theme_bw(base_size = 16) +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank()))

