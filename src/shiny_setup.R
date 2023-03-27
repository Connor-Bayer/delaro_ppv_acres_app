# Initiate simulation

# set seed in order to always get identical draws
set.seed(42)

juan_data_location = '/Users/connorlennon/Downloads/corn_pre_spray_model_03_23_2023.csv'

### best file that matches data is:
# /Users/connorlennon/Library/CloudStorage/OneDrive-Bayer/Delaro Data Request September 2021/all_years_validation_pre_spray_6_buac.csv"

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
preds = fread('/Users/connorlennon/Downloads/preds_file.csv')
cmby <- generate_cm_by(setDT(preds), by_column = 'CP_REGION')

cmby$iter <- 1 #only one iter on baseline model

perc_positive <- perc_pos_weights(cmby) # generate percent positivity

init_ppv <- ppv(cmby) # get actual model PPV

# Plot settings -----------------------------------------------------------
# set theme for plots
theme_set(theme_bw(base_size = 16) +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank()))

