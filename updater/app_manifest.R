## write manifest
appfiles = c('src/baseline_analysis.R',
             'src/shiny_setup.R',
             'src/histogram_module.R',
             'src/sankey_module.R',
             'src/table_module.R',
             'src/text_module.R',
             'app.R') ## add new modules to this, and if you want to update
                      ## app - move current app.R (with date) to archive
                      ## and then create new version in main repo.

rsconnect::writeManifest(appDir = '/Users/connorlennon/Documents/delaro_ppv_acres_app',
                         appFiles =appfiles,
                         verbose = TRUE)
