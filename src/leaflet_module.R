#' Leaflet UI
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#'
#' @return A leaflet output element for the UI

leafletUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns('map'))
}

#' Leaflet server
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#' @param counties_shp SpatialPolygonsDataFrame; polygons to be used in map
#' @param var_select character; name of the variable to be displayed
#' @param map_data reactive SpatialPolygonsDataFrame; updated data frame that
#'   includes user inputs to the editable summary table.
#'
#' @return A reactive leaflet map

leafletServer <- function(id, counties_shp, var_select, map_data) {
  moduleServer(id, function(input, output, session) {

    output$map <- renderLeaflet({
      leaflet(counties_shp) %>%
        addTiles() %>%
        addPolygons(color = "#444444",
                    weight = 1,
                    smoothFactor = 0.5,
                    fillColor = 'white',
                    fillOpacity = 0.7) %>%
        setView(lng = -92, lat = 42.5, zoom = 5)
    })

    observe({
      var_name <- ifelse(var_select() == '-', 'empty', var_select())
      colorData <- map_data()[[var_name]]

      # fill colors
      col_length <- length(unique(colorData))
      if(col_length < 2){
        pal <- colorFactor(
          palette = "#FFFFBF",
          domain = colorData,
          na.color = 'white')
      } else {
        pal <- colorNumeric(
          palette = "viridis",
          domain = colorData,
          na.color = 'white')
      }

      popup_dat <- paste0('<strong>State: </strong>',
                          map_data()$state,
                          paste0("<br><strong>", var_select(), ": </strong>"),
                          map_data()[[var_name]] # TODO add formatting with commas?
      )

      leafletProxy("map", data = map_data()) %>%
        clearGroup('polygons') %>%
        addPolygons(color = "#444444",
                    weight = 1,
                    smoothFactor = 0.5,
                    fillColor = pal(colorData),
                    fillOpacity = 0.7,
                    popup = popup_dat,
                    group = 'polygons'
        ) %>%
        addLegend("bottomleft", pal = pal, values = colorData, title = var_select(),
                  layerId = "colorLegend", na.label = '')
    })
  })
}
