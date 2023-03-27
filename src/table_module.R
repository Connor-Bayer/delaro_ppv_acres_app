#' Fancy table UI
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#'
#' @return A DT output element for UI

tableUI <- function(id) {
  ns <- NS(id)
  formattableOutput(ns('table'))
}

#' Editable data table server
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#' @param reactive_summary reactive data.table; reactive version of combined
#'   summary loaded from shiny_setup.R configuration file
#'
#' @return A format table server/object that allows displaying of a nicely
#'         formatted table

tableServer <- function(id, table_content, name, type) {
  #

  moduleServer(
    id,
    function(input, output, session) {
      if(type == 'ppv'){
        output$table <- renderFormattable({
          formattable::formattable(setnames(table_content(), c('CP Region', 'Lower 95 HDI PPV', 'Median PPV', 'Upper 95 HDI PPV', 'Acres Deployed')),
                                 list(`Median PPV` = color_tile("white","#89D329"),
                                      `Lower 95 HDI PPV`= color_tile("white","#89D329"),
                                      `Upper 95 HDI PPV`= color_tile("white","#89D329")))
      })}
      else if(type == 'overall') {
        output$table <- renderFormattable({
        formattable::formattable(setnames(table_content(), c('CP Region', 'PPV')),
                                 list(`PPV` = color_tile("white","#00BCFF")))
        })}
      else{
        stop('Table type not recognized')
      }

      return(reactive(reactive_summary))
    }
  )
}
