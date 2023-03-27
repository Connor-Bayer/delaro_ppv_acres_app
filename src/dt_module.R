#' Editable data table UI
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#'
#' @return A DT output element for UI

datatableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns('dt'))
}

#' Editable data table server
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#' @param reactive_summary reactive data.table; reactive version of combined
#'   summary loaded from shiny_setup.R configuration file
#'
#' @return A reactive DT object that can be edited by the user.

datatableServer <- function(id, reactive_summary) {
  moduleServer(
    id,
    function(input, output, session) {

      output$dt <- renderDT({
        isolate(reactive_summary())
      }, editable = list(target = 'cell', disable = list(columns = 1:7)),
      colnames = gsub('_', '\n', names(isolate(reactive_summary()))),
      options = list(dom = "t")
      )

      # record updates to table
      observeEvent(input$dt_cell_edit, {
        chg_row <- input$dt_cell_edit$row
        chg_col <- input$dt_cell_edit$col
        temp_table <- copy(reactive_summary())
        temp_table[chg_row, chg_col] <- as.numeric(input$dt_cell_edit$value)
        reactive_summary(temp_table)
      })

      return(reactive(reactive_summary))
    }
  )
}
