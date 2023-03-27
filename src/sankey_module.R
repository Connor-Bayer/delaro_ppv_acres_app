### Sankey diagram (html::* widget) utility functions for shiny
###

#' Sankey Diagram Widget UI
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#'
#' @return A html widget for UI
#'
sankeyUI <- function(id) {
  ns <- NS(id)
  sankeyNetworkOutput(ns('sankey'))
}

sankeyCM <- function(dat,node_dat) {
  dat[(target %in% c(max(target), (max(target) - 3))),
          value := sum(value), by = 'source']
  dat[(target %in% c((max(target)-1), (max(target) - 2))),
          value := sum(value), by = 'source']
  dat[(target == (max(target)-2)),
          value := 0, by = 'source']
  dat[(target == (max(target)-3)),
      value := 0, by = 'source']

  print('CM data: ')
  print(dat)
  node_dat[names %in% c("paid-out acres", "correctly enrolled acres"), names := '']

  return(list(flows = dat, nodes = node_dat))
}

sankeyNM <- function(dat,node_dat) {
  dat[(target %in% c(max(target), (max(target) - 3))),
          value :=  sum(value), by = 'source']
  dat[(target %in% c((max(target)-1), (max(target) - 2))),
          value := sum(value), by = 'source']
  dat[(target %in% c((max(target)-1),max(target))),
          value := 0, by = 'source']

  node_dat[names %in% c("avoided payouts", "lost trial successes"), names := '']
  print('NM data: ')
  print(dat)
  return(list(flows = dat, nodes = node_dat))
}

#' Sankey widget server
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#' @param data list; list of datasets simulated based on entered parameters
#' @param name character (optional); string that grabs correct data frame from
#'   list of dataframes in data - not used in this module but kept for consistency
#'   and to potentially add more sankey plots later if desired
#'
#' @return A renderPlot() reactive object
sankeyServer <- function(id, data, name){
  moduleServer(
  id,
  function(input, output, session, condition = name){

    if(condition == 'nm'){ ## program continues, but with no model

      dataset <- reactive({
        sankeyNM(dat = copy(data()$flows), node_dat = copy(data()$nodes))
      })
      clrbar = JS('d3.scaleOrdinal(["#10384F","#0B95C8","#43594A","#8b0000"])')

    } else if(condition == 'cm'){ ## program discontinues

      dataset <- reactive({
        sankeyCM(dat = copy(data()$flows), node_dat = copy(data()$nodes))
      })
      clrbar = JS('d3.scaleOrdinal(["#10384F","#0B95C8","#6DCE53","#FF3162"])')
    } else{
      dataset <- reactive({## program with model
        list(nodes = data()$nodes, flows = data()$flows)
      })
      clrbar = JS('d3.scaleOrdinal(["#10384F","#0B95C8","#43594A","#8b0000","#6DCE53","#FF3162"])')
    }



    ## output a ggplot that shows distribution of ppv or total payout
    #color <- "d3.scaleOrdinal() .domain(['avoided-payouts', 'lost-trial-successes']) .range(['#228B22', '#abf7b1'])"
    output$sankey <- renderSankeyNetwork({
      sankeyNetwork(Links = dataset()$flows,
                    Nodes = dataset()$nodes,
                    Source = "source",
                    Target = "target",
                    Value = "value",
                    NodeID = "names",
                    units = "Acres",
                    colourScale = clrbar,
                    fontSize = 12,
                    nodeWidth = 30)})



})}
