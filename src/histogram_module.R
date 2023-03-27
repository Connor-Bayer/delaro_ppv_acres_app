### histogram utility functions for shiny
###

#' Simple histogram UI
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#'
#' @return A plot output element for UI

histUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns('plot'), height = 700, width = 600)
}

#' Simple histogram server
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#' @param data list; list of datasets simulated based on entered parameters
#' @param name character (optional); string that grabs correct data frame from
#'   list of dataframes in data
#'
#' @return A renderPlot() reactive object
histServer_v2 <- function(id, data, name){
  moduleServer(
    id,
    function(input, output, session){
      dataset <- reactive({
        if(!is.null(name)){
          data()[[name]]
        }
        else
          data()[['default']]
      })
    ## output a ggplot that shows distribution of ppv or total payout
      output$plot <- renderPlot({
        (ggplot(data = dataset(), aes(x = ppv)) +
        geom_histogram(bins = 50, color = 'white',
                     fill = "#89D329", alpha = .8) + theme_minimal() +
        xlab('') + geom_vline(xintercept = unique(dataset()$value),
                              color = "#FF3162") +
        labs(title = paste0("Distribution of Overall PPV under chosen acre targets")) +
          theme(plot.title = element_text(size = 18),
                axis.text.x = element_text(size = 9)))/
          (ggplot(data = data()[['by_region']] %>% group_by(CP_REGION, iter) %>% summarize(TN = sum(TN), TP = sum(TP), FP = sum(FP), FN = sum(FN)), aes(x = TP/(FP+TP), fill = CP_REGION)) +
          geom_histogram(bins = 50, color = 'white', alpha = 1) +
          theme_minimal() +
          xlab('ppv') + geom_vline(xintercept = unique(dataset()$value),
                                  color = "#FF3162") +
          labs(fill = paste0("Region")) + facet_wrap(~CP_REGION, scales = "free_x")+                                                                # Change font size
            theme(strip.text.x = element_text(size = 12),
                  legend.text=element_text(size=9),
                  axis.title.x = element_text(size = 12),
                  axis.text.x = element_text(size = 9),
                  panel.margin.x = unit(15, "pt"),
                  panel.margin.y=unit(15, "pt")) +
            scale_x_continuous(limits=c(.15,.8))) +
          labs(caption = "Distribution of Expected PPV over 1000 iterations \n overall and by region, assuming annual OOS PPV is IID")
      }, height = 700, width = 600 )
  })

}
