#' Simple text UI
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#'
#' @return A text output element for UI

textUI <- function(id) {
  ns <- NS(id)
  htmlOutput(ns('text'))
}

#' Simple text server
#'
#' @param id character; an ID string that corresponds with the ID used to call
#'   the module's server function
#' @param val matrix or list; object from which outputs should be calculated
#' @param name character; name of a list element to subset to if needed. Default
#'   is NULL.
#' @param type character; choices are "precision" or "payout". Determines the
#'   text for output.
#'
#' @return A renderText() reactive object

textServer_2 <- function(id, data, name, type) {
  set.seed(42)
  moduleServer(id, function(input, output, session) {
    # subset to the appropriate part of the input
    v <- reactive({
      data()$by_region %>%summarize(TP = sum(TP)/max(iter), TN = sum(TN)/max(iter), FP = sum(FP)/max(iter), FN = sum(FN)/max(iter))
    })

    enrollment = reactive({round(v()['TP'], -2)}) #
    nopayout = reactive({round(v()['TN'], -2)*6}) #
    lst_acres = reactive({round(v()['FN'], -2)}) #
    payout = reactive({round(v()['FP'],-2)*6}) #

    # render text
    output$text <- renderText({
      if (type == 'acre_flow') {

        return(HTML(paste('<br> Under the acres chosen, Delaro Showcase provides ', "<B>",paste0("~",as.character(enrollment())),"</B>", ' acres with a sufficient BU/AC lift while avoiding ',"<B>", paste0("~$",as.character(nopayout())), "</B>",' in payouts <br>', 'The costs of the program would be',"<B>", paste0("~",as.character(lst_acres())), "</B>",' excluded acres where Delaro would have given a BU/AC lift and a total of ',"<B>", paste0("~$",as.character(payout())), "</B>",' in payouts. All numbers rounded to the nearest 100 acres.')))
      } else if (type == 'ppv') {
        return(paste('Expected PPV:', round(mean(data()$default$ppv),4), 'with a 95% chance that the true value lies between', round(HDInterval::hdi(data()$default$ppv, credMass = .95)['lower'],4), 'and', round(HDInterval::hdi(data()$default$ppv, credMass = .95)['upper'],4)))
      }
        else if (type == 'exp_total_payout') {

          num <- round(mean(v()), 2)
          return(paste('Expected total payout: $', num, 'across acres'))
        } else {
        stop('received an unexpected type')
      }
    })
  }
  )
}
