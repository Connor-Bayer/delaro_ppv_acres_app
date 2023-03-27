## app with uploadable data

#
# This is a Shiny web application. Goal - allow VP to explore different distributions
# of program acres and see the impact on distribution of key program metrics.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(pacman)

p_load(
  tidyverse,
  DT,
  data.table,
  leaflet,
  sf,
  MASS,
  patchwork,
  networkD3,
  formattable,
  shinyBS,
  nloptr,
  shinyjs
)

devtools::install_github('Connor-Bayer/themeBayer')
p_load(themeBayer)

source('src/baseline_analysis.R') ## analysis and data fcns
source('src/shiny_setup.R') ## initial variable declaration
source('src/histogram_module.R') ## module that handles histograms (by region)
source('src/sankey_module.R') ## sankey module
source('src/table_module.R') ## table handling/formatting
source('src/text_module.R') ## text handling *still undergoing development*

# source('src/map_module.R')
# source('src/map_setup.R') ## map module under development (maybe not useful here)

# Define UI for application that creates an HTML widget

ui_2 = fluidPage(
  #load style guides
  useShinyjs(),
  includeCSS("styles.css"),

  #make custom title with bayer logo
  tags$div(
    id = 'logo_header',
    img(src = "bayer_logo.png", width = "60px"),
    shiny::span("Delaro Showcase: Revisiting Metrics App")
  ),
  tags$div(
    id = "FILEUPLOAD",
    sidebarLayout(
      sidebarPanel(),mainPanel(
        br(),br(),br(),br(),br(),
    fileInput("preds", "Upload the corn_preds.rds file",
              multiple = F,
              accept = c(".rds")),
    ))),
    # preds <- reactive({
      #   infile <- input$preds
      #   if (is.null(infile)){
      #     return(NULL)
      #   }
      #   read_rds(infile$datapath)
      # }),
  shinyjs::hidden(tags$div(id = "TABSET",
  tabsetPanel(
    tabPanel("PPV Performance",
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 h5("Program Acres"),
                 width = 3,
                 sliderInput(
                   "prog_acres",
                   width = '100%',
                   label = "Total Program Acres",
                   value = 260000,
                   min = 10000,
                   max = max_program_acres,
                   step = 10000,
                   round = TRUE
                 ),
                 h5("Weight Program Acres?"),
                 sliderInput(
                   "strat_weight",
                   label = "Weight on Regional PPV",
                   value = 0,
                   min = 0,
                   max = 1
                 ),
                 bsTooltip(id = "strat_weight", title = paste0('0: Match 2023 regional distribution of acres', br(),br(), '1: Put all acres on region with highest PPV')),
                 sliderInput(
                   "threshold",
                   label = "Model Threshold",
                   value = threshold_baseline,
                   min = .2,
                   max = .7,
                   step = .025
                 ),
                 bsTooltip(id = "threshold", title = paste0('Model threshold: the higher the threshold the more acres will be filtered out of the trial')),


                 actionButton('button', 'Update Results')
               ),
               mainPanel = mainPanel(
                 tabsetPanel(

                   tabPanel("PPV",
                            br(),
                            textUI('ppvtext'),
                            histUI('plot1'),
                            h5("Summary of Regional Statistics"),
                            tableUI('table_summary')
                   ),
                   tabPanel("Acre Flow",
                            textUI('acre_flow'),
                            h5("Outcomes of Delaro Showcase w/ Model"),
                            sankeyUI('sankey'),
                            h5("Outcomes from Delaro Showcase w/ no Model"),
                            sankeyUI('sankey_nm'),
                            h5("Outcomes from Closing Delaro Showcase"),
                            sankeyUI('sankey_cm')))
               ))),
    tabPanel("Manual Inputs",
             tabsetPanel(
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   numericInput(
                     inputId = 'CS Central Plains',
                     label = 'Acres: Central Plains',
                     value =15000,
                     min = 0,
                     max = 200000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS Eastern Corn Belt',
                     label = 'Acres: Eastern Corn Belt',
                     value=10000,
                     min = 0,
                     max = 200000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS IL',
                     label = 'Acres: Illinois',
                     value=70000,
                     min = 0,
                     max = 200000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS Midwest',
                     label = 'Acres: Midwest',
                     value=45000,
                     min = 0,
                     max = 100000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS MN/WI',
                     label = 'Acres: Minnesota/Wisconsin',
                     value=40000,
                     min = 0,
                     max = 100000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS Northern Plains',
                     label = 'Acres: Northern Plains',
                     value=55000,
                     min = 0,
                     max = 100000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS West',
                     label = 'Acres: West',
                     value=20000,
                     min = 0,
                     max = 100000,
                     step = 1,
                     width = NULL
                   ),
                   numericInput(
                     inputId = 'CS South & East Coast',
                     label = 'Acres: South and East Coast',
                     value=5000,
                     min = 0,
                     max = 100000,
                     step = 1,
                     width = NULL
                   ),
                   sliderInput(
                     "threshold_manual",
                     label = "Model Threshold",
                     value = threshold_baseline,
                     min = .2,
                     max = .7,
                     step = .025
                   ),
                   bsTooltip(id = "threshold_manual", title = paste0('Model threshold: the higher the threshold the more acres will be filtered out of the trial')),

                   actionButton('manual_button', 'Update Results')
                 ),
                 mainPanel = mainPanel(tabsetPanel(
                   tabPanel(
                     "Manual PPV",
                     br(),
                     textUI('ppvtext.m'),
                     histUI('plot2'),
                     h5("Summary of Regional Statistics"),
                     tableUI('table_summary.m')
                   ),
                   tabPanel(
                     "Manual Acre Flow",
                     textUI('acre_flow.m'),
                     h5("Outcomes of Delaro Showcase w/ Model"),
                     sankeyUI('sankey2'),
                     h5("Outcomes from Delaro Showcase w/ no Model"),
                     sankeyUI('sankey_nm2'),
                     h5("Outcomes from Closing Delaro Showcase"),
                     sankeyUI('sankey_cm2')
                   )
                 )))

             ),



    ), tabPanel(
      'Assumptions/Explainer',
      h2("What's going on here?", align = 'center'),
      h4("Key Question: Acres -> PPV, is there a Relationship?", align = 'center'),
      plotOutput('acre_ppv_xyplot', height = 700, width = 600),
      h4("What is the model threshold?"),
      h6("The model threshold dictates how sure the model must be in order to consider someone a potential enrollee"),
      plotOutput('threshold_plot')

    )

    ))))

# preds <- reactive({
#   infile <- input$preds
#   if (is.null(infile)){
#     return(NULL)
#   }
#   read_rds(infile$datapath)
# })

server <- function(input, output, session){
  # plot reactives

  shinyjs::hide(id = "TABSET")
  observeEvent(input$preds, {
    if(!is.null(input$preds)){

      preds = reactive({read_rds(input$preds())})

      shinyjs::hide(id = "FILEUPLOAD")
      shinyjs::show(id = "TABSET")
    }
  })


  weight <- eventReactive(input$button, {c(input$strat_weight, (1 - input$strat_weight))}, ignoreNULL = FALSE)
  total_acres <- eventReactive(input$button, {input$prog_acres}, ignoreNULL = FALSE)
  manual_acres <- eventReactive(input$manual_button,
                                {c(`CS Central Plains` = input$`CS Central Plains`,
                                   `CS Eastern Corn Belt` = input$`CS Eastern Corn Belt`,
                                   `CS IL` = input$`CS IL`,
                                   `CS Midwest` = input$`CS Midwest`,
                                   `CS MN/WI` = input$`CS MN/WI`,
                                   `CS Northern Plains` = input$`CS Northern Plains`,
                                   `CS West` = input$`CS West`, ## no data (for me)
                                   `CS South & East Coast` = input$`CS South & East Coast`)
                                }, ignoreNULL = FALSE)
  threshold <- eventReactive(input$button, {input$threshold}, ignoreNULL = FALSE)
  threshold_manual <- eventReactive(input$manual_button, {input$threshold_manual}, ignoreNULL = FALSE)

  simulated_data_manual <- reactive({generate_data_object_shiny_app(.init_obj = 1,
                                                                    .total_acres = sum(manual_acres()),
                                                                    .tot_acre_ratio = 1,
                                                                    .num_regions = 8,
                                                                    .perc_positive = perc_positive,
                                                                    .acre_weights = manual_acres()/sum(manual_acres()),
                                                                    .init_ppv = init_ppv, preds = preds, spray_dist =F, update_weights = F, threshold = threshold_manual())})

  simulated_data <- reactive({generate_data_object_shiny_app(.init_obj = weight(),
                                                             .total_acres = total_acres(),
                                                             .tot_acre_ratio = total_acres()/260000,
                                                             .num_regions = 8,
                                                             .perc_positive = perc_positive,
                                                             .acre_weights = program_acre_weights,
                                                             .init_ppv = init_ppv, preds = preds, spray_dist =F, threshold = threshold())})


  reactive_summary <- reactive({simulated_data()$by_region %>%
      group_by(CP_REGION, iter) %>%
      summarize(TN = sum(TN), TP = sum(TP), FP = sum(FP), FN = sum(FN))%>%
      mutate(ppv = TP/(FP + TP)) %>% ungroup() %>% group_by(CP_REGION) %>% summarize(
        lower_95_ppv = round(HDInterval::hdi(ppv, credMass = .95)['lower'],4),
        median_ppv = round(median(ppv),4),
        upper_95_ppv = round(HDInterval::hdi(ppv, credMass = .95)['upper'],4),
        acres = round(median(TP + FP + TN + FN),-1))})

  reactive_summary_manual <- reactive({simulated_data_manual()$by_region %>%
      group_by(CP_REGION, iter) %>%
      summarize(TN = sum(TN), TP = sum(TP), FP = sum(FP), FN = sum(FN)) %>%
      mutate(ppv = TP/(FP + TP)) %>% ungroup() %>% group_by(CP_REGION) %>% summarize(
        lower_95_ppv = round(HDInterval::hdi(ppv, credMass = .95)['lower'],4),
        median_ppv = round(median(ppv),4),
        upper_95_ppv = round(HDInterval::hdi(ppv, credMass = .95)['upper'],4),
        acres = round(median(TP + FP + TN + FN),-1))
  })



  # expected payout distribution plot
  output$plot1 <- histServer_v2('plot1', data = simulated_data, name = 'default')
  output$sankey <- sankeyServer('sankey', data = simulated_data, name = 'd')
  output$acre_flow <- textServer_2('acre_flow', data = simulated_data, name = 'acre_flow', type = 'acre_flow')
  output$ppvtext <- textServer_2('ppvtext', data = simulated_data, name = 'ppv', type = 'ppv')

  output$table_summary <- tableServer('table_summary', table_content = reactive_summary, name = 'smry', type = 'ppv')
  #output$table_summary.acres <- tableServer('table_summary.acres', table_content = reactive_summary_acres, name = 'smry', type = 'acres')
  #output$table_summary.overall <- tableServer('table_summary', table_content = reactive_summary, name = 'smry', type = 'overall')


  output$plot2 <- histServer_v2('plot2', data = simulated_data_manual, name = 'default')
  output$sankey2 <- sankeyServer('sankey2', data = simulated_data_manual, name = 'd.m')
  output$acre_flow.m <- textServer_2('acre_flow.m', data = simulated_data_manual, name = 'acre_flow.m', type = 'acre_flow')
  output$ppvtext.m <- textServer_2('ppvtext.m', data = simulated_data, name = 'ppv', type = 'ppv')

  output$table_summary.m <- tableServer('table_summary.m', table_content = reactive_summary_manual, name = 'smry', type = 'ppv')

  #output$table_summary.acres.m <- tableServer('table_summary.m', table_content = reactive_summary_acres_manual, name = 'smry', type = 'acres')
  #output$table_summary.overall.m <- tableServer('table_summary.m', table_content = reactive_summary, name = 'smry', type = 'overall')


  output$sankey_nm <- sankeyServer('sankey_nm', data = simulated_data, name = 'nm')
  output$sankey_cm <- sankeyServer('sankey_cm', data = simulated_data, name = 'cm')
  output$sankey_nm2 <- sankeyServer('sankey_nm2', data = simulated_data_manual, name = 'nm')
  output$sankey_cm2 <- sankeyServer('sankey_cm2', data = simulated_data_manual, name = 'cm')

}

# Run the application
shinyApp(ui = ui_2, server = server)
