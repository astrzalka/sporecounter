#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage('sporecounter',
               theme = shinythemes::shinytheme("united"),
               tabPanel("Hypha analysis",
                        #sidebarLayout(
                        #sidebarPanel(
                        
                        fluidRow(
                          column(4,
                                 checkboxInput('example', 'Load example dataset?'),
                                 
                                           
                                 
                                 checkboxInput('header', 'Do txt files contain headers?', value = TRUE),
                                 fluidRow(
                                   column(6,
                                          h4('DNA'),
                                          fileInput("dane_1", 'Choose file - DNA profile',
                                                    accept=c('.txt')),
                                          sliderInput('s_1', 'Sigma value', value = 2, step = 0.1, min = 1, max = 10),
                                          sliderInput('procent_1', 'Background percentage', value = 0.05, step = 0.01, min = 0, max = 1),
                                          sliderInput('threshold_1', 'Filtering threshold (%)', value = 10, step = 1, min = 0, max = 100),
                                          checkboxInput('m_1', 'Use Markov smoothing?', value = FALSE)
                                   ),
                                   column(6,
                                          h4('Cell wall'),
                                          fileInput("dane_2", 'Choose file - cell wall profile',
                                                    accept=c('.txt')),
                                          sliderInput('s_2', 'Sigma value', value = 2, step = 0.1, min = 1, max = 10),
                                          sliderInput('procent_2', 'Background percentage', value = 0.05, step = 0.01, min = 0, max = 1),
                                          sliderInput('threshold_2', 'Filtering threshold', value = 10, step = 1, min = 0, max = 100),
                                          checkboxInput('m_2', 'Use Markov smoothing?', value = FALSE)
                                   )
                                 ),
                                 checkboxGroupInput('usun', 'Should boundary septs, chromosomes or spores be removed?', 
                                                    choices = c('First chromosome' = 'dna_first',
                                                                'Last chromosome' = 'dna_last',
                                                                'First sept' = 'sept_first',
                                                                'Last sept' = 'sept_last',
                                                                "First spore" = 'spore_first',
                                                                "Last spore" = "spore_last"), inline = TRUE,
                                                    selected = c('spore_first', "spore_last")),
                                 textInput('usun_spory', 'Remove specific spores? (names should be separated by commas)'),
                                 textInput('id', 'Hypha id', 'hypha_1'),
                                 textInput('szczep', 'Strain name (for later comparison)', 'strain'),
                                 downloadButton('download_data', 'Download result in txt format')
                                 
                                 #width=3
                                 
                          ),
                          
                          #mainPanel(
                          column(width = 8,
                                 plotOutput("wykres", height = "600px"),
                                 tableOutput("tabela")
                                 #width=9
                          )
                        )
                        
               ),
               tabPanel("Comparison",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput('wyniki', 'Load txt result files', multiple = TRUE),
                            radioButtons('wykres_type', 'Choose plot type',
                                         choices = c("Histogram" = 'hist',
                                                     'Boxplot' = 'boxplot',
                                                     'Density plot' = 'density'),
                                         selected = 'boxplot', inline = TRUE),
                            numericInput('hist_bin', 'Choose binwidth for histogram', value = 0.2, 
                                         step = 0.05),
                            numericInput('micro', 'Choose width of microcompartment', 
                                         value = 0.5, min = 0, step = 0.1),
                            numericInput('macro', 'Choose width of macrocompartment', 
                                         value = 3, min = 0, step = 0.1),
                            downloadButton('download_data_all', 'Download combined result file in txt format')
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Data",
                                       tableOutput("tabela_wyniki")
                              ),
                              tabPanel("Summary",
                                       tableOutput("tabela_podsumowanie")
                              ),
                              tabPanel("Plots",
                                       plotOutput('wykres_podsumowanie', height = "800px")
                              )
                            )
                            
                          )
                        )
               )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'sporecounter'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

