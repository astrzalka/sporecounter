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
    navbarPage('sporcounter',
               theme = shinythemes::shinytheme("united"),
               tabPanel("Analiza strzępki",
                        sidebarLayout(
                          sidebarPanel(
                            checkboxInput('example', 'Czy chcesz załadować przykładowe dane?'),
                            fileInput("dane_1", 'Wybierz pierwszy plik .txt',
                                      accept=c('.txt')),
                            fileInput("dane_2", 'Wybierz drugi plik .txt',
                                      accept=c('.txt')),
                            checkboxInput('header', 'Czy dane mają nagłówki?', value = TRUE),
                            numericInput('s_1', 'Średnia szerokość pików dla DNA', value = 2, step = 0.1),
                            checkboxInput('m_1', 'Czy zastosować wygładzanie Markova dla DNA?', value = FALSE),
                            numericInput('procent_1', 'Procent odejmowanego tła dla DNA', value = 0.05, step = 0.01),
                            numericInput('threshold_1', 'Próg filtrowania pików dla DNA', value = 10, step = 1, min = 0, max = 100),
                            numericInput('s_2', 'Średnia szerokość pików dla sept', value = 2, step = 0.1),
                            checkboxInput('m_2', 'Czy zastosować wygładzanie Markova dla sept?', value = FALSE),
                            numericInput('procent_2', 'Procent odejmowanego tła dla sept', value = 0.05, step = 0.01),
                            numericInput('threshold_2', 'Próg filtrowania pików dla sept', value = 10, step = 1, min = 0, max = 100),
                            checkboxGroupInput('usun', 'Czy chcesz usunąć graniczne septy lub chromosomy?', 
                                               choices = c('Pierwszy chromosom' = 'dna_first',
                                                           'Ostatni chromosom' = 'dna_last',
                                                           'Pierwsza septa' = 'sept_first',
                                                           'Ostatnia septa' = 'sept_last'), inline = TRUE),
                            textInput('usun_spory', 'Czy chcesz usunąć niektóre spory? (Podaj ich nazwy oddzielone przecinkami)'),
                            textInput('id', 'Identyfikator strzępki', 'strzepka_1'),
                            textInput('szczep', 'Podaj nazwę szczepu - do porównania z innymi', 'szczep'),
                            downloadButton('download_data', 'Pobierz wynik w formacie txt'),
                            
                            width=3
                            
                          ),
                          
                          mainPanel(
                            plotOutput("wykres", height = "600px"),
                            tableOutput("tabela"),
                            width=9
                          )
                        )
               ),
               tabPanel("Zebranie wyników",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput('wyniki', 'Wczytaj pliki txt z wynikami', multiple = TRUE),
                            radioButtons('wykres_type', 'Jak pokazać odległości pomiędzy przegrodami?',
                                         choices = c("Histogram" = 'hist',
                                                     'Boxplot' = 'boxplot',
                                                     'Wykres gęstości prawdopodobieństwa' = 'density'),
                                         selected = 'hist', inline = TRUE),
                            numericInput('hist_bin', 'Podaj szerość słupków histogramu', value = 0.2, 
                                         step = 0.1),
                            numericInput('micro', 'Podaj szerokość mikrokompartmentu', 
                                         value = 0.5, min = 0, step = 0.1),
                            numericInput('macro', 'Podaj szerokość makrokompartmentu', 
                                         value = 3, min = 0, step = 0.1)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Dane",
                                       tableOutput("tabela_wyniki")
                                       ),
                              tabPanel("Podsumowanie",
                                       tableOutput("tabela_podsumowanie")
                                       ),
                              tabPanel("Wykresy",
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

