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
    fluidPage(
      titlePanel("Spore counter"),
      sidebarLayout(
        sidebarPanel(
          checkboxInput('example', 'Czy chcesz załadować przykładowe dane?'),
          fileInput("dane_1", 'Wybierz pierwszy plik .txt',
                    accept=c('.txt')),
          fileInput("dane_2", 'Wybierz drugi plik .txt',
                    accept=c('.txt')),
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
                                         'Ostatnie septa' = 'sept_last'), inline = TRUE)
          
        ),
        
        mainPanel(
          tableOutput("tabela")
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

