#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  dane_1 <- reactive({
    
      inFile <- input$dane_1
      if (is.null(inFile))
        return(NULL)
      d <- read.table(inFile$datapath, header=FALSE, sep = '\t', quote = "\"")
      return(d)
  
      })
  
  dane_2 <- reactive({
    
    inFile <- input$dane_2
    if (is.null(inFile))
      return(NULL)
    d <- read.table(inFile$datapath, header=FALSE, sep = '\t', quote = "\"")
    return(d)
    
  })
  
  find_wynik <- reactive({
    
    ramka_1 <- dane_1()
    ramka_2 <- dane_2()
    
    
    wynik <- find_spory(ramka_1 = ramka_1,
                        ramka_2 = ramka_2,
                        s_1 = input$s_1,
                        m_1 = input$m_1,
                        procent_1 = input$procent_1,
                        threshold_1 = input$threshold_1,
                        s_2 = input$s_2,
                        m_2 = input$m_2,
                        procent_2 = input$procent_2,
                        threshold_2 = input$threshold_2,
                        usun = input$usun)
    
    return(wynik)
  })
  
  wynik_podsum <- reactive({
    
    wynik <- find_wynik()
    
    tabela <- find_spory_summarise(wynik_dna = wynik[[1]], wynik_sept = wynik[[2]])
    
    return(tabela)
    
  })
  
  output$tabela <- renderTable(wynik_podsum())
  
  
}