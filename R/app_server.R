#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  library(magrittr)
  library(patchwork)
  library(Peaks)
  library.dynam('Peaks', 'Peaks', lib.loc=NULL) 
  
  dane_1 <- reactive({
    
    if(input$example == TRUE){
      d <- data_1
      return(d)
    }
    
    inFile <- input$dane_1
    if (is.null(inFile))
      return(NULL)
    d <- read.table(inFile$datapath, header=input$header, sep = '\t', quote = "\"")
    
    if(ncol(d) == 3){
      d <- d[,-1]
      
    }
    
    colnames(d) <- c('V1', 'V2')
    return(d)
    
  })
  
  dane_2 <- reactive({
    
    if(input$example == TRUE){
      d <- data_2
      return(d)
    }
    inFile <- input$dane_2
    if (is.null(inFile))
      return(NULL)
    d <- read.table(inFile$datapath, header=input$header, sep = '\t', quote = "\"")
    
    if(ncol(d) == 3){
      d <- d[,-1]
    }
    
    colnames(d) <- c('V1', 'V2')
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
    
    tabela <- find_spory_summarise(wynik_dna = wynik[[1]], wynik_sept = wynik[[2]], strzepka = input$id)
    
    usun_spory <- sub(' ', '', unlist(stringr::str_split(input$usun_spory, ',')))
    
    tabela <- tabela %>% dplyr::filter(!(spora %in% usun_spory))
    
    tabela$parametry <- paste(input$s_1, input$m_1, input$procent_1, input$threshold_1,
                              input$s_2, input$m_2, input$procent_2, input$threshold_2,
                              sep = '_')
    
    return(tabela)
    
  })
  
  output$tabela <- renderTable({
    if ((is.null(input$dane_1)|is.null(input$dane_2))&input$example == FALSE)
      return(NULL)
    wynik_podsum()
  }
  )
  
  
  wykresInput <- reactive({
    
    ramka_1 <- dane_1()
    ramka_2 <- dane_2()
    
    wynik <- find_wynik()
    
    find_spory_plot(ramka_1 = ramka_1, 
                    ramka_2 = ramka_2,
                    wynik_dna = wynik[[1]],
                    wynik_sept = wynik[[2]])
    
  })
  
  # funckja pokazujące wykres w aplikacji
  output$wykres <- renderPlot({
    if ((is.null(input$dane_1)|is.null(input$dane_2))&input$example == FALSE)
      return(NULL)
    print(wykresInput())
  })
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste('wynik', input$id, '.txt', sep = '')
    },
    content = function(file) {
      write.table(wynik_podsum(), file)
    }
    
  )
  
}
