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
  
  ####  Code for the first tab - single hyphae analysis
  
  #read in data from file one and two
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
    
    d <- read.table(inFile$datapath, header=input$header, sep = '\t', quote = "\"")
    
    if(ncol(d) == 3){
      d <- d[,-1]
    }
    
    colnames(d) <- c('V1', 'V2')
    return(d)
    if (is.null(inFile))
      return(NULL)
  })
  
  
  # find septa and DNA positions
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
                        usun = input$usun,
                        filter_local_dna = input$filter_local_dna,
                        filter_local_int_dna = input$filter_local_int_dna,
                        filter_local_width_dna = input$filter_local_width_dna,
                        filter_local_sept = input$filter_local_sept,
                        filter_local_int_sept = input$filter_local_int_sept,
                        filter_local_width_sept = input$filter_local_width_sept
                        )
    
    
    
    return(wynik)
  })
  
  # creates a summary table icluding strain and hyphae id, ads column with parameters for reproducibility
  
  wynik_podsum <- reactive({
    
    wynik <- find_wynik()
    
    tabela <- find_spory_summarise(wynik_dna = wynik[[1]], wynik_sept = wynik[[2]], 
                                   strzepka = input$id, szczep = input$szczep)
    
    usun_spory <- sub(' ', '', unlist(stringr::str_split(input$usun_spory, ',')))
    
    # filter unwanted spores
    tabela <- tabela %>% dplyr::filter(!(spora %in% usun_spory))
    if("spore_first" %in% input$usun){
      tabela <- tabela %>% dplyr::filter(spora != 's_1')
    }
    if("spore_last" %in% input$usun){
      spore_last <- tail(unique(tabela$spora), 1)
      
      tabela <- tabela %>% dplyr::filter(spora != spore_last)
    }
    
    tabela$parametry <- paste(input$s_1, input$m_1, input$procent_1, input$threshold_1,
                              input$s_2, input$m_2, input$procent_2, input$threshold_2,
                              sep = '_')
    
    return(tabela)
    
  })
  
  #show summary table
  output$tabela <- renderTable({
    if ((is.null(input$dane_1)|is.null(input$dane_2))&input$example == FALSE)
      return(NULL)
    wynik_podsum()
  }
  )
  
  # make hyphae plot
  wykresInput <- reactive({
    
    ramka_1 <- dane_1()
    ramka_2 <- dane_2()
    
    wynik <- find_wynik()
    
    find_spory_plot(ramka_1 = ramka_1, 
                    ramka_2 = ramka_2,
                    wynik_dna = wynik[[1]],
                    wynik_sept = wynik[[2]])
    
  })
  
  # funkcja pokazująca wykres w aplikacji
  output$wykres <- renderPlot({
    if ((is.null(input$dane_1)|is.null(input$dane_2))&input$example == FALSE)
      return(NULL)
    print(wykresInput())
  })
  
  # download hyphae plot
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste('wynik', input$szczep, input$id, '.txt', sep = '')
    },
    content = function(file) {
      write.table(wynik_podsum(), file)
    }
    
  )
  
  
  #### Code for second tab - analysis od many hyphae
  
  # load multiple files into shiny using data.table and lapply
  dane_porownanie <-reactive({
    data.table::rbindlist(lapply(input$wyniki$datapath, read.table),
                          use.names = TRUE, fill = TRUE)
  })
  output$tabela_wyniki <- renderTable(dane_porownanie())
  
  # create summary table for all data
  podsumowanie <- reactive({
    
    dane <- dane_porownanie()
    
    dane %>%
      dplyr::group_by(szczep) %>%
      dplyr::distinct(szczep, strzepka, spora, .keep_all = TRUE) %>%
      dplyr::summarise(mean_dist_sept = mean(dist_sept),
                       median_dist_sept = median(dist_sept),
                       sd_dist_sept = sd(dist_sept),
                       n_bez_DNA = sum(is.na(DNA)),
                       proc_bez_DNA = n_bez_DNA/dplyr::n(),
                       proc_micro = sum(dist_sept <= input$micro)/dplyr::n(),
                       proc_macro = sum(dist_sept >= input$macro)/dplyr::n(),
                       n = dplyr::n()) -> dane_podsum
    
    return(dane_podsum)
    
  })
  
  # show table with all data
  output$tabela_podsumowanie <- renderTable(podsumowanie())
  
  # make summary plot with histogram and barplots
  podsumowanie_wykres <- reactive({
    
    dane <- dane_porownanie()
    
    
    
    dane_podsum <- podsumowanie()
    
    p1 <- ggplot2::ggplot(dane %>% dplyr::distinct(szczep, strzepka, spora, .keep_all = TRUE))
    
    # wybiera rodzaj wykresu
    if(input$wykres_type == 'hist'){
      p1 <- p1 + ggplot2::geom_histogram(ggplot2::aes(x = dist_sept, fill = szczep),
                                         binwidth = input$hist_bin, position = 'dodge')
    }
    
    if(input$wykres_type == 'density'){
      p1 <- p1 + ggplot2::geom_density(ggplot2::aes(x = dist_sept, color = szczep))
    }
    
    if(input$wykres_type == 'boxplot'){
      p1 <- p1 + ggplot2::geom_boxplot(ggplot2::aes(y = dist_sept, x = szczep), outlier.alpha = 0)+
        ggbeeswarm::geom_quasirandom(ggplot2::aes(y = dist_sept, x = szczep), alpha = 0.2)
    }
    
    # zaznacza micro i macrocomaprtmenty
    if(input$wykres_type %in% c('hist', 'density')){
      
      p1 <- p1 + ggplot2::geom_vline(xintercept = c(input$micro, input$macro), 
                                     linetype = 2, color = 'grey40')
      
    } else {
      
      p1 <- p1 + ggplot2::geom_hline(yintercept = c(input$micro, input$macro), 
                                     linetype = 2, color = 'grey40')
      
    }
    
    # opisy osi
    if(input$wykres_type %in% c('hist', 'density')){
      
      p1 <- p1 + ggplot2::xlab("Prespore length")
      
    } else {
      
      p1 <- p1 + ggplot2::ylab("Prespore length")
      
    }
    
    p1 <- p1 + ggplot2::scale_color_brewer(palette = 'Set1')+
      ggplot2::scale_fill_brewer(palette = 'Pastel1')
    
    p1 <- p1 + ggplot2::theme_bw()
    #print(p1)
    
    # plot showing percentage of spores lacking DNA
    dane_podsum %>% dplyr::mutate(proc_z_DNA = 1 - proc_bez_DNA) %>%
      dplyr::select(szczep, proc_z_DNA, proc_bez_DNA) %>%
      tidyr::pivot_longer(cols = dplyr::contains('DNA'),
                          names_to = 'DNA',
                          values_to = 'procent') -> dane_DNA
    
    p2 <- ggplot2::ggplot(dane_DNA, ggplot2::aes(x = szczep, y = procent, fill = DNA))
    
    p2 <- p2 + ggplot2::geom_col()
    
    p2 <- p2 + ggplot2::scale_fill_manual(values = c('red3', 'gray40'),
                                          name = 'DNA',
                                          labels = c('No', 'Yes'))
    
    p2 <- p2 + ggplot2::xlab('')
    
    p2 <- p2 + ggplot2::coord_flip()
    
    p2 <- p2 + ggplot2::scale_y_continuous(labels = scales::percent)
    
    p2 <- p2 + ggplot2::theme_bw()
    
    dane_podsum %>% dplyr::mutate(proc_norm = 1 - proc_micro - proc_macro) %>%
      dplyr::select(szczep, proc_norm, proc_micro, proc_macro) %>%
      tidyr::pivot_longer(cols = dplyr::contains('proc'),
                          names_to = 'spore',
                          values_to = 'procent') %>%
      dplyr::mutate(spore = factor(spore, levels = c('proc_macro', 'proc_norm', 'proc_micro'))) -> dane_DNA
    
    # plot showing percentage of micro and macro compartments
    p3 <- ggplot2::ggplot(dane_DNA, ggplot2::aes(x = szczep, y = procent, fill = spore))
    
    p3 <- p3 + ggplot2::geom_col()
    
    p3 <- p3 + ggplot2::scale_fill_manual(values = c('dodgerblue1', 'gray40', 'indianred3'),
                                          name = '',
                                          labels = c('Macrocompartment', 'Normal prespore', 'Microcompartment'))
    
    p3 <- p3 + ggplot2::xlab('')
    
    p3 <- p3 + ggplot2::coord_flip()
    
    p3 <- p3 + ggplot2::scale_y_continuous(labels = scales::percent)
    
    p3 <- p3 + ggplot2::theme_bw()
    
    print(p1 + p2 + p3 + plot_layout(ncol = 1, heights = c(4,1,1)))
  })
  
  
  # show summary plot
  output$wykres_podsumowanie <- renderPlot({
    if (is.null(input$wyniki))
      return(NULL)
    print(podsumowanie_wykres())
  })
  
  # download data from second tab - bound together from many files
  output$download_data_all <- downloadHandler(
    
    filename = function() {
      paste('wyniki_all', '.txt', sep = '')
    },
    content = function(file) {
      write.table(dane_porownanie(), file)
    }
    
  )
  
}
