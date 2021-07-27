# Funkcja do analizy sporulacji
find_spory <- function(ramka_1, ramka_2, s_1 = 2, m_1 = FALSE, procent_1 = 0.05, threshold_1 = 10, 
                       back_1=FALSE, s_2 = 1.25, m_2 = FALSE, procent_2 = 0.05, threshold_2 = 10, 
                       back_2=FALSE, min_dna = TRUE, min_sept = TRUE, usun = NA, nr = 1, 
                       filter_local_dna = FALSE, filter_local_int_dna = 1.05, filter_local_width_dna = 2,
                       filter_local_sept = FALSE, filter_local_int_sept = 1.05, filter_local_width_sept = 2,
                       ...){
  
  if(min_dna == TRUE){
    ramka_1[,2] <- ramka_1[,2] - min(ramka_1[,2])
  }
  if(min_sept == TRUE){
    ramka_2[,2] <- ramka_2[,2] - min(ramka_2[,2])
  }
  
  ramka_1$V3 <- 'szczep'
  ramka_2$V3 <- 'szczep'
  
  wynik_dna <- find_peaks(ramka_1, s = s_1, procent = procent_1, m = m_1, threshold = threshold_1, plot = FALSE,
                          filter_local = filter_local_dna, filter_local_int = filter_local_int_dna,
                          filter_local_width = filter_local_width_dna)
  wynik_sept <- find_peaks(ramka_2, s = s_2, procent = procent_2, m = m_2, threshold = threshold_2, plot = FALSE,
                           filter_local = filter_local_sept, filter_local_int = filter_local_int_sept,
                           filter_local_width = filter_local_width_sept)
  
  if ('dna_first' %in% usun){
    wynik_dna <- wynik_dna[-1,]
  }
  
  if ('dna_last' %in% usun){
    wynik_dna <- wynik_dna[-nrow(wynik_dna),]
  }
  
  if ('sept_first' %in% usun){
    wynik_sept <- wynik_sept[-1,]
  }
  
  if ('sept_last' %in% usun){
    wynik_sept<- wynik_sept[-nrow(wynik_sept),]
  }
  
  return(list(wynik_dna, wynik_sept))
  
}

# makes plots of analyzed hyphae with normalized fluorescence
find_spory_plot <- function(ramka_1, ramka_2, wynik_dna, wynik_sept){
  
  ramka_1 <- ramka_1 %>% dplyr::mutate(V2 = V2 - min(V2),
                                       V2 = V2/max(V2))
  
  ramka_2 <- ramka_2 %>% dplyr::mutate(V2 = V2 - min(V2),
                                       V2 = V2/max(V2))
  
  max_plot <- max(ramka_1$V1)
  
  p <- ggplot2::ggplot(wynik_sept)
  p1 <- p + ggplot2::geom_segment(ggplot2::aes(x = dist_tip, y = 1, yend = 2, xend = dist_tip), color = 'red3') + 
    ggplot2::geom_point(data = wynik_dna, ggplot2::aes(x = dist_tip, y = 1.5), color = 'blue')+
    ggplot2::ylim(0.5, 2.5)+
    ggplot2::theme_bw() + 
    ggplot2::xlim(-0.5,max_plot)+
    ggplot2::xlab('')+
    ggplot2::ylab('')+
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  
  
  
  p <- ggplot2::ggplot(ramka_1)
  p2 <- p + ggplot2::geom_line(ggplot2::aes(x = V1, y = V2), color = 'blue') + 
    ggplot2::geom_line(data = ramka_2, ggplot2::aes(x = V1, y = V2), color = 'red')+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    ggplot2::geom_vline(data = wynik_sept, ggplot2::aes(xintercept = dist_tip), color = 'grey20', linetype = 2)+
    ggplot2::xlim(-0.5,max_plot)+
    ggplot2::xlab('Distance to the tip')+
    ggplot2::ylab('Normalized fluorescence')
  
  print(p1 + p2 + plot_layout(ncol=1, heights = c(1,3)))
  
  
}

# summarizes analyzed hyphae - creates result table from find_spory() result
find_spory_summarise <- function(wynik_dna, wynik_sept, strzepka, szczep){
  
  x <- nrow(wynik_sept) + 1
  
  wynik <- data.frame(spora = rep(NA, x), 
                      strzepka = rep(strzepka, x),
                      szczep = rep(szczep, x),
                      sept_1 = rep(NA, x), 
                      sept_2 = rep(NA, x), 
                      DNA = rep(NA, x), 
                      dist_sept = rep(NA, x), 
                      liczba_spor =rep(NA, x),
                      dlugosc = rep(NA, x),
                      liczba_chr = rep(NA, x))
  
  for (i in 1 : (nrow(wynik_sept)+1)){
    if(i == 1){
      wynik$spora[i] <- paste0('s_', i)
      wynik$sept_1[i] <- 0
      wynik$sept_2[i] <- wynik_sept$dist_tip[i]
      wynik$dist_sept[i] <- wynik$sept_2[i] - wynik$sept_1[i]
      wynik$dlugosc[i] <- wynik_sept$dlug[i]
    }
    if(i == nrow(wynik_sept)+1){
      
      wynik$spora[i] <- paste0('s_', i)
      wynik$sept_1[i] <- wynik_sept$dist_tip[i-1]
      wynik$sept_2[i] <- wynik_sept$dlug[i-2]
      wynik$dist_sept[i] <- wynik$sept_2[i] - wynik$sept_1[i]
      wynik$dlugosc[i] <- wynik_sept$dlug[i-2]
      
    }
    if(i > 1 & i <= nrow(wynik_sept)){
      wynik$spora[i] <- paste0('s_', i)
      wynik$sept_1[i] <- wynik_sept$dist_tip[i-1]
      wynik$sept_2[i] <- wynik_sept$dist_tip[i]
      wynik$dist_sept[i] <- wynik$sept_2[i] - wynik$sept_1[i]
      wynik$dlugosc[i] <- wynik_sept$dlug[i]
      
    }
    
  }
  
  temp_caly <- wynik[0,]
  
  for(i in 1 : nrow(wynik)){
    
    dna <- subset(wynik_dna, dist_tip >= wynik$sept_1[i] & dist_tip <= wynik$sept_2[i])
    
    if (nrow(dna) == 1){
      wynik$DNA[i] <- dna$dist_tip[1]
    }
    if (nrow(dna) > 1){
      wynik$DNA[i] <- dna$dist_tip[1]
      
      for(j in 2:nrow(dna)){
        temp <- wynik[i,]
        temp$DNA <- dna$dist_tip[j]
        wynik <- rbind(wynik, temp)
      }
      
    }
    
  }
  
  wynik$liczba_spor <- nlevels(factor(wynik$spora))
  
  wynik <- wynik %>% dplyr::group_by(spora) %>%
    dplyr::mutate(liczba_chr = dplyr::n(),
                  
                  liczba_chr = as.integer(ifelse(is.na(DNA), 0, liczba_chr))) %>%
    
    
    dplyr::arrange(sept_1)
  
  # cat('Dlugosc strzepki: ', wynik$dlugosc[1], '\n', 
  #     'Ilosc spor : ', wynik$ilosc_spor[i], '\n',
  #     'Ilosc chromosomow: ', nrow(wynik_dna), '\n', 
  #     'Ilosc spor bez DNA: ', sum(is.na(wynik$DNA)), '\n',
  #     'Srednia odleglosc', round(mean(wynik$dist_sept),2))
  
  return(wynik)
  
}

# Funkcja do szukania pików fluorescencji w danych z time lapse
# ramka - tabela z danymi (kolumna 1 - odległości, 2 - intensywność fluorescencji, 3 - indeksy klatek)
# s - sigma - dokładność dopasowania pików, im wyższa tym mniej znajdzie
# m - jak ma wygładzać dane, przy FALSE znajduje więcej pików
# procent - ile tła ma odjąć, rozsądny zakres to od 1(całość) do 0.01 (1 procent)
# threshold - powyżej jakiego poziomu ma zaznaczać piki, procent wysokości najwyższego piku
# plot - czy ma rysować wykresy
# back - na razie niech zostanie FALSE 
# dodaj_nr - czy ma numerować piki ... 
find_peaks <- function (ramka, s = 2, m = FALSE, procent = 1, threshold=10, 
                        back=FALSE, plot=TRUE, lapse = 10, dodaj_nr = TRUE,
                        filter_local = FALSE, 
                        filter_local_int = 1.05, filter_local_width = 2, ...) { 
  # ładuje potrzebne pakiety (muszą być zainstalowane)
  #library(modeest)
  #library(Peaks)
  #library.dynam('Peaks', 'Peaks', lib.loc=NULL) 
  #library(dplyr)
  ilosc <- 0
  # sprawdzamy ile jest klatek
  ramka[,3]<-as.factor(ramka[,3])
  n <- nlevels(ramka[,3])
  # jakie są poziomy
  poziomy <- levels(ramka[,3])
  # robimy pętlę osobno dla każdej klatki
  for (i in 1:n) {
    # wybieramy klatkę
    x<-subset(ramka, ramka[,3] == poziomy[i])
    x1<-x
    # normalizacja przez baseline (moda)
    baza <- modeest::mlv(x[,2], method="shorth")
    # jaki procent tła ma odjąć
    baza2 <- baza[[1]] * procent
    # odejmujemy wartość baseline od intensywności i dzielimy przez baseline
    x[,2]<-(x[,2]-baza2[1])/baza2[1]
    #zamieniamy wartości ujemne na zera
    x[,2]<-replace(x[,2], x[,2]<0, 0)
    
    # szukanie pików
    piki<-Peaks::SpectrumSearch(x[,2], sigma=s, markov=m, threshold=threshold, background=back)
    # ilosc znalezionych pików
    ilosc <- sum(ilosc, length(piki$pos))
    # rysuje wykres z zaznaczonymi pikami, jeżeli plot == TRUE
    if (plot==TRUE){
      plot(x[,1],x[,2],type="l", main=i, ylim=c(0, (max(x[,2])+0.5)))
      lines(x[,1],piki$y,type="l",col="red")
      points(y=rep(1,length(piki$pos)),x=x[piki$pos,1],col="green",pch="+",cex=2)
      if(dodaj_nr == TRUE){
        text(y = rep(0.5, length(piki$pos)), x = sort(x[piki$pos,1]),
             labels = (1:ilosc)[(ilosc+1-length(piki$pos)):ilosc])
      }
    }
    if (length(piki[[1]]) > 0){
      # przygotowuje tabelę z wynikami
      wynik<-miejsca<-data.frame(dist_base=max(x[,1])-x[piki[[1]],1], # odległość od podstawy strzępki
                                 dist_tip =x[piki[[1]],1], # odległość od tipa
                                 int_raw = x1[piki[[1]],2],
                                 int_nor=round((x1[piki[[1]],2]-baza[[1]])/baza[[1]]), # intensywność fluorescencji zaokrąglona do liczby całkowitej
                                 dlug = max(x[,1]), # długość strzępki
                                 indeks = i,
                                 tlo = baza[[1]],
                                 tlo_nor = baza2[1],
                                 czas = (i * lapse)-lapse) # kolejna klatka

      if(filter_local){
        usun <- numeric(0)
        for(k in 1:nrow(wynik)){
          peak_maximum <- wynik$int_raw[k]
          min_width <- wynik$dist_tip[k] - filter_local_width/2
          max_width <- wynik$dist_tip[k] + filter_local_width/2

          locality <- subset(x1, V1 >= min_width & V1 < max_width)
          locality <- mean(locality$V2)

          if(peak_maximum/locality > filter_local_int){

          } else {
            usun <- c(usun, k)
          }

        }

        if(length(usun) > 0){
          wynik <- wynik[-usun,]
        }
      }
      
    } else { wynik <- NULL}
    # jeżeli obrót pętli inny niż jeden to dopisujemy wyniki do poprzednich
    if (i == 1){ wynik_kon = wynik} else {
      wynik_kon <-rbind(wynik_kon, wynik)
    }
    
  }
  
  # sortuje wyniki najpierw według klatek, potem według odległości od tipa
  wynik_kon<-dplyr::arrange(wynik_kon, indeks, dist_tip)
  # zwraca wynik
  return(wynik_kon)
}