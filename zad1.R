####################Funkcje pomocnicze#####################

#Obliczie kwartyli

kwartyl <- function(sr, q)
{
  poz.kwartyla = sum(sr$counts) * q #pozycja kwartyla 
  liczebnoscSkumulowana <- sr$counts
  for(i in 1:length(sr$counts))
  {
    liczebnoscSkumulowana[i] = sum(sr$counts[1:i]) #obliczenie liczebnosci skumulowanej dla każdego przedzialu
  }
  j = 1;
  while(TRUE) #pętla szukająca pozycji kwartyla
  {
    if(poz.kwartyla < liczebnoscSkumulowana[j])
      break
    else
      j = j + 1
  }
  dolnaWartoscPrzedzialu = sr$breaks[j] #dolna wartość przedziału kwartyla
  liczebnoscSkumulowanaPoprzedniego = liczebnoscSkumulowana[j - 1] #liczebność skumulowana przedziału poprzedzającego
  rozpietosc = sr$breaks[j + 1] - dolnaWartoscPrzedzialu
  wynik = dolnaWartoscPrzedzialu + ((poz.kwartyla - liczebnoscSkumulowanaPoprzedniego) * rozpietosc / liczebnoscSkumulowana[j])
  return(wynik)
}

##Obliczanie Dominant

dominantaSzczegolowy <- function(wektor) #Szereg szczegółowy
{
  uni_wekt <- unique(wektor) #wektor tylko z wartościami unikalnymi
  rozmiar <- length(uni_wekt) #rozmiar utworzonego wektora
  
  ilosc_wys <- vector(mode="numeric", length=rozmiar)#Vector ilości wystąpień unikalnych elemntów w wektorze
  
  for(i in 1:rozmiar)#Ustawienie poczatkowych wartosci wektora
  {
    ilosc_wys[i] <- sum(wektor == uni_wekt[i]) #Przypisanie ilości wsytąpień wartości unikalnej w danch 
  }
  #ifelse warunek, co gdy prawda, co gdy fałsz
  #Jeżeli długość wektora złożonego z elemntów o wartości równej maksymalnej w wartości w tym wektorze jest większa od jeden
  #True to zwróć NA (Not Available)
  #False to zwróć wartość z wektora unikalnego which.max(podaje indek elemntu o wartości max, a indexy ilosc_wys i uni_wek odpowiadają sobie)
  return (ifelse( length(ilosc_wys[ilosc_wys == max(ilosc_wys)]) > 1 , NA , uni_wekt[which.max(ilosc_wys)] ))
}

dominantaRozdzielczy <- function(wektor) #Szereg rozdzielczy
{
  nr_przedz = which.max(wektor$counts) #nr przedziału w histogramie o największej liczebności
  
  x0 = wektor$breaks[nr_przedz]             #dolna granica przedziału z modą
  n_minus = wektor$counts[nr_przedz - 1]   #liczebnosc przedzialu poprzedzającego modę
  n_0      = wektor$counts[nr_przedz]       #liczebnosc przedzialu z modą
  n_plus  = wektor$counts[nr_przedz + 1]   #liczebnosc przedziału następującego po modzie
  rozpietoscPrzedzialu = wektor$breaks[nr_przedz+1] - wektor$breaks[nr_przedz] #rozpietosc przedzialu z modą
  
  wynik = x0 + ( (n_0 - n_minus) / ( (n_0 - n_minus) + (n_0 - n_plus) ) * rozpietoscPrzedzialu) 
  return (wynik)
}

####################Funkcje wyznaczające wartości dla szeregów#####################

zad1_rozdzielczy <- function(wektor)#Rozdzielczy
{
  sr <- hist(wektor)
  #miary przeciętne
  sredniaArytmetyczna = sum(sr$counts * sr$mids) / sum(sr$counts)
  sredniaHarmoniczna = sum(sr$counts) / sum(sr$counts / sr$mids)
  sredniaGeometryczna = (prod(sr$mids ^ sr$counts)) ^ (1 / sum(sr$counts))
  kwartyl0.25 = kwartyl(sr, 0.25)
  kwartyl0.75 = kwartyl(sr, 0.75)
  mediana = kwartyl(sr, 0.5)
  modaDominanta = dominantaRozdzielczy(sr)
  
  #miary zróżnicowania
  wariancja = sum(((sr$mids-sredniaArytmetyczna) ^ 2) * sr$counts) / sum(sr$counts)
  rozstepMiedzycwiartkowy = kwartyl0.75 - kwartyl0.25
  odchylenieStandardowe = sqrt(wariancja)
  wspolczynnikZmiennosci = odchylenieStandardowe / sredniaArytmetyczna
  rozstepWynikow = max(sr$breaks) - min(sr$breaks)
  odchyleniePrzecietne = sum(abs(sr$mids - sredniaArytmetyczna)*sr$counts) / sum(sr$counts) #Odchylenie przeciętne od średniej arytmetycznej
  odchylenieCwiartkowe = (kwartyl0.75 - kwartyl0.25) / 2
  
  #miary asymetrii
  skosnosc = (sum(((sr$mids-sredniaArytmetyczna) ^ 3) * sr$counts) / sum(sr$counts)) / (odchylenieStandardowe ^ 3)
  
  #miary koncentracji
  kurtoza = (sum(((sr$mids-sredniaArytmetyczna) ^ 4) * sr$counts) / sum(sr$counts)) / (odchylenieStandardowe ^ 4)
  excess = kurtoza - 3
  
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, kwartyl0.25, kwartyl0.75, mediana, modaDominanta, rozstepWynikow, rozstepMiedzycwiartkowy, wariancja, odchylenieStandardowe, odchyleniePrzecietne, odchylenieCwiartkowe, wspolczynnikZmiennosci, skosnosc, kurtoza, excess)
  return(wynik)
}


zad1_szczegolowy <- function(wektor)#Szczegółowy
{
  ss = sort(wektor)
  
  #miary przeciętne
  sredniaArytmetyczna = mean(ss)
  sredniaHarmoniczna = 1 / mean(1 / ss)
  sredniaGeometryczna = prod(ss) ^ (1 / length(ss))
  kwartyl0.25 = quantile(ss, 0.25)
  kwartyl0.75 = quantile(ss, 0.75)
  mediana = median(ss)
  modaDominanta = dominantaSzczegolowy(ss)
  
  #miary zróżnicowania
  rozstepWynikow = max(ss) - min(ss)
  rozstepMiedzycwiartkowy = kwartyl0.75 - kwartyl0.25
  wariancja = var(ss)
  odchylenieStandardowe = sqrt(wariancja)
  wspolczynnikZmiennosci = odchylenieStandardowe / sredniaArytmetyczna
  odchyleniePrzecietne = sum(abs(ss - sredniaArytmetyczna)) / length(ss) #Odchylenie przeciętne od średniej arytmetycznej
  odchylenieCwiartkowe = (kwartyl0.75 - kwartyl0.25) / 2
  
  #miary asymetrii
  skosnosc = (sum((ss - sredniaArytmetyczna) ^ 3) / length(ss)) / (odchylenieStandardowe^ 3)
  
  #miary koncentracji
  kurtoza = (sum((ss - sredniaArytmetyczna) ^ 4) / length(ss)) / (odchylenieStandardowe^ 4)
  excess = kurtoza - 3
  
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, kwartyl0.25, kwartyl0.75, mediana, modaDominanta, rozstepWynikow, rozstepMiedzycwiartkowy, wariancja, odchylenieStandardowe, odchyleniePrzecietne, odchylenieCwiartkowe, wspolczynnikZmiennosci, skosnosc, kurtoza, excess)
  return(wynik)
}

###################Funkcja ostateczna###################

zad1<- function(wektor_stara, wektor_nowa)
{
  wynik_szczegolowy_stara = zad1_szczegolowy(wektor_stara)
  wynik_szczegolowy_nowa = zad1_szczegolowy(wektor_nowa)
  wynik_rozdzielczy_stara = zad1_rozdzielczy(wektor_stara)
  wynik_rozdzielczy_nowa = zad1_rozdzielczy(wektor_nowa)
  t = data.frame(szczegolowy_nowa = wynik_szczegolowy_nowa, szczegolowy_stara = wynik_szczegolowy_stara, rozdzielczy_nowa = wynik_rozdzielczy_nowa, rozdzielczy_stara = wynik_rozdzielczy_stara)
  row.names(t) = c("Srednia arytmetyczna:", "Srednia harmoniczna:", "Srednia geometryczna:", "Kwartyl 0.25", "Kwartyl 0.75:", "Mediana:", "Dominanta:","Rozstęp wyników:", "Rozstęp międzyćwiartkowy:", "Wariancja próbkowa:", "Odchylenie standardowe:", "Odchylenie od Średniej:", "Odchylenie od Mediany:", "Współczynnik zmienności:", "Skośność:", "Kurtoza:", "Excess:")
  print(t)
}