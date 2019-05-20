dane1 <- read.table("dane1.txt", quote="\"", comment.char="")
dane2 <- read.table("dane2.txt", quote="\"", comment.char="")
#zad1
stara <- as.vector(dane1[[1]], mode = "double")
nowa <- as.vector(dane2[[1]], mode = "double")
#rysowanie histogramow
#hist(stara, main = "Histogram wydajności pracy w starej hali",xlab = "Wydajność")
#hist(nowa, main = "Histogram wydajności pracw nowej hali",xlab = "Wydajność")
#szeregi rozdzielcze

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
zad1_rozdzielczy <- function(wektor)
{
  sr <- hist(wektor)
  #miary przeciętne
  sredniaArytmetyczna = sum(sr$counts * sr$mids) / sum(sr$counts)
  sredniaHarmoniczna = sum(sr$counts) / sum(sr$counts / sr$mids)
  sredniaGeometryczna = (prod(sr$mids ^ sr$counts)) ^ (1 / sum(sr$counts))
  kwartyl0.25 = kwartyl(sr, 0.25)
  kwartyl0.75 = kwartyl(sr, 0.75)
  mediana = kwartyl(sr, 0.5)
 
   #miary zróżnicowania
  wariancja = sum(((sr$mids-sredniaArytmetyczna) ^ 2) * sr$counts) / sum(sr$counts)
  rozstepMiedzycwiartkowy = kwartyl0.75 - kwartyl0.25
  odchylenieStandardowe = sqrt(wariancja)
  wspolczynnikZmiennosci = odchylenieStandardowe / sredniaArytmetyczna
  rozstepWynikow = max(sr$breaks) - min(sr$breaks)

  #miary asymetrii
  skosnosc = (sum(((sr$mids-sredniaArytmetyczna) ^ 3) * sr$counts) / sum(sr$counts)) / (odchylenieStandardowe ^ 3)

  #miary koncentracji
  kurtoza = (sum(((sr$mids-sredniaArytmetyczna) ^ 4) * sr$counts) / sum(sr$counts)) / (odchylenieStandardowe ^ 4)
  
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, kwartyl0.25, kwartyl0.75, mediana, rozstepWynikow, rozstepMiedzycwiartkowy, wariancja, odchylenieStandardowe, wspolczynnikZmiennosci, skosnosc, kurtoza)
  return(wynik)
}
#szeregi szczegółowe
zad1_szczegolowy <- function(wektor)
{
  ss = sort(wektor)

  #miary przeciętne
  sredniaArytmetyczna = mean(ss)
  sredniaHarmoniczna = 1 / mean(1 / ss)
  sredniaGeometryczna = prod(ss) ^ (1 / length(ss))
  kwartyl0.25 = quantile(ss, 0.25)
  kwartyl0.75 = quantile(ss, 0.75)
  mediana = median(ss)
  
  #miary zróżnicowania
  rozstepWynikow = max(ss) - min(ss)
  rozstepMiedzycwiartkowy = kwartyl0.75 - kwartyl0.25
  wariancja = var(ss)
  odchylenieStandardowe = sqrt(wariancja)
  wspolczynnikZmiennosci = odchylenieStandardowe / sredniaArytmetyczna

  #miary asymetrii
  skosnosc = (sum((ss - sredniaArytmetyczna) ^ 3) / length(ss)) / (odchylenieStandardowe^ 3)
  
  #miary koncentracji
  kurtoza = (sum((ss - sredniaArytmetyczna) ^ 4) / length(ss)) / (odchylenieStandardowe^ 4)
  
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, kwartyl0.25, kwartyl0.75, mediana, rozstepWynikow, rozstepMiedzycwiartkowy, wariancja, odchylenieStandardowe, wspolczynnikZmiennosci, skosnosc, kurtoza)
  return(wynik)
}

zad1<- function(wektor_stara, wektor_nowa)
{
  wynik_szczegolowy_stara = zad1_szczegolowy(wektor_stara)
  wynik_szczegolowy_nowa = zad1_szczegolowy(wektor_nowa)
  wynik_rozdzielczy_stara = zad1_rozdzielczy(wektor_stara)
  wynik_rozdzielczy_nowa = zad1_rozdzielczy(wektor_nowa)
  t = data.frame(szczegolowy_nowa = wynik_szczegolowy_nowa, szczegolowy_stara = wynik_szczegolowy_stara, rozdzielczy_nowa = wynik_rozdzielczy_nowa, rozdzielczy_stara = wynik_rozdzielczy_stara)
  row.names(t) = c("Srednia arytmetyczna:", "Srednia harmoniczna:", "Srednia geometryczna:", "Kwartyl 0.25", "Kwartyl 0.75:", "Mediana:", "Rozstęp wyników:", "Rozstęp międzyćwiartkowy:", "Wariancja próbkowa:", "Odchylenie standardowe:", "Współczynnik zmienności:", "Skośność:", "Kurtoza:")
  print(t)
}
test <- function(wektor) 
{
  
  wektor = sort(wektor) # sortowanie danych
  n = length(wektor) # sprawdzenie ilosci elementow
  
  # przyjmowanie odpowiedniego k z tablicy testu w zaleznosci od liczby elementow
  if(length(wektor) == 37) { k = 0.1457 }
  else { k = 0.1279 }
  
  p = pnorm((wektor - mean(wektor))/sd(wektor))
  # pnorm - The Normal Distribution - rozklad normalny
  # mean - Arithmetic Mean - srednia arytmetyczna
  # sd - Standard Deviation - odchylenie standardowe 
  
  Dplus = max(seq(1:n)/n - p)
  Dminus = max(p - (seq(1:n) - 1)/n)
  
  # seq - Sequence Generation
  
  D = max(Dplus, Dminus)
  
  cat("wartosc D wynosi:", D, "\n")
  cat("wartosc k wynosi:", k,"\n")
  
  if(D < k) 
  { cat("Wydajnosci pracy maja rozklad normalny.\n") } 
  else 
  { cat("Wydajnosci pracy nie maja rozkladu normalnego.\n") }
  
}

zad2 <- function(wektor_stara, wektor_nowa)
{
  cat("Dane 1 (stara hala):\n") 
  test(wektor_stara)
  cat("Dane 2 (nowa hala):\n") 
  test(wektor_nowa)
}

zad3<- function(vector)
{
  n=length(vector)   #wielkosc proby
  sigma=sd(vector)   #probkowe oszacowanie odchylenia standardowego
  
  d=qt(.975, df=n-1)*sigma/sqrt(n) #bezwzgedny blad szacunku
  xsr=mean(vector)  #srednia arytmetyczna
  
  interval=xsr+c(-d,d)
  precision=d/xsr*100
  
  print('Interwal‚ estymacji przedzialowej o dokladnosci 95%:')
  print(round(interval,4))
  print('Wzgledna precyzja oszacowania:')
  print(round(precision,4))
}
zad4<- function(wektor)
{
  
  #średnia z próby
  mu=mean(wektor)
  
  #odchylenie standardowe
  sigma<-sd(wektor)
  
  #ilosc danych
  n=length(wektor)
  
  #przedział ufności 95%
  round(mu+c(-1,1)*sigma/sqrt(n)*qnorm(.975),2)
  
  #bezwzględny błąd szacunku
  d=qt(.975, df=n-1)*sigma/sqrt(n)
  
  #przedział 
  interval=mu+c(-d,d)
  
  #precyzja 
  precision=d/mu*100
  
  print('Interwal‚ estymacji przedzialowej o dokladnosci 95%:')
  print(round(interval,4))
  
  print('Wzgledna precyzja oszacowania:')
  print(round(precision,4))
}
zad1(stara, nowa)
zad2(stara, nowa)
zad3(stara)
zad4(nowa)