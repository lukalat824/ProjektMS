#-------------------ZAD1-------------------

####################Funkcje pomocnicze#####################

#Obliczie kwartyli

kwartyl <- function(sr, q)
{
  poz.kwartyla = sum(sr$counts) * q #pozycja kwartyla 
  liczebnoscSkumulowana <- sr$counts
  for(i in 1:length(sr$counts))
  {
    liczebnoscSkumulowana[i] = sum(sr$counts[1:i]) #obliczenie liczebnosci skumulowanej dla kazdego przedzialu
  }
  j = 1;
  while(TRUE) #petla szukajaca pozycji kwartyla
  {
    if(poz.kwartyla < liczebnoscSkumulowana[j])
      break
    else
      j = j + 1
  }
  dolnaWartoscPrzedzialu = sr$breaks[j] #dolna wartosc przedzialu kwartyla
  liczebnoscSkumulowanaPoprzedniego = liczebnoscSkumulowana[j - 1] #liczebnosc skumulowana przedzialu poprzedzajacego
  rozpietosc = sr$breaks[j + 1] - dolnaWartoscPrzedzialu
  wynik = dolnaWartoscPrzedzialu + ((poz.kwartyla - liczebnoscSkumulowanaPoprzedniego) * rozpietosc / liczebnoscSkumulowana[j])
  return(wynik)
}

##Obliczanie Dominant

dominantaSzczegolowy <- function(wektor) #Szereg szczególowy
{
  uni_wekt <- unique(wektor) #wektor tylko z wartosciami unikalnymi
  rozmiar <- length(uni_wekt) #rozmiar utworzonego wektora
  
  ilosc_wys <- vector(mode="numeric", length=rozmiar)#Vector ilosci wystapien unikalnych elemntów w wektorze
  
  for(i in 1:rozmiar)#Ustawienie poczatkowych wartosci wektora
  {
    ilosc_wys[i] <- sum(wektor == uni_wekt[i]) #Przypisanie ilosci wsytapien wartosci unikalnej w danch 
  }
  #ifelse warunek, co gdy prawda, co gdy falsz
  #Jezeli dlugosc wektora zlozonego z elemntów o wartosci równej maksymalnej w wartosci w tym wektorze jest wieksza od jeden
  #True to zwróc NA (Not Available)
  #False to zwróc wartosc z wektora unikalnego which.max(podaje indek elemntu o wartosci max, a indexy ilosc_wys i uni_wek odpowiadaja sobie)
  return (ifelse( length(ilosc_wys[ilosc_wys == max(ilosc_wys)]) > 1 , NA , uni_wekt[which.max(ilosc_wys)] ))
}

dominantaRozdzielczy <- function(wektor) #Szereg rozdzielczy
{
  nr_przedz = which.max(wektor$counts) #nr przedzialu w histogramie o najwiekszej liczebnosci
  
  x0 = wektor$breaks[nr_przedz]             #dolna granica przedzialu z moda
  n_minus = wektor$counts[nr_przedz - 1]   #liczebnosc przedzialu poprzedzajacego mode
  n_0      = wektor$counts[nr_przedz]       #liczebnosc przedzialu z moda
  n_plus  = wektor$counts[nr_przedz + 1]   #liczebnosc przedzialu nastepujacego po modzie
  rozpietoscPrzedzialu = wektor$breaks[nr_przedz+1] - wektor$breaks[nr_przedz] #rozpietosc przedzialu z moda
  
  wynik = x0 + ( (n_0 - n_minus) / ( (n_0 - n_minus) + (n_0 - n_plus) ) * rozpietoscPrzedzialu) 
  return (wynik)
}

####################Funkcje wyznaczajace wartosci dla szeregów#####################

zad1_rozdzielczy <- function(wektor)#Rozdzielczy
{
  sr <- hist(wektor)
  #miary przecietne
  sredniaArytmetyczna = sum(sr$counts * sr$mids) / sum(sr$counts)
  sredniaHarmoniczna = sum(sr$counts) / sum(sr$counts / sr$mids)
  sredniaGeometryczna = (prod(sr$mids ^ sr$counts)) ^ (1 / sum(sr$counts))
  kwartyl0.25 = kwartyl(sr, 0.25)
  kwartyl0.75 = kwartyl(sr, 0.75)
  mediana = kwartyl(sr, 0.5)
  modaDominanta = dominantaRozdzielczy(sr)
  
  #miary zróznicowania
  rozstepMiedzycwiartkowy = kwartyl0.75 - kwartyl0.25
  rozstepWynikow = max(sr$breaks) - min(sr$breaks)
  wariancjaObciazona = sum(((sr$mids-sredniaArytmetyczna) ^ 2) * sr$counts) / sum(sr$counts)
  wariancjaNieobciazona = wariancjaObciazona * (sum(sr$counts)/(sum(sr$counts)-1))
  odchylenieStandardoweObc = sqrt(wariancjaObciazona)
  wspolczynnikZmiennosci = odchylenieStandardoweObc / sredniaArytmetyczna
  odchyleniePrzecietne = sum(abs(sr$mids - sredniaArytmetyczna)*sr$counts) / sum(sr$counts) #Odchylenie przecietne od sredniej arytmetycznej
  odchylenieCwiartkowe = (kwartyl0.75 - kwartyl0.25) / 2
  
  #miary asymetrii
  skosnosc = (sum(((sr$mids-sredniaArytmetyczna) ^ 3) * sr$counts) / sum(sr$counts)) / (odchylenieStandardoweObc ^ 3)
  
  #miary koncentracji
  kurtoza = (sum(((sr$mids-sredniaArytmetyczna) ^ 4) * sr$counts) / sum(sr$counts)) / (odchylenieStandardoweObc ^ 4)
  excess = kurtoza - 3
  
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, kwartyl0.25, kwartyl0.75, mediana, modaDominanta, rozstepWynikow, rozstepMiedzycwiartkowy, wariancjaObciazona, wariancjaNieobciazona, odchylenieStandardoweObc, odchyleniePrzecietne, odchylenieCwiartkowe, wspolczynnikZmiennosci, skosnosc, kurtoza, excess)
  return(wynik)
}


zad1_szczegolowy <- function(wektor)#Szczególowy
{
  ss = sort(wektor)
  
  #miary przecietne
  sredniaArytmetyczna = mean(ss)
  sredniaHarmoniczna = 1 / mean(1 / ss)
  sredniaGeometryczna = prod(ss) ^ (1 / length(ss))
  kwartyl0.25 = quantile(ss, 0.25)
  kwartyl0.75 = quantile(ss, 0.75)
  mediana = median(ss)
  modaDominanta = dominantaSzczegolowy(ss)
  
  #miary zróznicowania
  rozstepWynikow = max(ss) - min(ss)
  rozstepMiedzycwiartkowy = kwartyl0.75 - kwartyl0.25
  wariancjaNieobciazona = var(ss)
  wariancjaObciazona = var(ss) * (length(ss)-1)/length(ss)
  odchylenieStandardoweObc = sqrt(wariancjaObciazona)
  wspolczynnikZmiennosci = odchylenieStandardoweObc / sredniaArytmetyczna
  odchyleniePrzecietne = sum(abs(ss - sredniaArytmetyczna)) / length(ss) #Odchylenie przecietne od sredniej arytmetycznej
  odchylenieCwiartkowe = (kwartyl0.75 - kwartyl0.25) / 2
  
  #miary asymetrii
  skosnosc = (sum((ss - sredniaArytmetyczna) ^ 3) / length(ss)) / (odchylenieStandardoweObc^ 3)
  
  #miary koncentracji
  kurtoza = (sum((ss - sredniaArytmetyczna) ^ 4) / length(ss)) / (odchylenieStandardoweObc^ 4)
  excess = kurtoza - 3
  
  wynik <- c(sredniaArytmetyczna, sredniaHarmoniczna, sredniaGeometryczna, kwartyl0.25, kwartyl0.75, mediana, modaDominanta, rozstepWynikow, rozstepMiedzycwiartkowy, wariancjaObciazona, wariancjaNieobciazona, odchylenieStandardoweObc, odchyleniePrzecietne, odchylenieCwiartkowe, wspolczynnikZmiennosci, skosnosc, kurtoza, excess)
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
  row.names(t) = c("Srednia arytmetyczna:", "Srednia harmoniczna:", "Srednia geometryczna:", "Kwartyl 0.25", "Kwartyl 0.75:", "Mediana:", "Dominanta:","Rozstep wyników:", "Rozstep miedzycwiartkowy:", "Wariancja obciazona:", "Wariancja nieobciazona:", "Odchylenie standardowe:", "Odchylenie od Sredniej:", "Odchylenie od Mediany:", "Wspólczynnik zmiennosci:", "Skosnosc:", "Kurtoza:", "Excess:")
  print(t)
}

#-------------------ZAD2-------------------
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

###################Funkcja ostateczna###################
zad2 <- function(wektor_stara, wektor_nowa)
{
  cat("Dane 1 (stara hala):\n") 
  test(wektor_stara)
  cat("Dane 2 (nowa hala):\n") 
  test(wektor_nowa)
}

#-------------------ZAD3-------------------

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

#-------------------ZAD4-------------------

zad4<- function(wektor)
{
  
  #srednia z próby
  mu=mean(wektor)
  
  #odchylenie standardowe
  sigma<-sd(wektor)
  
  #ilosc danych
  n=length(wektor)
  
  #przedzial ufnosci 95%
  round(mu+c(-1,1)*sigma/sqrt(n)*qnorm(.975),2)
  
  #bezwzgledny blad szacunku
  d=qt(.975, df=n-1)*sigma/sqrt(n)
  
  #przedzial 
  interval=mu+c(-d,d)
  
  #precyzja 
  precision=d/mu*100
  
  print('Interwal‚ estymacji przedzialowej o dokladnosci 95%:')
  print(round(interval,4))
  
  print('Wzgledna precyzja oszacowania:')
  print(round(precision,4))
}

#-------------------ZAD5-------------------

test_fishera<- function(dane1, dane2, p_istotnosci)
{
  cat("TEST FISHERA\n")
  cat("H0: Wariancje wydajnosci pracy sa sobie rowne\n")
  cat("H1: Wariancje wydajnosci pracy sa rozne od siebie\n")
  
  wariancja1 = var(dane1)
  wariancja2 = var(dane2)
  
  n1 = length(dane1)
  n2 = length(dane2)
  
  if(wariancja1 > wariancja2)
  {
    Statystyka = wariancja1 / wariancja2
    Kwantyl = qf(1 - p_istotnosci, n1 - 1, n2 - 1)
  }
  else
  {
    Statystyka = wariancja2 / wariancja1
    Kwantyl = qf(1 - p_istotnosci, n2 - 1, n1 - 1)
  }
  
  cat("Statystyka testowa F = ", Statystyka, "\n")
  cat("Obszar krytyczny K_0 = (", Kwantyl, ", +oo)\n")
  
  if (Statystyka > Kwantyl)
  {
    cat("Wartosc statystyki zawiera sie w obszarze krytycznym.\n")
    cat("Odrzucamy hipoteze zerowa na rzecz hipotezy alternatywnej.\n")
    cat("Na poziomie istotnosci ", p_istotnosci, "mozna przyjac hipoteze alternatywna.\n")
    
    return (0)
  }
  else
  {
    cat("Wartosc statystyki NIE zawiera sie w obszarze krytycznym.\n")
    cat("Brak podstaw do odrzucenia hipotezy zerowej.\n")
    return (1)
  }
}

test_CochranaCoxa <- function(dane1, dane2, p_istotnosci)
{
  cat("Sprwdzamy czy srednia wydajnosc pracy przy produkcji elementu w starej hali za pomoca testu Cochrana-Coxa")  
  cat("H0: Srednia wydajnosc pracy w obu fabrykach jest rowna.\n")
  cat("H1: Srednia wydajnosc pracy w starej fabryce jest wieksza.\n")
  wariancja1 = var(dane1)
  wariancja2 = var(dane2)
  n1 = length(dane1)
  n2 = length(dane2)
  Statystyka = (mean(dane1) - mean(dane2)) / (sqrt((dane1/n1) + (dane2/n2)))
  Stopnie_swobody = round(((dane1/n1 + dane2/n2)^2) / (((dane1/n1)^2)/(n1+1) + ((dane2/n2)^2)/(n2+1)) - 2)
  Kwantyl = qt(1 - p_istotnosci, Stopnie_swobody)
  cat("Statystyka = ", Statystyka, "\n")
  cat("Obszar krytyczny K = < ", Kwantyl, " , + oo)\n" )
  if( Statystyka < Kwantyl)
  {
    cat("Brak podstaw by odrzucic hipoteze zerowa.\n 
        Na poziomie istotnosci 0,05 nie mozna twierdzic ze wydajnosc
        pracy przy produkcji elementu w starej hali jest wieksza.")
  }
  else
  {
    cat("Odrzucamy hipoteze zerowa na rzeczy hipotezy alternatywnej.\n
        Na poziomie istotnosci 0,05 mozna twierdzic ze wydajnosc
        pracy przy produkcji elementu w starej hali jest wieksza.")
  }
}

test_tStudenta <- function(dane1, dane2, p_istotnosci)
{
  cat("TEST T-STUDENTA\n")
  cat("H0: Srednia wydajnosc pracy w hali starej i nowej jest taka sama\n")
  cat("H1: Srednia wydajnosc pracy w hali starej jest wieksza\n")
  
  wariancja1 = var(dane1)
  wariancja2 = var(dane2)
  n1 = length(dane1)
  n2 = length(dane2)
  Statystyka = (mean(dane1) - mean(dane2)) / sqrt( (1/n1 + 1/n2)*(n1*wariancja1 + n2*wariancja2)/(n1+n2-2) )
  Kwantyl = qt(1 - p_istotnosci, n1 + n2 - 2)
  
  cat("Statystyka = ", Statystyka, "\n")
  cat("Obszar krytyczny K_0 = < ", Kwantyl, " , +oo)\n")
  
  if( Statystyka < Kwantyl)
  {
    cat("Wartosc statystyka NIE miesci sie w obszarze krytycznym.\n")
    cat("Brak podstaw do odrzucenia hipotezy zerowej.\n")
  }
  else
  {
    cat("Wartosc statystyki miesci sie w obszarze krytycznym.\n")
    cat("Odrzucamy hipoteze zerowa na rzecz hipotezy alterantywnej.\n")
  }
}

zad5 <- function (dane1, dane2, p_istotnosci)
{
  wynik_testu = test_fishera(dane1,dane2,p_istotnosci)
  if(wynik_testu == 1)
  {
    test_tStudenta(dane1,dane2,p_istotnosci)
  }
  else
    {
    test_CochranaCoxa(dane1,dane2,p_istotnosci)
  }
}