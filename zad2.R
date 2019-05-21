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