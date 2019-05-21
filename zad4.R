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