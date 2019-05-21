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