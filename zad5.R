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
    return (0)
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
  test_fishera(dane1,dane2,p_istotnosci)
  test_CochranaCoxa(dane1,dane2,p_istotnosci)
  test_tStudenta(dane1,dane2,p_istotnosci)
}