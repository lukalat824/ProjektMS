##########Wczytanie###########
source("Zadania.R")
##########Odczyt danych###########
dane1 <- read.table("dane1.txt", quote="\"", comment.char="")
dane2 <- read.table("dane2.txt", quote="\"", comment.char="")

##########Tworzenie wektorów z odczytanych danych##########
stara <- as.vector(dane1[[1]], mode = "double")
nowa <- as.vector(dane2[[1]], mode = "double")

##########Rysowanie diagramów##########
#hist(stara, main = "Histogram wydajności pracy w starej hali",xlab = "Wydajność")
#hist(nowa, main = "Histogram wydajności pracw nowej hali",xlab = "Wydajność")

##########Wywołanie funkcji##########
zad1(stara, nowa)
zad2(stara, nowa)
zad3(stara)
zad4(nowa)
zad5(stara,nowa,0.05)