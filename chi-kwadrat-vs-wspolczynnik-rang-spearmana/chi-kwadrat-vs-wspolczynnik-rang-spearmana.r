# author:tomasz.rzodkiewka@gmail.com

# uruchomienie skryptu
# Ctrl + L
# setwd(Sys.getenv("RZODKIEW_GITHUB_PATH"))
# source("chi-kwadrat-vs-wspolczynnik-rang-spearmana.r")

# przygotowanie srodowiska
rm(list=ls(all=TRUE))
library(psych) # http://cran.r-project.org/web/packages/psych/index.html

dane_ankieta_resondenci_liczba = 300
dane_ankieta_pytania = c("czy_twoi_rodzice_maja_psa", "czy_lubisz_zwierzeta")
dane_ankieta_wyniki = array(0, c(dane_ankieta_resondenci_liczba, length(dane_ankieta_pytania)))
colnames(dane_ankieta_wyniki) = dane_ankieta_pytania

dane_pseudoankieta_wiersze_naglowki = c("czy_twoi_rodzice_maja_psa_0", "czy_twoi_rodzice_maja_psa_1")
dane_pseudoankieta_kolumny_naglowki = c("czy_lubisz_zwierzeta_0", "czy_lubisz_zwierzeta_1")
dane_pseudoankieta_podsumowanie = array(0, c(length(dane_pseudoankieta_wiersze_naglowki), length(dane_pseudoankieta_kolumny_naglowki)))
rownames(dane_pseudoankieta_podsumowanie) = dane_pseudoankieta_wiersze_naglowki
colnames(dane_pseudoankieta_podsumowanie) = dane_pseudoankieta_kolumny_naglowki

#                             czy_lubisz_zwierzeta_0 czy_lubisz_zwierzeta_1
# czy_twoi_rodzice_maja_psa_0                     80                     70
# czy_twoi_rodzice_maja_psa_1                     50                    100

dane_pseudoankieta_podsumowanie[1,1] = 80
dane_pseudoankieta_podsumowanie[1,2] = 70
dane_pseudoankieta_podsumowanie[2,1] = 50
dane_pseudoankieta_podsumowanie[2,2] = 100

for(i in 1:nrow(dane_ankieta_wyniki)) {
  # czy_twoi_rodzice_maja_psa
  dane_ankieta_wyniki[i,1] = i > dane_pseudoankieta_podsumowanie[1,1] + dane_pseudoankieta_podsumowanie[1,2]
  # czy_lubisz_zwierzeta
  if(dane_ankieta_wyniki[i,1] == 0) {
    dane_ankieta_wyniki[i,2] = i > dane_pseudoankieta_podsumowanie[1,1]
  }else if(dane_ankieta_wyniki[i,1] == 1) {
    dane_ankieta_wyniki[i,2] = i > dane_pseudoankieta_podsumowanie[1,1] + dane_pseudoankieta_podsumowanie[1,2] + dane_pseudoankieta_podsumowanie[2,1]
  }
}

dane_ankieta_podsumowanie = table(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2])

chisq.test(dane_pseudoankieta_podsumowanie, correct=TRUE)
chisq.test(dane_ankieta_podsumowanie, correct=TRUE)

cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="pearson")
cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="spearman")
cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="kendall")

# tetrachoric(dane_pseudoankieta_podsumowanie, correct=TRUE)
# phi(dane_pseudoankieta_podsumowanie, digits=8)
