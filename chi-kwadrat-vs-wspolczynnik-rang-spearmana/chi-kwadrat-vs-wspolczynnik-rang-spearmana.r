# author:tomasz.rzodkiewka@gmail.com

# uruchomienie skryptu
# Ctrl + L
# setwd(Sys.getenv("RZODKIEW_GITHUB_PATH"))
# source(".\\statistics-draft\\chi-kwadrat-vs-wspolczynnik-rang-spearmana\\chi-kwadrat-vs-wspolczynnik-rang-spearmana.r")

# cor.test
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.test.html
# chisq.test
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/chisq.test.html
# tetrachoric
# http://finzi.psych.upenn.edu/R/library/psych/html/tetrachor.html

# start
print(format(Sys.time(),"Start %X"))

# przygotowanie srodowiska
rm(list=ls(all=TRUE))
library(psych) # http://cran.r-project.org/web/packages/psych/index.html

# liczba kombinacji liczb naturalnych a, b, c, d, takich, ze a + b + c + d == constSum
abcdConstSumCount = function(constSum){
  result = 0
  a = 0
  while(a <= constSum) {
    b = 0
    while(b <= constSum - a) {
      c = 0
      while(c <= constSum - a - b) {
	    result = result + 1
        c = c + 1
      }
      b = b + 1
    }
    a = a + 1
  }
  return(result)
}

# wypelnia i zwraca arrayConstSum, taka ze arrayConstSum[i, 1] + arrayConstSum[i, 2] + arrayConstSum[i, 3] + arrayConstSum[i, 4] == constSum
abcdConstSumFill = function(constSum, arrayConstSum){
  i = 1
  a = 0
  while(a <= constSum) {
    b = 0
    while(b <= constSum - a) {
      c = 0
      while(c <= constSum - a - b) {
	    arrayConstSum[i, 1] = a
		arrayConstSum[i, 2] = b
		arrayConstSum[i, 3] = c
		arrayConstSum[i, 4] = constSum - a - b - c
	    i = i + 1
        c = c + 1
      }
      b = b + 1
    }
    a = a + 1
  }
  return(arrayConstSum)
}

# sprawdza, czy prawidlowo (czy sumy sie zgadzaja) wypelniona tabela
abcdConstSumCheck = function(constSum, arrayConstSum){
  rowSums = rowSums(arrayConstSum)
  return (constSum == min(rowSums) & constSum == max(rowSums))
}

#                             czy_lubisz_zwierzeta_0 czy_lubisz_zwierzeta_1
# czy_twoi_rodzice_maja_psa_0                      a                      b
# czy_twoi_rodzice_maja_psa_1                      c                      d

# ankieta
dane_ankieta_resondenci_liczba = 300
dane_ankieta_pytania = c("czy_twoi_rodzice_maja_psa", "czy_lubisz_zwierzeta")

# ankieta - podsumowanie
dane_ankieta_podsumowanie_wiersze_naglowki = c("czy_twoi_rodzice_maja_psa_0", "czy_twoi_rodzice_maja_psa_1")
dane_ankieta_podsumowanie_kolumny_naglowki = c("czy_lubisz_zwierzeta_0", "czy_lubisz_zwierzeta_1")
dane_ankieta_podsumowanie = array(0, c(length(dane_ankieta_podsumowanie_wiersze_naglowki), length(dane_ankieta_podsumowanie_kolumny_naglowki)))
rownames(dane_ankieta_podsumowanie) = dane_ankieta_podsumowanie_wiersze_naglowki
colnames(dane_ankieta_podsumowanie) = dane_ankieta_podsumowanie_kolumny_naglowki

# ankieta - podsumowanie - kombinacje
dane_ankieta_podsumowanie_kombinacje_liczba = abcdConstSumCount(dane_ankieta_resondenci_liczba)
dane_ankieta_podsumowanie_kombinacje = array(0, c(dane_ankieta_podsumowanie_kombinacje_liczba, length(dane_ankieta_pytania) * length(dane_ankieta_pytania)))
dane_ankieta_podsumowanie_kombinacje = abcdConstSumFill(dane_ankieta_resondenci_liczba, dane_ankieta_podsumowanie_kombinacje)
# sprawdzam, czy prawidlowo wypelniona
abcdConstSumCheck(dane_ankieta_resondenci_liczba, dane_ankieta_podsumowanie_kombinacje)

# wprowadzam "wyniki"
dane_ankieta_wyniki = array(0, c(dane_ankieta_resondenci_liczba, length(dane_ankieta_pytania)))
colnames(dane_ankieta_wyniki) = dane_ankieta_pytania

#
for(i in 1:nrow(dane_ankieta_podsumowanie_kombinacje)) {
  for(j in 1:nrow(dane_ankieta_wyniki)) {
    # czy_twoi_rodzice_maja_psa
    dane_ankieta_wyniki[j,1] = j > dane_ankieta_podsumowanie_kombinacje[i,1] + dane_ankieta_podsumowanie_kombinacje[i,2]
    # czy_lubisz_zwierzeta
    if(dane_ankieta_wyniki[j,1] == 0) {
      dane_ankieta_wyniki[j,2] = j > dane_ankieta_podsumowanie_kombinacje[i,1]
    }else if(dane_ankieta_wyniki[j,1] == 1) {
      dane_ankieta_wyniki[j,2] = j > dane_ankieta_podsumowanie_kombinacje[i,1] + dane_ankieta_podsumowanie_kombinacje[i,2] + dane_ankieta_podsumowanie_kombinacje[i,3]
    }
  }

  # nie, bo sa kombinacje z kilkoma (>1) komorkami o liczebnosci 0
  # dane_ankieta_podsumowanie = table(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2])
  dane_ankieta_podsumowanie = dane_ankieta_podsumowanie_kombinacje[i,]
  dane_ankieta_podsumowanie_phi = phi(dane_ankieta_podsumowanie, digits=8)
  dane_ankieta_wyniki_rho = cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="spearman", exact=FALSE)$estimate

  dane_ankieta_podsumowanie_phi = round(dane_ankieta_podsumowanie_phi, 8)
  dane_ankieta_wyniki_rho = round(dane_ankieta_wyniki_rho, 8)

  # jesli jeden jest liczba, a inny jest nieliczba
  if (xor(is.nan(dane_ankieta_podsumowanie_phi), is.na(dane_ankieta_wyniki_rho))) {
    print("phi != rho")
  }
  # jesli oba sa liczba i roznia sie
  if (!is.nan(dane_ankieta_podsumowanie_phi) & !is.na(dane_ankieta_wyniki_rho) & dane_ankieta_podsumowanie_phi != dane_ankieta_wyniki_rho) {
    print("phi != rho")
  }

  # inne  
  # phi(dane_ankieta_podsumowanie, digits=8)  
  # chisq.test(dane_pseudoankieta_podsumowanie, correct=TRUE)
  # tetrachoric(dane_pseudoankieta_podsumowanie, correct=TRUE)
  # cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="spearman", exact=FALSE)
  # cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="pearson")
  # cor.test(dane_ankieta_wyniki[,1], dane_ankieta_wyniki[,2], method="kendall")
}

print(format(Sys.time(),"End %X"))
