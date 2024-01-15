


#PARTIE 1 : ANALYSE DESCRIPTIVE

#1 
install.packages('quantmod')
library(quantmod)
date_debut <- as.Date("2020-01-01")
date_fin <- as.Date("2023-01-01")
gold <- getSymbols("GC=F", auto.assign = FALSE, from = date_debut, to = date_fin, src = "yahoo", header=TRUE)


#2
plot(gold$`GC=F.Close`, ylim = c(1400,2100), xlab = 'Date', ylab = 'prix de clôture de lor (USD/once troy)', main = 'Prix de clôture de lor en fonction du temps, en USD/once troy')
sd(gold$`GC=F.Close`)
summary(gold$`GC=F.Close`)# Il ne semnle y avoir de tendance sur le prix de cloture de l'or entre 2020 et 2023. On a une moyenne a 1790USD/once troy, avec une ecart type de 101 USD/once troy. 
#Nous ne pouvons pas non plus remarquer de saisonalité.
#Le maximun du prix observé sur cette période est de 2051 USD/once troy, tandis que le minimum est de 1477.

library(tseries)
adf_test1 <- adf.test(gold$`GC=F.Close`)
print(adf_test1) #Notre série n'est pas stationnaire puisque p-value > alpha, on ne peut pas rejeter l'hypothese nulle et conclure que nous avons une serie stationnaire. 
# Il nous faudra de.

#PARTIE 2 : MODELISATION 
#1onc différiencer la série pour obtenir une serie stationnair

  #A
y_diff <- diff(gold$`GC=F.Close`, difference=1)
adf_test2 <- adf.test(na.omit(y_diff))
print(adf_test2) #notre série est stationnaire, nous pouvons tracer les ACF et pACF pour determiner le modele a utiliser.
acf.results1 <-acf(na.omit(y_diff),lag.max = 9) #L'ACF n'est pas interpretable, il va falloir effectuer des modifications a notre série pour l'analyser
pacf.results1 <-pacf(na.omit(y_diff), lag.max = 9) #les résultats semble indiquer un terme autorégressif d'ordre 1, mais il va falloir effectuer d'autres analyses pour confirmer cela 
  
  #B
install.packages('tseries')
install.packages('forecast')
library(forecast)
library(tseries)
meilleur_modele <- auto.arima(y_test)
summary(meilleur_modele) #Le modele estimé renvoie un modele d'autoregression 0, de differenciation 1, et de moyenne mobile 0. De part les résultats précédents, je pense qu'il faut ajouter un ordre d'autorégression a ce modele, bien que les coefficients du modele indiquent que le modele semble bien capturer la strucutre temporelle.

  #C
#Premiere etape : on differencie une fois pour avoir une serie stationnaire
y_test_diff <- diff(y_test,differences  = 1)
plot(y_test_diff)
adf_test2 <-adf.test(na.omit(y_test_diff))
print(adf_test2) #la serie est stationnaire
acf.results <-acf(na.omit(y_test_diff),lag.max = 8) #6
pacf.results <-pacf(na.omit(y_test_diff), lag.max = 8) #6
#Deuxieme etape : on recherche le meilleur modele a partir de la serie differenciee
meilleur_modele <- auto.arima(y_test_diff)
summary(meilleur_modele)
y_test_modele <- arima(y_test_diff,order = c(0,0,0))
serie_residuelle <- y_test_modele$residuals
plot(serie_residuelle) #la  courbe semble stationnaire
#Troisieme etape : on verifie la stationnarité avec adf.test
adf_test3 <- adf.test(na.omit(serie_residuelle))
print(adf_test3)
#Augmented Dickey-Fuller Test
#data:  serie_residuelle
#Dickey-Fuller = -9.6038, Lag order = 8, p-value = 0.01
#alternative hypothesis: stationary
# On rejete l'hypothese donc la serie est stationnaire

# Test d'indépendance des résidus
box_test <- Box.test(serie_residuelle, lag = 8, type = "Ljung-Box")
print(box_test) #Ho est rejeté donc il y a une auto-corrélation
#Box-Ljung test
#data:  serie_residuelle
#X-squared = 17.099, df = 8, p-value = 0.02909
acf(na.omit(serie_residuelle),lag.max = 8)
pacf(na.omit(serie_residuelle), lag.max = 8)

#2
y_pred <- gold[694:756,4]
# Faire des prédictions avec le modèle ARIMA
predictions <- forecast(y_test_modele, h = 63)
# Afficher les prédictions
plot(predictions)
