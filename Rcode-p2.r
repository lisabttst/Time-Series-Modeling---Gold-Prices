library(quantmod)

library(tseries)

library(forecast)

rm(list=ls())

# 1. Téléchargement des données

# Retrieve the data

GC <- getSymbols("GC=F", auto.assign = FALSE, from = "2020-01-01", to = "2022-12-31", src = "yahoo")

# Rename the columns

colnames(GC) <- gsub("=F", "", colnames(GC))

# Display the price series in a new window (Window 1)

chartSeries(GC$GC.Close, name = "Prix de Clôture de l'Or",
            
            theme = "white", type = "line", TA = NULL)

# Effectuer une analyse descriptive

# Display summary in a new window (Window 2)

summary(GC$GC.Close)
sd(GC$GC.Close)

# 2. Division des données

train_end <- as.Date("2022-09-30")
test_start <- as.Date("2022-10-01")
test_end <- as.Date("2022-12-31")
train_data <- window(GC$GC.Close,, end=train_end)
test_data <- window(GC$GC.Close,, start=test_start, end=test_end)

# 2.1(a) Tracer la fonction d'auto-correlation (ACF) et d'auto-correlation partielle (PACF).
# Extract the price series (no differencing)

price_series <- GC$GC.Close

# Plotting the price series and its ACF and PACF in new windows

par(mfrow=c(3,1))  # Creating a 3-row subplot

# Plot the price series
plot(price_series, main="Gold Price (GC)", col="blue")

# Plot the ACF of the price series
acf(price_series, main="ACF of Gold Price (GC)")

# Plot the PACF of the price series
pacf(price_series, main="PACF of Gold Price (GC)")

#ajouter commentaires

# ADF Test to check for stationarity
adf_test <- adf.test(price_series, alternative="stationary", k=0)
print("ADF Test:")
print(adf_test)  # la série n’est pas stationnaire à l’ordre 0, il faut différencier.

# 2.1(b) Importer le package "tseries" et "forecast". Utiliser une commande pour trouver le meilleur modèle.

# Sélectionner la série temporelle des prix de l'or (GC.Close)

price_series <- GC$GC.Close

# Utiliser auto.arima pour trouver le meilleur modèle ARIMA

best_model <- auto.arima(price_series)

# Afficher le meilleur modèle identifié

print("Best ARIMA Model:")

print(best_model)  # Cela confirme qu’il y a besoin de différencier à l’ordre 1.

# Différencier la série temporelle une fois pour la rendre stationnaire

differenced_series <- diff(price_series, differences = 1)



# 2.1(c) Enlever la tendance et faire des tests d'indépendance et de stationnarité sur la série résiduelle.

# Charger la bibliothèque forecast si ce n'est pas déjà fait

library(forecast)

# Ajuster un modèle ARIMA (0,1,0) pour enlever la tendance

model_010 <- arima(price_series, order=c(0,1,0))

# Obtenir les résidus du modèle

residuals_010 <- residuals(model_010)

# Tests d'indépendance et de stationnarité sur les résidus

# Utilisez adf.test pour le test de Dickey-Fuller

adf_test <- adf.test(residuals_010)

print("ADF Test for (0,1,0) Model:")

print(adf_test)

# Utilisez kpss.test pour le test KPSS

kpss_test <- kpss.test(residuals_010)

print("KPSS Test for (0,1,0) Model:")

print(kpss_test)

#Test d'indépendance

box_test <- Box.test(residuals_010, type = 'Ljung-Box')
print(box_test)
#commentaires 

# Tracer l'histogramme des résidus pour le modèle (0,1,0)
dev.off()
hist(residuals_010, main="Histogram of Residuals (0,1,0)", col="lightblue", freq=FALSE)

#commentaires

# 2.2. En suivant la méthode de "Back-testing", et en calculant plusieurs outils statistiques nécessaire, trouver une approximation du "Goodness of fit".

# Ajuster un modèle ARIMA(0,1,0) aux données d'apprentissage

model_010 <- arima(train_data, order=c(0,1,0))

# Prédire les données de test

forecast_010 <- forecast(model_010, h=length(test_data))

# Simuler des données basées sur la prévision ARIMA(0,1,0) avec de la volatilité

predicted_prices_010 <- rnorm(length(test_data), mean = forecast_010$mean, sd = sd(train_data))

# Calculer MAE et RMSE sur les données de test

MAE_010 <- mean(abs(predicted_prices_010 - test_data))

RMSE_010 <- sqrt(mean((predicted_prices_010 - test_data)^2))

# Afficher MAE et RMSE pour ARIMA(0,1,0)

data.frame(Model = "ARIMA(0,1,0)",
           
           MAE = MAE_010,
           
           RMSE = RMSE_010)

# Échelle MAE et RMSE

data_range <- range(test_data)

data_min <- min(test_data)

data_max <- max(test_data)

scaled_MAE_010 <- MAE_010 / (data_max - data_min)

scaled_RMSE_010 <- RMSE_010 / (data_max - data_min)

# Afficher MAE et RMSE échelés pour ARIMA(0,1,0)

scaled_results <- data.frame(
  
  Model = "ARIMA(0,1,0)",
  
  Scaled_MAE = scaled_MAE_010,
  
  Scaled_RMSE = scaled_RMSE_010
  
)

print("Scaled MAE and RMSE for ARIMA(0,1,0):")

print(scaled_results)


# Create a new plot with smaller figure margins
dev.off()
# Create a new plot with smaller figure margins
options(repr.plot.width = 10, repr.plot.height = 6)  # Adjust plot size as needed


# Set smaller figure margins
par(mar = c(3, 3, 2, 2))  # Use smaller margins (bottom, left, top, right)



# Define the zoomed-in time frame (July 2022 to December 2022)
xlim_zoomed <- as.Date(c("2022-01-01", "2022-12-31"))



# Plot the training data with the specified time frame
plot(index(train_data), train_data, type = "l", col = "green", xlab = "Date", ylab = "Price",
     main = "Gold Price Forecast", ylim = c(1200, 2000), xlim = xlim_zoomed)



# Plot the testing data within the time frame
lines(index(test_data), test_data, type = "l", col = "black")



# Plot the mean forecast for ARIMA(0,1,0) model on the testing data within the time frame
lines(index(test_data), predicted_prices_010, col = "blue")



# Add confidence intervals (assuming you have calculated them)
# You can adjust the multiplier (e.g., 1.96) for the desired confidence level
polygon(c(index(test_data), rev(index(test_data))),
        c(predicted_prices_010 + 1.96 * sd(predicted_prices_010),
          rev(predicted_prices_010 - 1.96 * sd(predicted_prices_010))),
        col = rgb(0, 0, 1, alpha = 0.3), border = NA)



# Add a legend at the bottom (remove 'inset' and 'xpd')
legend("bottom", legend = c("Training Data", "Actual Test Data", "ARIMA(0,1,0) Forecast"),
       col = c("green", "black", "blue"), lty = 1)

# Définir le texte
text_box <- c(paste("MAE_010: ", round(MAE_010,2)),paste("RMSE_010: ", round(RMSE_010,2)),paste("Scaled MAE_010: ", round(scaled_MAE_010,2)), paste("Scaled RMSE_010: ", round(scaled_RMSE_010,2)))

# Coordonnées du texte
x <- as.Date("2022-07-01")
y_start <- 1600  # Vous devrez peut-être ajuster cette valeur

# Ajouter le texte au plot ligne par ligne
text_y <- seq(y_start, length.out = length(text_box), by = -35)  # ajuster le "by" selon l'espacement désiré
for (i in 1:length(text_box)) {
  text(x, text_y[i], text_box[i], cex=0.8, pos=1, offset=1)
}

# Ajouter un rectangle autour du texte
# Calculez d'abord la largeur/hauteur approximative du texte pour dessiner le rectangle
width <- max(strwidth(text_box, cex=0.8, units="user")) + 20
height <- max(strheight(text_box, cex=0.8, units="user")) * length(text_box) + 20
