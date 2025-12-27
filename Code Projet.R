### Projet ###
library(readxl)
library(tseries)
library(rugarch)
library(fGarch)
library(tseries)
library(forecast)
library(crypto2)
library(dplyr)
library(xts)
library(quantmod)
library(FinTS)
library(rugarch)

# récupération des données dans Excel 
data_WTI <- read_excel("/Users/Coraline/Documents/Maîtrise/M1/Hiver 2025/ECN 6578/Projet/Indicateurs financier_Petrole_WTI_2010-2025_final.xlsx", sheet ="Data 1",skip = 2)

data_WTI <- data_WTI[-nrow(data_WTI), ]
WTI_prix <- data_WTI$`Weekly Cushing, OK WTI Spot Price FOB  (Dollars per Barrel)`

## Test pour la stationnarité ##
adf.test(WTI_prix, alternative = "stationary")

#Test de box-cox
b<-boxcox(WTI_prix ~ 1)
lambda <- b$x[which.max(b$y)]

#transformation des prix en rendement #
WTI_return <- diff(WTI_prix)/ head(WTI_prix, -1)
head(WTI_return)
adf.test(WTI_return,, alternative = "stationary")

###############
P0 <- head(WTI_prix,1)

reconstructed_prices <- numeric(length(WTI_return) + 1)
reconstructed_prices[1] <- P0  

# Application des rendements pour reconstruire les prix
for (i in 1:length(WTI_return)) {
  reconstructed_prices[i + 1] <- reconstructed_prices[i] * (1 + WTI_return[i])
}

################

## Test pour la normalité ##
# test de Jarque-Berra #
jarque.bera.test(WTI_return)

# test de Shapiro-Wilk #
shapiro.test(WTI_return)

# QQplot #
qqnorm(WTI_return)
qqline(WTI_return, col="red")

## Analyse ACF et PACF ##
par(mfrow=c(2,1))
acf(WTI_return, main="ACF des rendements des prix du WTI")
pacf(WTI_return, main="ACF des rendements des prix du WTI")
dev.off()

## modèle ARMA ##
# On retire les 9 dernières semaines pour choisir notre modèle et 
# faire nos prévisions
WTI_return_tronquer<-head(WTI_return,-9)

fit_arma = auto.arima(WTI_return_tronquer,max.p = 5, max.q = 8)
summary(fit_arma)

# Tests de différents ARMA # 
# ajustement 1 (ARMA(1,2))
ajustement1 <- arima(WTI_return_tronquer,
                     order=c(1,0,2),
                     seasonal=list(order=c(0,0,0),periode =9))

BIC(ajustement1)

# ajustement 2 (ARMA(2,1))
ajustement2 <- arima(WTI_return_tronquer,
                     order=c(2,0,1),
                     seasonal=list(order=c(0,0,0),periode =9))

BIC(ajustement2)

# Comparaison des modèles ARMA(1,2) et ARMA(2,2)
par(mfrow=c(2,1))
acf(residuals(ajustement1), main="Graphique ACF ARMA(1,2)")
acf(residuals(fit_arma), main="Graphique ACF ARMA(2,2)")
dev.off()

## Choix du modèle ARMA(2,2)
Box.test(residuals(fit_arma), 
         type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

# Récupération du R^2 manuellement
R2 <- 1 - var(residuals(fit_arma)) / var(WTI_return_tronquer)

# Prévison à l'aide du modèle ARMA
forecast_arma <- forecast(fit_arma, h = 9)
predicted_returns <- as.numeric(forecast_arma$mean)

full_returns <- c(WTI_return_tronquer, predicted_returns)

reconstructed_prices <- numeric(length(full_returns) + 1)
reconstructed_prices[1] <- P0  

# reconstruction des prix 
for (i in 1:length(full_returns)) {
  reconstructed_prices[i + 1] <- reconstructed_prices[i] * (1 + full_returns[i])
}

print(tail(reconstructed_prices,10))

##################

## modèle ARMA-GARCH ##

# Test pour ARCH 
model_Arch_1<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), 
                         mean.model = list(armaOrder = c(2, 2), include.mean = TRUE), 
                         distribution.model = "norm",fixed.pars = list(mu=0))
fit_Arch_1 = ugarchfit(data =WTI_return_tronquer , spec = model_Arch_1, method="BFGS")
show(fit_Arch_1)

#GARCH(1,1)
model_Garch_11<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                           mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), 
                           distribution.model = "norm",fixed.pars = list(mu=0))
fit_Garch_11 = ugarchfit(data =WTI_return_tronquer , spec = model_Garch_11, method="BFGS")
show(fit_Garch_11)

#GARCH(2,1)
model_Garch_21<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), 
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                           distribution.model = "norm",fixed.pars = list(mu=0))
fit_Garch_21 = ugarchfit(data =WTI_return_tronquer , spec = model_Garch_21, method="BFGS")
show(fit_Garch_21)

#GARCH(2,2)
model_Garch_22<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), 
                           mean.model = list(armaOrder = c(2, 2), include.mean = TRUE), 
                           distribution.model = "norm",fixed.pars = list(mu=0))
fit_Garch_22 = ugarchfit(data =WTI_return_tronquer , spec = model_Garch_22, method="BFGS")
show(fit_Garch_22)

# Vérification du modèle choisit
Box.test(residuals(fit_Garch_11, standardize = TRUE), 
         type = "Ljung-Box", lag = 10)


Box.test(residuals(fit_Garch_22, standardize = TRUE), 
         type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

show(fit_Garch_11)
show(fit_Garch_22)

forecast_Garch_11 <- ugarchforecast(fit_Garch_11, n.ahead = 10)
rendements_prevus <- as.numeric(fitted(forecast_Garch_11))
full_returns_garch <- c(WTI_return_tronquer,rendements_prevus)
prix_prevus <- P0 * cumprod(1 + full_returns_garch)
reconstructed_prices_garch[1] <- P0  


# reconstruction des prix 
for (i in 1:length(full_returns_garch)) {
  reconstructed_prices_garch[i + 1] <- reconstructed_prices_garch[i] * (1 + full_returns_garch[i])
}

print(tail(reconstructed_prices_garch,10))

# choix du modèle ARMA-GARCH
garch_model_10 <- garchFit(~ arma(2,2) + garch(1,0), data = data_WTI_tronque, trace = FALSE)
show(garch_model_10)
garch_model_01 <- garchFit(~ arma(2,2) + garch(1,1), data = data_WTI_tronque, trace = FALSE)
show(garch_model_01)
garch_model_11 <- garchFit(~ arma(2,2) + garch(1,1), data = data_WTI_tronque, trace = FALSE)
show(garch_model_11)
# Récupération du R^2
R2 <- 1 - var(residuals(garch_model_11)) / var(data_WTI_tronque)

# Prévision à l'aide du modèle ARMA-GARGH
forecast_garch <- predict(fit_Garch_11, h = 9)
predicted_returns_garch <- as.numeric(forecast_garch$meanForecast)

full_returns_garch <- c(data_WTI_tronque,predicted_returns_garch)
reconstructed_prices_garch <- numeric(length(predicted_returns_garch) + 1)
reconstructed_prices_garch[1] <- P0  
# reconstruction des prix 
for (i in 1:length(full_returns_garch)) {
  reconstructed_prices_garch[i + 1] <- reconstructed_prices_garch[i] * (1 + full_returns_garch[i])
}

print(tail(reconstructed_prices_garch,10))
