library("lmtest") # test Durbin-Watson.
library("robustbase")

#Ne pas oublier d'importer les fichiers de données
wd <- getwd()

GRAPH_1 <- read.delim2(paste(wd, "GRAPH_1.csv",  sep = "/"), header = FALSE)
TFP <- read.delim2(paste(wd, "TFP.csv",  sep = "/"))
GDP <- read.delim2(paste(wd, "GDP.csv",  sep = "/"))
EMP <- read.delim2(paste(wd, "EMP.csv",  sep = "/"))
HTW <- read.delim2(paste(wd, "HTW.csv",  sep = "/"))

# 1ère colonne : Etats-Unis ; 2ème : Europe
plot(
  GRAPH_1$V1,
  type = "l",
  col = "black",
  lwd = 2,
  main = "Taux de Chômage: Europe vs Etats-Unis (1983:1-2019:1)"
)
lines(GRAPH_1$V2,
      type = "l",
      col = "red",
      lwd = 2)
legend(
  "bottomleft",
  col = c("black", "red"),
  c("E-U", "Europe"),
  lty = 1,
  border = NULL,
  bg = NULL
)

# Graphe 1: Données trimestrielles de chômage
year = 1950:2014
par(mfrow = c(2, 2))
matplot(
  year,
  TFP,
  type = 'o',
  pch = 1,
  col = "black",
  main = "Productivité des Facteurs"
)
lines(
  year,
  apply(TFP, 1, median),
  col = "red",
  type = "l",
  lwd = 3
)
lines(year,
      apply(TFP, 1, mean),
      col = "blue",
      type = "l",
      lwd = 3)
matplot(
  year,
  log(GDP),
  type = 'o',
  pch = 1,
  col = "black",
  main = "PIB Réel"
)
lines(
  year,
  apply(log(GDP), 1, median),
  col = "red",
  type = "l",
  lwd = 3
)
lines(
  year,
  apply(log(GDP), 1, mean),
  col = "blue",
  type = "l",
  lwd = 3
)
matplot(
  year,
  log(HTW),
  type = 'o',
  pch = 1,
  col = "black",
  main = "Heures Emploi"
)
lines(
  year,
  apply(log(HTW), 1, median),
  col = "red",
  type = "l",
  lwd = 3
)
lines(
  year,
  apply(log(HTW), 1, mean),
  col = "blue",
  type = "l",
  lwd = 3
)
matplot(
  year,
  log(EMP * 10 ^ 6),
  type = 'o',
  pch = 1,
  col = "black",
  main = "Population Active - Emploi"
)
lines(
  year,
  apply(log(EMP * 10 ^ 6), 1, median),
  col = "red",
  type = "l",
  lwd = 3
)
lines(
  year,
  apply(log(EMP * 10 ^ 6), 1, mean),
  col = "blue",
  type = "l",
  lwd = 3
)

# Statistiques descriptives : données par pays + médiane et moyenne
VECTORMATRIX = matrix(rep(0, 48), nrow = 12)
VECTORMATRIX2 = matrix(rep(0, 60), nrow = 12)
VECTORMATRIX3 = VECTORMATRIX
DW = VECTORMATRIX

# Vecteurs vides pour stocker les résultats de régression
dummy = seq(1, 65)
dummy[dummy < 59] = 0
dummy[dummy >= 59] = 1
# Variable muette pour capter l'effet crise de 2008

for (j in 1:12) {
  #fit=lm(log(EMP[,j])~log(GDP[,j]))
  fit = lmrob(log(EMP[, j]) ~ log(GDP[, j]))
  
  # Régression univariée
  store = dwtest(log(EMP[, j]) ~ log(GDP[, j]))
  DW[j, 1] = store[["statistic"]]
  DW[j, 2] = store[["p.value"]]
  summary(fit)
  residuals(fit)
  VECTORMATRIX[j, ] = summary(fit)$coefficients[2, ]
  fit = lm(log(EMP[, j]) ~ log(GDP[, j]) + log(HTW[, j]) + log(TFP[, j]))
  
  # Régression multivariée
  VECTORMATRIX2[j, ] = c(fit$coefficients, summary(fit)[["r.squared"]])
  fit = lm(log(EMP[, j]) ~ log(GDP[, j]) + log(HTW[, j]) + log(TFP[, j]) +
             dummy)
  
  # Régression multivariée + variable muette
  VECTORMATRIX3[j, ] = summary(fit)$coefficients[5, ]
  store = dwtest(log(EMP[, j]) ~ log(GDP[, j]) + log(HTW[, j]) + log(TFP[, j]))
  DW[j, 3] = store[["statistic"]]
  DW[j, 4] = store[["p.value"]]
  end
}

mean(resid(fit))
bptest(fit) # Breusch-Pagan test

plot(density(resid(fit)))
qqnorm(resid(fit)) # on vérifie la normalité

g1 = rnorm(65, VECTORMATRIX[2, 1], VECTORMATRIX[2, 2])
g2 = rnorm(65, VECTORMATRIX[6, 1], VECTORMATRIX[6, 2])
t.test(g1, g2)
# Test de Student du coefficient Okun.