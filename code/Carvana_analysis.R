install.packages("quarto")
install.packages(c("quarto", "rmarkdown"))

library(quarto)
library(tidyverse)
library(magrittr)
install.packages('tinytex')
tinytex::install_tinytex()


library(ggplot2)
# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)

# Supposons que df est votre dataframe
df <- read.csv("C:/Users/rahma/OneDrive/Documents/GL/R/vehicles.csv")
# Afficher les premières lignes des données
head(df)


# Remplacer les valeurs manquantes dans chaque colonne numérique par la valeur médiane de la colonne
#df <- df %>% 
  #mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))

# Supprimer les lignes en double
df <- df %>% distinct(.keep_all=TRUE)
#supprimer les cols qui contiennet que les val nulles
df<-df[,colSums(is.na(df))< nrow(df)]
# Supprimer les lignes avec des valeurs manquantes
df <- na.omit(df)
#supprimer les lignes avec des espaces 
df<- subset(df, !apply(df,1,function(x) all(trimws(x)=="")))

# Afficher les données nettoyées
head(df)

df <- df[, !(names(df) == "size")]



write.csv(df, "C:/Users/rahma/OneDrive/Documents/GL/R/vehicles_cleaned.csv", row.names = FALSE)
names(df)

# Chargement des bibliothèques nécessaires
library(ggplot2)

# Lecture des données
data <- read.csv("C:/Users/rahma/OneDrive/Documents/GL/R/vehicles_cleaned.csv")










#Analyse de la corrélation entre le prix et le kilométrage (odometer) 
library(dplyr)
library(ggplot2)

# Définition du seuil pour filtrer les données
threshold <- 0.95 # Par exemple, vous pouvez choisir le quantile à 95%

# Filtrage des données pour exclure les valeurs aberrantes
filtered_data <- data %>%
  filter(!is.na(odometer)) %>%
  filter(odometer < quantile(odometer, threshold, na.rm = TRUE))

# Calculer la moyenne des prix pour chaque kilométrage
average_prices <- filtered_data %>%
  group_by(odometer) %>%
  summarize(average_price = mean(price, na.rm = TRUE))

# Visualisation du nuage de points avec les données filtrées et la courbe de moyenne des prix
ggplot() +
  geom_point(data = filtered_data, aes(x = odometer, y = price)) +
  geom_smooth(data = average_prices, aes(x = odometer, y = average_price), method = "loess", se = FALSE) +
  labs(title = "Corrélation entre le prix et le kilométrage (Données filtrées)", x = "Kilométrage", y = "Prix")

#### great one ema akhyeb wahed fel great ones xD  ####





# Diagramme en camembert (pie chart) des conditions des véhicules
ggplot(data, aes(x = "", fill = condition)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des conditions des véhicules", fill = "Condition") +
  theme_void()
###### great one #####

# Histogramme des années de fabrication
ggplot(data, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution des années de fabrication des véhicules", x = "Année de fabrication", y = "Nombre d'annonces")
###### great one#######


# Diagramme en barres des types de carburant
ggplot(data, aes(x = fuel)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Répartition des types de carburant", x = "Type de carburant", y = "Nombre d'annonces") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####### great one######


# Diagramme en violon des types de transmission
ggplot(data, aes(x = transmission, y = year)) +
  geom_violin(fill = "skyblue") +
  labs(title = "Répartition des types de transmission", x = "Type de transmission", y = "Année de fabrication")
##### great one  ######

# Diagramme en barres des fréquences des fabricants
freq_fabricants <- table(data$manufacturer)
top_10_fabricants <- sort(freq_fabricants, decreasing = TRUE)[1:10]  # Sélection des 10 premiers fabricants

# Création du graphique en barres
barplot(top_10_fabricants, col = "skyblue", main = "Top 10 des Fabricants par Fréquence",
        xlab = "Fabricant", ylab = "Fréquence", las = 2)

######## great one############

# Diagramme en barres empilées des types de transmission par condition des véhicules
ggplot(data, aes(x = condition, fill = transmission)) +
  geom_bar() +
  labs(title = "Types de Transmission par Condition des Véhicules", x = "Condition", y = "Nombre d'annonces", fill = "Transmission")
######## great one #########




# Charger les données
data <- read.csv("C:/Users/rahma/OneDrive/Documents/GL/R/vehicles_cleaned.csv")

# Sélectionner les caractéristiques (variables prédictives) et la variable cible (prix)
features <- c("year", "odometer", "fuel", "transmission")  
target <- "price"  

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)  
train_index <- sample(1:nrow(data), 0.8 * nrow(data))  
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Entraîner le modèle de régression linéaire
model <- lm(price ~ ., data = train_data[, c(features, target)])

# Prédire les prix sur l'ensemble complet des données
all_predictions <- predict(model, newdata = data)

# Calculer l'erreur quadratique moyenne (RMSE)
rmse <- sqrt(mean((all_predictions - data$price)^2))

# Calculer le coefficient de détermination (R²)
r_squared <- summary(model)$r.squared

# Afficher les résultats
cat("Erreur Quadratique Moyenne (RMSE) :", rmse, "\n")
cat("Coefficient de Détermination (R²) :", r_squared, "\n")

library(ggplot2)
ggplot(data, aes(x = transmission, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Régression linéaire et courbe de prédiction", x = "transmission", y = "Prix")














