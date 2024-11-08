# Cargar librerías necesarias
library(readr)
library(arules)

# Leer el archivo CSV
data <- read_csv('C:\\Users\\NOrellana\\Documents\\U\\Maestria\\data mining\\proyecto No1\\departamento y causas.csv')

# Convertir columnas relevantes a factores
data$`Grupos de edad` <- as.factor(data$`Grupos de edad`)
data$Sexo <- as.factor(data$Sexo)

# Remover filas donde Sexo es "Total sexo" o "Ignorado"
data_filtrado <- subset(data, !(Sexo %in% c("Total sexo", "Ignorado")))

# Convertir las columnas numéricas en factores
data_filtrado$`Primera consulta` <- as.factor(gsub(",", "", data_filtrado$`Primera consulta`))
data_filtrado$Reconsulta <- as.factor(gsub(",", "", data_filtrado$Reconsulta))
data_filtrado$Emergencia <- as.factor(gsub(",", "", data_filtrado$Emergencia))
data_filtrado$`Primera consulta y emergencia` <- as.factor(gsub(",", "", data_filtrado$`Primera consulta y emergencia`))
data_filtrado$`Reconsulta y emergencia` <- as.factor(gsub(",", "", data_filtrado$`Reconsulta y emergencia`))


# Aplicar el algoritmo Apriori
reglas <- apriori(data_filtrado, parameter = list(supp = 0.1, conf = 0.5))

# Filtrar reglas que no tengan elementos vacíos en el antecedente y el consecuente
reglas_no_vacias <- subset(reglas, size(lhs(reglas)) > 0 & size(rhs(reglas)) > 0)

# Inspeccionar las primeras reglas generadas
inspect(reglas[1:100])
# Convertir a formato transaccional
transacciones <- as(data_filtrado, "transactions")
frecuentes <- arules::fim4r(method = 'fpgrowth', transactions = transacciones, support = 0.1, confidence = 0.5)
inspect(frecuentes)

```{r}
library(dplyr)
library(readr)

data_kmeans <- read_csv('C:\\Users\\NOrellana\\Documents\\U\\Maestria\\data mining\\proyecto No1\\departamento y causas.csv')

# Convertir columnas con números en formato texto a valores numéricos
data_kmeans <- data_kmeans %>%
  mutate(across(c(Total, `Primera consulta`, Reconsulta, Emergencia, 
                  `Primera consulta y emergencia`, `Reconsulta y emergencia`), 
                ~ as.numeric(gsub(",", "", .))))
data_kmeans <- na.omit(data_kmeans)
# Seleccionar solo las columnas numéricas relevantes para el clustering
data_numeric <- data_kmeans %>% 
  select(Total, `Primera consulta`, Reconsulta, Emergencia, 
         `Primera consulta y emergencia`, `Reconsulta y emergencia`)

# Escalar los datos para el clustering (normalización)
data_scaled <- scale(data_numeric)

# Aplicar K-means con 4 clusters
set.seed(32)  # Para reproducibilidad
kmeans_result <- kmeans(data_scaled, centers = 5)
# Agregar los resultados de los clusters al conjunto de datos original
data_kmeans$Cluster <- as.factor(kmeans_result$cluster)

# Mostrar los resultados de cada cluster
print(aggregate(data_numeric, by = list(Cluster = data_kmeans$Cluster), mean))

# Si quieres ver la asignación de cluster para cada observación
print(data_kmeans)

#Graficas
# Cargar librerías necesarias
library(ggplot2)

# Cargar y preprocesar los datos (usar el código anterior para cargar y procesar 'data')

# Aplicar PCA para reducir a 2 dimensiones
pca <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
data_pca <- as.data.frame(pca$x[, 1:2])  # Solo tomar las dos primeras componentes principales
data_pca$Cluster <- as.factor(data_kmeans$Cluster)  # Agregar los clusters como factor

# Graficar los clusters
ggplot(data_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Clustering K-means (4 clusters) en 2D (PCA)",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()
