# Proyecto de Minería de Datos en Salud Pública

Este proyecto analiza datos de salud pública en Guatemala, aplicando técnicas de minería de datos como **reglas de asociación** (Apriori) y **clustering K-means**. Los datos provienen de un archivo CSV que contiene información sobre consultas y emergencias en hospitales nacionales, categorizadas por género y grupo de edad.

## Requisitos

### Versión de R

Este proyecto fue desarrollado y probado en **R versión 4.x.x**. Se recomienda tener una versión reciente para asegurar la compatibilidad con las librerías necesarias.

### Librerías

Antes de ejecutar el código, asegúrate de instalar las siguientes librerías de R:

```r
install.packages("readr")
install.packages("arules")
install.packages("dplyr")
install.packages("ggplot2")
