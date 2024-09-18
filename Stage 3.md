---
title: "Hackbio 3"
author: "Astrid Liliana Vargas Sanchez"
date: "2024-09-09"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Read data
library(readr)

data <- read.csv("E:/Documentos/Maestria Lili/Proyectos/Hackbio/Stage 3/Número de casos notificados.csv")

library(dplyr) 
cholera_data <- data %>% filter(data$Year >= 2010)
```


```{r}
# Load packages
library(ggplot2)
library(sf)   
```

```{r}

library(rnaturalearth)
library(rnaturalearthdata)

# Load the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

```


```{r}
names(cholera_data)
# Rename column in cholera_data
cholera_data <- cholera_data %>%
  rename(country = Countries..territories.and.areas)
```


```{r}
# Join cholera data with the countries' shapefile
cholera_map <- world %>%
  left_join(cholera_data, by = c("name" = "country"))
names(cholera_map)
```

```{r}
# Create the map
ggplot(data = cholera_map) +
  geom_sf(aes(fill = Number.of.reported.cases.of.cholera)) +
  scale_fill_gradient(low = "lightgoldenrod1", high = "#FF6A6A", na.value = "white") +
  labs(title = "Distribution of Cholera Reported Cases between 2010 and 2016",
       fill = "Cases") +
  theme(panel.background = element_rect(fill = "lightskyblue1"), plot.title  = element_text(face = "bold",
                                                                                            size = 16))
```

```{r}


```

