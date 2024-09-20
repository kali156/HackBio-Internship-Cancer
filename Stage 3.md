# Global Cholera Situation

Cholera is a gastrointestinal disease caused by the bacterium Vibrio cholerae, primarily transmitted through contaminated water and food. Its incubation period is short, ranging from two hours to five days. Most affected individuals are asymptomatic or present mild symptoms. However, less than 20% of those infected develop severe acute diarrhea, which can lead to rapid dehydration and, without appropriate treatment, may result in death.

Despite being treatable with rehydration solutions, cholera remains a significant threat, especially in vulnerable communities with limited access to healthcare services. Over the past two centuries, the world has experienced seven cholera pandemics, with the seventh, which began in 1961, still ongoing.

While the disease is primarily concentrated in regions of Africa and the Eastern Mediterranean, cases have also been reported in the Americas, Europe, and Oceania. Figure 1 illustrates the countries that have reported the highest number of cholera cases or outbreaks.

```{r}
options(scipen=3)
# Read data
library(readr)

data <- read.csv

```


```{r}
# Load packages
library(ggplot2)
library(sf)   
library(rnaturalearth)
library(rnaturalearthdata)
```
## Global distribution of reported cholera cases

While the disease is primarily concentrated in regions of Africa and the Eastern Mediterranean, cases have also been reported in the Americas, Europe, and Oceania. Figure 1 illustrates the countries that have reported the highest number of cholera cases or outbreaks.


```{r}
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
# Create the map. "Global distribution of reported cholera cases from 2010 to 2016"
graph1 <- ggplot(data = cholera_map) +
  geom_sf(aes(fill = Number.of.reported.cases.of.cholera)) +
  scale_fill_gradient(low = "lightgoldenrod1", high = "#FF6A6A", na.value = "white") +
  labs(title = "Global distribution of reported cholera cases from 2010 to 2016",
       fill = "Cases") +
  theme(panel.background = element_rect(fill = "lightskyblue1"), plot.title  = element_text(face = "bold",
                                                                                            size = 10))
ggsave("graph1.png", plot = graph1, width = 6, height = 4, dpi = 300)
```

**Figure 1. Incidence of cholera cases reported to WHO.**
*Note: countries in blank did not report any cholera cases.*

## Global Comparison of Cholera Cases by Year

Figure 2 presents the number of reported cases from 1949 to 2016. A significant increase is noted in 1991, corresponding to the epidemic that originated in Peru that year. Cholera epidemics typically last from 5 to 10 years, which explains the subsequent decrease in cases (1). In October 2010, the epidemic re-emerged in Haiti, the poorest country in Latin America, which had been devastated by an earthquake in January of that same year (2).

```{r}
# Convert years to factor
data$Year <- as.factor(data$Year)

# Create the graph
graph2 <- ggplot(data, aes(y = factor(Year), x = Number.of.reported.cases.of.cholera)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ylab("Years") +
  xlab("Number of cases") +
  ggtitle("Global comparison of cholera cases by year")+
  theme_classic()+
  theme(plot.title  = element_text(face = "bold",
                                   size = 10))+
  theme(axis.text.y = element_text(size = 4))
ggsave("graph2.png", plot = graph2, width = 6, height = 4, dpi = 300)
```

**Figure 2. Global comparison of cholera cases by year**

## Global comparison of cholera deaths by year

The lack of hospital care and treatments rendered cholera a lethal disease, which explains the high number of deaths in the initial reported years. Figure 3 shows that the majority of deaths occurred during the first three years of the 1991 epidemic, followed by a significant decline in the subsequent years. Between 2010 and 2011, a notable increase in mortality was recorded, associated with the epidemic that affected the region during those years (1,2).

```{r}
# Read data
library(readr)

data2 <- read.csv

# Rename column in data2
data2 <- data2 %>%
  rename(death = Number.of.reported.deaths.from.cholera)

# Convert number of deaths to numeric
data2$death <- as.numeric(data2$death)

# Remove NA data
colSums(is.na(data2))
df_complete <- data2[complete.cases(data2),]
any(is.na(df_complete))
```

```{r}
# Convert years to factor
data2$Year <- as.factor(data2$Year)

# Create the graph
graph3 <- ggplot(data2, aes(y = factor(Year), x = death)) +
  geom_bar(stat = "identity", fill = "#90EE90") +
  ylab("Years") +
  xlab("Number of deaths") +
  ggtitle("Global comparison of cholera deaths by year")+
  theme_classic()+
  theme(plot.title  = element_text(face = "bold",
                                   size = 10))+
  theme(axis.text.y = element_text(size = 4))
ggsave("graph3.png", plot = graph3, width = 6, height = 4, dpi = 300)
```

**Figure 3. Global comparison of cholera deaths by year**

## Reported cases by country and year

The countries that reported the highest number of cholera cases during the epidemic that began in 1991 were Peru, Nigeria, and Brazil. In contrast, for the epidemic that started in 2010, the countries with the most reported cases were Haiti, Somalia, and Zimbabwe. Figure 4.

```{r}
# Reported cases by country and year
# Rename column in data
data <- data %>%
  rename(cases = Number.of.reported.cases.of.cholera)

```


```{r}
#Split the dataframe into several dataframes by country
dataframes <- split(data, data$country)

# Create graphs for each DataFrame

for (country in names(dataframes)) {
  df_country <- dataframes[[country]]
  
    p <- ggplot(dataframes[[country]], aes(y = factor(Year), x = cases)) +
      geom_bar(stat = "identity", fill = "#FFA07A") +
      labs(title = paste("Reported cases in", country), 
           x = "Cases", y = "Year")+
      theme_classic()+
      theme(plot.title  = element_text(face = "bold",
                                   size = 10))+
      theme(axis.text.y = element_text(size = 4))
  
    # Save graphs
    ggsave(filename = paste0("Grafico_", country, ".png"), plot = p)
}

```
**Figure 4.Reported cases by country and year**

Find the complete charts at the following link: 

https://github.com/Liliana223/HackBio-Internship-Cancer/tree/main/Charts%20of%20reported%20cases%20by%20country%20-%20I

https://github.com/Liliana223/HackBio-Internship-Cancer/tree/main/Charts%20of%20reported%20cases%20by%20country%20-%20II

## Reported deaths by country and year

The countries that reported the highest number of cholera deaths during the epidemic that began in 1991 were Nigeria, the Democratic Republic of the Congo, and Peru. In contrast, for the epidemic that started in 2010, the countries with the most reported deaths were Haiti, Nigeria, and Somalia. Figure 5.

```{r}
# Rename column in data
data2 <- data2 %>%
  rename(country = Countries..territories.and.areas)
```

```{r}
#Split the dataframe into several dataframes by country
dataframes2 <- split(data2, data2$country)

# Create graphs for each DataFrame

for (country in names(dataframes2)) {
  df_country <- dataframes2[[country]]
  
    p <- ggplot(dataframes2[[country]], aes(y = factor(Year), x = death)) +
      geom_bar(stat = "identity", fill = "#CD5C5C") +
      labs(title = paste("Reported deaths in", country), 
           x = "Cases", y = "Year")+
      theme_classic()+
      theme(plot.title  = element_text(face = "bold",
                                   size = 10))+
      theme(axis.text.y = element_text(size = 4))
  
    # Save graphs
    ggsave(filename = paste0("Grafico_", country, ".png"), plot = p)
}
```
*Figure 5. Reported deaths by country and year*

Find the complete charts at the following link: 

https://github.com/Liliana223/HackBio-Internship-Cancer/tree/main/Charts%20of%20reported%20deaths%20by%20country%20-%20I

https://github.com/Liliana223/HackBio-Internship-Cancer/tree/main/Charts%20of%20reported%20deaths%20by%20country%20-%20II

## References
1. Lippi D, Gotuzzo E, Caini S. Cholera.Microbiol Spectr. 2016 Aug;4(4). doi: 10.1128/microbiolspec.PoH-0012-2015.PMID: 27726771.
2. Harvez CB, Ávila y. VS. La epidemia de cólera en América Latina: reemergencia y morbimortalidad [Internet]. Scielosp.org. 2013 [Cited on September 20, 2024]. Available at: https://www.scielosp.org/pdf/rpsp/2013.v33n1/40-46



