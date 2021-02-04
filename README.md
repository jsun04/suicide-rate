# suicide-rate
This is a undergraduate level class project about suicide rate mostly done in RStudio.
---
title: "Proposal of S.T.A.T. Project"
author: "Dalia Avila, Alan Phan, Jiayin Sun, Brian Tran, and Eric Wu"
date: "April 26, 2019"
output:
  word_document: default
  html_document: default
subtitle: Suicide Rate from 1985 to 2016 by Country
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**Assumption**

**MLR**
First, we will use Multiple linear regression (MLR) for examining the linear correlations between two or more independent variables(GDP,G.I.,Silent, Boomeres, X, Z) and a dependent variable (suicide) for the dataset. suicide rate as the response and four predictors compare between male and female suicide rate from 1987-2016.

**Normality**
We will test for the normality that we are assume variables that we used in the analysis are normally distributed. We will test normality by examing mean differences and predicting analyses.
Skewness and Kurtosis:Skewness should be within the range ±2.  Kurtosis values should be within range of ±7
Shapiro-Wilk’s W test: Wilk’s test should not be significant to meet the assumption of normality.
Kolmogorov-Smirnov test: This test should not be significant to meet the assumption of normality. 

**independence**
T Tests, ANOVA tests, to getting results from your sample that reflect the population. 
1.The observations between groups should be independent, which basically means the groups are made up of different people. 
2.The observations within each group must be independent. 
If we violate the independence assumption, we run the risk that all results will be wrong.
Avoid Violating the Assumption is to make sure data is independent. 

**pattern**
We will testing the relationship between the independent variable(s) and the dependent variable.
Scatter plot:linearity assumption
Q-Q plot:errors between observed and predicted values are normally distributed.
No multicollinearity in the data. The multicollinearity occurs when the independent variables are highly correlated with each other.

**equality**
We will exam the equal variances across different groups or samples, mean differences on an independent grouping variable (e.g., t-tests and analyses of variance – ANOVAs/MANOVAs).  

## **Abstract** 

Suicide is one of the leading causes of death worldwide. According to the World Health Organization, around 800,000 people commit suicide every year, which is one person every 40 seconds (https://www.who.int/mental_health/prevention/suicide/suicideprevent/en/). /*how to cite this source?*/ 

A recent report from The American Foundation for Suicide Prevention stated that suicide is the 10th leading cause of death in the United States alone (https://afsp.org/about-suicide/suicide-statistics/) /* how to cite this source */. Despite these alarming statistics, the epidemiology of such behavior is generally limited within the academic sphere. Although lacking in academic discussions, the study of suicide plays an important component in addressing policymakers on the need for suicide prevention and mental health advocacy. New research and results can help improve contemporary comprehensions of suicide within scientific literature and aid in efforts to decrease the significant loss of life by self-immolation. 
We hope to contribute to this advancement surrounding suicide awareness by investigating possible factors that could influence one's decision to end their lives. In particular, we will analyze suicide rates for multiple countries worldwide and demographic factors such as gender, socio-economic status, age and generation. Other factors, such as the year the data was collected, the country's gross domestic product per year, the country's gross domestic product per capita and country's population are also considered. We hope to draw conclusions on potential factors that attribute to higher suicide rates and provide clarity within the academic-policy domain on prospective mental health reform and suicide intervention. 



## **Data** 


Our analysis will come from an open-source data set, called "Suicide Rates Overview 1985 to 2016". The data set consists of twelve variables: country, year, sex, age group, count of suicides, population, suicide rate, country-year composite key, HDI for year, gdp_for_year, gdp_per_capita and generation (based on age grouping average) for multiple countries.  (https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016)

Another dataset joined for analysis includes the Prosperity Index from The Legatum Institute which promotes policies that create pathways from poverty to prosperity. They describe these conditions of prosperity as the combination of nine pillars: Economic Quality, Business Environment, Governance, Personal Freedom, Social Capital, Safety and Security, Education, Health, and the Natural Environment. The Legatum Institute uses data for 149 countries over eleven years, then track the journeys made by countries towards or away from prosperity. By combining the Prosperity Index as a predictor for suicide rate analysis, it is hoped to capture some nuiances of inherent differences between the country's structure and status.

## **Statistical Packages/Programming** 

We will perform our anaylsis using R, an open source programming language and free software enviroment most commonly used for statistical computing and graphics. We will use statistical packages in R, such as `tidyverse`,`dipylr` and `ggplot2` for exploratory data anaylsis and visualization of the data set. 


## **Questions to Consider** 

Our data analysis will attempt to answer the following questions:

1. Is gender (male/female) a significant indicator of a country's suicide rate ?

2. Is the generation (G.I., Silent, Boomers, X, Millennials, Z) a person born in a significant indicator of a c country's suicide rate?

3. Is a country's gross domestic product (GDP) a significant indicator of a country's suicide rate?

4. Is a country's prosperity index (based on the pillars of: economic quality, business environment, governance, personal freedom, social capital, safety and security, education, health, and natural environment) a significant indicator of a country's suicide rate?

5. Are interaction effects (ie is suicide rate influenced by factor combinations of gender-GDP, gender-generation, gender-population, generation-GDP,generation-prosperity index,etc. ) significant indicators of a country's suicide rates? If so, which ones?



## **Model Specification**

The model we will explore is multiple linear regression model using suicide rate as the response and four predictors. The predictors we will use will be the gross domestic product (GDP), generation (G.I., Silent, Boomers, X, Millennials, Z), prosperity index (based on the pillars of: economic quality, business environment, governance, personal freedom, social capital, safety and security, education, health, and natural environment) and compare between males and females.
Multilinear Regression model:


 $$Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_3 +\beta_4 X_4 + \epsilon \\\epsilon \sim N(0, \sigma^2)$$


## **Purpose**

The use of the data will be to determine if there is a correlation between suicide rate and factors of their respective countries such as generation, gross domestic product, and the country's prosperity index. The data will also be subset between sex in order to study the differences between males and females. 



## **Procedure**

Our evaluation of the dataset will join other datasets. Namely, we will join the data to another that includes the prosperity index in order to use the prosperity index as a predictor for suicide rate by country. Further, we will join the data to other datasets in order to add coordinate information and continents to the suicides dataset for mapping visualizations. 

The data analysis procedure will be done by:


 1. Exploratory Analysis
    - Visualization of Countries in World Map of Suicide rates by GDP.
    - Visualization of Countries in World Map Suicide rates by Prosperity Index.
    - 
    
 2. Model fitting
    - Simple linear regression
    - Multilinear/Polynomial regression
    - Logistic Regression
 
 3. Reviewing model assumptions to verify validity of model
    - Normality 
    - Constant variance
    - 
 
 4. Plotting suicide rates vs. dependent variables used
    - QQ Plot
    - Residuals vs. Fitted values
 
 5. Evaluating models
 
    - Misclassification Error Rate
    - Sensitivity
    - Specificity

**Libraries and packages going to be used in this project.**
```{r}
library(funModeling)
library(tidyverse)
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(readxl)
library(gapminder)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos) # Error in st_as_sfc.SpatialPolygons(sp::geometry(x), ...) : package rgeos required for finding out which hole belongs to which exterior ring
```
      
**data preparation and loading files**
```{r}
# loading data from suicide file
suicides <- read.csv("/Users/sunemilyjiayin/Creative Cloud Files/master.csv",header=TRUE)
glimpse(suicides)  # outputting a glimpse of the data

# loading data from prosperity index file
prosperity <- read_excel("/Users/sunemilyjiayin/Creative Cloud Files/PI.xlsx")
glimpse(prosperity)  # outputting a glimpse of the data

# loading data from country_coords file
coords<-read_excel("/Users/sunemilyjiayin/Creative Cloud Files/country_coord.xlsx",sheet=1)
coords
glimpse(coords)

# take prosperity dataset and group_by rank
prosperity.grouped <- prosperity %>% group_by(country) %>% 
  summarise(mean.rank.PI = mean(rank_PI, na.rm = TRUE)) #Get the mean rank per country
suicides <- suicides %>%
  mutate(country = as.character(country))

# checking for mismatches in both tables 
glimpse(suicides)

#use anti_join function for mismatches in both tables 
suicides.anti <- suicides %>%
  anti_join(prosperity.grouped, by = c("country" = "country")) %>%
  count(country, sort = TRUE)
#filter prosperity by the year
suicides.pi <- suicides %>%
  left_join(prosperity.grouped, by = c("country" = "country")) %>%
  arrange(mean.rank.PI)
glimpse(suicides.pi)

```
      

#scatterplot year vs. suicides rates suit by gender
**As we can se the highest suicides year occurs from 1990-2000 and 2012. More male than female people commited suicide.**
```{r}
#basic analysis
#plot of the suicide rate of each year
plot1<-ggplot(data = suicides.pi, mapping = aes(x = year,y = suicides.100k.pop)) +
geom_point(aes(color=sex))+
  ggtitle("suicides rates by year")
plot1
```

#scatterplot age vs. suicides rates suit by age
**75+ group have the highest suicide rates. 5-14 years old group have lowest suicide rates.**
```{r}
#plot of the suicide rate of age group
plot2<-ggplot(data=suicides.pi,mapping = aes(x = age,y = suicides.100k.pop)) +
geom_point(aes(color=sex))
plot2
#histogram suicides rates vs. count suit by age
plot3<-ggplot(data=suicides.pi)+
geom_histogram(aes(x=suicides.100k.pop,fill=age),binwidth = 15)
plot3
#ploy graph suicides rates vs. count suit by age
plot6<-ggplot(data=suicides.pi)+
geom_freqpoly(aes(x=suicides.100k.pop,fill=age,col=age),binwidth=15)
plot6
#boxplot suicides rates by age
plot9<-ggplot(data = suicides.pi, mapping = aes(x =age, y = suicides.100k.pop)) +
geom_boxplot(aes(col=age))+
  coord_flip()+
  ggtitle("boxplot suicides rates by age")
plot9
```

#histogram suicides rates vs. count suit by sex
**More male committed suicide than female and the 75+ group have the highest suicide rates.**
```{r}
plot4<-ggplot(data=suicides.pi)+
geom_histogram(aes(x=suicides.100k.pop,fill=sex),binwidth=15)
plot4
#ploy graph suicides rates vs. count suit by sex
plot5<-ggplot(data=suicides.pi)+
geom_freqpoly(aes(x=suicides.100k.pop,fill=sex,col=sex),binwidth=15)
plot5
#boxplot suicides rates by sex
plot8<-ggplot(data = suicides.pi, mapping = aes(x = sex, y = suicides.100k.pop)) +
geom_boxplot(aes(col=sex))+
  ggtitle("suicides rate by gender")
plot8
```


#scatterplot of the gdp vs. suicide rate by age
**Higher gdp country have lower suicide rates. Lower gdp have higher suicide rates.**
```{r}
plot7<-ggplot(data=suicides.pi,aes(x=gdp_for_year....,y=suicides.100k.pop))+
  geom_point(aes(col=age))+
  ggtitle("scatterplot of the gdp vs. suicide rate by age")
plot7
#boxplot gdp vs suicides rates by age.
plot14<-ggplot(data=suicides.pi,aes(x=gdp_for_year....,y=suicides.100k.pop))+
  geom_boxplot(aes(col=age))
plot14

#scatterplot gdp vs suicides rate by sex
#plot of the suicide rate of each year
plot13<-ggplot(data = suicides.pi, mapping = aes(x = gdp_for_year....,y = suicides.100k.pop)) +
geom_point(aes(color=sex))
plot13
```

#prosperity index each year averaged
**There is no difference and no obvious trend between suicide rate by year and suicide rate sorted by mean.rank.PI.**
```{r}
plot10<-ggplot(data = suicides.pi, mapping = aes(x = year, y = suicides.100k.pop)) +
geom_point(aes(col=mean.rank.PI))+
  ggtitle("suicides rate by year")
plot10
```


#world map
```{r}
# loading data from country_coords file
coords<-read_excel("/Users/sunemilyjiayin/Creative Cloud Files/country_coord.xlsx",sheet=1)
coords
glimpse(coords)
# loading data from suicide file
suicides <- read.csv("/Users/sunemilyjiayin/Creative Cloud Files/master.csv",header=TRUE)
glimpse(suicides)  # outputting a glimpse of the data

length(suicides$country)

suicides_modified <- suicides %>% 
                     left_join(coords, by = c("country" = "name"))
library(gapminder)
names(gapminder)

 suicides_modified <- suicides_modified %>% 
                  left_join(gapminder,by = c("country" = "country"))%>%
                  filter(!is.na(HDI.for.year),!is.na(continent))
 suicides_modified
 #View(suicides_modified)
library(maps)
 map()
points(coords$longitude, coords$latitude, pch=".", col="red", cex = 5)
# loading data from prosperity index file
prosperity <- read_excel("/Users/sunemilyjiayin/Creative Cloud Files/PI.xlsx")
glimpse(prosperity)  # outputting a glimpse of the data
suicides <- suicides %>%
  mutate(country = as.character(country))

# checking for mismatches in both tables 
#glimpse(suicides)

suicides.anti <- suicides %>%
  anti_join(prosperity, by = c("country" = "country")) %>%
  count(country, sort = TRUE)
#View(suicides.p)
suicides.anti
suicides.pi <- suicides %>%
  inner_join(prosperity, by = c("country" = "country")) 

#View(suicides.pi)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos) # Error in st_as_sfc.SpatialPolygons(sp::geometry(x), ...) : package rgeos required for finding out which hole belongs to which exterior ring

length(suicides$country)
suicides_modified <- suicides %>% left_join(coords, 
                                            by = c("country" = "name"))
names(gapminder)
suicides_modified <- suicides_modified %>% left_join(gapminder,
                                by = c("country" = "country"))
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
class(world)
world
#left_join(world,suicides,by="brk_name")
ggplot(data =world) +
  geom_sf() +
  geom_point(data = suicides_modified, aes(x = suicides_modified$longitude, y =suicides_modified$ latitude,fill=suicides.100k.pop), alpha = 0.5)
coords
#graph the data to see if there is any difference between countries and suicide rates.
plot11<-ggplot(data =world) +
  geom_sf() +
geom_point(data=suicides_modified,mapping=aes(x = suicides_modified$longitude, y =suicides_modified$ latitude,fill=year.x))
plot11
#suicides_modified
```

#animated suicides rate by world map
```{r}
# install.packages('devtools')
#devtools::install_github('thomasp85/gganimate')
#install.packages("gifski")
#devtools::update_github()
library(gapminder)
library(gganimate)
#suicides_modified
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
class(world)
animateworldmap<-ggplot(data =world) +
  geom_sf()+
  geom_point(data = suicides_modified, aes(x = suicides_modified$longitude, y =suicides_modified$ latitude,fill=suicides.100k.pop))+
  scale_color_identity(country_colors)+
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}',  y = 'suicide rates') +
  transition_time(year.x) +
  ease_aes('linear')
animateworldmap
#ggplot2::ggsave(animate,animation=last_animation(),path=NULL)

```

#animated suicides rate by year by continent
**According to our animated graph, Europe have the highest rate because there are more countries in our dataset are in Europe. Africa have the lowest because we only have two countries in Africa which cannot represent the whole Africa continent. Asia is the second highest.**
```{r}
# install.packages('devtools')
#devtools::install_github('thomasp85/gganimate')
#install.packages("gifski")
#devtools::update_github()
library(gapminder)
library(gganimate)
#suicides_modified
animate<-ggplot(suicides_modified, aes(gdp_for_year...., suicides.100k.pop, colour = pop)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_color_identity(country_colors)+
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP', y = 'suicide rates') +
  transition_time(year.x) +
  ease_aes('linear')
animate
```

**According to this two animated graph we could see the suicide rate in each continent. However, the first graph suppose to show different color in different country. When suicide rate is higher the color gets darker. We have not figure out how to achieve that part. So, we use points to represent the countries. However, the data came from several tables and ggplot function use specific method to join them together, so the problem clearly shows on the graph that these points came from different area and did not show why they choose to let the points jump around in the first graph. The data that we have also could not represent the whole population because in the dataset we only have two countries in Africa which limited our analysis about the suicide rate on Africa continent. Also, the table finished in the middle of 2016. The animated graph only up to 2015 which also limits our analysis about the whole year.**

**Exploratory analysis conclusion: 1. More male committed suicide than female. 2. 75+ age group have the highest suicide rates. 5-14 years old age group have the lowest suicide rates. 3. The year does not have a huge impact on dataset. 4. mean.rank. PI also does not affect suicide rates. 5.Higher gdp country have lower suicide rates. Lower gdp country have higher suicide rates.**


**At an initial glance at the data, we can see a few relationships. Males show a higher max suicide rate when compared to females. Another is that there seems to be some negative correlation between G.D.P. and suicide rates. However H.D.I. doesn't seem to indicate a relationship. In addition, we can see that there appears to be a difference among the rates of suicide and their generation or respective age group.**


## **Data Cleaning**
The next chunk of code we will proceed with cleaning the data a by creating a new variable that groups the data into separate  groups by a range of years. This will be used to help visualize the data better when we look at the data more in-depth.

```{r,  collapse=T, echo = FALSE, warning=F}

# loading data from prosperity index file
prosperity <- read_excel("D:/STAT167 Intro Data Science/Datasets/PI.xlsx")
glimpse(prosperity)  # outputting a glimpse of the data
suicides <- suicides %>%
  mutate(country = as.character(?..country))

# loading data from country coordinates file
coord <- read_excel("D:/STAT167 INtro Data Science/Datasets/country_coord.xlsx")

# grouping data in the Prosperity Index file
prosperity.grouped <- prosperity %>% group_by(country) %>% #take prosperity and group_by rank
  summarise(mean.rank.PI = mean(rank_PI, na.rm = TRUE)) #Get the mean rank per country

# creating a character of the variable '?..country' to join with other data files
suicides <- suicides %>%
  mutate(country = as.character(?..country)) %>% 
  filter(year != 2016)  # removing year 2016 due to incomplete annual data


###  "suicides.pi" DATAFRAME - Ready for modeling and analysis
suicides.pi <- suicides %>%
  left_join(prosperity.grouped, by = c("country" = "country")) %>%
  arrange(mean.rank.PI) %>%
  select(-?..country)

# using package 'gapminder' for continent data to use for subsetting our mapping visualizations
library(gapminder)
continents <- gapminder %>%
  select(country, continent)

###  "suicides.pi" DATAFRAME joining with continents variable
suicides.pi <- suicides.pi %>%
  left_join(continents, by = c("country" = "country")) 

### "suicides.pi.coord" DATAFRAME - Ready for mapping visualizations
suicides.pi.coord <- suicides.pi %>%
  left_join( coord, by = c("country" = "name")) %>%
  select(country, everything())
```


## **Exploratory analysis**
In this section, we looked into our dataset to look for rough trends. Through plotting and graphing, we can see how some variables have changed over time. With this, we can start making inferences about some of the variables and their relationships with one another.
```{r}
suicides_modified <- suicides %>% left_join(coord, 
                                            by = c("?..country" = "name")) 
#allows access to countries through coordinates

suicides_modified <- suicides_modified %>% left_join(gapminder,
                                                      by = c("?..country" = "country"))
#groups each country by continent
 
suicides_modified_cl <- suicides_modified %>% filter(!is.na(continent))
#clean up data by removing na values from continent

suicides_modified_cl %>% 
  group_by(continent, 
           sex, 
           suicides_no, 
           ?..country) %>%
  summarize(total_suicides = sum(suicides_no)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = continent, 
                         y = total_suicides,
                         fill = sex),
           stat = "identity",
           position = "dodge"
           ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#displays total suicides of each continent, with extra information about gender

suicides_modified_cl %>%
  group_by(?..country, continent) %>%
  summarize(
    African_country = sum(!is.na(?..country))) %>%
  filter(continent == "Africa")
#displays which African countries are in the dataset
```
**We wanted a quick look at how distributed suicides were around the world, and to see if anything was weird. The Americas, Asia, and Europe having a high suicide rate was expected, but Africa having such a low total suicide was definitely intriguing. We then found out that there are only two African countries in our dataset: South Africa and Mauritius. This is definitely a problem in our dataset that may affect our results.**

```{r}
suicides_modified_cl %>%
  group_by(continent, year.x) %>%
  summarize(suicides_per100k = sum(suicides.100k.pop)) %>%
  ggplot() + 
  geom_line(aes(x = year.x, y = suicides_per100k , color = continent))
#line graph to display suicide rates for each continent
```
**Europe has the highest suicide rate by far for each continent. Because of this, we wanted to look more into Europe's suicide rate by itself, to check if there were any outliers that inflates our data.**

```{r}
suicides_modified_cl %>%
  group_by(?..country, 
            continent, 
            year.x) %>%
  summarize(suicides_per100k = sum(suicides.100k.pop)) %>%
  filter(continent == "Europe") %>%
  ggplot() + 
  geom_line(aes(x = year.x, 
                y = suicides_per100k, 
                color = ?..country))
#many line graphs, each representing a country's suicide rate
```
**We noticed that Hungary had the highest suicide rates of any European country, despite its large dip in recent years. We were curious as to why this was the case, and wanted to see if one of our main predictors had any relationship to the line's trend.**

```{r}
suicides_modified_cl %>%
  group_by(year.x) %>%
  summarize(total_gdp = sum(gdp_per_capita....)) %>%
  ggplot() +
  geom_line(aes(x = year.x, 
                y = total_gdp), 
            color = "red")
#line graph to graph Hungary's gdp over the years
```

**Just by intuition, we felt that gdp would probably have a huge factor in determining a country's suicide rates. By graphing Hungary's gdp over the years, we noticed a large, constant increase in the country's gdp over the years. We can ignore the dip in 2016 because our dataset ends in the middle of 2016. By looking at Hungary's dip in suicide rates and the increase in gdp, we can see a relationship between the two. They are most likely inversely correlated. gdp may be an important predictor for suicide rates. In conclusion, we can assume that as suicide rates increase, gdp decreases, and vice versa.**
 
##**Model 1 : Simple Linear Models**

Our first model is a simple linear model using Generation, Sex, and the Average Prosperity Index Rank as predictors for suicide rate.

```{r, echo=F, warning=F}
################ LM1 full
model.full.lm <- lm(suicides_no ~ generation + sex + mean.rank.PI , data = suicides.pi.coord)

### CHECKING ASSUMPTIONS LM1 FULL
# normality on LM1
par(mfrow=c(1, 2))
resid.full.lm <-model.full.lm$residuals
qqnorm(resid.full.lm,  main = "Q-Q Plot of Full Linear Model 1")
qqline(resid.full.lm)

# variance on LM1
plot(fitted(model.full.lm), resid.full.lm,  xlab = "fitted", main = "Residuals of Linear Model 1")
abline(0,0)
```
Figure: Plots of assumptions for Linear Model 1: Generation, Sex, Mean Prosperity Index Rank

**The Normal QQ-Plot of our first linear model shows that normality is not satisfied. The second plot of the Residuals against predicted also shows our variance assumption is not satisfied. Therefore, this model cannot be further analyzed. **

```{r}
################ LM2 GPD
model.gdp.lm <- lm(suicides_no ~ gdp_per_capita.... , data = suicides.pi.coord)

### CHECKING ASSUMPTIONS LM1 FULL
par(mfrow=c(1, 2))
# normality on LM1
resid.gdp.lm <- model.gdp.lm$residuals
qqnorm(resid.gdp.lm,  main = "Q-Q Plot of Linear Model2: GDP")
qqline(resid.gdp.lm)

# variance on LM1
plot(fitted(model.gdp.lm), resid.gdp.lm,  xlab = "fitted", main = "Residuals of  Linear Model2: GDP")
abline(0,0)
```
Figure: Plots of assumptions for Linear Model 2: GDP

**The Normal QQ-Plot of our second linear model shows that normality is not satisfied. The second plot of the Residuals against predicted also shows our variance assumption is not satisfied. Therefore, this model cannot be further analyzed. **

##**Model 2 : Polynomial LInear Regression**

**Modeling Assumptions to Polynomial Linear Regression**

Since the simple linear models failed to meet the assumptions, a polynomial model was attempted to see if the assumptions could be met. The polynomial model that was attempted was suicide rates ~ population + gdp + mean PI + age + sex.

```{r, echo = FALSE}
#Check how well a polynomial regression is for our variables chosen
#glimpse(suicides.pi)
d <- 10 #Set the max exponent to be 10
MSE.Population <- rep(0, d) #create a table to keep track of the MSE for first variable
MSE.GDP<- rep(0, d) #create a table to keep track of the MSE for the second variable
MSE.Mean.PI <- rep(0, d)
for(d_1 in 1:d){
  glm.fit <- glm(data = suicides.pi, formula = suicides.100k.pop ~ poly(population, d_1)) #GLM for population at a certain degree
  MSE.Population[d_1] <- cv.glm(suicides.pi, glm.fit, K = 10)$delta[1] #MSE for a K-cross validation at K = 10
}
for(d_2 in 1:d){
  glm.fit <- glm(data = suicides.pi, formula = suicides.100k.pop ~ poly(gdp_for_year...., d_2)) #GLM for gdp_per_year at a certain degree
  MSE.GDP[d_2] <- cv.glm(suicides.pi, glm.fit, K = 10)$delta[1] #MSE for a K-cross validation at K = 10
}
for(d_3 in 1:d){
  glm.fit <- glm(data = suicides.pi, formula = suicides.100k.pop ~ poly(mean.rank.PI, d_3)) #GLM for gdp_per_capita at a certain degree
  MSE.Mean.PI[d_3] <- cv.glm(suicides.pi, glm.fit, K = 10)$delta[1]
}

tibble(MSE.Population, MSE.GDP, MSE.Mean.PI) #Turn the MSE into a table for readability
d1 <- which.min(MSE.Population) #find the lowest polynomial for Population
d2 <- which.min(MSE.GDP) #Find the lowest polynomial for GDP
d3 <- which.min(MSE.Mean.PI) #Find the lowest polynomial for Mean.PI

Population.Degree <- d1
GDP.Degree <- d2
mean.PI.Degree <- d3

tibble(Population.Degree, GDP.Degree, mean.PI.Degree) #Show off the polynomial degree in a readable table
```


First it is necessary to determine the best Degree polynomial to use for the quantitative data population, gdp, and mean PI. The table above shows the mean squared error for the polynomial function of the quantitative data up to a degree of 10. The next table indicates the best degree polynomial to use for the polynomial function. As indicated by the table, population should have a degree of 4, GDP should have a degree of 6 and mean PI should have a degree of 10.

```{r}
glm.poly = glm(data = suicides.pi, formula = suicides.100k.pop ~ poly(population, d1) + poly(gdp_for_year...., d2) +
           sex + age + poly(mean.rank.PI, d3))
par(mfrow = c(2,2))
plot(glm.poly)
```

Before we can analyse the model specified earlier, the assumptions must be met. The plot above indicates that both the error normality and the error independence assumptions are both violated. Therefore, the model cannot be used to predict the suicide rates.

Note: We had tried to do a BoxCox transformation on the model in order to remedy the violation, unfortunately since some of the predicted values have a negative value, it is not possible. A Yeo-Johnson transformation was also attempted but that failed as well.

##**Model 3 : KNN Model**
Since both the simple linear model and the polynomial regression model failed to meet the assumptions, a nonparametric model such as K-Nearest Neighbor will be used. The predictors used in this model will be a mixture of Year, GDP, and MeanPI. The model will try and predict whether or not there will be a high suicide rate based on the given predictors. A high suicide rate was determined by taking the mean of all the suicide rates in the data set. Population is excluded in this model because the suicide rates already include population in the model and therefore will be redundant. Population was included in the polynomial regression model because the addition of transforming the population into a polynomial may yield some important information. For the KNN model, K = 3, K = 5, and K = 7 were chosen as potential models.

```{r, echo = FALSE}
suicide_update <- suicides.pi %>%
  mutate(High.Low = ifelse(suicides.100k.pop > 12.8161, "High", "Low"))

#k = 3
suicide_update$High.Low.knn.pred1 <- 
  knn(train = dplyr::select(suicide_update, year, gdp_for_year....),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, gdp_for_year....),
      k = 3)

suicide_update$High.Low.knn.pred2 <-
  knn(train = dplyr::select(suicide_update, year, mean.rank.PI),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, mean.rank.PI),
      k = 3)

suicide_update$High.Low.knn.pred3 <-
  knn(train = dplyr::select(suicide_update, mean.rank.PI, gdp_for_year....),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, mean.rank.PI, gdp_for_year....),
      k = 3)

suicide_update$High.Low.knn.pred4 <-
  knn(train = dplyr::select(suicide_update, year, gdp_for_year...., mean.rank.PI),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, gdp_for_year...., mean.rank.PI),
      k = 3)

#K = 5
suicide_update$High.Low.knn.pred5 <- 
  knn(train = dplyr::select(suicide_update, year, gdp_for_year....),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, gdp_for_year....),
      k = 5)

suicide_update$High.Low.knn.pred6 <-
  knn(train = dplyr::select(suicide_update, year, mean.rank.PI),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, mean.rank.PI),
      k = 5)

suicide_update$High.Low.knn.pred7 <-
  knn(train = dplyr::select(suicide_update, mean.rank.PI, gdp_for_year....),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, mean.rank.PI, gdp_for_year....),
      k = 5)

suicide_update$High.Low.knn.pred8 <-
  knn(train = dplyr::select(suicide_update, year, gdp_for_year...., mean.rank.PI),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, gdp_for_year...., mean.rank.PI),
      k = 5)

#K = 7
suicide_update$High.Low.knn.pred9 <- 
  knn(train = dplyr::select(suicide_update, year, gdp_for_year....),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, gdp_for_year....),
      k = 7)

suicide_update$High.Low.knn.pred10 <-
  knn(train = dplyr::select(suicide_update, year, mean.rank.PI),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, mean.rank.PI),
      k = 7)

suicide_update$High.Low.knn.pred11 <-
  knn(train = dplyr::select(suicide_update, mean.rank.PI, gdp_for_year....),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, mean.rank.PI, gdp_for_year....),
      k = 7)

suicide_update$High.Low.knn.pred12 <-
  knn(train = dplyr::select(suicide_update, year, gdp_for_year...., mean.rank.PI),
      cl = suicide_update$High.Low,
      test = dplyr::select(suicide_update, year, gdp_for_year...., mean.rank.PI),
      k = 7)
      

class.error1 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred1)   
class.error2 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred2)
class.error3 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred3)
class.error4 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred4)

class.error5 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred5)   
class.error6 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred6)
class.error7 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred7)
class.error8 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred8)

class.error9  <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred9)   
class.error10 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred10)
class.error11 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred11)
class.error12 <- mean(suicide_update$High.Low != suicide_update$High.Low.knn.pred12)


K.values <- matrix(c("K = 3", "K = 5", "K = 7"))
Model <- matrix(c("Year, GDP", "Year, MeanPI", "MeanPI, GDP", "Year, MeanPI, GDP"))

Error.matrix <- matrix(c(class.error1, class.error2, class.error3, class.error4, class.error5, 
                         class.error6, class.error7,class.error8, class.error9, 
                         class.error10, class.error11, class.error12),
             nrow = 4,
             ncol = 3)

colnames(Error.matrix) <- K.values
rownames(Error.matrix) <- Model

Error.matrix
cm <- as.matrix(table(actual = suicide_update$High.Low, predicted = suicide_update$High.Low.knn.pred7))
cm

specificity <- cm[1,1] / (cm[1,1] + cm[1,2])
sensitivity <- cm[2,2] / (cm[2,1] + cm[2,2])

tibble(sensitivity, specificity)
```

From the table created, the best model with the lowest error rate is MeanPi and GDP at K = 5. Looking at the specificity and sensitivity rates, the model provide a decently high sensitivity but a low specificity. However, a high sensitivity is preferred as it indicates that the model is good at predicted low suicide rates meaning it can help prevent suicide rates.

##**Model #4 : Logistic Regression**

```{r, echo = FALSE}
# classification 
# use the ifelse() function to classify whether the country 
# is above or below the mean suicide rate 
# use the mutate() function to create a new variable for the classification
# use the format:
# data <- data %>%
#         mutate(descriptive_statistics)
suicide_update <- suicides.pi %>%
                  mutate(rate.class = ifelse(suicides.100k.pop > 12.8161, 1,0))
# use the glimpse() function to check
glimpse(suicide_update)

# logistic regression model
# use the glm() function to create a logistic regression model
# use the format:
# model <- glm(response ~ variable, family = binomial(), data)
names(suicide_update)
log.rate <- glm(rate.class ~ . -country , family = binomial(),data = suicide_update)
# use the summary() function to get the output 
summary(log.rate)

# create a logistic regression with categorical predictors
# use the glm() function to create the logistic regression
# use the format:
model <- glm(rate.class ~ year + sex +age, family = binomial(), data = suicide_update)
model
summary(model)

# misclassification rate 
# use the predict() function to find the predictions
class.predict <- predict(model, type = "response")
class.predict

# create a new variable to specify median rate
# use the mutate() function
# use the format:
# data <- data %>% 
#         mutate(descriptive_statistics)
suicide_update <- suicide_update %>%
                  mutate(above.below  = ifelse(rate.class == 1, "Yes","No"))
names(suicide_update)
class.predict.type <- ifelse(class.predict > 0.5, "Yes", "No") %>% as.factor()
class.predict.type

mean(suicide_update$above.below != class.predict.type)

# create a logistic regression with categorical predictors
# use the glm() function to create the logistic regression
# use the format:
model <- glm(rate.class ~ year + sex +age+gdp_for_year.... + sex*age, family = binomial(), data = suicide_update)
model
summary(model)
```


##**Model #5 : Linear Discriminant Analysis (LDA)**

```{r, echo=FALSE}
# using the mean of all the country's suicide rate to create a binary variable 
# rate.class variable is defined as 1 for high or 0 for low suicide rate compared to mean
suicide_update <- suicides.pi %>%
                  mutate(rate.class = ifelse(suicides.100k.pop > 12.8161, 1, 0), na.rm = TRUE)

library(MASS)
#--------------------------  LDA    MODEL 
# fitting LDA model where rate.class is the response and the predictors are:
# mean prosperity rank and GDP per capita 
lda.fit = lda(rate.class ~ mean.rank.PI + gdp_per_capita...., data = suicide_update)
lda.fit # output LDA model

```
The LDA model output shows the probabilities already in the data to be 67.04% for suicide rate to be below the world average and 33.96% for the suicide rate to be above the world average suicide rate.

Secondly, the group means for prosperity index ranking and GDP for each group, 0 for below average suicide rate and 1 for above average suicide rate.

Lastly, the coefficients indicate the influence each predictor has on the suicide rate. For each predictor the greater the coefficient is the more it influences the suicide rate by country. However, in this output we see that there is a negative influence for both.

```{r, warning=FALSE, echo=FALSE}
# creating predicted values for the LDA model
lda.fit.pred <- predict(lda.fit)
names(lda.fit.pred)  # output names for the predicted values 

# misclassification error rate of LDA model
mean(lda.fit.pred$class != suicide_update$rate.class)

```
**LDA Model Evaluation**

The misclassification error rate of the LDA model is 0.3146782 or 31% which is high. To validate our Linear Discriminant Analysis we proceed with our assumptions. 

**LDA Model Assumptions**
```{r, echo=FALSE, warning=FALSE}
# Assumptions for LDA model
# boxplot of GDP per capita
gdp_bp <- ggplot(data = suicides.pi)+ geom_boxplot(aes(gdp_per_capita...., suicides.100k.pop),
                                                   color = "purple")+ ggtitle("Boxplot of GDP")
# boxplot of the Average Prosperity Index Ranking
mean.RPI <- ggplot(data = suicides.pi)+ geom_boxplot(aes(mean.rank.PI, suicides.100k.pop), 
                                                     color = "blue")+ ggtitle("Boxplot of PI Rank")
# Plotting the two boxplots side-by-side to compare the variances of the two predictors
grid.arrange(gdp_bp, mean.RPI, nrow = 1)
```

Figure: Boxplot of GDP and Boxplot of Prosperity Index (PI) Rank.


```{r, echo=FALSE}
# Assumption of Normality for predictors used for LDA model
par(mfrow=c(1, 2))
# normality plot of GDP per capita variable
qqnorm(suicides.pi$gdp_per_capita....,  main = "Q-Q Plot of GDP per capita")
qqline(suicides.pi$gdp_per_capita....)

# normality plot of Prosperity Index Rank variable
qqnorm(suicides.pi$mean.rank.PI,  main = "Q-Q Plot of Prosperity Index Rank")
qqline(suicides.pi$mean.rank.PI)
```

The assumptions for the model are normality of the variables and homogeneity of variances. The side-by-side boxplots of GDP and Prosperity Index Rankings against the suicide rates show that there is similarity in the variance among the two predictors used in the LDA model. Then we proceed to look at the plots for the normality of each predictor variable. The QQ Plots show that the normality assumption for the GDP per capita is not satisfied since most of our points do not fall close to the QQ line. The second QQ plot shows the Prosperity Index Rank and it also appears to fail the normality assumption. Although some of our points are close to the line, the points at the tails of our data a far away to be able to validate our normality assumption. As such, the Linear Discriminant Analysis does not provide a good approximation of suicides rates based on the GDP per capita and average Prosperity Index Rank.


## **Conclusion**

> There is no trend over time in suicide rates within countries (correlation = - 0.05919378)

> Male suicide rates are higher than female males use more lethal methods

> Country suicide rates were lower as GDP higher 

> Best model with minimum misclassification error rate IS logistic regression model with interaction


## **Acknowledgments**

Background: Brian Tran & Dalia Avila

Data cleaning/joining: Alan Phan & Dalia Avila

Exploratory Analysis: Eric Wu, & Jiayin Sun

Visualizations: Eric Wu, Jiayin Sun, Dalia Avila, Brian Tran

Polynomial Regression: Alan Phan

Logistic Regression: Brian Tran

Linear Discriminant Analysis: Dalia Avila

Conclusions: Alan Phan, Brian Tran, Dalia Avila


## **References**

1. Rusty. "Suicide Rates Overview 1985 to 2016." Kaggle, 1 Dec. 2018, www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016.

2. "Downloads." Legatum Prosperity Index 2018, Legatum Institute, 19 Dec. 2018, www.prosperity.com/about/resources.

3. "Countries.csv  |  Dataset Publishing Language  |  Google Developers." Google, Google, 20 Jan. 2012, www.developers.google.com/public-data/docs/canonical/countries_csv.

4. Schumacher, Helene. "Future - Why More Men than Women Die by Suicide." BBC, BBC, 18 Mar. 2019, www.bbc.com/future/story/20190313-why-more-men-kill-themselves-than-women.
