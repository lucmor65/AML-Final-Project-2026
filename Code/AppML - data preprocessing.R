
#### Libraries ####

library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggplot2)


#### Imputation I ####

library(missForest)
library(VIM)

#data_mice

data_final <- dplyr::select(data_efter_slet,CONSTRUCTION_YEAR,BUILDINGS,FLOORS,WETROOMS,RESIDENTIAL_AREA,BASEMENT_AREA,CONSERVATORY_AREA,ROOF_TYPE,WATER_SUPPLY_TYPE,HEATING_TYPE,OUTER_WALLS,HOUDEN10KM) %>% 
  mice(m=1,method='pmm') %>% complete()

data_final <- cbind(dplyr::select(data_efter_slet,POLICY,EXPOSURE,YEAR,CLAIM_COUNT,CLAIM_SIZE,DEDUCTIBLE),data_final)

data_mice <- data_final

write.csv(data_mice,"C:/Users/Nbchr/OneDrive/Desktop/data_mice.csv", row.names = FALSE)

data_mice <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_mice.csv")

#data_randomforest

set.seed(1)
data_final <- dplyr::select(data_efter_slet,CONSTRUCTION_YEAR,BUILDINGS,FLOORS,WETROOMS,RESIDENTIAL_AREA,BASEMENT_AREA,CONSERVATORY_AREA,ROOF_TYPE,WATER_SUPPLY_TYPE,HEATING_TYPE,OUTER_WALLS,HOUDEN10KM) %>% missForest(maxiter = 10, ntree = 100)

data_randonforest <- data_final$ximp

data_randomforest <- cbind(dplyr::select(data_efter_slet,POLICY,EXPOSURE,YEAR,CLAIM_COUNT,CLAIM_SIZE,DEDUCTIBLE),data_randonforest)

write.csv(data_randomforest,"C:/Users/Nbchr/OneDrive/Desktop/data_randomforest.csv", row.names = FALSE)

data_rf <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_randomforest.csv")

data_rf <- mutate(data_rf,CLAIM_SIZE_INDEX = CLAIM_SIZE*indeksfunk(YEAR))

write.csv(data_rf,"C:/Users/Nbchr/OneDrive/Desktop/data_rf.csv", row.names = FALSE)


#data_knn

data_knn <- dplyr::select(data_efter_slet,CONSTRUCTION_YEAR,BUILDINGS,FLOORS,WETROOMS,RESIDENTIAL_AREA,BASEMENT_AREA,CONSERVATORY_AREA,ROOF_TYPE,WATER_SUPPLY_TYPE,HEATING_TYPE,OUTER_WALLS,HOUDEN10KM) %>% kNN(k = 4)

data_knn <- dplyr::select(data_knn,CONSTRUCTION_YEAR,BUILDINGS,FLOORS,WETROOMS,RESIDENTIAL_AREA,BASEMENT_AREA,CONSERVATORY_AREA,ROOF_TYPE,WATER_SUPPLY_TYPE,HEATING_TYPE,OUTER_WALLS,HOUDEN10KM)

data_knn <- cbind(dplyr::select(data_efter_slet,POLICY,EXPOSURE,YEAR,CLAIM_COUNT,CLAIM_SIZE,DEDUCTIBLE),data_knn)

data_knn<-mutate(data_knn,CLAIM_SIZE_INDEX = CLAIM_SIZE*indeksfunk(YEAR))

write.csv(data_knn,"C:/Users/Nbchr/OneDrive/Desktop/data_knn.csv", row.names = FALSE)


#### Imputation II ####
 
data_mice <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_mice.csv")
data_rf <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_rf.csv")
data_knn <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_knn.csv")
data_efter_slet <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_efter_slet.csv")

IMPUTED <- mutate(data_efter_slet,
                          IMPUTED = case_when(
                            is.na(HOUDEN10KM) ~ 1,
                            is.na(CONSTRUCTION_YEAR) ~ 1,
                            is.na(FLOORS) ~ 1,
                            is.na(WETROOMS) ~ 1,
                            TRUE              ~ 0
                          )) %>% select(IMPUTED)

data_mice <- data.frame(data_mice,IMPUTED)
data_rf <- data.frame(data_rf,IMPUTED)
data_knn <- data.frame(data_knn,IMPUTED)

df_HOUDEN10KM <- data.frame(data_mice$HOUDEN10KM, data_rf$HOUDEN10KM, data_knn$HOUDEN10KM, IMPUTED)
df_HOUDEN10KM <- rename(df_HOUDEN10KM,
             mice = data_mice.HOUDEN10KM,
             rf = data_rf.HOUDEN10KM,
             knn = data_knn.HOUDEN10KM
)

df_HOUDEN10KM_imp <- df_HOUDEN10KM %>% filter(IMPUTED == 1)
df_HOUDEN10KM$IMPUTED <- NULL
df_HOUDEN10KM_imp$IMPUTED <- NULL

sum(df_HOUDEN10KM$dif)
sum(data_mice$IMPUTED)

ggplot(df_HOUDEN10KM %>%  pivot_longer(cols = everything(),names_to = "method", values_to = "value"), x =) +
  geom_density()

p1 <- ggplot((df_HOUDEN10KM %>% pivot_longer(cols = everything(),names_to = "method",values_to = "value")), aes(x = value, fill = method, color = method)) +
  geom_density(alpha = 0.3) +
  labs( title = "Density of total dataset")

p2 <- ggplot((df_HOUDEN10KM_imp %>% pivot_longer(cols = everything(),names_to = "method",values_to = "value")), aes(x = value, fill = method, color = method)) +
  geom_density(alpha = 0.3) +
  labs(x = "HOUDEN10KM", title = "Density of imputed values")

p1 / p2


#### Se data ####

file.exists("C:/Users/Nbchr/OneDrive/Desktop/data_mice_encoded_aggr.csv")
data_encoded <- read.csv("C:/Users/Nbchr/OneDrive/Desktop/data_mice_encoded_aggr.csv")

sum(data_encoded$CLAIM_SIZE_INDEX)



#### leg med data ####

data_encoded <- mutate(data_encoded, VALUE = (log(CLAIM_SIZE_INDEX + 1) - 0.157948)/(10*1.193459))

max(data_encoded$VALUE)

mean(data_encoded$VALUE) # 0.157948
sd(data_encoded$VALUE) # 1.193459

ggplot(data_encoded, aes(x=VALUE)) +
  geom_density()

max(data_encoded$VALUE)
min(data_encoded$VALUE)

a <- (data_encoded$BASEMENT_AREA - mean(data_encoded$BASEMENT_AREA))/sd(data_encoded$BASEMENT_AREA)

max(a)
min(a)

plot(density(a), main = "Simple Density Plot")

max(plot(density(x), main = "Simple Density Plot"))

max(data_encoded$WETROOMS) # 12
max(data_encoded$FLOORS) # 3
max(data_encoded$BUILDINGS) # 10
max(data_encoded$DEDUCTIBLE) # 18360

summary(data_encoded)



