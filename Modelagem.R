# libs --------------------------------------------------------------------

if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(tidymodels)){install.packages("tidymodels"); library(tidymodels)}
if(!require(factoextra)){install.packages("factoextra"); library(factoextra)}
if(!require(haven)){install.packages("haven"); library(haven)}
if(!require(leaflet)){install.packages("leaflet"); library(leaflet)}
if(!require(sf)){install.packages("sf"); library(sf)}
if(!require(lwgeom)){install.packages("lwgeom"); library(lwgeom)}
if(!require(clue)){install.packages("clue"); library(clue)}
if(!require(geosphere)){install.packages("geosphere"); library(geosphere)}

# dados brutos ------------------------------------------------------------

dados_brutos <- read_delim('data/dados_brutos.csv', col_names = TRUE, delim = "|")

#distribuição das estrelas
summary(dados_brutos[, 'stars'])

#gráfico por estrelas
rating_frq <- as.data.frame(table(dados_brutos$stars))

ggplot(rating_frq,aes(Var1,Freq)) +   
  geom_bar(aes(fill = Var1), position = "dodge", stat="identity",fill="palegreen") + 
  labs(x = "Stars")    

#####

#Count of restaurants for each count of reviews
freq_rest <- dados_brutos %>% 
  count(name)

freq_rest %>% select(-name) %>%  boxplot(n)

summary(freq_rest)

vector <- as.vector(freq_rest['n']) %>% 
  count(n)

summary(vector)

vector %>% 
  ggplot(aes(n, nn)) +
  geom_bar(stat ='identity', col = 'blue') +
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 200)) +
  ggtitle("Count of restaurants for each count of reviews") +
  labs(x = "Count of reviews", y = "count of restaurants") +
  geom_vline(xintercept = mean(vector$nn), col = 'red', size = 0.5)

#Count of users for each count of reviews
freq_user <- dados_brutos %>% 
  count(user_id)

summary(freq_user)

vector1 <- as.vector(freq_user['n']) %>% 
  count(n)

vector1 %>% 
  ggplot(aes(n, nn)) +
  geom_bar(stat ='identity', col = 'blue') +
  coord_cartesian(xlim = c(0, 100)) +
  ggtitle("Count of users for each count of reviews") +
  labs(x = "Count of reviews", y = "count of users") +
  geom_vline(xintercept = mean(vector$nn), col = 'red', size = 0.5)

# lendo a base ------------------------------------------------------------

dados <- read_delim('data/dados_yelp.csv', delim = '|')

#verificando se está tudo ok com a base
class(dados)
any(is.na(dados))
any(is.character(dados))

head(dados)

#dimensão da base
dim(dados)

#retirando linhas que há tudo zero
myfun <- function(dados) {
  y <- select_if(dados, is_numeric)
  rowSums(abs(y), na.rm=T)
}

dados$SUMROW <- myfun(dados)

dados1 <- dados %>%
  filter(SUMROW > 10) %>%
  select(-SUMROW)

set.seed(123)

#tirando um usuário da base para teste
sample <- sample(nrow(dados1), 1)

dados2 <- dados1[-sample,]
user_test <- dados1[sample,]

head(dados2)

# PCA ---------------------------------------------------------------------

receita_pca <- dados2 %>%
  select(-user_id) %>% 
  recipe(~ .) %>%
  step_pca(all_numeric(), threshold = .80) %>% 
  prep

dados_pca <- dados2 %>% 
  select(user_id) %>% 
  bind_cols(juice(receita_pca))

dados_pca_aux <- juice(receita_pca)

rownames(dados_pca_aux) <- dados_pca_aux$user_id

head(dados_pca_aux)

# cotovelo ----------------------------------------------------------------

#método do cotovelo para k ideal
criterio <- function(k) kmeans(dados_pca_aux, k)$tot.withinss

estudo <- tibble(k = 2:100) %>% 
  mutate(w = map_dbl(k, criterio))

estudo %>% 
  ggplot(aes(k, w)) + 
  geom_point(size = 3) + 
  geom_line() + 
  labs(y = "total within sum of squares", x = "k") + 
  scale_x_continuous(breaks = 1:100)


# clusterização por restaurante -------------------------------------------

# especifica k (escolhi k = 41)
kmedias <- kmeans(dados_pca_aux, centers = 41)

dados_pca <- dados_pca %>% 
  mutate(cluster = kmedias$cluster)

head(dados_pca)


# gráf PC1xPC2 ------------------------------------------------------------

dados_pca %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(PC001, PC002, color = cluster)) + 
  geom_point(size = 3, alpha = .5) + 
  theme(legend.position = "top")

# gráf PC1xPC3 ------------------------------------------------------------

dados_pca %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(PC001, PC003, color = cluster)) + 
  geom_point(size = 3, alpha = .5) + 
  theme(legend.position = "top")

# gráf PC1xPC4 ------------------------------------------------------------

dados_pca %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(PC001, PC004, color = cluster)) + 
  geom_point(size = 3, alpha = .5) + 
  theme(legend.position = "top")

# gráf PC1xPC5 ------------------------------------------------------------

dados_pca %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot(aes(PC001, PC005, color = cluster)) + 
  geom_point(size = 3, alpha = .5) + 
  theme(legend.position = "top")



# predição ----------------------------------------------------------------

receita_pca_test <- user_test %>%
  select(-user_id) %>% 
  recipe(~ .) %>%
  step_pca(all_numeric(), threshold = .80) %>% 
  prep

dados_pca_test <- user_test %>% 
  select(user_id) %>% 
  bind_cols(juice(receita_pca_test))

dados_pca_aux_test <- juice(receita_pca_test)
rownames(dados_pca_aux_test) <- dados_pca_aux_test$user_id

kmedias_predicao <- cl_predict(kmedias, dados_pca_aux_test)

kmedias_predicao

#ele pertence ao cluster 35
user_test <- user_test %>% 
  mutate(cluster = kmedias_predicao)


# filtrando usuários do cluster 35 ----------------------------------------

users_filt <- dados_pca %>% 
  filter(cluster == kmedias_predicao)

head(users_filt)

users_filt_01 <- users_filt %>% 
  select(user_id) %>% 
  inner_join(dados2, users_filt, by = "user_id")

head(users_filt_01)

users_filt_02 <- users_filt_01 %>% 
  select(-user_id) %>% 
  map_dfr( ~ sum(.x>0)) %>% 
  pivot_longer(cols = everything()) %>% 
  arrange(value)

users_filt_02 <- users_filt_02[order(-users_filt_02$value),]

head(users_filt_02)

# geolocalização ----------------------------------------------------------

atributos <- read_delim('data/atributos.csv', delim = "|", col_names = FALSE)

colnames(atributos) <- c('name', 'latitude', 'longitude')

localiz <- as.data.frame(atributos)

center_lon = median(localiz$longitude,na.rm = TRUE)
center_lat = median(localiz$latitude,na.rm = TRUE)

#plotando todos os restaurantes no mapa
leaflet(localiz) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude)  %>%
  setView(lng=center_lon, lat=center_lat,zoom = 13)

#plotando as filiais do restaurante que o cluster 35 mais gostou
capriotti <- atributos %>% filter(name == "Capriotti's Sandwich Shop")
localiz_capriotti <- as.data.frame(capriotti)
capriotti_lon = median(localiz_capriotti$longitude,na.rm = TRUE)
capriotti_lat = median(localiz_capriotti$latitude,na.rm = TRUE)

leaflet(localiz_capriotti) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude)  %>%
  setView(lng=center_lon, lat=center_lat,zoom = 13)

#####

#pegando os restaurantes que os usuários do cluster 35 mais gostaram
atributos1 <- inner_join(users_filt_02, atributos, by = "name")

head(atributos1)

# indicar a posição de referência
lat_rest  <- atributos1[1,]$latitude 
  
long_rest <- atributos1[1,]$longitude 

# distância máxima em metros
max_dist <- 5000

atributos1 %>%
  mutate(distancia = map2_dbl(latitude, longitude, ~distm(c(.y, .x), c(long_rest, lat_rest)))) %>% 
  filter(distancia < max_dist) %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~longitude, ~latitude, label = ~as.character(name))

