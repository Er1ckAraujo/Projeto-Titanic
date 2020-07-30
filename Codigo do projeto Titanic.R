# Esse projeto foi realizado com o banco de dados do Keagle: https://www.kaggle.com/c/titanic/data
# Instalando pacotes usados no projeto

install.packages(c("rmarkdown", "pkgname", "tibble", "tidyverse","dplyr","tibble",
                   "magrittr", "ggplot2","patchwork","corrplot", "mice","finalfit",
                   "missForest", "esquisse", "binomTools", "rpart", "rpart.plot",
                   "caret", "readr","mfx","caret","pRoc", "ResourceSelection",
                   "modEvA", "foreign","stargazer"))

# Mudando a categoria das colunas

library(readr)
train <- read_csv("Projeto Titanic/train.csv")

library(tibble)
library(dplyr)
train <- as.tibble(train)
train <- train %>%
            mutate(PassengerId = as.character(train$PassengerId),
            Survived = as.factor(train$Survived),
            Pclass = as.factor(train$Pclass),
            Sex = as.factor(train$Sex),
            Age = as.numeric(train$Age),
            SibSp = as.numeric(train$SibSp),
            Parch = as.numeric(train$Parch),
            Fare = as.numeric(train$Fare),
            Embarked = as.character(train$Embarked))


# Estatísticas descritivas
view(train)
summary(train)
head(train)

library(ggplot2)

  SexoClasse1 <- ggplot(train) +
                 aes(x = Sex, fill = Pclass) +
                 geom_bar() +
                 scale_fill_hue() +
                 labs(x = "Sexo", y = "Contagem", title = "Perfil dos Passageiros ", subtitle = "Momento do embarque", fill = "Classes") +
                 theme_minimal()
  
  SexoClasse2 <- train %>%
                filter(Survived %in% "1") %>%
                ggplot() +
                aes(x = Sex, fill = Pclass) +
                geom_bar() +
                scale_fill_hue() +
                labs(x = "Sexo", y = "Contagem", title = "Perfil dos Passageiros", subtitle = "Sobreviventes", fill = "Classes") +
                theme_minimal()
  
library(patchwork)
  
  SexoClasse1 + SexoClasse2 + plot_layout(ncol = 1)
  
# SUPOSIÇÕES:  Mulheres possuem mais chance de sobrevivência que homens.
              # Homens da terceira classe foram as maiores vitimas.

# Idade
library(ggplot2)

  Idade1 <- train %>%
              filter(!is.na(Age)) %>%
              ggplot() +
              aes(x = Age) +
              geom_histogram(bins = 30L, fill = "#0c4c8a") +
              labs(x = "Idade", y = "Contagem", title = "Idade dos Passageiros", subtitle = "Momento do embarque") +
              theme_minimal()


  Idade2 <- train %>%
              filter(Survived %in% "1") %>%
              filter(!is.na(Age)) %>%
              ggplot() +
              aes(x = Age) +
              geom_histogram(bins = 30L, fill = "#0c4c8a") +
              labs(x = "Idade", y = "Contagem", title = "Idade dos Passageiros", subtitle = "Sobreviventes") +  
              theme_minimal()

  
library(patchwork)
  Idade1 +  Idade2 + plot_layout(ncol = 1)
  
# SUPOSIÇÕES: A distribuição das idades parece não ter mudado tanto, podemos perceber que
# os valores mais extremos foram preservados, como o passageiro mais velho e o mais novo.

  
# Criar estimativas das idades faltantes
  summary(train)
  glimpse(train) #resumo dos dados
  sum(is.na(train))/(891*12) # 8% de dados ausentes
  
# Criando a imputação de dados com iteração zero
# para defifir parametros

library(mice)    
library(tibble)  
library(dplyr)
  
  imp <- train
  
  imp <- imp %>% # Modificando as classes das variáveis
    mutate(PassengerId = as.integer(imp$PassengerId),
           Survived = as.integer(imp$Survived),
           Pclass = as.integer(imp$Pclass),
           Name = as.character(imp$Name),
           Sex = as.character(imp$Sex),
           Age = as.numeric(imp$Age),
           SibSp = as.integer(imp$SibSp),
           Parch = as.integer(imp$Parch),
           Ticket = as.double(imp$Ticket),
           Fare = as.numeric(imp$Fare),
           Cabin = as.double(imp$Cabin),
           Embarked = as.character(imp$Embarked)
           
    )
  
  glimpse(imp)
  
  
  
  init <- mice(data = imp, maxit = 0)
  meth <- init$method
  predM <- init$predictorMatrix

  head(imp) 
  
  # Definindo o metodo de execução
  names(imp)
  
  meth[c(3:12)] = "cart" #Árvore de classificação e regressão
  
  imputed <- mice(imp, method = meth %>% as.vector(), predictorMatrix = predM, m=5)
  
  imp1 <- complete(imputed, 1)
  imp2 <- complete(imputed, 2)
  imp3 <- complete(imputed, 3)
  imp4 <- complete(imputed, 4)
  imp5 <- complete(imputed, 5)

library(finalfit)
  missing_glimpse(imp1)%>% 
    DT::datatable()
  
# montando o novo dataframe completo
# função de moda para variáveis categoricas
  
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  train_completo <- data.frame(PassengerId = train$PassengerId,
    Survived = train$Survived,
    Pclass = apply(cbind(imp1$Pclass, imp2$Pclass, imp3$Pclass, imp4$Pclass, imp5$Pclass), 1, mode),
    Sex = apply(cbind(imp1$Sex, imp2$Sex, imp3$Sex, imp4$Sex, imp5$Sex), 1, mode),
    Age = apply(cbind(imp1$Age, imp2$Age, imp3$Age, imp4$Age, imp5$Age),1, mean),
    SibSp = apply(cbind(imp1$SibSp, imp2$SibSp, imp3$SibSp, imp4$SibSp, imp5$SibSp),1, mean),
    Parch = apply(cbind(imp1$Parch, imp2$Parch, imp3$Parch, imp3$Parch, imp5$Parch),1, mean),
    Fare = train$Fare)
  
  train_completo <- as.tibble(train_completo)
  
  view(train_completo)
  
  sum(is.na(train_completo))
  
  head(train_completo)

  summary(train_completo$Age)
# Analisando a qualidade da nossa imputação
  
  library(tidyr)
  grafico_imput = function(real,imputado,x){
    data.frame("Antes de imputar" = real,
               "Depois de imputar" = imputado) %>% 
                pivot_longer(everything()) %>% 
                na.omit() %>% 
                ggplot(mapping = aes(value,col= name))+
                geom_density()+
                theme_minimal()+
                xlab(x)+
                ylab("Densidade")+
                theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.2))+
                scale_color_discrete(labels = c("Antes de imputar","Depois de imputar"))+
                guides(color=guide_legend(title=""))
    
  } 


grafico_imput(train$Age, train_completo$Age, "Age")



# QUAL A PROBABILIDADE DE SOBREVIVeNCIA DO TITANIC?
# Análise de regressão logistica

train_completo <- train_completo %>% #mudando a categoria de algumas variáveis
  mutate(Pclass = as.factor(train_completo$Pclass),
         Sex = as.factor(train_completo$Sex))


head(train_completo)

# Arvore de decisão
library(rpart)
library(rpart.plot)

arvore <- rpart(Survived~Pclass+Sex+Age, method = "class", data = train_completo)

summary(arvore)

rpart.plot(arvore)

summary(train_completo)

modelo <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare, data = train_completo,
              family = binomial(link="logit"))
summary(modelo)

# As variáveis explicativas Parch e Fare, possuem um p-valor mt alto
# portanto não possuem influencia significativa para o modelo, vamos refazer 
# o modelo sem elas.

anova(modelo, test = "Chisq")
# Variância com modelo nulo: 1186.66
# com a inclusão das variáveis, foi reduzido para 779.79

exp(modelo$coefficients)

# com os coeficientes exponencializados temos as razões de chance de cada
# variável do modelo
# Sendo uma pessoa do Sexo masc suas chances de sobrevivência diminuem
# 93.54017% ((0.06459829-1)*100)
# Sendo uma pessoa da classe 3 suas chances de sobrevivencia diminuem em
# -90.61211% ((0.0938789-1)*100)

# PREPARANDO O BANCO DE DADOS TESTE PARA EXECUÇÃO DO MODELO

# Criando uma imputação para as variáveis Age e Fare do banco test
library(readr)
test <- read_csv("Projeto Titanic/test.csv")
View(test)

summary(test)

library(mice)    
library(tibble)  
library(dplyr)


impp <- test

impp <- impp %>% # Modificando as classes das variáveis
  mutate(PassengerId = as.integer(impp$PassengerId),
         Pclass = as.integer(impp$Pclass),
         Name = as.character(impp$Name),
         Sex = as.character(impp$Sex),
         Age = as.numeric(impp$Age),
         SibSp = as.integer(impp$SibSp),
         Parch = as.integer(impp$Parch),
         Ticket = as.double(impp$Ticket),
         Fare = as.numeric(impp$Fare),
         Cabin = as.double(impp$Cabin),
         Embarked = as.character(impp$Embarked))

glimpse(impp)

init2 <- mice(data = impp, maxit = 0)
meth2 <- init2$method
predM2 <- init2$predictorMatrix

head(impp)

# Definindo o método de execução

names(impp)

meth2[c(2:11)] = "cart" #Árvore de classificação e regressão

imputedd <- mice(impp, method = meth2 %>% as.vector(), predictorMatrix = predM2, m=5)

impp1 <- complete(imputedd, 1)
impp2 <- complete(imputedd, 2)
impp3 <- complete(imputedd, 3)
impp4 <- complete(imputedd, 4)
impp5 <- complete(imputedd, 5)

library(finalfit)
missing_glimpse(impp3)%>% 
  DT::datatable()

# montando o novo dataframe completo
head(test)

test_completo <- tibble(PassengerId = test$PassengerId,
                            Pclass = test$Pclass,
                            Sex = test$Sex,
                            Age = apply(cbind(impp1$Age, impp2$Age, impp3$Age, impp4$Age, impp5$Age),1, mean),
                            SibSp = test$SibSp,
                            Parch = test$Parch,
                            Fare = apply(cbind(impp1$Fare, impp2$Fare, impp3$Fare, impp4$Fare, impp5$Fare),1, mean))

view(test_completo)

sum(is.na(test_completo))

head(test_completo)

#aplicando o modelo
test_completo <- test_completo %>%
  mutate(PassengerId = as.character(PassengerId),
         Pclass = as.factor(Pclass),
         Sex = as.factor(Sex))

test_completo$SurviveId <- predict(modelo, newdata = test_completo, type = "response")

test_completo$Survive <- ifelse(test_completo$SurviveId>0.5,1,0)

head(test_completo)
