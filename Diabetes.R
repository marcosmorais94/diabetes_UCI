# DATASET UCI - DIABETES
# O objetivo desse script é encontrar um modelo preditivo que consiga 
# identificar quando uma pessoa terá ou não diabetes

#Link do dataset: https://archive.ics.uci.edu/ml/datasets/Early+stage+diabetes+risk+prediction+dataset.

#Dicionário dos dados
# Age 1.20-65
# Sex 1. Male, 2.Female
# Polyuria 1.Yes, 2.No.
# Polydipsia 1.Yes, 2.No.
# sudden weight loss 1.Yes, 2.No.
# weakness 1.Yes, 2.No.
# Polyphagia 1.Yes, 2.No.
# Genital thrush 1.Yes, 2.No.
# visual blurring 1.Yes, 2.No.
# Itching 1.Yes, 2.No.
# Irritability 1.Yes, 2.No.
# delayed healing 1.Yes, 2.No.
# partial paresis 1.Yes, 2.No.
# muscle stiness 1.Yes, 2.No.
# Alopecia 1.Yes, 2.No.
# Obesity 1.Yes, 2.No.
# Class 1.Positive, 2.Negative.

#Definindo diretório de trabalho
setwd('c:/FCD/R/UCI/Diabetes')
getwd()

# 1 - Carga do Dataset e pacotes
DB <- read.csv('diabetes_data_upload.csv')


library(caret) #Pacote de Machine Learning
library(Amelia)
library(rmarkdown)
library(ROCR)

# 2 - Análise Exploratória dos dados
View(DB)
str(DB) #Será necessário transformar os dados em factor
hist(DB$Age)
# Os dados da idade estão bem distribuídos no histograma, podemos avançar na análise

missmap(DB) # Não há dados missing

round(prop.table(table(DB$class))*100,2) 
# O DataSet veio com um distribução muito maior de casos positivos.
# Isso implica em um modelo mais tendecioso para os casos positivos.
# Em função de ser um dataset de aprendizado, o modelo será treinado e testado com essas condições
# Em um caso prático, teríamos que buscar mais registros negativos para balancear o dataset.


# 3 - Pré Processamento dos dados

#É uma boa prática preservar o dataset original
DB2 <- DB 

#Função para transformar as variaveis em tipo factor
for (i in 2:17) {
  DB2[,i] <- as.factor(DB2[,i])
}

# 4 - Divisão Treino e Teste Modelo

BD_treino <- DB2[1:312,]
BD_teste <- DB2[313:520,]

# 5 - Treinamento Modelo de Regressão Logísitca

modelo1 <- glm(formula = 'class ~ .', data = BD_treino, family = 'binomial')

#O summary mostra que a variável Polyuria e Polydipsia são muito representativas
#no modelo de regressão logistica. 

#Polyuria é a urina constante e Polydipsia é sintoma de sede e/ou boca seca.
#Ambos os sintomas são característicos da diabetes.

summary(modelo1)

#Dataset de treino sem a variável target
BD_teste1 <- BD_teste[,1:16]

# 6 - Prevendo resultados (Dataset de teste)

resultado_modelo <- predict(modelo1, newdata = BD_teste1, type = 'response')

#Variável resposta com valores convertidos em binário
resultado<- ifelse(resultado_modelo > 0.5, 1, 0) 

#Variável target com valores convertidos em binário
BD_teste$Testemodelo <- ifelse(BD_teste$class == "Positive", 1, 0)

#A Confusion Matrix mostra uma acurácia de 84% em relação ao que foi previsto no
# dataset de treino e o observado originalmente no mesmo dataset.

#É uma acurária excelente para a v1 do modelo.
confusionMatrix(table(data = resultado_final, reference = BD_teste$Testemodelo))

resultado_final <- prediction(resultado_modelo, BD_teste$Testemodelo)
# 7 Apresentação Resultados do Modelo

# Função para Plot ROC 
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col = "black",lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8,xaxs = "i", yaxs = "i")
  abline(0,1, col = "red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

# Plot Curva ROC
par(mfrow = c(1, 2))
plot.roc.curve(resultado_final, title.text = "Curva ROC")

