#Pedro Passos
# Classes M = 1 - maligno e B = 2 - benigno
# 30 caracteristicas e 569 dados              


#1-Tratando os dados
H = read.delim("wdbc.data", sep=",", header=FALSE)
labels = read.delim("labels_dados.txt")

#Tirando o campo ID
H =  H[,-1]
#pegando as classes da base e tirando essa coluna do dataset
classeDiagnostico = H[,1]
H = H[,-1]

#Para plotar os graficos das caracteristicas com os respectivos nomes
labels = labels[-1,] # tirando o ID
labels = labels[-1,] # tirando a classe

#pegando numero de linhas e colunas
m = ncol(H)
n = nrow(H)

#gerando os graficos
for(j in 1:m){
  x11()
  hist(H[,j], main=labels[j,1], xlab="")
}
for(j in 1:m){ dev.off() }  # só pra apagar todos os plots

#2-Criando o modelo
# QDA quadratic
library(MASS)
set.seed(1234)
N = n
treino = sample(1:N, round(2/3*N)) # 2/3 pra treinar modelo
modelo = qda(H[treino,] , classeDiagnostico[treino])

#nrow(H[treino,]) - qtd cjt treino
#nrow(H[-treino,]) - qtd cjt teste

# quero prever classes que eu já sabia pra ver como foi o treino
predicaoTreino = predict(modelo, H[treino,])


matrixdeconfusaoTreino = matrix(0, 2, 2) 
#Calculando matrix de confusao completa e direta (treinamento)
k = length(unique(classeDiagnostico)) # pra saber quantas classes tem

#Definindo um classe como padrao para gerar matriz confusao
classe_positiva = "M"  # Defina a classe de interesse como positiva

for (i in 1:2) {
  for (j in 1:2) {
    verdadeiros_positivos = sum((predicaoTreino$class == classe_positiva) & (classeDiagnostico[treino] == classe_positiva))
    falsos_positivos = sum((predicaoTreino$class != classe_positiva) & (classeDiagnostico[treino] == classe_positiva))
    verdadeiros_negativos = sum((predicaoTreino$class != classe_positiva) & (classeDiagnostico[treino] != classe_positiva))
    falsos_negativos = sum((predicaoTreino$class == classe_positiva) & (classeDiagnostico[treino] != classe_positiva))
    
    matrixdeconfusaoTreino[1, 1] = verdadeiros_positivos
    matrixdeconfusaoTreino[1, 2] = falsos_positivos
    matrixdeconfusaoTreino[2, 1] = falsos_negativos
    matrixdeconfusaoTreino[2, 2] = verdadeiros_negativos
  }
}


# quero prever classes de tudo menos o treino (teste) - aquilo qn n sei
predicaoTeste = predict(modelo, H[-treino,])

matrixdeconfusaoTeste = matrix(0, 2, 2) 

classe_positiva = "M"  # Defina a classe de interesse como positiva

for (i in 1:2) {
  for (j in 1:2) {
    verdadeiros_positivos = sum((predicaoTeste$class == classe_positiva) & (classeDiagnostico[-treino] == classe_positiva))
    falsos_positivos = sum((predicaoTeste$class != classe_positiva) & (classeDiagnostico[-treino] == classe_positiva))
    verdadeiros_negativos = sum((predicaoTeste$class != classe_positiva) & (classeDiagnostico[-treino] != classe_positiva))
    falsos_negativos = sum((predicaoTeste$class == classe_positiva) & (classeDiagnostico[-treino] != classe_positiva))
    
    matrixdeconfusaoTeste[1, 1] = verdadeiros_positivos
    matrixdeconfusaoTeste[1, 2] = falsos_positivos
    matrixdeconfusaoTeste[2, 1] = falsos_negativos
    matrixdeconfusaoTeste[2, 2] = verdadeiros_negativos
  }
}


#Calculando algumas metricas

#Acuracia treinamento
verdadeiros_positivos =  matrixdeconfusaoTreino[1, 1]
verdadeiros_negativos =  matrixdeconfusaoTreino[2, 2]
acuraciaTreinamento = (verdadeiros_positivos + verdadeiros_negativos) / nrow(H[treino,])
#Acuracia Teste
verdadeiros_positivos = matrixdeconfusaoTeste[1, 1]
verdadeiros_negativos = matrixdeconfusaoTeste[2, 2]
acuraciaTeste = (verdadeiros_positivos + verdadeiros_negativos) / nrow(H[-treino,])


#Especificidade treinamento
verdadeiros_negativos = matrixdeconfusaoTreino[2, 2]
falsos_positivos = matrixdeconfusaoTreino[1, 2]
especificidadeTreinamento = verdadeiros_negativos / (verdadeiros_negativos + falsos_positivos)
#Especificidade Teste
verdadeiros_negativos = matrixdeconfusaoTeste[2, 2]
falsos_positivos = matrixdeconfusaoTeste[1, 2]
especificidadeTeste = verdadeiros_negativos / (verdadeiros_negativos + falsos_positivos)


# F1 Score treinamento
verdadeiros_positivos = matrixdeconfusaoTreino[1, 1]
falsos_positivos = matrixdeconfusaoTreino[1, 2]
precisaoTreinamento= verdadeiros_positivos / (verdadeiros_positivos + falsos_positivos)

falsos_negativos = matrixdeconfusaoTreino[2, 1]

revocacaoTreinamento = verdadeiros_positivos / (verdadeiros_positivos + falsos_negativos)

f1_scoreTreinamento = 2 * (precisaoTreinamento * revocacaoTreinamento) / (precisaoTreinamento + revocacaoTreinamento)

# F1 Score teste
verdadeiros_positivos = matrixdeconfusaoTeste[1, 1]
falsos_positivos = matrixdeconfusaoTeste[1, 2]
precisaoTeste= verdadeiros_positivos / (verdadeiros_positivos + falsos_positivos)

falsos_negativos = matrixdeconfusaoTeste[2, 1]

revocacaoTeste = verdadeiros_positivos / (verdadeiros_positivos + falsos_negativos)

f1_scoreTeste = 2 * (precisaoTeste * revocacaoTeste) / (precisaoTeste + revocacaoTeste)