#Pedro Passos
# Criando um classificador 
#install.packages("MASS")
#library (MASS)
#Pegando 2/3 dos dados para treinamento - LDA

#1 - Lendo e olhando os dados
D = read.delim("wine.data", sep=",", header=FALSE)
featurenames = read.delim("medicoes-dos-vinhos.txt")

tiposdevinhos = D[,1] # pegando label do tipo de vinho
D = D[,-1] # já usei os labels, posso tirar da tabela
featurenames = featurenames[-1,] # nao precisa da linha 1 mais

#pegando qtd de linhas e colunas e vendo como dados são
m = ncol(D)
n = nrow(D)

for(j in 1:m){
  x11()
  hist(D[,j], main=featurenames[j,1], xlab="")
}

for(j in 1:m){ dev.off() }  # só pra apagar todos os plots

# 2 - Comecando com a criacao do modelo
# LDA quadratic
library(MASS)
set.seed(1234)
N = n
treino = sample(1:N, round(2/3*N)) # 2/3 pra treinar modelo
modelo = lda(D[treino,] , tiposdevinhos[treino])

#nrow(D[treino,]) - qtd cjt treino
#nrow(D[-treino,]) - qtd cjt teste

# quero prever classes que eu já sabia pra ver como foi o treino
predicaoTreino = predict(modelo, D[treino,]) 

# genrando matriz de confusao treino
matrixdeconfusaoTreino = matrix(0, 3, 3) 

# 3-criando matrix confusão para cada caso de maneira individual
# Tudo q ta na diagonal esta certo e tudo que não é erro(multiclasse)

#Criando para cada caso
# Disse que ia ser classe 1 e era classe 1 mesmo
matrixdeconfusaoTreino[1,1] = sum(
  (predicaoTreino$class == 1) &
    (tiposdevinhos[treino] == 1)
)

# Disse que ia ser classe 2 e era classe 2 mesmo
matrixdeconfusaoTreino[2,2] = sum(
  (predicaoTreino$class == 2) &
    (tiposdevinhos[treino] == 2)
)

#  Disse que ia ser classe 3 e era classe 3 mesmo
matrixdeconfusaoTreino[3,3] = sum(
  (predicaoTreino$class == 3) &
    (tiposdevinhos[treino] == 3)
)


#Calculando matrix de confusao completa e direta (treinamento)
k = length(unique(tiposdevinhos)) # pra saber quantas classes tem
for(i in 1:k){
  for(j in 1:k){
    matrixdeconfusaoTreino[i,j] = sum(
      (predicaoTreino$class == i) &
        (tiposdevinhos[treino] == j)
    )
  }
}


# quero prever classes de tudo menos o treino (teste)
predicaoTeste = predict(modelo, D[-treino,]) 

# Matrix confusao direta (teste)
matrixdeconfusaoTeste = matrix(0, 3, 3) 
for(i in 1:k){
  for(j in 1:k){
    matrixdeconfusaoTeste[i,j] = sum(
      (predicaoTeste$class == i) &
        (tiposdevinhos[-treino] == j)
    )
  }
}


######################################################

# "Trasformando" matriz confusão multiclasses em uma matriz confusão binaria
# Para isso, os casos em que são de fato classe 2 e classe 3, foram somados 
# e colocados como se fosse uma unica coisa

# Treino
classe_positiva = 1  # Defina a classe de interesse como positiva 

# Inicialize a matriz de confusão binária
matrizdeconfusaobinariaTreinamento = matrix(0, 2, 2)

# Calcule a matriz de confusão binária teste (-treino)
for (i in 1:2) {
  for (j in 1:2) {
    verdadeiros_positivos = sum((predicaoTreino$class == classe_positiva) & (tiposdevinhos[treino] == classe_positiva))
    falsos_positivos = sum((predicaoTreino$class != classe_positiva) & (tiposdevinhos[treino] == classe_positiva))
    verdadeiros_negativos = sum((predicaoTreino$class != classe_positiva) & (tiposdevinhos[treino] != classe_positiva))
    falsos_negativos = sum((predicaoTreino$class == classe_positiva) & (tiposdevinhos[treino] != classe_positiva))
    
    matrizdeconfusaobinariaTreinamento[1, 1] = verdadeiros_positivos
    matrizdeconfusaobinariaTreinamento[1, 2] = falsos_positivos
    matrizdeconfusaobinariaTreinamento[2, 1] = falsos_negativos
    matrizdeconfusaobinariaTreinamento[2, 2] = verdadeiros_negativos
  }
}

# Exiba a matriz de confusão binária
print(matrizdeconfusaobinariaTreinamento)


#Teste

# Defina a classe positiva e negativa
classe_positiva = 1  # Defina a classe de interesse como positiva (ajuste conforme necessário)

# Inicialize a matriz de confusão binária
matrizdeconfusaobinariaTeste = matrix(0, 2, 2)

# Calcule a matriz de confusão binária teste (-treino)
for (i in 1:2) {
  for (j in 1:2) {
    verdadeiros_positivos = sum((predicaoTeste$class == classe_positiva) & (tiposdevinhos[-treino] == classe_positiva))
    falsos_positivos = sum((predicaoTeste$class != classe_positiva) & (tiposdevinhos[-treino] == classe_positiva))
    verdadeiros_negativos = sum((predicaoTeste$class != classe_positiva) & (tiposdevinhos[-treino] != classe_positiva))
    falsos_negativos = sum((predicaoTeste$class == classe_positiva) & (tiposdevinhos[-treino] != classe_positiva))
    
    matrizdeconfusaobinariaTeste[1, 1] = verdadeiros_positivos
    matrizdeconfusaobinariaTeste[1, 2] = falsos_positivos
    matrizdeconfusaobinariaTeste[2, 1] = falsos_negativos
    matrizdeconfusaobinariaTeste[2, 2] = verdadeiros_negativos
  }
}

# Exiba a matriz de confusão binária
print(matrizdeconfusaobinariaTeste)



#Calculando algumas metricas


#Acuracia treinamento
verdadeiros_positivos = matrizdeconfusaobinariaTreinamento[1, 1]
verdadeiros_negativos = matrizdeconfusaobinariaTreinamento[2, 2]
acuraciaTreinamento = (verdadeiros_positivos + verdadeiros_negativos) / nrow(D[treino,])


#Acuracia Teste
verdadeiros_positivos = matrizdeconfusaobinariaTeste[1, 1]
verdadeiros_negativos = matrizdeconfusaobinariaTeste[2, 2]
acuraciaTeste = (verdadeiros_positivos + verdadeiros_negativos) / nrow(D[-treino,])


#Especificidade treinamento
verdadeiros_negativos = matrizdeconfusaobinariaTreinamento[2, 2]
falsos_positivos = matrizdeconfusaobinariaTreinamento[1, 2]
especificidadeTreinamento = verdadeiros_negativos / (verdadeiros_negativos + falsos_positivos)

#Especificidade Teste

verdadeiros_negativos = matrizdeconfusaobinariaTeste[2, 2]
falsos_positivos = matrizdeconfusaobinariaTeste[1, 2]
especificidadeTeste = verdadeiros_negativos / (verdadeiros_negativos + falsos_positivos)

# F1 Score treinamento
verdadeiros_positivos = matrizdeconfusaobinariaTreinamento[1, 1]
falsos_positivos = matrizdeconfusaobinariaTreinamento[1, 2]
precisaoTreinamento= verdadeiros_positivos / (verdadeiros_positivos + falsos_positivos)

falsos_negativos = matrizdeconfusaobinariaTreinamento[2, 1]

revocacaoTreinamento = verdadeiros_positivos / (verdadeiros_positivos + falsos_negativos)

f1_scoreTreinamento = 2 * (precisaoTreinamento * revocacaoTreinamento) / (precisaoTreinamento + revocacaoTreinamento)

# F1 Score teste
verdadeiros_positivos = matrizdeconfusaobinariaTeste[1, 1]
falsos_positivos = matrizdeconfusaobinariaTeste[1, 2]
precisaoTeste= verdadeiros_positivos / (verdadeiros_positivos + falsos_positivos)

falsos_negativos = matrizdeconfusaobinariaTeste[2, 1]

revocacaoTeste = verdadeiros_positivos / (verdadeiros_positivos + falsos_negativos)

f1_scoreTeste = 2 * (precisaoTeste * revocacaoTeste) / (precisaoTeste + revocacaoTeste)

