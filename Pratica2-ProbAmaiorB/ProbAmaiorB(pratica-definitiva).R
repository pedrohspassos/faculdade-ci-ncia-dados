# Pedro Henrique Passos   


# Lendo os conjuntos de dados
A = read.delim("dados_A.txt", header=TRUE, sep=" ")
B = read.delim("dados_B.txt", header=TRUE, sep=" ")

# Convertendo em valores numericos
A = as.numeric(A[,1])
B = as.numeric(B[,1])

# Calculando os parametros da distribuilçao normal
muA = mean(A)
sigmaA = sd(A)
muB = mean(B)
sigmaB = sd(B)

# Funcao usada para calcular a densidade de uma distribuicao normal
f = function(x, mu, sigma){
  denominador = sqrt(2 * pi) * sigma
  expoente = -((x - mu)^2) / (2 * sigma^2)
  resultado = exp(expoente) / denominador
  return(resultado)
}

# Matriz que armezana os produtos das densidades de cada distribuicao normal
Z = matrix(0, 1000, 1000)
x = seq(min(A), max(A), len=1000)
y = seq(min(B), max(B), len=1000)


# Montando a matriz 
for(i in 1:1000){
  for(j in 1:1000){
    Z[i,j] = f(x[i],muA,sigmaA) * f(y[j],muB,sigmaB)
  }
}

# Calculando integral onde P (a>b) (diagonal principal pra cima)
integral = 0
for(i in 1:1000){
  for(j in i:1000){  # Percorre os elementos na diagonal principal e acima dela
    integral = integral + Z[i, j]
  }
}

#Realizando a soma de todos os elementos da matriz z (para normalizar o valor probabilidade)
#funcao apply(),propria do R, percorre a matriz a transformando em um vetor unidimensional
#funcao sum(),propria do R, somando todos elementos da matriz transformada em vetor

#soma_total = sum(apply(Z, c(1, 2)))
soma_total = sum(Z)

# Calculando a probabilidade 
probabilidade = integral / soma_total

print(probabilidade)


# Criar o gráfico de contorno
niveis_contorno = seq(min(Z), max(Z), length.out = 20)
contour(x = unique(x), y = unique(y), z = matrix(Z, nrow = 1000), nlevels = length(niveis_contorno), levels = niveis_contorno, col = heat.colors(length(niveis_contorno)), xlab = 'X (Gaussiana A)', ylab = 'Y (Gaussiana B)', main = 'Gráfico de Contorno de Z (Produto de Gaussianas)')

# Adicionar barra de cores
color.legend(levels = niveis_contorno, col = heat.colors(length(niveis_contorno)), align = "rb")

# Adicionar rótulos
title(main = 'Gráfico de Contorno de Z (Produto de Gaussianas)')
xlabel('X (Gaussiana A)')
ylabel('Y (Gaussiana B)')