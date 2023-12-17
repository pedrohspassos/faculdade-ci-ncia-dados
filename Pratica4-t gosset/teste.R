#Pedro Passos
#Inicio (exemplo1)

muA = 8.0
sigmaA = 2.0
nA = 50
muB = 9.5
sigmaB = 1.0
nB = 45

#A=B (sem peso)
  A = rnorm(nA, muA, sigmaA)
  B = rnorm(nB,  muA,  sigmaA)

  muA_obtido = mean(A)
  sigmaA_obtido = sd(A)

  muB_obtido = mean(B)
  sigmaB_obtido = sd(B)

  # Restante da função  - diferença sem peso(se necessário)
  S = 1E6
  D = (1/0)*(1:S) # inicializando vetor

  for (i in 1:S) {
    a = rnorm(nA, muA_obtido, sigmaA_obtido)
    b = rnorm(nB, muB_obtido, sigmaB_obtido)
    D[i] = (mean(a) - mean(b)) 
  }

#Calculando integral

  dados = D

  info_hist = hist(dados, breaks = 50, plot = FALSE, density = FALSE)
  delta = info_hist$breaks[2] - info_hist$breaks[1]
  indice_meio = length(info_hist$counts) %/% 2 + 1
  soma_atual = info_hist$counts[indice_meio] * delta
  porcentagem_alvo = 0.95
  i = 1

  while (soma_atual < porcentagem_alvo) {
    soma_atual = soma_atual +
      (info_hist$counts[indice_meio - i] * delta) +
      (info_hist$counts[indice_meio + i] * delta)
    i = i + 1
  }

  indice_esquerdo = indice_meio - (i - 1)
  indice_direito = indice_meio + (i - 1)

  cat("Intervalo =", info_hist$breaks[indice_esquerdo], info_hist$breaks[indice_direito], "\n")
  cat("Soma =", soma_atual, "\n")
  
  abline(v = info_hist$breaks[indice_esquerdo], col = 'red', lty = 2, lwd = 2, label = 'Borda Esquerda')
  abline(v = info_hist$breaks[indice_direito], col = 'green', lty = 2, lwd = 2, label = 'Borda Direita')

#Plotando histograma
  
  hist(dados, breaks = 50, main = "Histograma de D", xlab = "D", col = "skyblue", border = "black")
  legend("topright", legend = "D", fill = "skyblue", border = "black")



# Diferença calculada com Gosset (ponderamento) - (exemplo1)

muA = 8.0
sigmaA = 2.0
nA = 50
muB = 9.5
sigmaB = 1.0
nB = 45

#histograma
A = rnorm(nA, muA, sigmaA)
B = rnorm(nB,  muB,  sigmaB)

muA_obtido = mean(A)
sigmaA_obtido = sd(A)

muB_obtido = mean(B)
sigmaB_obtido = sd(B)

# Diferença calculada com Gosset (ponderamento)
S = 1E6
D = (1/0)*(1:S) # inicializando vetor

for (i in 1:S) {
    a = rnorm(nA, muA_obtido, sigmaA_obtido)
    b = rnorm(nB, muB_obtido, sigmaB_obtido)

    peso = 1 / sqrt(sigmaA_obtido^2 / nA + sigmaB_obtido^2 / nB)
    D[i] = (mean(a) - mean(b)) * peso
}

#Calculando integral

  dados = D

  info_hist = hist(dados, breaks = 50, plot = FALSE, density = TRUE)
  delta = info_hist$breaks[2] - info_hist$breaks[1]
  indice_meio = length(info_hist$counts) %/% 2 + 1
  soma_atual = info_hist$counts[indice_meio] * delta
  porcentagem_alvo = 0.95
  i = 1

  while (soma_atual < porcentagem_alvo) {
    soma_atual = soma_atual +
      (info_hist$counts[indice_meio - i] * delta) +
      (info_hist$counts[indice_meio + i] * delta)
    i = i + 1
  }

  indice_esquerdo = indice_meio - (i - 1)
  indice_direito = indice_meio + (i - 1)

  cat("Intervalo =", info_hist$breaks[indice_esquerdo], info_hist$breaks[indice_direito], "\n")
  cat("Soma =", soma_atual, "\n")
  
  abline(v = info_hist$breaks[indice_esquerdo], col = 'red', lty = 2, lwd = 2, label = 'Borda Esquerda')
  abline(v = info_hist$breaks[indice_direito], col = 'green', lty = 2, lwd = 2, label = 'Borda Direita')

#Plotando histograma
  
  hist(dados, breaks = 50, main = "Histograma de D", xlab = "D", col = "skyblue", border = "black")
  legend("topright", legend = "D", fill = "skyblue", border = "black")


# A == B +  Diferença calculad com Gosset (ponderamento) (exemplo3)
muA = 8.0
sigmaA = 2.0
nA = 50
muB = 9.5
sigmaB = 1.0
nB = 45

  A = rnorm(nA, muA, sigmaA)
  B = rnorm(nB,  muA,  sigmaA)

  muA_obtido = mean(A)
  sigmaA_obtido = sd(A)

  muB_obtido = mean(B)
  sigmaB_obtido = sd(B)

  # Diferença calculada com Gosset (ponderamento)
S = 1E6
D = (1/0)*(1:S) # inicializando vetor

for (i in 1:S) {
    a = rnorm(nA, muA_obtido, sigmaA_obtido)
    b = rnorm(nB, muB_obtido, sigmaB_obtido)

    peso = 1 / sqrt(sigmaA_obtido^2 / nA + sigmaB_obtido^2 / nB)
    D[i] = (mean(a) - mean(b)) * peso
}

#Calculando integral

  dados = D

  info_hist = hist(dados, breaks = 50, plot = FALSE, density = TRUE)
  delta = info_hist$breaks[2] - info_hist$breaks[1]
  indice_meio = length(info_hist$counts) %/% 2 + 1
  soma_atual = info_hist$counts[indice_meio] * delta
  porcentagem_alvo = 0.95
  i = 1

  while (soma_atual < porcentagem_alvo) {
    soma_atual = soma_atual +
      (info_hist$counts[indice_meio - i] * delta) +
      (info_hist$counts[indice_meio + i] * delta)
    i = i + 1
  }

  indice_esquerdo = indice_meio - (i - 1)
  indice_direito = indice_meio + (i - 1)

  cat("Intervalo =", info_hist$breaks[indice_esquerdo], info_hist$breaks[indice_direito], "\n")
  cat("Soma =", soma_atual, "\n")
  
  abline(v = info_hist$breaks[indice_esquerdo], col = 'red', lty = 2, lwd = 2, label = 'Borda Esquerda')
  abline(v = info_hist$breaks[indice_direito], col = 'green', lty = 2, lwd = 2, label = 'Borda Direita')

#Plotando histograma
  
  hist(dados, breaks = 50, main = "Histograma de D", xlab = "D", col = "skyblue", border = "black")
  legend("topright", legend = "D", fill = "skyblue", border = "black")