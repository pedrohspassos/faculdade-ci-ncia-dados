# Pedro Passos

# Solicitando numero bins
numero_bins = as.numeric(readline("Entre com o número de bins: "))

#Pegando os valores dos bins
# Solicitar os dados

mensagem = "Insira os dados separados por espaço: "
cat(mensagem)
entrada_usuario = readline()

# Dividir a entrada do usuário em uma lista de valores
dados = as.numeric(strsplit(entrada_usuario, " ")[[1]])

# Exibir os dados
print(dados)


########### calculando bins #############

valor_min = min(dados)
valor_max = max(dados)
tamanho_bins = (valor_max - valor_min)/numero_bins

# Criar vetor de bins

# Inicializar vetor de bins
bins = numeric(numero_bins + 1)


# Preencher o vetor de bins usando um loop for
for (i in 0:numero_bins) {
  bins[i + 1] = valor_min + i * tamanho_bins
}

# Exibir os bins
print(bins)

#######################################
#### Calculando frequencia #####

# vetor de zeros com tamanho numero bins
frequencia = rep(0, numero_bins)

# Supondo que você tenha os vetores 'dado', 'bins' e 'frequencia' já definidos

# Loop para calcular frequências
for (val in dados) {
  for (i in 0:numero_bins) {
    if ((bins[i] <= val && val < bins[i + 1]) || (i == numero_bins && val == bins[i + 1])) {
      frequencia[i] = frequencia[i] + 1
      break
    }
  }
}

#Mostrando frequencias
# Loop para exibir frequências e intervalos
for (i in seq_len(numero_bins)) {
  cat(sprintf("%d | %.6f - %.6f\n", frequencia[i], bins[i], bins[i] + tamanho_bins))
}

# pegando o total de dados
total_dados = length(dados)

##################################3
# Calculando densidade
densidade = numeric(length(frequencia))

for (i in seq_along(frequencia)) {
  densidade[i] <- frequencia[i] / total_dados
}

## mostrando a densidade
for (i in seq(numero_bins)) {
  cat(sprintf("%.3f | %.6f - %.6f\n", densidade[i], bins[i], bins[i + 1]))
}

