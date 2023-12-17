#Pedro Passos (library (Pareto))
#FALTA ARRUMAR GRIDE E TESTAR GRAFICOS DA ALEATORIA - ERRO VALOR AUNSENTE NO IF

library(Pareto) #pacote
tamanho = 5000
dados_pareto = rPareto(tamanho, t=1000, alpha = 2.5)
theta = min(dados_pareto)
logaritmo_dados = log(dados_pareto)
subtracao = sum(logaritmo_dados)-(tamanho*log(theta))
alpha = tamanho / subtracao


#Inicio(criando os dados)
library(Pareto)
tamanho = 1000
dados_pareto = rPareto(tamanho, t=10, alpha = 2.5)
write.table(dados_pareto, file="dados_pareto.txt", col.names=FALSE)

#extraindo dados
#base dados 1 (theta=10, alfa=2.5)
dados = read.delim("dados_pareto.txt", header=FALSE, sep=" ")
dados = dados[,2]
#base dados 2 (theta=13.5, alfa = 2.5)

dados = read.delim("dadosPareto.txt", header=FALSE, sep=" ")
dados = as.numeric(dados[,1])


#facilitar a busca os valores (min)
dados = sort(dados)
tam = length(dados)

#pegando seq do tamanho dos dados (sera usado ppara gerar pds)
sequencia = seq(from = dados[1], to = dados[length(dados)], length.out = tam)

#1)pegando alguns parametros (parametros usados para criar base dados) 
theta = min(dados) #menor valor da base dados 
alfa = 2.5

#2)estimando os parametros a partir da base dados
theta_estimado =  min(dados)
#calculando alfa
logaritmo_dados = log(dados)
subtracao = sum(logaritmo_dados)-(tam*log(theta))
alfa_estimado = tam / subtracao

#3)pegando parametros aleatorios
set.seed(123)
valores_aleatorios_theta =  runif(100, min = 1.0, max = 250.0)
valores_aleatorios_alfa =runif(100, min = 1.0, max = 25.0)

melhor_L= 0
melhor_theta_aleatorio= NULL
melhor_alfa_aleatorio= NULL

#Gride (ARRUMAR)
for(possivel_alfa in valores_aleatorios_alfa){
  
  for(possivel_theta in valores_aleatorios_theta){
   
    #Calculando possivel melhor L
    produto = 1
    for(x in seq_len(length(dados))){
      
      produto = produto * (possivel_alfa * (possivel_theta^possivel_alfa)) / (dados[x]^(possivel_alfa + 1))
      
    }
  
    possivel_L = produto
    if( possivel_L > melhor_L){

      melhor_L = possivel_L
      melhor_theta_aleatorio = possivel_theta
      melhor_alfa_aleatorio = possivel_alfa
    }

  }
}
#Após o gride, tenho melhor alfa e melhor beta



#Aplicando PDF do Pareto

#1)Distribuicao Real
melhor_theta = 1000 
alfa = 2.5
distribuicao_real = (1/0)*(1:tam)
for (i in 1:tam){
  distribuicao_real[i] =  (alfa * (theta^alfa)) / (sequencia[i]^(alfa + 1))
   
}


#2)Distribuicao estimada
theta_estimado =  min(dados)
logaritmo_dados = log(dados)
subtracao = sum(logaritmo_dados)-(tam*log(theta))
alfa_estimado = tam / subtracao

distribuicao_estimada = (1/0)*(1:tam)
for (i in 1:tam){
  distribuicao_estimada[i] =  (alfa_estimado * (theta_estimado^alfa_estimado)) / (sequencia[i]^(alfa_estimado + 1))
   
}

#3)Distribuicao aletoria (gride)theta_estimado =  min(dados)


distribuicao_aleatoria = (1/0)*(1:tam)
for (i in 1:tam){
  distribuicao_aleatoria[i] =  (melhor_alfa_aleatorio * (melhor_theta_aleatorio^melhor_alfa_aleatorio)) / (sequencia[i]^(melhor_alfa_aleatorio + 1))
}



#Plotando

# Estimado
plot(sequencia, distribuicao_estimada, type='l', col='red', lty=1,
     xlab='x', ylab='Densidade de Probabilidade',
     main='Distribuição Estimada de Pareto',
     xlim=c(min(sequencia), max(sequencia)),
     ylim=c(0, max(distribuicao_estimada)),
     lwd=2,
     sub=paste('Estimada (alpha=', formatC(alfa_estimado, digits=2, format="f"), ', theta=', formatC(theta_estimado, digits=2, format="f"), ')'))

legend('topright', legend=paste('Estimada (alpha=', formatC(alfa_estimado, digits=2, format="f"), ', theta=', formatC(theta_estimado, digits=2, format="f"), ')'),
       col='red', lty=1, lwd=2)


#Real
plot(sequencia, distribuicao_real, type='l', col='blue', lty=1,
     xlab='x', ylab='Densidade de Probabilidade',
     main='Distribuição Real de Pareto',
     xlim=c(min(sequencia), max(sequencia)),
     ylim=c(0, max(distribuicao_real)),
     lwd=2,
     sub=paste('Real (alfa=', formatC(alfa, digits=2, format="f"), ', theta=', formatC(theta, digits=2, format="f"), ')'))

legend('topright', legend=paste('Real (alfa=', formatC(alfa, digits=2, format="f"), ', theta=', formatC(theta, digits=2, format="f"), ')'),
       col='blue', lty=1, lwd=2)


# Aleatoria
plot(sequencia, distribuicao_aleatoria, type='l', col='black', lty=1,
     xlab='x', ylab='Densidade de Probabilidade',
     main='Distribuição Aleatória de Pareto',
     xlim=c(min(sequencia), max(sequencia)),
     ylim=c(0, max(distribuicao_aleatoria)),
     lwd=2,
     sub=paste('Aleatória (alpha=', formatC(melhor_alfa_aleatorio, digits=2, format="f"), ', theta=', formatC(melhor_theta_aleatorio, digits=2, format="f"), ')'))

legend('topright', legend=paste('Aleatória (alpha=', formatC(melhor_alfa_aleatorio, digits=2, format="f"), ', theta=', formatC(melhor_theta_aleatorio, digits=2, format="f"), ')'),
       col='black', lty=1, lwd=2)

#Distribuicao sobrepostas

# Crie o gráfico com a distribuição estimada (estimada e real)
plot(sequencia, distribuicao_estimada, type='l', col='red', lty=1,
     xlab='x', ylab='Densidade de Probabilidade',
     main='Distribuição de Pareto',
     xlim=c(min(sequencia), max(sequencia)),
     ylim=c(0, max(max(distribuicao_estimada), max(distribuicao_real))),
     lwd=2,
     sub=paste('Estimada (alpha=', formatC(alfa_estimado, digits=2, format="f"), ', theta=', formatC(theta_estimado, digits=2, format="f"), ')'))

# Adicione a curva da distribuição real ao gráfico existente
lines(sequencia, distribuicao_real, type='l', col='blue', lty=1,
      lwd=2,
      sub=paste('Real (alpha=', formatC(alfa, digits=2, format="f"), ', theta=', formatC(theta, digits=2, format="f"), ')'))

# Adicione uma legenda ao gráfico
legend('topright', legend=c('Estimada', 'Real'),
       col=c('red', 'blue'), lty=1, lwd=2)

#(juncao estimada, real, aleatoria)

# Gráfico principal
plot(sequencia, distribuicao_estimada, type='l', col='red', lty=1,
     xlab='x', ylab='Densidade de Probabilidade',
     main='Distribuições de Pareto',
     xlim=c(min(sequencia), max(sequencia)),
     ylim=c(0, max(c(distribuicao_estimada, distribuicao_real, distribuicao_aleatoria))),
     lwd=2)

# Linha para a Distribuição Real
lines(sequencia, distribuicao_real, col='blue', lty=1, lwd=2)
# Linha para a Distribuição Aleatória
lines(sequencia, distribuicao_aleatoria, col='black', lty=1, lwd=2)

# Legenda
legend('topright', legend=c(
  paste('Estimada (alpha=', formatC(alfa_estimado, digits=2, format="f"), ', theta=', formatC(theta_estimado, digits=2, format="f"), ')'),
  paste('Real (alpha=', formatC(alfa, digits=2, format="f"), ', theta=', formatC(theta, digits=2, format="f"), ')'),
  paste('Aleatória (alpha=', formatC(melhor_alfa_aleatorio, digits=2, format="f"), ', theta=', formatC(melhor_theta_aleatorio, digits=2, format="f"), ')')
),
col=c('red', 'blue', 'black'), lty=1, lwd=2)
