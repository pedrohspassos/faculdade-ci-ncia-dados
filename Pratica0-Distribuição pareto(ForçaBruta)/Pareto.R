# Pedro Henrique Da Silva Passos   
library(Pareto)

tam = 5000

#rPareto(qtd dados,theta, alpha) -> passados pelo criador
dados_pareto = rPareto(tam, t = 1000, alpha = 2.5)

  #calculos para facilitar a formula

logaritmo_dados = log(dados_pareto)

theta = min(dados_pareto) #encontrando o primeiro parametro da distribuição de pareto --> theta

subtracao = sum(logaritmo_dados)- (tam * log(theta))

# encotrado o segundo parametro da distribuição --> alpha
f = function(tamanho){
  return(tamanho/subtracao)
}

alpha = f(tam)

print(alpha, theta)

