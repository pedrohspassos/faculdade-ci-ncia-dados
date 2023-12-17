 library(Pareto)

 tamanho = 5000
 dados_pareto = rPareto(tamanho, t=1000, alpha = 2.5)
 theta = min(dados_pareto)
 logaritmo_dados = log(dados_pareto)
 subtracao = sum(logaritmo_dados)-(tamanho*log(theta))
 alpha = tamanho / subtracao
 alpha
 theta

 # forca bruta
 
 for (i in 1:100){
   for (j in 1:1000){
     Z[i,j] = L(param1[i], param2[i], Dados)
     if(Z[i,j] > melhorateagora){
       melhori = i
       melhorj = j
     }
   }
 }