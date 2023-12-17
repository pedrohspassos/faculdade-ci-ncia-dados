#Pedro Henrique Da Silva Passos

#Primeira parte, verificando se a diferenca da ZERO
#Definido uma semente para os numeros pseudoaleatorios
set.seed(1234)

#Gerando cjt de dados aleatorios de acordo com uma distribuição normal
#Usando os mesmos parametos para analisar a diferença
A = rnorm(20, 8.0, 2.0)
B = rnorm(15, 8.0, 2.0)

mA = mean(A)
mB = mean(B)
sA = sd(A)
sB = sd(B)

#Verificando a estocasticidade

S = 1E6 # qtd de simulacoes	
D1 = (1/0)*(1:S) # inicializando vetor

for(i in 1:S){
  a = rnorm(20, mA, sA)
  b = rnorm(15, mB, sB)
  D1[i] = mean(a) - mean(b)
}
#Verificando se alguma diferença deu zero
count1=0
for (i in 1:S){
  if (D1[i] == 0){
    count1 = count1 + 1
  }
}
if (count1 == 0){
  print("Não houve caso em que a diferença entre as medias deu exatamente ZERO")
}else{
  print(count1)
}
#Em nenhum dos casos, a diferença deu exatamente zero
hist(D1, br=30)
abline(v=mean(D1),col = "red")
mean(D1)


############################################################

#Segunda parte usando a proposta t de Gosset:

#Primeiro para parametros distintos
set.seed(1234)

tamanho_X = 20
tamanho_Y = 15

#Aprendendo os parametros
X = rnorm(tamanho_X, 10.0, 1.5)
Y= rnorm(tamanho_Y, 8.0, 2.0)


mX = mean(X) # já aprendemos que é o melhor jeito de aprender
sX = sd(X)
mY = mean(Y) # já aprendemos que é o melhor jeito de aprender
sY = sd(Y)


#Verificando a estocasticidade

S = 1E6 # qtd de simulacoes	
D2 = (1/0)*(1:S) # inicializando vetor

for(i in 1:S){
  x = rnorm(tamanho_X, mX, sX)
  y = rnorm(tamanho_Y, mY, sY)
  
  #APLICANDO t de gosset = D2
  valor_dentro_raiz = ((mX^2)/ tamanho_X) + ((mY^2)/ tamanho_Y)
  raiz = sqrt(valor_dentro_raiz)
  peso = 1/raiz
  D2[i] = (mean(x)- mean(y)) * peso
    
}
h1=hist(D2, br=100)
hist(D2,br=100)
mean(D2)
#Usando o mesmo parametro

#Verificando a estocasticidade

S = 1E6 # qtd de simulacoes	
D2 = (1/0)*(1:S) # inicializando vetor

mXY = mY
sXY = sY

for(i in 1:S){
  X = rnorm(tamanho_X, mXY, sXY)
  Y = rnorm(tamanho_Y, mXY, sXY)
  
  #APLICANDO t de gosset = D2
  valor_dentro_raiz = ((mXY^2)/ tamanho_X) + ((mXY^2)/ tamanho_Y)
  raiz = sqrt(valor_dentro_raiz)
  peso = 1/raiz
  D2[i] = (mean(X)- mean(Y)) * peso
  
}

#Verificando se alguma diferença deu zero

count2=0
for (i in 1:S){
  if (D2[i] == 0){
    count2 = count2 + 1
  }
}
if (count2 == 0){
  print("Não houve caso em que a diferença entre as medias deu exatamente ZERO")
}else{
  print(count2)
}

h2=hist(D2, br=100)
hist(D2, br=100)

