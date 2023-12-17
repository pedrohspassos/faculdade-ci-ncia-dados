#Pedro Henrique
	A = read.delim("dados_A.txt", header=TRUE, sep=" ")
	B = read.delim("dados_B.txt", header=TRUE, sep=" ")
	
	# convertendo em números (coisa de R...)
	A = as.numeric(A[,1])
	B = as.numeric(B[,1])
	
	# aprendendo os parametros do modelo a partir dos dados
	muA = mean(A)
	sigmaA = sd(A)
	muB = mean(B)
	sigmaB = sd(B)
	
	# f = 1/sqrt(pi) * 1/sigma * exp( -1/2 * (x-mu)^2 / sigma^2 )
	f = function(x, mu, sigma){
  denominador = sqrt(2 * pi) * sigma
  expoente = -((x - mu)^2) / (2 * sigma^2)
  resultado = exp(expoente) / denominador
  return(resultado)
}
	minx = min(A,B)
	maxx = max(A,B)
	
	x = seq(minx, maxx, len=10000) # do menor até maior 10000 pontos
	yA = f(x, muA, sigmaA)
	yB = f(x, muB, sigmaB)

	# Probabilidade P(A > B)
	integral_AB = sum(pmax(yA - yB, 0))  # Somente os valores positivos
	integral_A = sum(yA)
	prob_A_maior_B = integral_AB / integral_A
	
	plot(x, yA, col="red") ; rug(A, col="red")
	points(x, yB, col="blue"); rug(B, col="blue")
	
	
	### agora 2D
	integral = 0
	Z = matrix(0, 1000, 1000)
	x = seq(min(A), max(A), len=1000)
	y = seq(min(B), max(B), len=1000)
	for(i in 1:1000){
		for(j in 1:1000){
			Z[i,j] = f(x[i],muA,sigmaA) * f(y[j],muB,sigmaB)
			if(x[i] > y[j]){ integral = integral + Z[i,j]  }
		}
	}

prob_A_maior_B = integral * diff(x[1:2]) * diff(y[1:2])

## ou 
integral = 0
# Montando a matriz 
for(i in 1:10000){
  for(j in 1:10000){
    Z[i,j] = f(x[i],muA,sigmaA) * f(y[j],muB,sigmaB)
    if ((f(x[i],muA,sigmaA) > f(y[j],muB,sigmaB ))){# A > B
      integral = integral + Z[i, j]
    } 
  } 
}

soma_total = sum(apply(Z, c(1, 2)))
probAmaiorB = integral / soma_total

	
# Criar o gráfico de contorno
niveis_contorno = seq(min(Z), max(Z), length.out = 20)
contour(x = unique(x), y = unique(y), z = matrix(Z, nrow = 1000), nlevels = length(niveis_contorno), levels = niveis_contorno, col = heat.colors(length(niveis_contorno)), xlab = 'X (Gaussiana A)', ylab = 'Y (Gaussiana B)', main = 'Gráfico de Contorno de Z (Produto de Gaussianas)')

# Adicionar barra de cores
color.legend(levels = niveis_contorno, col = heat.colors(length(niveis_contorno)), align = "rb")

# Adicionar rótulos
title(main = 'Gráfico de Contorno de Z (Produto de Gaussianas)')
xlabel('X (Gaussiana A)')
ylabel('Y (Gaussiana B)')
	
	
	
	
	
	
	
	
	
	
