#Pedro Passos
#library("mvtnorm")
k = 3
x = c(0.5, 0.5, 0.5)
mi = c(0, 100, 50)  # média
sigma = matrix(c(64.9, 33.2, -24.4,
                  33.2, 56.4, -24.1,
                  -24.4, -24.1, 75.6), nrow = 3, ncol = 3, byrow = TRUE)  # matriz de covariância

#Calculando PDF da funcao normal multivariada (multivariaet normal distribution) e testando a do pacote e a minha

# Minha Mvt
	f = function(x, mi, sigma,k){
		k = ncol(sigma)
		dentro = -1/2 * t(x-mi) %*% solve(sigma) %*% (x-mi)
		out = (2*pi)^(-k/2) * 1/sqrt(det(sigma)) * exp( dentro )
		return(out)
	}

minha_pdf = f(x, mi, sigma, k)
print(minha_pdf)  

#Pegando pacote da internet / #install.packages("mvtnorm") 
#library("mvtnorm")
pacote_pdf = dmvnorm(x,mi,sigma)
print(pacote_pdf)

# Fazendo grid

N = 100
	Z = matrix(0, ncol=N, nrow=N)
	x1 = x2 = seq(-3, 3, len=N) # x1 = x2 porque eu quero quadrado
	for(i in 1:N){
		for(j in 1:N){
			x = c(x1[i], x2[j]) # é o ponto do grid da vez
			Z[i,j] = f( x , m = mu, S = Sigma)
		}
	}
	
  #plotando
	image(x1, x2, Z)


# Simulando dados que seguem uma mvtnorm 3-d com parâmetros da minha cabeca
simulando = rmvnorm(10000,mi,sigma)

#mostrando que o sigma é a matriz de covariancia
sigmaEstimado = cov(simulando)

#mostrando que o mi é a media estimada
miEstimado = colMeans(simulando)

#Mostrando matrix correlacao

matrixCorrelacao = cor(simulando)


