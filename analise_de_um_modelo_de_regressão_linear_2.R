#ola nesse código analisamos uma regressão linear com parametros diferentes 


#Com os dados constantes da "tabela3", responda:

#Estime a fun��o: M=f(Y, r, P), sendo a demanda nominal por moeda (M); renda nacional real agregada (Y); 
# taxa de juros de longo prazo (r) e deflator de pre�o impl�cito (P, como uma medida geral do n�vel de pre�o). 
# Coloque no script do R a fun��o encontrada!

install.packages("readxl")
library(readxl)
base = read_excel('tabela3.xlsx')
attach(base)
modelo = lm(formula = M ~ Y + r + P )
summary(modelo)


#Interprete os coeficientes estimados e descreva a rela��o que existe entre as vari�veis 
#     explicatitiva e a vari�vel explicada. Considere o n�vel de 5% de signific�ncia. Explique!


#A equa��o mostra que o coeficiente B0(intercept). est�o acima do nivel de 5% de signific�ncia logo adotamos a h�potese nula que eles assumem valor igual a 0.
#A equa��o mostra que o coeficiente das v�riaveis r  e P. est�o acima do nivel de 5% de signific�ncia logo adotamos a h�potese nula que eles assumem valor igual a 0. 
# O pr da variavel Y � de 0.00032 ou seja dentro do padr�o de significancia do erro logo � significante.
#Assim assumimos a hip�tese alternativa que pelo menos um coeficiente � diferente que 0.


# 	Interprete o coeficiente de determina��o.Explique!

# O R-Squared do nosso modelo ficou entre aproximadamente 0.9546 , um coeficiente alto que  indica que o modelo explica a variabilidade dos dados de resposta ao redor de sua m�dia.

#  Considerando a regress�o e o teste de signific�ncia global F, deve-se aceitar ou rejeitar a seguinte hip�tese nula:
#    H0: de que os coeficientes estimados s�o conjuntamente nulos, em n�vel de 5% de signific�ncia. Explique.

#analisando o teste de significancia global com n�vel de significancia de 5%, o p-value fica entre 0,000000001437 . os coeficientes assumem valores diferentes de 0, nesse caso os coeficientes s�o diferente de 0,
# assim rejeitamos h0 e aceitamos a hipotese alternativa.



#  H� problema de multicolinearidade ou n�o no modelo encontrado? Explique aqui no script atrav�s dos testes ensinados. Quest�es sem explica��es n�o ser�o analisadas!

cor.test(base$Y,base$P)
# existe correla��o muito forte entre Y e P
cor.test(base$Y,base$r)
# existe correla��o muito forte entre Y e r
cor.test(base$r,base$P)
# existe correla��o forte entre P e r
#ou seja a multicolinearidade no modelo 

# H� problema de heteroscedasticidade ou n�o? Explique aqui no script atrav�s dos testes ensinados.

plot(modelo)
# o gr�fico apresenta uma forte dispers�o dos dados logo existe heterocscedacida

# Verifique atrav�s dos testes necess�rios se os res�duos possuem distribui��o normal e m�dia zero. Explique aqui no script.

modelo = lm(formula = M ~ Y + r + P )
base$estimado <- modelo$fitted.values
base$residuos <- modelo$residuals
base$residuos.padronizados =  rstandard(modelo)

hist(base$residuos.padronizados)
summary(base$residuos.padronizados)
summary(base$residuos)
plot(modelo)
# possuem residuos de media 0 ou envolta dela e a normalidade dos residuos foi confirmada no valor igual a 0. logo esse pressuposto n�o foi violado.


# Verifique atrav�s do teste de Durbin-Watson se h� autorrela��o entre os res�duos. Explique aqui no script.
install.packages("car")
modelo = lm(formula = M ~ Y + r + P )
library(car)
durbinWatsonTest(modelo)
# o durbin watson test � de 1.379794, menor que 1,5  logo h� autorrela��o entre os res�duos, violando mais um pressuposto.


