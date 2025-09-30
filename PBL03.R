#-----------------------------------------------------------------------
# 1. PREPARAÇÃO DO AMBIENTE E DADOS
#-----------------------------------------------------------------------

# Carrega a biblioteca 'quantmod', essencial para baixar e manipular dados financeiros.
library(quantmod)


# Define os tickers (códigos das ações) que serão analisados e baixa os dados históricos
# do Yahoo Finance. O período definido é a partir de 02 de fevereiro de 2020.
getSymbols(c("PETR4.SA", "VALE3.SA"), src = "yahoo", from = as.Date("2020-02-02"))


# Calcula os retornos diários para PETR4 e VALE3.
# O cálculo é feito usando a diferença dos logaritmos dos preços de fechamento ajustados.
# na.omit() remove o primeiro valor NA que surge do cálculo da diferença.
# CORREÇÃO 1: A fórmula para retornos simples foi corrigida de "- 2" para "- 1".
ret_petr4 <- na.omit(exp(diff(log(PETR4.SA$PETR4.SA.Adjusted))) - 1)
ret_vale3 <- na.omit(exp(diff(log(VALE3.SA$VALE3.SA.Adjusted))) - 1)

# Plota os retornos diários de cada ativo para inspeção visual.
plot(ret_petr4)
plot(ret_vale3)

#-----------------------------------------------------------------------
# 2. CÁLCULO DOS PARÂMETROS PARA A SIMULAÇÃO
#-----------------------------------------------------------------------

# Calcula o 'mu' (drift ou retorno esperado anualizado) para cada ação.
# CORREÇÃO 2: A fórmula de anualização foi corrigida para usar a base "+1" e subtrair "1",
# que é a abordagem padrão para retornos simples.
mu_petr4 <- (((1 + mean(ret_petr4))^252) - 1)
mu_vale3 <- (((1 + mean(ret_vale3))^252) - 1)

# Calcula o 'sigma' (volatilidade anualizada) para cada ação.
# Esta é a forma padrão: desvio padrão dos retornos diários multiplicado pela raiz quadrada de 252 (dias úteis no ano).
sigma_petr4 <- (sd(ret_petr4) * sqrt(252))
sigma_vale3 <- (sd(ret_vale3) * sqrt(252))

# Calcula a correlação entre os retornos diários de PETR4 e VALE3.
cor2 <- cor(ret_petr4, ret_vale3)

#-----------------------------------------------------------------------
# 3. SIMULAÇÃO DE MONTE CARLO (MÉTODO 1 - Corrigido)
#-----------------------------------------------------------------------

# Obtém o último preço de fechamento ajustado como o preço inicial (S0) para a simulação.
S0_petr4 <- as.numeric(last(PETR4.SA$PETR4.SA.Adjusted))
S0_vale3 <- as.numeric(last(VALE3.SA$VALE3.SA.Adjusted))

# Define uma semente para o gerador de números aleatórios para que os resultados sejam reprodutíveis.
set.seed(3)

# Define os parâmetros da simulação.
N = 20000  # Número de trajetórias (simulações) a serem geradas.
T = 252    # Horizonte de tempo em dias (1 ano útil).
dt = 1/T   # Passo de tempo padrão.

# Cria matrizes vazias para armazenar os preços simulados.
# CORREÇÃO 4: A estrutura da matriz foi ajustada para T+1 linhas.
petr4_matriz <- matrix(nrow = T + 1, ncol = N)
# O preço inicial S0 é definido na primeira linha, que é a abordagem padrão.
petr4_matriz[1, ] <- S0_petr4

vale3_matriz <- matrix(nrow = T + 1, ncol = N)
vale3_matriz[1, ] <- S0_vale3


# Loop principal da simulação, baseado no Movimento Geométrico Browniano.
# CORREÇÃO 4: O loop agora itera da segunda linha (t=2) até o final.
for (t in 2:(T + 1)){
  # CORREÇÃO 5: O loop interno agora cobre todas as N simulações, de 1 a N.
  for(j in 1:N){
    # Gera os choques aleatórios correlacionados.
    # CORREÇÃO 3: Implementação padrão para gerar variáveis aleatórias correlacionadas.
    epsilon_petr4 = rnorm(1, 0, 1)
    epsilon_vale3 = cor2 * epsilon_petr4 + sqrt(1 - cor2^2) * rnorm(1, 0, 1)

    # Equação de discretização do Movimento Geométrico Browniano para simular o próximo preço.
    # CORREÇÃO 4: A referência ao preço anterior foi corrigida para [t-1].
    petr4_matriz[t, j] = petr4_matriz[t-1, j] + mu_petr4 * petr4_matriz[t-1, j] * dt + sigma_petr4 * petr4_matriz[t-1, j] * epsilon_petr4 * sqrt(dt)
    vale3_matriz[t, j] = vale3_matriz[t-1, j] + mu_vale3 * vale3_matriz[t-1, j] * dt + sigma_vale3 * vale3_matriz[t-1, j] * epsilon_vale3 * sqrt(dt)
  }
}

# Calcula os retornos das séries de preços simuladas.
# CORREÇÃO 1: Usando a fórmula correta de cálculo de retornos.
retorno_petr4_sim <- na.omit(exp(diff(log(petr4_matriz))) - 1)
retorno_vale3_sim <- na.omit(exp(diff(log(vale3_matriz))) - 1)

#-----------------------------------------------------------------------
# 4. VERIFICAÇÃO DE SANIDADE (Sanity Check)
#-----------------------------------------------------------------------

# Esta seção verifica se a correlação foi preservada na simulação.
# CORREÇÃO 5: Usando um vetor para armazenar os resultados, que é mais simples e apropriado.
vetor_cor <- numeric(N)

# Loop para calcular a correlação entre PETR4 e VALE3 para cada uma das N simulações.
# O loop foi corrigido para iterar de 1 a N.
for(j in 1:N){
  vetor_cor[j] <- cor(retorno_vale3_sim[, j], retorno_petr4_sim[, j])
}

# Plota um histograma das correlações simuladas.
hist(vetor_cor)
# Calcula a média das correlações. O valor deve ser próximo ao 'cor2' original.
# Adicionado na.rm = TRUE para remover possíveis NAs caso alguma simulação não tenha variância.
mean(vetor_cor, na.rm = TRUE)
print(paste("Correlação Original:", cor2))
print(paste("Correlação Média da Simulação:", mean(vetor_cor, na.rm = TRUE)))

#-----------------------------------------------------------------------
# 5. CÁLCULO DE PROBABILIDADES
#-----------------------------------------------------------------------

# Pergunta 1: Probabilidade de PETR4 subir e VALE3 cair em 1 ano.

prob_q1 <- logical(N) # Vetor lógico é mais eficiente que c()
# CORREÇÃO 6: O loop foi ajustado para iterar de 1 a N.
for (i in 1:N) {
  # Verifica se no final do período (linha T+1), o preço de PETR4 é maior que o inicial
  # E se o preço de VALE3 é menor que o inicial.
  # A linha final é T+1, que corresponde a 253.
  if((petr4_matriz[T+1, i] > S0_petr4) & (vale3_matriz[T+1, i] < S0_vale3)){ # nolint
    prob_q1[i] = TRUE
  } else {
    prob_q1[i] = FALSE
  }
}

# A probabilidade é a média dos casos TRUE.
resp_q1 <- mean(prob_q1)
cat("Prob. de PETR4 subir e VALE3 cair em 1 ano:", resp_q1, "\n")


# Pergunta 2: Probabilidade do retorno de PETR4 ser 20% maior que o de VALE3.
prob_q2 <- logical(N)
for (i in 1:N) {
  # Verifica se o retorno total de PETR4 é >= 1.2 vezes o retorno total de VALE3.
  # CORREÇÃO 6: A referência ao preço inicial [1,i] agora está correta devido à
  # reestruturação da matriz.
  if((petr4_matriz[T+1, i] / petr4_matriz[1, i]) >= 1.2 * (vale3_matriz[T+1, i] / vale3_matriz[1, i])){
    prob_q2[i] = TRUE
  } else {
    prob_q2[i] = FALSE
  }
}

# Calcula e imprime a probabilidade.
resp_q2 <- mean(prob_q2)
cat("Prob. do retorno de PETR4 ser 20% maior que o de VALE3:", resp_q2, "\n")


#-----------------------------------------------------------------------
# 6. SIMULAÇÃO DE MONTE CARLO (MÉTODO 2) E PERGUNTA 3
#-----------------------------------------------------------------------

# Reconfigura os parâmetros para uma nova simulação de 3 anos.
N = 1000   # 1000 simulações.
T = 756    # 756 dias (aprox. 3 anos úteis, 252 * 3).
dt = 1/T   # Passo de tempo.

# Reinicia as matrizes com as novas dimensões e com o preço inicial na primeira linha (padrão).
vale3_matriz <- matrix(nrow = T + 1, ncol = N)
vale3_matriz[1, ] <- S0_vale3

petr4_matriz <- matrix(nrow = T + 1, ncol = N)
petr4_matriz[1, ] <- S0_petr4

# Loop de simulação. Esta implementação já estava mais próxima do padrão.
for (t in 2:(T + 1)){
  for(j in 1:N){
    # Geração de choques correlacionados (já estava correto aqui).
    epsilon_petr4 = rnorm(1, 0, 1)
    epsilon_vale3 = cor2 * epsilon_petr4 + sqrt(1 - cor2^2) * rnorm(1, 0, 1)

    # Equação do Movimento Geométrico Browniano.
    petr4_matriz[t, j] = petr4_matriz[t-1, j] + mu_petr4 * petr4_matriz[t-1, j] * dt + sigma_petr4 * petr4_matriz[t-1, j] * epsilon_petr4 * sqrt(dt)
    vale3_matriz[t, j] = vale3_matriz[t-1, j] + mu_vale3 * vale3_matriz[t-1, j] * dt + sigma_vale3 * vale3_matriz[t-1, j] * epsilon_vale3 * sqrt(dt)
  }
}

# Pergunta 3: Probabilidade de ambas as ações subirem a cada ano, por 3 anos.
prob_q3 <- logical(N)
for (i in 1:N) {
  # Condição para verificar se os preços no final de cada ano são maiores que os do ano anterior. # nolint
  # Ano 1: Preço[dia 252] > Preço Inicial (índice 252+1=253)
  # Ano 2: Preço[dia 504] > Preço[dia 252] (índice 504+1=505)
  # Ano 3: Preço[dia 756] > Preço[dia 504] (índice 756+1=757)
  cond_ano1 <- (petr4_matriz[253, i] > S0_petr4) & (vale3_matriz[253, i] > S0_vale3) # nolint
  cond_ano2 <- (petr4_matriz[505, i] > petr4_matriz[253, i]) & (vale3_matriz[505, i] > vale3_matriz[253, i]) # nolint
  cond_ano3 <- (petr4_matriz[757, i] > petr4_matriz[505, i]) & (vale3_matriz[757, i] > vale3_matriz[505, i]) # nolint

  if(cond_ano1 & cond_ano2 & cond_ano3){ # nolint
    prob_q3[i] = TRUE
  } else {
    prob_q3[i] = FALSE
  }
}

# Calcula e imprime a probabilidade.
resp_q3 <- mean(prob_q3)
cat("Prob. de ambas subirem a cada ano por 3 anos:", resp_q3, "\n")