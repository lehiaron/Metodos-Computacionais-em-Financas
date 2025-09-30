#-----------------------------------------------------------------------
# 1. CARREGAMENTO DOS PACOTES
#-----------------------------------------------------------------------

# Carrega a biblioteca 'quantmod' para obter dados de ações e índices do mercado financeiro.
library(quantmod)
# Carrega a biblioteca 'GetBCBData' para obter dados de séries temporais do Banco Central do Brasil (ex: Selic).
library(GetBCBData)


#-----------------------------------------------------------------------
# 2. OBTENÇÃO DOS DADOS
#-----------------------------------------------------------------------

# Baixa os dados históricos de PETR4.SA e do índice Bovespa (^BVSP) do Yahoo Finance
# para o período especificado.
getSymbols(c("PETR4.SA","^BVSP"),from = "2019-01-18", to="2021-01-20")

# Define as datas de início e fim para a busca de dados.
data_inicio = "2019-01-18"
data_final = "2021-01-20"

# Define o ID da série da taxa Selic no sistema do BCB.
my.id = c("Taxa de Juros - Selic" = 11)
# Busca a série histórica da taxa Selic diária no período especificado.
selic = gbcbd_get_series(my.id,last.date = data_final,first.date = data_inicio)


#-----------------------------------------------------------------------
# 3. TRATAMENTO E PREPARAÇÃO DOS DADOS
#-----------------------------------------------------------------------

# Extrai os preços de fechamento ajustados e remove valores ausentes (NA).
price_petr = na.omit(PETR4.SA$PETR4.SA.Adjusted)
price_mkt = na.omit(BVSP$BVSP.Adjusted)

# Converte a série da Selic para o formato xts (eXtensible Time Series) para alinhar com os outros dados.
rf = xts(selic$value,order.by = selic$ref.date)
# Converte a taxa de porcentagem para decimal (ex: 5% -> 0.05).
rf = rf / 100 
# Converte a taxa de juros simples diária para contínua (logarítmica), para consistência com os retornos.
rf = log(1+rf)

# Calcula os retornos logarítmicos diários para o ativo e para o mercado.
ret_petr = na.omit((diff(log(price_petr))))
ret_mkt = na.omit((diff(log(price_mkt))))

# Junta as séries de retornos em um único objeto, alinhando pelas datas.
retornos = na.omit(merge(ret_petr,ret_mkt))

# Calcula o "excesso de retorno" subtraindo a taxa livre de risco (rf) do retorno do ativo e do mercado.
# Este é um passo fundamental para o modelo CAPM.
excesso_ret_petr = retornos[,1] - rf 
excesso_ret_mkt = retornos[,2] - rf


#-----------------------------------------------------------------------
# 4. APLICAÇÃO DO MODELO CAPM (Capital Asset Pricing Model)
#-----------------------------------------------------------------------

# Roda uma regressão linear (lm) do excesso de retorno do ativo contra o excesso de retorno do mercado.
capm = lm(excesso_ret_petr ~ excesso_ret_mkt)
# O coeficiente 'beta' é a inclinação da reta de regressão, medindo a sensibilidade do ativo ao mercado.
beta = capm$coefficients[2]

# Calcula a taxa livre de risco anualizada (média da taxa diária * 252 dias úteis).
risk_free = as.numeric(mean(rf)*252)
# Calcula o retorno esperado (mu) anualizado para PETR4 usando a fórmula do CAPM:
# E(R) = Rf + beta * (E(Rm) - Rf)
mu_petr = as.numeric(risk_free + beta*mean(excesso_ret_mkt)*252)
# Calcula a volatilidade (sigma) anualizada de PETR4.
sigma_petr = sd(ret_petr)*sqrt(252)


#-----------------------------------------------------------------------
# 5. SIMULAÇÃO DE MONTE CARLO
#-----------------------------------------------------------------------

# Define a semente para garantir que os resultados aleatórios sejam reprodutíveis.
set.seed(123)

# Parâmetros da simulação
S0 = as.numeric(PETR4.SA$PETR4.SA.Adjusted[nrow(PETR4.SA),]) # Preço inicial do ativo (último preço da série).
T = 2               # Horizonte da simulação em anos.
dt = 1/252          # Passo de tempo (1 dia útil).
dias = 507          # Número total de dias a serem simulados (aprox. 2 anos).
paths = 10000       # Número de trajetórias de preço a serem simuladas.

# Calcula o componente de 'drift' para a simulação log-normal, usando o 'mu' do CAPM.
drift = (mu_petr - 0.5 * sigma_petr^2) * dt
# Calcula o componente de 'variância' (choque aleatório) para a simulação.
variance = sigma_petr * sqrt(dt)

# Cria uma matriz 'St' para armazenar os preços simulados, inicializada com S0.
St = matrix(S0, nrow = dias+1, ncol = paths)
# Cria uma matriz 'Zt' com números aleatórios de uma distribuição normal padrão.
Zt = matrix(rnorm(dias*paths), nrow = dias, ncol = paths)

# Loop para gerar as trajetórias de preço.
for (t in 2:(dias+1)) { 
  for(j in 1:paths) { 
    # Fórmula do Movimento Geométrico Browniano para o próximo preço.
    St[t,j] = St[t-1,j] * exp(drift + variance*Zt[t-1,j])  
  } 
}

# Extrai o preço final de cada uma das 10.000 trajetórias simuladas.
ST_final = St[508, ]


#-----------------------------------------------------------------------
# 6. CÁLCULO DO PAYOFF DO COE
#-----------------------------------------------------------------------
# A estrutura do COE é definida por 3 cenários baseados no preço final.

# Cenário 0: Preço final cai abaixo de R$ 8.50 (barreira de proteção de capital).
Caso0 = ST_final < 8.5
# Calcula a probabilidade do Cenário 0 (média de TRUEs e FALSEs).
P0 <- mean(Caso0)

# Cenário 2: Preço final sobe acima de 65% do valor da barreira (alta rentabilidade, travada).
Caso2 = ST_final > 8.5*1.65
# Calcula a probabilidade do Cenário 2.
P2 <- mean(Caso2)

# Cenário 1: O preço final fica entre as duas barreiras.
Caso1 = 1 - (P2 + P0) # A probabilidade é o que sobra.
# A linha abaixo é redundante, mas confirma o cálculo.
# P1 <- mean(Caso1)

# Cálculo do Payoff Esperado do COE, assumindo um investimento inicial de R$ 1000.

# Calcula o retorno total para cada trajetória simulada.
rET = ST_final/S0 - 1
# Filtra apenas os retornos que foram positivos.
Ret_positivo = rET[rET > 0]
# Dentre os positivos, filtra os que estão abaixo do limite de 65% (relevante para o Cenário 1).
Ret_final = Ret_positivo[Ret_positivo < 0.65]

# Calcula o Payoff esperado ponderando o resultado de cada cenário por sua probabilidade.
# Cenário 0: Recebe 1000 de volta (capital protegido).
# Cenário 1: Recebe 1000 * (1 + retorno médio das trajetórias que caíram neste cenário).
# Cenário 2: Recebe um valor fixo de 1210 (retorno de 21% travado).
Payoff = P0*1000 + (((1+mean(Ret_final))*Caso1)*1000) + P2*1210 
cat("Payoff Esperado: ", Payoff, "\n")

# Calcula o fator de retorno total e anualiza para encontrar a rentabilidade a.a.
# A operação (Payoff/1000) dá o fator de retorno total para 2 anos. A raiz quadrada anualiza.
RetAA <- (Payoff/1000)^(1/2) - 1
cat("Retorno Anualizado Esperado: ", paste0(round(RetAA*100, 2), "%"), "\n")