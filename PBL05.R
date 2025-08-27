#-----------------------------------------------------------------------
# 1. CONFIGURAÇÃO INICIAL E PARÂMETROS
#-----------------------------------------------------------------------

# Instalar e carregar pacotes necessários
# install.packages("quantmod")
# install.packages("lubridate")
library(quantmod)
library(lubridate)

# Parâmetros dos Ativos e do Mercado
tickers <- c("PETR4.SA", "VALE3.SA")
weights <- c(0.5, 0.5)

# Parâmetros da Opção
K <- 40       # Preço de exercício (Strike)
T <- 1/12     # Maturidade em anos (1 mês)

# Taxa de juros livre de risco (Selic). Usaremos uma taxa aproximada de 10.5% a.a.
# É importante usar uma taxa atual para maior precisão.
risk_free_rate <- 0.105 

# Parâmetros da Simulação de Monte Carlo
n_simulations <- 100000 # Número de simulações (quanto maior, mais preciso)
set.seed(123)           # Para reprodutibilidade dos resultados

#-----------------------------------------------------------------------
# 2. COLETA E PREPARAÇÃO DE DADOS
#-----------------------------------------------------------------------
cat("Baixando dados históricos...\n")

end_date <- today()
start_date <- end_date - years(1)

# Usar a função getSymbols do quantmod para baixar os dados
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

# Combinar os preços de fechamento ajustados em um único objeto
prices <- merge(Ad(PETR4.SA), Ad(VALE3.SA))
prices <- na.omit(prices) # Remover dias em que um dos ativos não foi negociado

# Calcular os retornos logarítmicos diários
log_returns <- diff(log(prices), lag = 1)
log_returns <- na.omit(log_returns)

colnames(log_returns) <- c("PETR4_ret", "VALE3_ret")

#-----------------------------------------------------------------------
# 3. CALIBRAÇÃO (Cálculo dos parâmetros estatísticos)
#-----------------------------------------------------------------------
cat("Calibrando os parâmetros a partir dos dados...\n")

# Média dos retornos diários
mean_daily_returns <- colMeans(log_returns)

# Volatilidades diárias (desvio padrão dos retornos)
vol_daily <- apply(log_returns, 2, sd)

# Anualizar as volatilidades (multiplicar pelo sqrt do número de dias úteis no ano)
trading_days <- 252
vol_annual <- vol_daily * sqrt(trading_days)

# Calcular a correlação entre os retornos dos ativos
correlation <- cor(log_returns)[1, 2]

# Preços iniciais (último preço de fechamento disponível)
S0_petr <- as.numeric(last(prices[,1]))
S0_vale <- as.numeric(last(prices[,2]))

# Imprimir parâmetros calculados
cat("\n--- Parâmetros Calibrados ---\n")
cat(paste("Preço Inicial PETR4 (S0):", round(S0_petr, 2), "\n"))
cat(paste("Preço Inicial VALE3 (S0):", round(S0_vale, 2), "\n"))
cat(paste("Volatilidade Anualizada PETR4:", scales::percent(vol_annual[1]), "\n"))
cat(paste("Volatilidade Anualizada VALE3:", scales::percent(vol_annual[2]), "\n"))
cat(paste("Correlação entre PETR4 e VALE3:", round(correlation, 4), "\n"))
cat("-----------------------------\n\n")

#-----------------------------------------------------------------------
# 4. PRECIFICAÇÃO VIA SIMULAÇÃO DE MONTE CARLO
#-----------------------------------------------------------------------
cat("Iniciando a simulação de Monte Carlo...\n")

# Gerar números aleatórios normais correlacionados
# Z1 e Z2 são variáveis normais padrão independentes
Z1 <- rnorm(n_simulations, 0, 1)
Z2 <- rnorm(n_simulations, 0, 1)

# Criar uma variável correlacionada para o segundo ativo
# Z_petr será Z1
# Z_vale será uma combinação de Z1 e Z2 para ter a correlação desejada
Z_petr <- Z1
Z_vale <- correlation * Z1 + sqrt(1 - correlation^2) * Z2

# Simular os preços no vencimento (ST) para cada ativo
# Usamos a fórmula do Movimento Browniano Geométrico sob a medida risco-neutro
# O drift (deriva) é a taxa livre de risco 'r'
ST_petr <- S0_petr * exp((risk_free_rate - 0.5 * vol_annual[1]^2) * T + vol_annual[1] * sqrt(T) * Z_petr)
ST_vale <- S0_vale * exp((risk_free_rate - 0.5 * vol_annual[2]^2) * T + vol_annual[2] * sqrt(T) * Z_vale)

# Calcular o valor da cesta no vencimento para cada simulação
ST_basket <- weights[1] * ST_petr + weights[2] * ST_vale

# Calcular o payoff da call para cada simulação: max(ST_cesta - K, 0)
payoffs <- pmax(ST_basket - K, 0)

# O preço da opção é a média dos payoffs descontada a valor presente
monte_carlo_price <- mean(payoffs) * exp(-risk_free_rate * T)

#-----------------------------------------------------------------------
# 5. PRECIFICAÇÃO VIA BLACK-SCHOLES (APROXIMAÇÃO PARA A CESTA)
#-----------------------------------------------------------------------
cat("Calculando o preço via fórmula de Black-Scholes para a cesta...\n")

# Preço inicial da cesta
S0_basket <- weights[1] * S0_petr + weights[2] * S0_vale

# Calcular a volatilidade da cesta
# A variância da cesta é: w1^2*sd1^2 + w2^2*sd2^2 + 2*w1*w2*sd1*sd2*corr
var_basket <- (weights[1]^2 * vol_annual[1]^2) + 
              (weights[2]^2 * vol_annual[2]^2) + 
              (2 * weights[1] * weights[2] * vol_annual[1] * vol_annual[2] * correlation)
vol_basket <- sqrt(var_basket)

# Função para a fórmula de Black-Scholes
black_scholes_call <- function(S, K, T, r, sigma) {
  d1 <- (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(price)
}

# Calcular o preço usando os parâmetros da cesta
black_scholes_price <- black_scholes_call(S0_basket, K, T, risk_free_rate, vol_basket)

#-----------------------------------------------------------------------
# 6. RESULTADOS
#-----------------------------------------------------------------------
cat("\n--- RESULTADOS DA PRECIFICAÇÃO ---\n")
cat(paste("Preço da Opção (Monte Carlo): R$", round(monte_carlo_price, 4), "\n"))
cat(paste("Preço da Opção (Black-Scholes para Cesta): R$", round(black_scholes_price, 4), "\n"))
cat("------------------------------------\n")