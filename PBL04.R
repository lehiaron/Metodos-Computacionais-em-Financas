# Limpa o ambiente de trabalho para começar do zero
rm(list = ls())

# --------------------------------------------------------------------------
# 1. DEFINIÇÃO DOS PARÂMETROS DO MERCADO E DA OPÇÃO
# --------------------------------------------------------------------------
S0 <- 40      # Preço inicial do ativo (R$ 40)
K <- 40       # Preço de exercício (Strike, R$ 40)
T <- 0.5      # Tempo até a maturidade (6 meses = 0.5 anos)
rf <- 0.05    # Taxa de juros livre de risco (5% a.a.)
sigma <- 0.25 # Volatilidade do ativo (25% a.a.)
mu <- 0.10    # Drift real do ativo (10% a.a.) - Usado apenas para fins de contexto.
              # Para precificação (risco-neutro), usamos rf.

# Parâmetros para a simulação de Monte Carlo
N <- 1000000  # Número de simulações (um número alto para maior precisão)


# --------------------------------------------------------------------------
# 2. CÁLCULO PELO MODELO DE BLACK-SCHOLES
# --------------------------------------------------------------------------
# A fórmula de Black-Scholes é um padrão da indústria para precificar opções.

black_scholes <- function(S, K, T, r, sigma, option_type = "call") {
  #' Calcula o preço de uma opção europeia usando a fórmula de Black-Scholes.
  #'
  #' @param S Preço atual do ativo
  #' @param K Preço de exercício (strike)
  #' @param T Tempo até a maturidade (em anos)
  #' @param r Taxa de juros livre de risco
  #' @param sigma Volatilidade do ativo
  #' @param option_type 'call' para opção de compra, 'put' para opção de venda
  
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (option_type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (option_type == "put") {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Tipo de opção inválido. Use 'call' ou 'put'.")
  }
  
  return(price)
}

# Calculando os preços de Call e Put usando Black-Scholes
bs_call_price <- black_scholes(S0, K, T, rf, sigma, option_type = "call")
bs_put_price <- black_scholes(S0, K, T, rf, sigma, option_type = "put")


# --------------------------------------------------------------------------
# 3. CÁLCULO PELA SIMULAÇÃO DE MONTE CARLO
# --------------------------------------------------------------------------
# A simulação de Monte Carlo estima o preço da opção simulando milhares de
# possíveis preços futuros do ativo e calculando o payoff médio.

monte_carlo_option_price <- function(S, K, T, r, sigma, N, option_type = "call") {
  #' Calcula o preço de uma opção europeia usando a Simulação de Monte Carlo.
  #'
  #' @param S Preço atual do ativo
  #' @param K Preço de exercício (strike)
  #' @param T Tempo até a maturidade (em anos)
  #' @param r Taxa de juros livre de risco (usada como drift)
  #' @param sigma Volatilidade do ativo
  #' @param N Número de simulações
  #' @param option_type 'call' para opção de compra, 'put' para opção de venda

  # Gera N números aleatórios de uma distribuição normal padrão
  Z <- rnorm(N, mean = 0, sd = 1)
  
  # Simula os preços finais do ativo (ST) usando o Movimento Browniano Geométrico
  # ST = S0 * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)
  ST <- S * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)
  
  # Calcula o payoff para cada preço simulado.
  # pmax é a versão vetorizada da função max().
  if (option_type == "call") {
    payoff <- pmax(ST - K, 0)
  } else if (option_type == "put") {
    payoff <- pmax(K - ST, 0)
  } else {
    stop("Tipo de opção inválido. Use 'call' ou 'put'.")
  }
  
  # Calcula o preço da opção como a média dos payoffs trazida a valor presente
  price <- exp(-r * T) * mean(payoff)
  
  return(price)
}

# Para garantir que os resultados sejam reproduzíveis, definimos uma semente aleatória.
set.seed(42)
mc_call_price <- monte_carlo_option_price(S0, K, T, rf, sigma, N, option_type = "call")
# Re-semeamos para obter um resultado de put diferente, mas ainda reprodutível. 
# Ou podemos usar o mesmo Z para put e call, o que é mais eficiente. 
# Para manter a lógica idêntica à do código Python original, vamos chamar a função de novo.
set.seed(43) # Usando uma semente diferente para a put, para não usar o mesmo Z
mc_put_price <- monte_carlo_option_price(S0, K, T, rf, sigma, N, option_type = "put")


# --------------------------------------------------------------------------
# 4. EXIBIÇÃO DOS RESULTADOS
# --------------------------------------------------------------------------
# A função cat() é usada para imprimir texto formatado no console.

cat("--- Precificação de Opções Europeias ---\n")
cat(sprintf("Parâmetros: S0=%g, K=%g, T=%g anos, rf=%.2f%%, sigma=%.2f%%\n\n", 
            S0, K, T, rf*100, sigma*100))

cat("--- 1) Opção de Compra Europeia (Call) ---\n")
cat(sprintf("Preço pela fórmula de Black-Scholes: R$ %.4f\n", bs_call_price))
cat(sprintf("Preço pela simulação de Monte Carlo (%s simulações): R$ %.4f\n\n", 
            format(N, big.mark = ","), mc_call_price))

cat("--- 2) Opção de Venda Europeia (Put) ---\n")
cat(sprintf("Preço pela fórmula de Black-Scholes: R$ %.4f\n", bs_put_price))
cat(sprintf("Preço pela simulação de Monte Carlo (%s simulações): R$ %.4f\n", 
            format(N, big.mark = ","), mc_put_price))