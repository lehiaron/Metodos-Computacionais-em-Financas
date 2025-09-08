# Função para calcular o preço de uma opção de compra (call) Bermudana usando o modelo de árvore binomial.
#
# Args:
#   S0 (numeric): Preço inicial do ativo.
#   K (numeric): Preço de exercício (strike).
#   T (numeric): Tempo até o vencimento em anos.
#   r (numeric): Taxa de juros livre de risco.
#   sigma (numeric): Volatilidade do ativo.
#   N (integer): Número de passos na árvore binomial.
#   exercise_dates (vector): Um vetor de tempos (em anos) nos quais a opção pode ser exercida.
#
# Returns:
#   numeric: O preço da opção de compra Bermudana no tempo 0.
price_bermudan_call_binomial <- function(S0, K, T, r, sigma, N, exercise_dates) {
  
  # Tamanho de cada passo no tempo
  dt <- T / N
  
  # Parâmetros do modelo binomial (Cox-Ross-Rubinstein)
  u <- exp(sigma * sqrt(dt)) # Fator de alta
  d <- 1 / u                   # Fator de baixa
  p <- (exp(r * dt) - d) / (u - d) # Probabilidade risk-neutra de uma alta
  
  # 1. Inicializa os preços do ativo no vencimento (passo N)
  # Cria um vetor com todos os preços possíveis do ativo no final da árvore
  asset_prices <- S0 * d^(N:0) * u^(0:N)
  
  # 2. Calcula o valor da opção no vencimento (payoff)
  # pmax é o "parallel maximum", equivalente ao np.maximum
  option_values <- pmax(0, asset_prices - K)
  
  # 3. Converte as datas de exercício em passos de tempo correspondentes na árvore
  exercise_steps <- as.integer(round(exercise_dates / dt))
  
  # 4. Itera de trás para frente na árvore (do vencimento até o início)
  # O loop vai de N-1 até 0
  for (i in (N - 1):0) {
    
    # Preços do ativo neste passo de tempo
    asset_prices <- S0 * d^(i:0) * u^(0:i)
    
    # Calcula o valor de continuação (valor esperado da opção no próximo passo, descontado)
    # Note que R usa indexação baseada em 1, então os slices são [1:(i+1)] e [2:(i+2)]
    continuation_value <- exp(-r * dt) * (p * option_values[1:(i + 1)] + (1 - p) * option_values[2:(i + 2)])
    
    # Verifica se a data atual é uma data de exercício permitida
    if (i %in% exercise_steps) {
      # Se for, o valor da opção é o máximo entre exercer agora (valor intrínseco)
      # e esperar (valor de continuação).
      intrinsic_value <- pmax(0, asset_prices - K)
      option_values <- pmax(intrinsic_value, continuation_value)
    } else {
      # Se não for uma data de exercício, o valor é apenas o de continuação.
      option_values <- continuation_value
    }
  }
  
  # O preço da opção hoje é o primeiro valor no vetor de valores da opção
  return(option_values[1])
}

# --- Parâmetros do Problema ---
S0 <- 50.0      # Preço inicial do ativo: R$ 50
K <- 50.50      # Preço de exercício: R$ 50.50
T <- 3.0        # Vencimento: 3 anos
r <- 0.05       # Taxa livre de risco (rf): 5%
sigma <- 0.25   # Volatilidade (σ): 25%
N <- 3000       # Número de passos na árvore (para maior precisão)

# --- 1) Opção com Exercício Anual ---
# As datas de exercício são no final do ano 1, 2 e 3.
exercise_dates_yearly <- c(1.0, 2.0, 3.0)
price_yearly <- price_bermudan_call_binomial(S0, K, T, r, sigma, N, exercise_dates_yearly)

# --- 2) Opção com Exercício Mensal ---
# Gera um vetor de datas de exercício para cada final de mês ao longo de 3 anos.
# seq() cria uma sequência de 1/12 a 3, com passo de 1/12.
exercise_dates_monthly <- seq(from = 1/12, to = T, by = 1/12)
price_monthly <- price_bermudan_call_binomial(S0, K, T, r, sigma, N, exercise_dates_monthly)

# --- Apresentação dos Resultados ---
# A função cat() é usada para imprimir texto formatado no console.
cat("============================================================\n")
cat("Precificação de Opções de Compra (Call) Bermudanas\n")
cat("============================================================\n")
cat("\nParâmetros do Modelo:\n")
# sprintf() formata strings, similar ao f-string do Python
cat(sprintf("  - Preço Inicial do Ativo (S0): R$ %.2f\n", S0))
cat(sprintf("  - Preço de Exercício (K):    R$ %.2f\n", K))
cat(sprintf("  - Vencimento (T):            %.0f anos\n", T))
cat(sprintf("  - Taxa Livre de Risco (r):   %.2f%%\n", r * 100))
cat(sprintf("  - Volatilidade (σ):          %.2f%%\n", sigma * 100))
cat("------------------------------------------------------------\n")
cat("\nResultados:\n")
cat(sprintf("1) Preço da Opção com Exercício Anual:   R$ %.4f\n", price_yearly))
cat(sprintf("2) Preço da Opção com Exercício Mensal:  R$ %.4f\n", price_monthly))
cat("============================================================\n")
