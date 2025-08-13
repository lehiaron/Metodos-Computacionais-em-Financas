# ==============================================================================
# SEÇÃO DE PARAMETRIZAÇÃO
# ==============================================================================
# Parâmetros gerais da simulação
P0 <- 100.0       # Preço inicial do ativo
T <- 1.0          # Tempo total da simulação em anos
N_STEPS <- 252    # Número de passos (dias úteis em um ano)
N_SIMS <- 100     # Número de simulações (trajetórias de preço)
dt <- T / N_STEPS # Tamanho do passo de tempo

# Parâmetros específicos dos modelos
mu <- 0.20        # Drift (retorno esperado anual)
sigma <- 0.30     # Volatilidade anual

# Parâmetros para modelos específicos
P_bar <- 110.0    # Nível de reversão à média (para o Processo 3)
p_jump <- 0.02    # Probabilidade de um salto em um passo de tempo (para o Processo 5)
sigma_j <- 0.50   # Volatilidade do tamanho do salto (para o Processo 5)

# ==============================================================================
# FUNÇÕES DE SIMULAÇÃO
# ==============================================================================

simulate_process_1 <- function(p0, n_steps, n_sims, dt, mu, sigma) {
  # 1) ΔP(t+1) = μΔt + σN(0, Δt) - Movimento Browniano Aritmético
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_sims)
  prices[1, ] <- p0
  
  for (i in 2:(n_steps + 1)) {
    random_shock <- rnorm(n_sims, mean = 0, sd = sqrt(dt))
    delta_p <- mu * dt + sigma * random_shock
    prices[i, ] <- prices[i-1, ] + delta_p
  }
  return(prices)
}

simulate_process_2 <- function(p0, n_steps, n_sims, dt, mu, sigma) {
  # 2) ΔP(t+1) = μP(t)Δt + σP(t)N(0, Δt) - Movimento Browniano Geométrico
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_sims)
  prices[1, ] <- p0
  
  for (i in 2:(n_steps + 1)) {
    random_shock <- rnorm(n_sims, mean = 0, sd = sqrt(dt))
    delta_p <- mu * prices[i-1, ] * dt + sigma * prices[i-1, ] * random_shock
    prices[i, ] <- prices[i-1, ] + delta_p
  }
  return(prices)
}

simulate_process_3 <- function(p0, n_steps, n_sims, dt, mu, p_bar, sigma) {
  # 3) ΔP(t+1) = μ(P_bar - P(t))Δt + σP(t)N(0, Δt) - Reversão à Média
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_sims)
  prices[1, ] <- p0
  
  for (i in 2:(n_steps + 1)) {
    random_shock <- rnorm(n_sims, mean = 0, sd = sqrt(dt))
    drift <- mu * (p_bar - prices[i-1, ]) * dt
    volatility_term <- sigma * prices[i-1, ] * random_shock
    delta_p <- drift + volatility_term
    prices[i, ] <- prices[i-1, ] + delta_p
  }
  return(prices)
}

simulate_process_4 <- function(p0, n_steps, n_sims, dt, mu, sigma) {
  # 4) ΔP(t+1) = μP(t)Δt + σP(t)N(ΔP(t)/P(t-1), Δt) - Modelo com 'Memória'
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_sims)
  prices[1, ] <- p0
  
  # Para o primeiro passo, não há P(t-1), então o retorno anterior é 0
  prices[2, ] <- prices[1, ] + (mu * prices[1, ] * dt + sigma * prices[1, ] * rnorm(n_sims, 0, sqrt(dt)))
  
  if (n_steps > 1) {
    for (i in 3:(n_steps + 1)) {
      prev_return <- (prices[i-1, ] - prices[i-2, ]) / prices[i-2, ]
      random_shock <- rnorm(n_sims, mean = prev_return, sd = sqrt(dt))
      delta_p <- mu * prices[i-1, ] * dt + sigma * prices[i-1, ] * random_shock
      prices[i, ] <- prices[i-1, ] + delta_p
    }
  }
  return(prices)
}

simulate_process_5 <- function(p0, n_steps, n_sims, dt, mu, sigma, p_jump, sigma_j) {
  # 5) ΔP(t+1) = μP(t)Δt + σP(t)N1(0,Δt) + σ_j*Bern(p)*P(t)*N2(0,Δt) - Difusão com Salto
  prices <- matrix(0, nrow = n_steps + 1, ncol = n_sims)
  prices[1, ] <- p0
  
  for (i in 2:(n_steps + 1)) {
    diffusion_shock <- rnorm(n_sims, 0, sqrt(dt))
    jump_trigger <- rbinom(n_sims, size = 1, prob = p_jump)
    jump_size_shock <- rnorm(n_sims, 0, sqrt(dt))
    
    diffusion_term <- sigma * prices[i-1, ] * diffusion_shock
    jump_term <- sigma_j * jump_trigger * prices[i-1, ] * jump_size_shock
    
    delta_p <- mu * prices[i-1, ] * dt + diffusion_term + jump_term
    prices[i, ] <- prices[i-1, ] + delta_p
  }
  return(prices)
}

plot_simulation <- function(prices, title) {
  # Função para plotar os resultados da simulação.
  time_vector <- seq(0, T, length.out = N_STEPS + 1)
  
  # Usa matplot para plotar todas as simulações de uma vez
  matplot(x = time_vector, y = prices,
          type = "l",       # 'l' for lines
          lty = 1,          # solid line type
          col = rgb(0.2, 0.4, 0.8, alpha = 0.5), # cor azul com transparência
          main = title,
          xlab = "Tempo (Anos)",
          ylab = "Preço do Ativo",
          panel.first = grid()) # Adiciona um grid ao fundo
}

# ==============================================================================
# EXECUÇÃO PRINCIPAL
# ==============================================================================

# Simulação e plotagem para cada processo
prices_1 <- simulate_process_1(P0, N_STEPS, N_SIMS, dt, mu, sigma)
plot_simulation(prices_1, "1) Movimento Browniano Aritmético")

prices_2 <- simulate_process_2(P0, N_STEPS, N_SIMS, dt, mu, sigma)
plot_simulation(prices_2, "2) Movimento Browniano Geométrico")

prices_3 <- simulate_process_3(P0, N_STEPS, N_SIMS, dt, mu, P_bar, sigma)
plot_simulation(prices_3, "3) Processo de Reversão à Média")

prices_4 <- simulate_process_4(P0, N_STEPS, N_SIMS, dt, mu, sigma)
plot_simulation(prices_4, "4) Processo com 'Memória' no Choque Aleatório")

prices_5 <- simulate_process_5(P0, N_STEPS, N_SIMS, dt, mu, sigma, p_jump, sigma_j)
plot_simulation(prices_5, "5) Processo de Difusão com Salto")