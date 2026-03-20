# =============================================================================
# Quinto-cap - Generación de Datos de Transacciones con Fraude
# Detección de Fraude con Dataset Desbalanceado
# Autor: Isaac Esteban Haro Torres
# =============================================================================

set.seed(42)

# -----------------------------------------------------------------------------
# 1. GENERACIÓN DE DATOS DE TRANSACCIONES
# -----------------------------------------------------------------------------

generate_fraud_transactions <- function(n_transactions = 50000, fraud_rate = 0.015) {
  
  cat("💳 Generando", n_transactions, "transacciones con", 
      round(fraud_rate * 100, 2), "% de fraude...\n")
  
  # Categorías de comercios
  merchant_categories <- c("Grocery", "Gas Station", "Restaurant", "Online Shopping",
                           "Travel", "Entertainment", "Healthcare", "Electronics",
                           "Clothing", "Home Improvement")
  
  # Inicializar dataframe
  transactions <- data.frame(
    transaction_id = paste0("TXN", sprintf("%07d", 1:n_transactions)),
    stringsAsFactors = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # FEATURES DE USUARIO
  # ---------------------------------------------------------------------------
  
  transactions$user_id <- paste0("USER", sprintf("%05d", 
                                   sample(1:5000, n_transactions, replace = TRUE)))
  
  transactions$user_age <- round(rnorm(n_transactions, 42, 15))
  transactions$user_age <- pmax(18, pmin(80, transactions$user_age))
  
  transactions$account_age_days <- round(rexp(n_transactions, rate = 0.002))
  transactions$account_age_days <- pmax(1, pmin(3650, transactions$account_age_days))
  
  transactions$credit_limit <- round(rlnorm(n_transactions, meanlog = 8, sdlog = 0.8))
  transactions$credit_limit <- pmax(500, pmin(50000, transactions$credit_limit))
  
  transactions$avg_transaction <- round(rlnorm(n_transactions, meanlog = 4.5, sdlog = 0.7))
  
  # ---------------------------------------------------------------------------
  # FEATURES DE TRANSACCIÓN
  # ---------------------------------------------------------------------------
  
  transactions$amount <- round(rlnorm(n_transactions, meanlog = 3.5, sdlog = 1.2))
  transactions$amount <- pmax(1, pmin(10000, transactions$amount))
  
  transactions$merchant_category <- sample(merchant_categories, n_transactions, 
                                            replace = TRUE,
                                            prob = c(0.20, 0.12, 0.15, 0.18, 0.05,
                                                      0.10, 0.05, 0.05, 0.05, 0.05))
  
  transactions$transaction_hour <- sample(0:23, n_transactions, replace = TRUE,
                                           prob = c(1,1,1,1,1,1,2,3,5,7,8,9,10,9,8,7,6,5,5,6,7,6,4,2))
  
  transactions$day_of_week <- sample(c("Monday", "Tuesday", "Wednesday", "Thursday",
                                        "Friday", "Saturday", "Sunday"),
                                      n_transactions, replace = TRUE,
                                      prob = c(0.15, 0.15, 0.15, 0.15, 0.16, 0.12, 0.12))
  
  transactions$distance_from_home <- round(rexp(n_transactions, rate = 0.1), 1)
  transactions$distance_from_home <- pmin(500, transactions$distance_from_home)
  
  transactions$is_online <- sample(0:1, n_transactions, replace = TRUE, prob = c(0.45, 0.55))
  
  transactions$card_present <- if_else(transactions$is_online == 1, 0, 
                                        sample(0:1, n_transactions, replace = TRUE, 
                                               prob = c(0.2, 0.8)))
  
  # ---------------------------------------------------------------------------
  # FEATURES DE VELOCIDAD
  # ---------------------------------------------------------------------------
  
  transactions$velocity_1h <- rpois(n_transactions, lambda = 1.5)
  transactions$velocity_1h <- pmin(10, transactions$velocity_1h)
  
  transactions$velocity_24h <- rpois(n_transactions, lambda = 5)
  transactions$velocity_24h <- pmin(30, transactions$velocity_24h)
  
  transactions$deviation_from_avg <- round(
    (transactions$amount - transactions$avg_transaction) / 
      transactions$avg_transaction, 2
  )
  
  # ---------------------------------------------------------------------------
  # GENERACIÓN DE FRAUDE (con patrones realistas)
  # ---------------------------------------------------------------------------
  
  cat("   Generando patrones de fraude...\n")
  
  # Probabilidad base de fraude
  fraud_prob <- rep(fraud_rate, n_transactions)
  
  # Factores que incrementan probabilidad de fraude
  
  # 1. Monto muy alto comparado con el promedio
  high_amount <- transactions$deviation_from_avg > 2
  fraud_prob[high_amount] <- fraud_prob[high_amount] + 0.08
  
  # 2. Transacción online de alto valor
  high_value_online <- transactions$is_online == 1 & transactions$amount > 500
  fraud_prob[high_value_online] <- fraud_prob[high_value_online] + 0.12
  
  # 3. Múltiples transacciones en poco tiempo
  high_velocity <- transactions$velocity_1h > 5
  fraud_prob[high_velocity] <- fraud_prob[high_velocity] + 0.10
  
  # 4. Transacción en hora inusual (madrugada)
  unusual_hour <- transactions$transaction_hour %in% c(0, 1, 2, 3, 4, 5)
  fraud_prob[unusual_hour] <- fraud_prob[unusual_hour] + 0.03
  
  # 5. Distancia grande desde casa
  far_distance <- transactions$distance_from_home > 200
  fraud_prob[far_distance] <- fraud_prob[far_distance] + 0.05
  
  # 6. Cuenta nueva con transacción alta
  new_account_high <- transactions$account_age_days < 30 & transactions$amount > 300
  fraud_prob[new_account_high] <- fraud_prob[new_account_high] + 0.15
  
  # 7. Electrónica/joyería de alto valor
  electronics_high <- transactions$merchant_category %in% c("Electronics") & 
                       transactions$amount > 800
  fraud_prob[electronics_high] <- fraud_prob[electronics_high] + 0.08
  
  # 8. Sin tarjeta presente (CNP fraud)
  cnp_fraud <- transactions$card_present == 0 & transactions$amount > 200
  fraud_prob[cnp_fraud] <- fraud_prob[cnp_fraud] + 0.06
  
  # Limitar probabilidades
  fraud_prob <- pmax(0.001, pmin(0.95, fraud_prob))
  
  # Generar target de fraude
  transactions$is_fraud <- as.integer(runif(n_transactions) < fraud_prob)
  
  # Ajustar para que coincida aproximadamente con fraud_rate
  current_fraud_rate <- mean(transactions$is_fraud)
  if (current_fraud_rate > fraud_rate * 1.5) {
    # Reducir fraudes
    fraud_indices <- which(transactions$is_fraud == 1)
    keep_fraud <- sample(fraud_indices, size = round(n_transactions * fraud_rate))
    transactions$is_fraud <- 0
    transactions$is_fraud[keep_fraud] <- 1
  }
  
  # ---------------------------------------------------------------------------
  # FEATURES ADICIONALES PARA USUARIOS CON FRAUDE
  # ---------------------------------------------------------------------------
  
  # Los fraudes tienden a tener ciertas características
  fraud_idx <- which(transactions$is_fraud == 1)
  
  if (length(fraud_idx) > 0) {
    # Incrementar ligeramente monto para fraudes
    transactions$amount[fraud_idx] <- round(transactions$amount[fraud_idx] * 
                                              runif(length(fraud_idx), 1.2, 2.0))
    
    # Más probabilidad de ser online
    transactions$is_online[fraud_idx] <- sample(0:1, length(fraud_idx), 
                                                 replace = TRUE, prob = c(0.3, 0.7))
  }
  
  # Codificar día de semana
  day_encoding <- c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, 
                    "Thursday" = 4, "Friday" = 5, "Saturday" = 6, "Sunday" = 7)
  transactions$day_of_week_encoded <- day_encoding[transactions$day_of_week]
  
  # Codificar categoría de comercio
  category_list <- unique(transactions$merchant_category)
  transactions$merchant_category_encoded <- match(transactions$merchant_category, 
                                                   category_list)
  
  cat("   Transacciones generadas exitosamente!\n")
  
  return(transactions)
}

# -----------------------------------------------------------------------------
# 2. FUNCIÓN PRINCIPAL
# -----------------------------------------------------------------------------

generate_all_data <- function(n_transactions = 50000, fraud_rate = 0.015, 
                               output_dir = "data") {
  
  cat("🚨 Quinto-cap - Generando datos de transacciones...\n\n")
  
  # Crear directorio
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generar datos
  transactions <- generate_fraud_transactions(n_transactions, fraud_rate)
  
  # Guardar
  cat("\n💾 Guardando datos en", output_dir, "...\n")
  write.csv(transactions, file.path(output_dir, "transactions.csv"), row.names = FALSE)
  
  # Resumen
  cat("\n📈 RESUMEN DE DATOS GENERADOS:\n")
  cat("================================\n")
  cat("Total transacciones:", nrow(transactions), "\n")
  cat("Features:", ncol(transactions) - 2, "\n")
  cat("\nDistribución de clases:\n")
  cat("   Legítimas:", sum(transactions$is_fraud == 0), 
      "(", round(100 * (1 - mean(transactions$is_fraud)), 2), "%)\n")
  cat("   Fraudulentas:", sum(transactions$is_fraud == 1), 
      "(", round(100 * mean(transactions$is_fraud), 4), "%)\n")
  cat("\nRatio de desbalance:", 
      round(sum(transactions$is_fraud == 0) / sum(transactions$is_fraud == 1), 1), ":1\n")
  
  cat("\n✅ Datos generados exitosamente!\n")
  
  return(transactions)
}

# Ejecutar si es el script principal
if (!interactive()) {
  data <- generate_all_data()
}
