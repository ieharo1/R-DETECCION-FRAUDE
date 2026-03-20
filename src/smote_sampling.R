# =============================================================================
# Quinto-cap - SMOTE y Técnicas de Muestreo
# Detección de Fraude con Dataset Desbalanceado
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(DMW2)
library(dplyr)

# -----------------------------------------------------------------------------
# 1. ANÁLISIS DE DESBALANCE
# -----------------------------------------------------------------------------

analyze_imbalance <- function(data, target_col = "is_fraud") {
  
  cat("📊 Analizando desbalance de clases...\n")
  
  if (!target_col %in% names(data)) {
    cat("   Error: Columna target no encontrada.\n")
    return(NULL)
  }
  
  class_counts <- table(data[[target_col]])
  total <- sum(class_counts)
  
  majority_class <- names(class_counts)[which.max(class_counts)]
  minority_class <- names(class_counts)[which.min(class_counts)]
  
  majority_count <- class_counts[majority_class]
  minority_count <- class_counts[minority_class]
  
  imbalance_ratio <- majority_count / minority_count
  
  cat("\n   Distribución de clases:\n")
  cat("      Mayoritaria (", majority_class, "): ", majority_count, 
      " (", round(100 * majority_count / total, 2), "%)\n", sep = "")
  cat("      Minoritaria (", minority_class, "): ", minority_count, 
      " (", round(100 * minority_count / total, 4), "%)\n", sep = "")
  cat("\n   Ratio de desbalance: ", round(imbalance_ratio, 1), ":1\n", sep = "")
  
  # Métricas de desbalance
  imbalance_metrics <- list(
    total = total,
    majority_class = majority_class,
    majority_count = majority_count,
    minority_class = minority_class,
    minority_count = minority_count,
    imbalance_ratio = imbalance_ratio,
    minority_percentage = round(100 * minority_count / total, 4)
  )
  
  return(imbalance_metrics)
}

# -----------------------------------------------------------------------------
# 2. SMOTE - Synthetic Minority Over-sampling Technique
# -----------------------------------------------------------------------------

apply_smote <- function(data, target_col = "is_fraud", 
                         perc_over = 200, perc_under = 300, k = 5) {
  
  cat("\n🔄 Aplicando SMOTE...\n")
  cat("   Over-sampling:", perc_over, "%\n")
  cat("   Under-sampling:", perc_under, "%\n")
  cat("   k vecinos:", k, "\n")
  
  # Separar features y target
  target_idx <- which(names(data) == target_col)
  
  if (length(target_idx) == 0) {
    cat("   Error: Columna target no encontrada.\n")
    return(data)
  }
  
  # SMOTE requiere solo variables numéricas
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, target_col)
  
  data_numeric <- data[, numeric_cols, drop = FALSE]
  data_numeric[[target_col]] <- data[[target_col]]
  
  # Aplicar SMOTE
  tryCatch({
    data_smote <- SMOTE(
      as.formula(paste(target_col, "~ .")),
      data = data_numeric,
      perc.over = perc_over,
      perc.under = perc.under,
      k = k
    )
    
    cat("   Registros originales:", nrow(data), "\n")
    cat("   Registros después de SMOTE:", nrow(data_smote), "\n")
    
    # Verificar nueva distribución
    new_dist <- table(data_smote[[target_col]])
    cat("   Nueva distribución:\n")
    cat("      Clase 0:", new_dist[1], "(", round(100 * new_dist[1] / sum(new_dist), 2), "%)\n")
    cat("      Clase 1:", new_dist[2], "(", round(100 * new_dist[2] / sum(new_dist), 2), "%)\n")
    
    return(data_smote)
    
  }, error = function(e) {
    cat("   Error aplicando SMOTE:", e$message, "\n")
    cat("   Usando alternative approach...\n")
    
    # Fallback: oversampling simple
    minority_data <- data[data[[target_col]] == 1, ]
    majority_data <- data[data[[target_col]] == 0, ]
    
    # Oversample minoritaria
    n_majority <- nrow(majority_data)
    n_minority <- nrow(minority_data)
    
    # Repetir minoritaria hasta equilibrar
    repeat_factor <- ceiling(n_majority / n_minority)
    minority_oversampled <- minority_data[rep(1:n_minority, repeat_factor), ]
    minority_oversampled <- minority_oversampled[1:n_majority, ]
    
    data_balanced <- rbind(majority_data, minority_oversampled)
    data_balanced <- data_balanced[sample(1:nrow(data_balanced)), ]
    
    cat("   Dataset balanceado manualmente:\n")
    cat("      Registros totales:", nrow(data_balanced), "\n")
    
    return(data_balanced)
  })
}

# -----------------------------------------------------------------------------
# 3. ADASYN - Adaptive Synthetic Sampling
# -----------------------------------------------------------------------------

apply_random_undersampling <- function(data, target_col = "is_fraud") {
  
  cat("\n🔄 Aplicando Random Under-sampling...\n")
  
  # Separar clases
  minority_data <- data[data[[target_col]] == 1, ]
  majority_data <- data[data[[target_col]] == 0, ]
  
  n_minority <- nrow(minority_data)
  
  # Muestrear mayoría para igualar minoría
  majority_sampled <- majority_data[sample(1:nrow(majority_data), n_minority), ]
  
  # Combinar
  data_balanced <- rbind(minority_data, majority_sampled)
  data_balanced <- data_balanced[sample(1:nrow(data_balanced)), ]
  
  cat("   Registros originales:", nrow(data), "\n")
  cat("   Registros después de under-sampling:", nrow(data_balanced), "\n")
  cat("   Distribución: 50% - 50%\n")
  
  return(data_balanced)
}

# -----------------------------------------------------------------------------
# 4. PREPARACIÓN DE DATOS PARA MODELO
# -----------------------------------------------------------------------------

prepare_features <- function(data, exclude_cols = NULL) {
  
  cat("\n📊 Preparando features para modelado...\n")
  
  # Columnas a excluir
  default_exclude <- c("transaction_id", "user_id", "is_fraud")
  exclude_cols <- union(default_exclude, exclude_cols)
  
  # Features disponibles
  feature_cols <- setdiff(names(data), exclude_cols)
  
  # Separar features y target
  X <- data[, feature_cols, drop = FALSE]
  y <- data$is_fraud
  
  # Convertir categóricas a dummy variables
  categorical_cols <- names(X)[sapply(X, is.character)]
  
  if (length(categorical_cols) > 0) {
    cat("   Creando dummy variables para:", paste(categorical_cols, collapse = ", "), "\n")
    
    for (col in categorical_cols) {
      # One-hot encoding manual
      levels <- unique(X[[col]])
      for (lvl in levels[-1]) {  # Excluir primera categoría para evitar multicolinealidad
        new_col <- paste0(col, "_", lvl)
        X[[new_col]] <- as.integer(X[[col]] == lvl)
      }
      X[[col]] <- NULL
    }
  }
  
  # Eliminar columnas con NA
  X <- X[, colSums(is.na(X)) == 0, drop = FALSE]
  
  cat("   Features finales:", ncol(X), "\n")
  cat("   Muestras:", nrow(X), "\n")
  
  return(list(X = X, y = y, feature_cols = names(X)))
}

# -----------------------------------------------------------------------------
# 5. FUNCIÓN PRINCIPAL DE MUESTREO
# -----------------------------------------------------------------------------

run_sampling <- function(data, method = "smote", output_dir = "outputs") {
  
  cat("\n🔄 TÉCNICAS DE MUESTREO\n")
  cat("======================\n\n")
  
  # 1. Analizar desbalance original
  imbalance_orig <- analyze_imbalance(data)
  
  # 2. Aplicar técnica de muestreo
  if (method == "smote") {
    data_sampled <- apply_smote(data, perc_over = 200, perc_under = 300, k = 5)
  } else if (method == "random_under") {
    data_sampled <- apply_random_undersampling(data)
  } else if (method == "none") {
    cat("\n⚠️  Sin muestreo - Usando dataset original desbalanceado\n")
    data_sampled <- data
  } else {
    cat("\n⚠️  Método no reconocido, usando SMOTE por defecto\n")
    data_sampled <- apply_smote(data)
  }
  
  # 3. Analizar nuevo balance
  imbalance_new <- analyze_imbalance(data_sampled)
  
  # 4. Preparar features
  prepared <- prepare_features(data_sampled)
  
  # Guardar resultados
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Guardar información de desbalance
  imbalance_info <- data.frame(
    stage = c("Original", "After Sampling"),
    majority_count = c(imbalance_orig$majority_count, imbalance_new$majority_count),
    minority_count = c(imbalance_orig$minority_count, imbalance_new$minority_count),
    imbalance_ratio = c(imbalance_orig$imbalance_ratio, imbalance_new$imbalance_ratio),
    minority_percentage = c(imbalance_orig$minority_percentage, imbalance_new$minority_percentage),
    stringsAsFactors = FALSE
  )
  
  write.csv(imbalance_info, file.path(output_dir, "imbalance_info.csv"), row.names = FALSE)
  
  cat("\n✅ Muestreo completado!\n")
  
  return(list(
    data_original = data,
    data_sampled = data_sampled,
    X = prepared$X,
    y = prepared$y,
    feature_cols = prepared$feature_cols,
    imbalance_original = imbalance_orig,
    imbalance_sampled = imbalance_new
  ))
}
