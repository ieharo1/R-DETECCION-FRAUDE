# =============================================================================
# Quinto-cap - Visualización de Resultados
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)

# -----------------------------------------------------------------------------
# 1. DISTRIBUCIÓN DE CLASES (IMBALANCE)
# -----------------------------------------------------------------------------

plot_class_imbalance <- function(original_data, sampled_data, output_dir = "plots") {
  
  cat("📊 Generando gráfico de desbalance de clases...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos
  imbalance_data <- data.frame(
    Clase = c("Legítima", "Fraude", "Legítima", "Fraude"),
    Cantidad = c(
      sum(original_data$is_fraud == 0),
      sum(original_data$is_fraud == 1),
      sum(sampled_data$is_fraud == 0),
      sum(sampled_data$is_fraud == 1)
    ),
    Etapa = c("Original", "Original", "Después de SMOTE", "Después de SMOTE"),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot(imbalance_data, aes(x = Clase, y = Cantidad, fill = Clase)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Etapa, ncol = 1) +
    scale_fill_manual(values = c("Legítima" = "#2166AC", "Fraude" = "#B2182B")) +
    geom_text(aes(label = scales::comma(Cantidad)), vjust = -0.5, size = 4, fontface = "bold") +
    labs(
      title = "Desbalance de Clases - Antes y Después de SMOTE",
      subtitle = "Técnica de muestreo para dataset desbalanceado",
      x = "Clase",
      y = "Cantidad de Transacciones",
      caption = "Fuente: Quinto-cap - Detección de Fraude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 12, face = "bold")
    )
  
  ggsave(file.path(output_dir, "class_imbalance.png"), 
         plot = p, width = 10, height = 7, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "class_imbalance.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 2. COMPARACIÓN DE TÉCNICAS DE MUESTREO
# -----------------------------------------------------------------------------

plot_smote_comparison <- function(imbalance_info, output_dir = "plots") {
  
  cat("📊 Generando comparación de técnicas de muestreo...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos
  comparison_data <- data.frame(
    Métrica = c("Ratio Desbalance", "Ratio Desbalance", 
                "% Minoría", "% Minoría"),
    Valor = c(
      imbalance_info$imbalance_ratio[1],
      imbalance_info$imbalance_ratio[2],
      imbalance_info$minority_percentage[1],
      imbalance_info$minority_percentage[2]
    ),
    Etapa = c("Original", "SMOTE", "Original", "SMOTE"),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot(comparison_data, aes(x = Etapa, y = Valor, fill = Etapa)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Métrica, scales = "free_y", ncol = 1) +
    scale_fill_manual(values = c("Original" = "#D6604D", "SMOTE" = "#006837")) +
    geom_text(aes(label = round(Valor, 2)), vjust = -0.5, size = 4, fontface = "bold") +
    labs(
      title = "Impacto de SMOTE en el Dataset",
      subtitle = "Comparación de métricas de desbalance",
      x = "Etapa",
      y = "Valor",
      caption = "Fuente: Análisis de muestreo"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 11, face = "bold")
    )
  
  ggsave(file.path(output_dir, "smote_comparison.png"), 
         plot = p, width = 10, height = 7, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "smote_comparison.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 3. CURVA PRECISION-RECALL
# -----------------------------------------------------------------------------

plot_precision_recall_curve <- function(rf_eval, xgb_eval, output_dir = "plots") {
  
  cat("📊 Generando curva Precision-Recall...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Calcular PR curve para cada modelo
  calculate_pr_curve <- function(probs, actual) {
    thresholds <- sort(unique(probs), decreasing = TRUE)
    
    pr_data <- data.frame()
    
    for (thresh in thresholds) {
      pred <- as.integer(probs >= thresh)
      cm <- table(Actual = actual, Predicted = pred)
      
      TP <- ifelse("1" %in% colnames(cm) & "1" %in% rownames(cm), cm["1", "1"], 0)
      FP <- ifelse("1" %in% colnames(cm) & "0" %in% rownames(cm), cm["0", "1"], 0)
      FN <- ifelse("0" %in% colnames(cm) & "1" %in% rownames(cm), cm["1", "0"], 0)
      
      precision <- TP / max(TP + FP, 1)
      recall <- TP / max(TP + FN, 1)
      
      pr_data <- rbind(pr_data, data.frame(
        threshold = thresh,
        precision = precision,
        recall = recall
      ))
    }
    
    return(pr_data)
  }
  
  pr_rf <- calculate_pr_curve(rf_eval$probabilities, rf_eval$actual)
  pr_rf$model <- "Random Forest"
  
  pr_xgb <- calculate_pr_curve(xgb_eval$probabilities, xgb_eval$actual)
  pr_xgb$model <- "XGBoost"
  
  pr_combined <- rbind(pr_rf, pr_xgb)
  
  p <- ggplot(pr_combined, aes(x = recall, y = precision, color = model)) +
    geom_line(linewidth = 1.5) +
    geom_point(aes(x = rf_eval$recall, y = rf_eval$precision), 
               color = "#2166AC", size = 4, shape = 19) +
    geom_point(aes(x = xgb_eval$recall, y = xgb_eval$precision), 
               color = "#B2182B", size = 4, shape = 19) +
    scale_color_manual(values = c("Random Forest" = "#2166AC", "XGBoost" = "#B2182B")) +
    labs(
      title = "Curva Precision-Recall",
      subtitle = "Mejor que ROC para datasets desbalanceados (punto = threshold 0.5)",
      x = "Recall (Sensitivity)",
      y = "Precision (Positive Predictive Value)",
      caption = "Fuente: Evaluación de modelos de fraude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(output_dir, "precision_recall_curve.png"), 
         plot = p, width = 10, height = 7, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "precision_recall_curve.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 4. MATRIZ DE CONFUSIÓN
# -----------------------------------------------------------------------------

plot_confusion_matrix <- function(cm, output_dir = "plots") {
  
  cat("📊 Generando matriz de confusión...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Convertir a dataframe
  cm_df <- as.data.frame(cm)
  colnames(cm_df) <- c("Actual", "Predicted", "Count")
  
  # Calcular porcentajes por fila
  cm_df <- cm_df %>%
    group_by(Actual) %>%
    mutate(Percentage = round(100 * Count / sum(Count), 1))
  
  p <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste(Count, "\n", Percentage, "%", sep = "")), 
              size = 6, fontface = "bold") +
    scale_fill_gradient(low = "#FFF5F5", high = "#C62828", name = "Cantidad") +
    labs(
      title = "Matriz de Confusión",
      subtitle = "Resultados del modelo en datos de test",
      x = "Predicho",
      y = "Real",
      caption = "Filas = Real | Columnas = Predicho\nTN=0,0 | FP=0,1 | FN=1,0 | TP=1,1"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  ggsave(file.path(output_dir, "confusion_matrix.png"), 
         plot = p, width = 8, height = 6, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "confusion_matrix.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 5. IMPORTANCIA DE VARIABLES
# -----------------------------------------------------------------------------

plot_feature_importance <- function(rf_importance, xgb_importance, 
                                     output_dir = "plots") {
  
  cat("📊 Generando gráfico de importancia de variables...\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Preparar datos RF
  if (!is.null(rf_importance)) {
    rf_imp <- head(rf_importance, 15)
    rf_imp$model <- "Random Forest"
    rf_imp$value <- rf_imp$importance
    rf_imp <- rf_imp[, c("variable", "model", "value")]
  } else {
    rf_imp <- NULL
  }
  
  # Preparar datos XGB
  if (!is.null(xgb_importance)) {
    xgb_imp <- head(xgb_importance, 15)
    xgb_imp$model <- "XGBoost"
    xgb_imp$value <- xgb_imp$Gain
    xgb_imp <- xgb_imp[, c("Feature", "model", "value")] %>%
      rename(variable = Feature)
  } else {
    xgb_imp <- NULL
  }
  
  # Combinar
  combined <- rbind(rf_imp, xgb_imp)
  
  p <- ggplot(combined, aes(x = reorder(variable, value), y = value, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    scale_fill_manual(values = c("Random Forest" = "#2166AC", "XGBoost" = "#B2182B")) +
    labs(
      title = "Importancia de Variables - Detección de Fraude",
      subtitle = "Top 15 variables más predictivas",
      x = "Variable",
      y = "Importancia (Gain/Impurity)",
      caption = "Fuente: Feature Importance Analysis"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 9),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(output_dir, "feature_importance.png"), 
         plot = p, width = 10, height = 8, dpi = 300)
  
  cat("   Guardado en:", file.path(output_dir, "feature_importance.png"), "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN DE VISUALIZACIÓN COMPLETA
# -----------------------------------------------------------------------------

generate_all_visualizations <- function(original_data, sampled_data, 
                                         imbalance_info, rf_result, xgb_result,
                                         output_dir = "plots") {
  
  cat("\n📈 GENERANDO VISUALIZACIONES\n")
  cat("============================\n\n")
  
  plot_class_imbalance(original_data, sampled_data, output_dir)
  plot_smote_comparison(imbalance_info, output_dir)
  plot_precision_recall_curve(rf_result$eval, xgb_result$eval, output_dir)
  plot_confusion_matrix(rf_result$eval$confusion_matrix, output_dir)
  plot_feature_importance(rf_result$importance, xgb_result$importance, output_dir)
  
  cat("\n✅ Todas las visualizaciones generadas!\n")
}
