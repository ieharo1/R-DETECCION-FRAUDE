# =============================================================================
# 🚨 Quinto-cap - DETECCIÓN DE FRAUDE
# =============================================================================
# Proyecto: Detección de fraude con dataset desbalanceado
# Técnicas: SMOTE, Random Forest, XGBoost, Clasificación desbalanceada
# Autor: Isaac Esteban Haro Torres
# Fecha: 2026
# =============================================================================

# Limpiar entorno
rm(list = ls())

# Configurar opciones
options(stringsAsFactors = FALSE)
options(scipen = 999)
options(warn = -1)

# Mensaje de inicio
cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                ║\n")
cat("║     🚨 Quinto-cap - SISTEMA DE DETECCIÓN DE FRAUDE             ║\n")
cat("║                                                                ║\n")
cat("║     SMOTE + Random Forest + XGBoost para Dataset Desbalanceado ║\n")
cat("║     Desarrollado por: Isaac Esteban Haro Torres                ║\n")
cat("║                                                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# 1. CARGA DE LIBRERÍAS
# =============================================================================

cat("📦 Cargando librerías...\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(caret)
  library(randomForest)
  library(xgboost)
  library(ROCR)
  library(DMwR2)
})

cat("   ✅ Librerías cargadas exitosamente!\n\n")

# =============================================================================
# 2. GENERACIÓN DE DATOS SIMULADOS
# =============================================================================

cat("💳 GENERANDO DATOS SIMULADOS\n")
cat("============================\n\n")

# Ejecutar script de generación de datos
source("src/data_generation.R")

# Generar datos
set.seed(42)
fraud_data <- generate_all_data(n_transactions = 50000, fraud_rate = 0.015, 
                                 output_dir = "data")

cat("\n")

# =============================================================================
# 3. APLICACIÓN DE SMOTE
# =============================================================================

cat("🔄 APLICACIÓN DE SMOTE\n")
cat("=====================\n\n")

# Ejecutar script de muestreo
source("src/smote_sampling.R")

# Aplicar SMOTE
sampling_result <- run_sampling(fraud_data, method = "smote", output_dir = "outputs")

original_data <- sampling_result$data_original
sampled_data <- sampling_result$data_sampled

cat("\n")

# =============================================================================
# 4. ENTRENAMIENTO DE MODELOS
# =============================================================================

cat("🤖 ENTRENAMIENTO DE MODELOS\n")
cat("==========================\n\n")

# Ejecutar script de entrenamiento
source("src/model_training.R")

# Entrenar modelos
model_results <- train_all_models(sampling_result, output_dir = "outputs")

cat("\n")

# =============================================================================
# 5. VISUALIZACIÓN DE RESULTADOS
# =============================================================================

cat("📈 GENERANDO VISUALIZACIONES\n")
cat("============================\n\n")

# Ejecutar script de visualización
source("src/visualization.R")

# Generar visualizaciones
generate_all_visualizations(
  original_data = original_data,
  sampled_data = sampled_data,
  imbalance_info = sampling_result$imbalance_sampled,
  rf_result = model_results$rf,
  xgb_result = model_results$xgb,
  output_dir = "plots"
)

cat("\n")

# =============================================================================
# 6. RESUMEN FINAL
# =============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    ✅ PROYECTO COMPLETADO                      ║\n")
cat("╠════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                ║\n")
cat("║  📊 Datos procesados:                                          ║\n")
cat("║     •", nrow(original_data), "transacciones originales                     ║\n")
cat("║     •", nrow(sampled_data), "transacciones después de SMOTE                ║\n")
cat("║     •", ncol(sampling_result$X), "features para modelado                   ║\n")
cat("║                                                                ║\n")
cat("║  🔄 SMOTE aplicado:                                            ║\n")
cat("║     • Ratio original:", format(round(sampling_result$imbalance_original$imbalance_ratio, 1), nsmall = 1), ":1                          ║\n")
cat("║     • Ratio después:", format(round(sampling_result$imbalance_sampled$imbalance_ratio, 1), nsmall = 1), ":1                          ║\n")
cat("║                                                                ║\n")
cat("║  🤖 Modelos entrenados:                                        ║\n")
cat("║     • Random Forest (500 árboles con class weights)           ║\n")
cat("║     • XGBoost (", format(model_results$xgb$best_nrounds, nsmall = 0), "rounds optimizados)                       ║\n")
cat("║                                                                ║\n")
cat("║  📈 Métricas del Mejor Modelo (", model_results$best_model, "):\n", sep = "")

best_eval <- if (model_results$best_model == "XGBoost") {
  model_results$xgb$eval
} else {
  model_results$rf$eval
}

cat("║     • Accuracy:", format(round(best_eval$accuracy, 4), nsmall = 4), "                          ║\n")
cat("║     • Precision:", format(round(best_eval$precision, 4), nsmall = 4), "                         ║\n")
cat("║     • Recall:", format(round(best_eval$recall, 4), nsmall = 4), "                           ║\n")
cat("║     • F1-Score:", format(round(best_eval$f1, 4), nsmall = 4), "                         ║\n")
cat("║     • AUC-ROC:", format(round(best_eval$auc_roc, 4), nsmall = 4), "                          ║\n")
cat("║     • AUC-PR:", format(round(best_eval$auc_pr, 4), nsmall = 4), "                           ║\n")
cat("║                                                                ║\n")
cat("║  📁 Archivos generados:                                        ║\n")
cat("║     • data/transactions.csv                                   ║\n")
cat("║     • outputs/fraud_predictions.csv                           ║\n")
cat("║     • outputs/fraud_alerts.csv                                ║\n")
cat("║     • outputs/model_metrics.csv                               ║\n")
cat("║     • plots/*.png (5 visualizaciones)                         ║\n")
cat("║     • models/*.rds (2 modelos)                                ║\n")
cat("║                                                                ║\n")
cat("║  👨‍💻 Desarrollado por Isaac Esteban Haro Torres                 ║\n")
cat("║                                                                ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# FIN DEL PROGRAMA
# =============================================================================
