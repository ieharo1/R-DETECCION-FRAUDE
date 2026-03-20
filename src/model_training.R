# =============================================================================
# Quinto-cap - Entrenamiento de Modelos para Detección de Fraude
# Autor: Isaac Esteban Haro Torres
# =============================================================================

library(caret)
library(randomForest)
library(xgboost)

# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN DE VALIDACIÓN CRUZADA ESTRATIFICADA
# -----------------------------------------------------------------------------

create_stratified_cv <- function(y, n_folds = 5) {
  
  cat("📊 Creando validación cruzada estratificada (", n_folds, "-fold)...\n", sep = "")
  
  set.seed(42)
  folds <- createFolds(y, k = n_folds, list = TRUE, returnTrain = FALSE)
  
  # Verificar estratificación
  cat("   Verificando estratificación...\n")
  for (i in 1:length(folds)) {
    fold_rate <- mean(y[folds[[i]]] == 1)
    cat("      Fold", i, "- Fraud rate:", round(fold_rate * 100, 2), "%\n")
  }
  
  return(folds)
}

# -----------------------------------------------------------------------------
# 2. ENTRENAMIENTO DE RANDOM FOREST
# -----------------------------------------------------------------------------

train_random_forest_fraud <- function(X_train, y_train) {
  
  cat("\n🌲 Entrenando Random Forest para Detección de Fraude...\n")
  
  # Configurar cross-validation
  set.seed(42)
  train_control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "up",  # Over-sampling en cada fold
    verboseIter = FALSE,
    savePredictions = "final"
  )
  
  # Convertir target a factor
  y_train_factor <- factor(y_train, levels = c(0, 1), labels = c("Legitimate", "Fraud"))
  
  # Grid de hiperparámetros
  tune_grid <- expand.grid(
    mtry = c(floor(sqrt(ncol(X_train))), floor(ncol(X_train) / 3), 15)
  )
  
  # Entrenar modelo con pesos de clase
  class_weights <- ifelse(y_train == 1, 10, 1)  # Peso 10x para fraudes
  
  set.seed(42)
  rf_model <- train(
    as.data.frame(X_train),
    y_train_factor,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    metric = "ROC",
    importance = "impurity",
    num.trees = 500,
    case.weights = class_weights,
    verbose = FALSE
  )
  
  cat("   Mejor mtry:", rf_model$bestTune$mtry, "\n")
  cat("   Número de árboles:", rf_model$finalModel$num.trees, "\n")
  
  # Importancia de variables
  if (!is.null(rf_model$finalModel$variable.importance)) {
    importance_df <- data.frame(
      variable = names(rf_model$finalModel$variable.importance),
      importance = rf_model$finalModel$variable.importance,
      stringsAsFactors = FALSE
    )
    importance_df <- importance_df[order(-importance_df$importance), ]
    
    cat("\n   Top 5 variables importantes:\n")
    for (i in 1:min(5, nrow(importance_df))) {
      cat("      ", importance_df$variable[i], ":", 
          round(importance_df$importance[i], 4), "\n")
    }
  } else {
    importance_df <- NULL
  }
  
  return(list(
    model = rf_model,
    importance = importance_df
  ))
}

# -----------------------------------------------------------------------------
# 3. ENTRENAMIENTO DE XGBOOST
# -----------------------------------------------------------------------------

train_xgboost_fraud <- function(X_train, y_train) {
  
  cat("\n🚀 Entrenando XGBoost para Detección de Fraude...\n")
  
  # Preparar matrices
  x_train <- as.matrix(X_train)
  y_train_vec <- y_train
  
  dtrain <- xgb.DMatrix(data = x_train, label = y_train_vec)
  
  # Calcular ratio de pesos para clases desbalanceadas
  n_neg <- sum(y_train == 0)
  n_pos <- sum(y_train == 1)
  scale_pos_weight <- n_neg / n_pos
  
  cat("   Scale pos weight:", round(scale_pos_weight, 2), "\n")
  
  # Parámetros
  params <- list(
    objective = "binary:logistic",
    booster = "gbtree",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3,
    gamma = 0,
    reg_alpha = 0.1,
    reg_lambda = 1,
    scale_pos_weight = scale_pos_weight,
    nthread = 4,
    seed = 42
  )
  
  # Cross-validation para encontrar mejor número de rounds
  set.seed(42)
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 300,
    nfold = 5,
    stratified = TRUE,
    early_stopping_rounds = 30,
    verbose = 0,
    maximize = FALSE,
    eval_metric = "auc"
  )
  
  # Mejor número de rounds
  best_nrounds <- cv$best_iteration
  cat("   Mejor número de rounds:", best_nrounds, "\n")
  
  # Entrenar modelo final
  set.seed(42)
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  # Importancia de variables
  importance <- xgb.importance(model = xgb_model)
  
  if (nrow(importance) > 0) {
    cat("\n   Top 5 variables importantes:\n")
    for (i in 1:min(5, nrow(importance))) {
      cat("      ", importance$Feature[i], ":", 
          round(importance$Gain[i], 4), "\n")
    }
  }
  
  return(list(
    model = xgb_model,
    importance = importance,
    best_nrounds = best_nrounds
  ))
}

# -----------------------------------------------------------------------------
# 4. PREDICCIÓN Y EVALUACIÓN
# -----------------------------------------------------------------------------

evaluate_fraud_model <- function(model, X_test, y_test, model_type = "rf") {
  
  cat("\n📊 Evaluando modelo", model_type, "...\n")
  
  # Predicciones
  if (model_type == "rf") {
    pred_probs <- predict(model$model, newdata = as.data.frame(X_test), type = "prob")
    if (is.data.frame(pred_probs)) {
      prob_fraud <- pred_probs$Fraud
    } else {
      prob_fraud <- pred_probs
    }
  } else {
    x_test <- as.matrix(X_test)
    prob_fraud <- predict(model$model, newdata = x_test)
  }
  
  # Clase predicha (threshold por defecto 0.5)
  pred_binary <- as.integer(prob_fraud > 0.5)
  
  # Matriz de confusión
  cm <- table(Actual = y_test, Predicted = pred_binary)
  
  # Calcular métricas
  TP <- cm[2, 2]
  TN <- cm[1, 1]
  FP <- cm[1, 2]
  FN <- cm[2, 1]
  
  accuracy <- (TP + TN) / sum(cm)
  precision <- TP / max(TP + FP, 1)
  recall <- TP / max(TP + FN, 1)  # Sensibilidad
  f1 <- 2 * precision * recall / max(precision + recall, 0.001)
  specificity <- TN / max(TN + FP, 1)
  
  # AUC-ROC
  library(ROCR)
  pred_obj <- prediction(prob_fraud, y_test)
  auc_roc <- performance(pred_obj, "auc")@y.values[[1]]
  
  # AUC-PR (Precision-Recall)
  # Calcular manualmente
  thresholds <- sort(unique(prob_fraud), decreasing = TRUE)
  pr_auc <- 0
  prev_recall <- 0
  prev_precision <- 1
  
  for (thresh in thresholds) {
    pred_temp <- as.integer(prob_fraud >= thresh)
    cm_temp <- table(Actual = y_test, Predicted = pred_temp)
    
    tp_temp <- ifelse("1" %in% colnames(cm_temp) & "1" %in% rownames(cm_temp), 
                       cm_temp["1", "1"], 0)
    fp_temp <- ifelse("1" %in% colnames(cm_temp) & "0" %in% rownames(cm_temp), 
                       cm_temp["0", "1"], 0)
    fn_temp <- ifelse("0" %in% colnames(cm_temp) & "1" %in% rownames(cm_temp), 
                       cm_temp["1", "0"], 0)
    
    prec_temp <- tp_temp / max(tp_temp + fp_temp, 1)
    rec_temp <- tp_temp / max(tp_temp + fn_temp, 1)
    
    pr_auc <- pr_auc + (rec_temp - prev_recall) * (prec_temp + prev_precision) / 2
    prev_recall <- rec_temp
    prev_precision <- prec_temp
  }
  
  cat("   Accuracy:", round(accuracy, 4), "\n")
  cat("   Precision:", round(precision, 4), "\n")
  cat("   Recall (Sensitivity):", round(recall, 4), "\n")
  cat("   Specificity:", round(specificity, 4), "\n")
  cat("   F1-Score:", round(f1, 4), "\n")
  cat("   AUC-ROC:", round(auc_roc, 4), "\n")
  cat("   AUC-PR:", round(pr_auc, 4), "\n")
  
  return(list(
    predictions = pred_binary,
    probabilities = prob_fraud,
    actual = y_test,
    confusion_matrix = cm,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1,
    auc_roc = auc_roc,
    auc_pr = pr_auc
  ))
}

# -----------------------------------------------------------------------------
# 5. OPTIMIZACIÓN DE THRESHOLD
# -----------------------------------------------------------------------------

optimize_threshold <- function(probabilities, actual, metric = "f1") {
  
  cat("\n🎯 Optimizando threshold...\n")
  
  thresholds <- seq(0.1, 0.9, by = 0.02)
  metrics <- data.frame()
  
  for (thresh in thresholds) {
    pred <- as.integer(probabilities >= thresh)
    cm <- table(Actual = actual, Predicted = pred)
    
    TP <- cm[2, 2]
    TN <- cm[1, 1]
    FP <- cm[1, 2]
    FN <- cm[2, 1]
    
    precision <- TP / max(TP + FP, 1)
    recall <- TP / max(TP + FN, 1)
    f1 <- 2 * precision * recall / max(precision + recall, 0.001)
    
    metrics <- rbind(metrics, data.frame(
      threshold = thresh,
      precision = precision,
      recall = recall,
      f1 = f1,
      stringsAsFactors = FALSE
    ))
  }
  
  # Encontrar mejor threshold
  if (metric == "f1") {
    best_idx <- which.max(metrics$f1)
  } else if (metric == "recall") {
    best_idx <- which.max(metrics$recall)
  } else {
    best_idx <- which.max(metrics$precision)
  }
  
  best_threshold <- metrics$threshold[best_idx]
  
  cat("   Mejor threshold para", metric, ":", best_threshold, "\n")
  cat("   F1 en mejor threshold:", round(metrics$f1[best_idx], 4), "\n")
  cat("   Precision:", round(metrics$precision[best_idx], 4), "\n")
  cat("   Recall:", round(metrics$recall[best_idx], 4), "\n")
  
  return(list(
    best_threshold = best_threshold,
    metrics = metrics
  ))
}

# -----------------------------------------------------------------------------
# 6. FUNCIÓN PRINCIPAL DE ENTRENAMIENTO
# -----------------------------------------------------------------------------

train_all_models <- function(sampled_data, output_dir = "outputs") {
  
  cat("\n🤖 ENTRENAMIENTO DE MODELOS\n")
  cat("==========================\n\n")
  
  # Extraer datos
  X <- sampled_data$X
  y <- sampled_data$y
  
  # Train/Test split estratificado
  set.seed(42)
  train_index <- createDataPartition(y, p = 0.8, list = FALSE)
  
  X_train <- X[train_index, ]
  X_test <- X[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  cat("   Train samples:", nrow(X_train), "\n")
  cat("   Test samples:", nrow(X_test), "\n")
  cat("   Features:", ncol(X_train), "\n")
  cat("   Fraud rate train:", round(mean(y_train) * 100, 2), "%\n")
  cat("   Fraud rate test:", round(mean(y_test) * 100, 2), "%\n")
  
  # Entrenar Random Forest
  rf_result <- train_random_forest_fraud(X_train, y_train)
  
  # Entrenar XGBoost
  xgb_result <- train_xgboost_fraud(X_train, y_train)
  
  # Evaluar modelos
  rf_eval <- evaluate_fraud_model(rf_result, X_test, y_test, "rf")
  xgb_eval <- evaluate_fraud_model(xgb_result, X_test, y_test, "xgb")
  
  # Optimizar threshold
  rf_threshold <- optimize_threshold(rf_eval$probabilities, y_test, "f1")
  xgb_threshold <- optimize_threshold(xgb_eval$probabilities, y_test, "f1")
  
  # Comparar modelos
  cat("\n📊 COMPARACIÓN DE MODELOS:\n")
  cat("--------------------------\n")
  cat("Random Forest - F1:", round(rf_eval$f1, 4), 
      "| AUC-ROC:", round(rf_eval$auc_roc, 4), 
      "| Recall:", round(rf_eval$recall, 4), "\n")
  cat("XGBoost       - F1:", round(xgb_eval$f1, 4), 
      "| AUC-ROC:", round(xgb_eval$auc_roc, 4), 
      "| Recall:", round(xgb_eval$recall, 4), "\n")
  
  # Seleccionar mejor modelo (basado en F1)
  if (xgb_eval$f1 > rf_eval$f1) {
    best_model <- "XGBoost"
    best_result <- xgb_result
    best_eval <- xgb_eval
  } else {
    best_model <- "Random Forest"
    best_result <- rf_result
    best_eval <- rf_eval
  }
  
  cat("\n🏆 MEJOR MODELO:", best_model, "\n")
  
  # Guardar modelos
  if (!dir.exists("models")) {
    dir.create("models")
  }
  
  saveRDS(rf_result$model, "models/random_forest_model.rds")
  saveRDS(xgb_result$model, "models/xgboost_model.rds")
  
  # Guardar predicciones
  predictions_df <- data.frame(
    actual = y_test,
    rf_prediction = rf_eval$predictions,
    rf_probability = rf_eval$probabilities,
    xgb_prediction = xgb_eval$predictions,
    xgb_probability = xgb_eval$probabilities,
    stringsAsFactors = FALSE
  )
  
  write.csv(predictions_df, file.path(output_dir, "fraud_predictions.csv"), row.names = FALSE)
  
  # Guardar alertas de fraude (predicciones con threshold optimizado)
  fraud_alerts <- predictions_df %>%
    filter(rf_probability >= rf_threshold$best_threshold) %>%
    mutate(
      alert_level = case_when(
        rf_probability >= 0.8 ~ "HIGH",
        rf_probability >= 0.5 ~ "MEDIUM",
        TRUE ~ "LOW"
      )
    )
  
  write.csv(fraud_alerts, file.path(output_dir, "fraud_alerts.csv"), row.names = FALSE)
  
  # Guardar métricas
  metrics_df <- data.frame(
    model = c("Random Forest", "XGBoost"),
    accuracy = c(rf_eval$accuracy, xgb_eval$accuracy),
    precision = c(rf_eval$precision, xgb_eval$precision),
    recall = c(rf_eval$recall, xgb_eval$recall),
    f1 = c(rf_eval$f1, xgb_eval$f1),
    auc_roc = c(rf_eval$auc_roc, xgb_eval$auc_roc),
    auc_pr = c(rf_eval$auc_pr, xgb_eval$auc_pr),
    stringsAsFactors = FALSE
  )
  
  write.csv(metrics_df, file.path(output_dir, "model_metrics.csv"), row.names = FALSE)
  
  cat("\n✅ Entrenamiento completado!\n")
  
  return(list(
    rf = list(model = rf_result, eval = rf_eval, threshold = rf_threshold),
    xgb = list(model = xgb_result, eval = xgb_eval, threshold = xgb_threshold),
    best_model = best_model
  ))
}
