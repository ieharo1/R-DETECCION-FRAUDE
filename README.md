# 🚨 Detección de Fraude

<p align="center">
  <img src="https://img.icons8.com/color/200/000000/security-checked.png" alt="Fraud Detection Logo" width="200"/>
</p>

---

## 📱 Descripción

Detección de Fraude es un sistema de **Detección de Fraude** desarrollado en **R** que utiliza técnicas avanzadas de **Machine Learning** para identificar transacciones fraudulentas en datasets desbalanceados, implementando **SMOTE** (Synthetic Minority Over-sampling Technique) para mejorar la detección de casos minoritarios y modelos de clasificación optimizados para maximizar **Precision** y **Recall**.

> El sistema replica un escenario real de fraude financiero donde las transacciones fraudulentas representan menos del 1% del total, requiriendo técnicas especializadas para su detección efectiva.

---

## ✨ Características

### Funcionalidades Implementadas ✅

- ✅ **Generación de Datos Simulados** - Transacciones con fraude realista
- ✅ **Dataset Desbalanceado** - ~1% de casos fraudulentos
- ✅ **Análisis de Desbalance** - Visualización de distribución de clases
- ✅ **SMOTE** - Synthetic Minority Over-sampling Technique
- ✅ **ADASYN** - Adaptive Synthetic Sampling (opcional)
- ✅ **Random Forest** - Modelo ensemble para clasificación
- ✅ **XGBoost** - Gradient Boosting optimizado
- ✅ **Validación Cruzada Estratificada** - Manteniendo proporción de clases
- ✅ **Métricas para Desbalance** - Precision, Recall, F1, AUC-PR
- ✅ **Curva Precision-Recall** - Mejor que ROC para desbalance
- ✅ **Matriz de Confusión** - Análisis de falsos positivos/negativos
- ✅ **Threshold Optimization** - Búsqueda de threshold óptimo
- ✅ **Exportación de Resultados** - Alertas de fraude generadas

### Próximamente 🔄

- 🧠 **Deep Learning** - Autoencoders para detección de anomalías
- 🔍 **Isolation Forest** - Detección no supervisada de anomalías
- 📊 **Ensemble Voting** - Combinación de múltiples modelos
- ⚡ **Online Learning** - Detección en tiempo real
- 🌐 **API REST** - Servicio de validación de transacciones
- 📈 **Análisis Temporal** - Patrones de fraude por período

---

## 🛠️ Stack Tecnológico

| Componente | Tecnología | Versión |
|------------|------------|---------|
| Lenguaje | R | 4.3.x |
| IDE | RStudio | 2024.x |
| Data Manipulation | tidyverse | 2.0.x |
| Visualización | ggplot2 | 3.5.x |
| Machine Learning | caret | 6.0.x |
| SMOTE | DMwR2 | 0.1.x |
| Random Forest | randomForest | 4.7.x |
| XGBoost | xgboost | 1.7.x |
| Métricas | ROCR | 1.0.x |
| Prec-Recall | PRROC | 1.3.x |

---

## 📁 Estructura del Proyecto

```
R-DETECCION-FRAUDE/
├── 📂 data/
│   └── transactions.csv
├── 📂 src/
│   ├── data_generation.R
│   ├── smote_sampling.R
│   ├── model_training.R
│   ├── model_evaluation.R
│   └── visualization.R
├── 📂 plots/
│   ├── class_imbalance.png
│   ├── smote_comparison.png
│   ├── precision_recall_curve.png
│   ├── confusion_matrix.png
│   └── feature_importance.png
├── 📂 models/
│   ├── random_forest_model.rds
│   └── xgboost_model.rds
├── 📂 outputs/
│   ├── fraud_predictions.csv
│   ├── fraud_alerts.csv
│   └── model_metrics.csv
├── main.R
├── Dockerfile
├── .dockerignore
└── README.md
```

---

## 🚀 Cómo Ejecutar el Proyecto

### 1. Clonar el Repositorio
```bash
git clone https://github.com/ieharo1/R-DETECCION-FRAUDE.git
cd R-DETECCION-FRAUDE
```

### 2. Ejecutar con R Local
```bash
Rscript main.R
```

### 3. Ejecutar con Docker
```bash
# Construir la imagen
docker build -t fraud-detection .

# Ejecutar el contenedor
docker run --rm fraud-detection
```

### 4. Ejecutar en RStudio
1. Abrir `main.R` en RStudio
2. Ejecutar línea por línea o source completo
3. Revisar outputs en carpetas generadas

---

## 📊 Dataset de Transacciones

### Features Principales
```r
variable | descripción | tipo
---------|-------------|------
transaction_id | ID único de transacción | string
amount | Monto de transacción ($) | numérica
merchant_category | Categoría de comercio | categórica
transaction_hour | Hora de transacción | numérica
day_of_week | Día de semana | categórica
distance_from_home | Distancia de casa (km) | numérica
is_online | Transacción online (0/1) | binaria
card_present | Tarjeta presente (0/1) | binaria
velocity_1h | Transacciones en última hora | numérica
velocity_24h | Transacciones en últimas 24h | numérica
avg_transaction | Promedio histórico del usuario | numérica
deviation_from_avg | Desviación del promedio | numérica
```

### Target
```r
variable | descripción
---------|------------
is_fraud | Es fraude (1) o legítima (0)
```

---

## 🎯 Técnicas de Muestreo

### SMOTE (Synthetic Minority Over-sampling Technique)

```
Algoritmo:
1. Para cada ejemplo minoritario x:
2.   Encontrar k vecinos más cercanos
3.   Seleccionar aleatoriamente un vecino x'
4.   Generar punto sintético: x_new = x + λ(x' - x)
5.   Donde λ ∈ [0, 1] es aleatorio
```

### Parámetros SMOTE
```r
- perc.over: 200 (sobremuestrear al 200%)
- perc.under: 300 (submuestrear mayoría al 300%)
- k: 5 (número de vecinos)
```

---

## 📈 Métricas para Dataset Desbalanceado

| Métrica | Descripción | Importancia |
|---------|-------------|-------------|
| **Precision** | De los predichos como fraude, ¿cuántos lo son? | Evitar falsos positivos |
| **Recall** | De los fraudes reales, ¿cuántos detectamos? | No dejar fraude sin detectar |
| **F1-Score** | Media armónica de Precision y Recall | Balance entre ambas |
| **AUC-PR** | Área bajo curva Precision-Recall | Mejor que AUC-ROC para desbalance |
| **MCC** | Mattheus Correlation Coefficient | Correlación entre predicciones y reales |

---

## 🎯 Optimización de Threshold

El threshold por defecto (0.5) no es óptimo para datasets desbalanceados.

```r
# Búsqueda de threshold óptimo
thresholds <- seq(0.1, 0.9, by = 0.05)
best_threshold <- thresholds[which.max(F1_scores)]

# Threshold típico para fraude: 0.2-0.3
```

---

## 📊 Visualizaciones Generadas

1. **Class Imbalance** - Distribución antes/después de SMOTE
2. **SMOTE Comparison** - Comparación de técnicas de muestreo
3. **Precision-Recall Curve** - Curva PR comparativa
4. **Confusion Matrix** - Matriz de confusión del mejor modelo
5. **Feature Importance** - Variables más predictivas de fraude

---

## 👨‍💻 Desarrollado por Isaac Esteban Haro Torres

**Ingeniero en Sistemas · Full Stack Developer · Automatización · Data**

### 📞 Contacto

- 📧 **Email:** zackharo1@gmail.com
- 📱 **WhatsApp:** [+593 988055517](https://wa.me/593988055517)
- 💻 **GitHub:** [ieharo1](https://github.com/ieharo1)
- 🌐 **Portafolio:** [ieharo1.github.io](https://ieharo1.github.io/portafolio-isaac.haro/)

---

## 📄 Licencia

© 2026 Isaac Esteban Haro Torres - Todos los derechos reservados.

---

⭐ Si te gustó el proyecto, ¡dame una estrella en GitHub!
