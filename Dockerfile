FROM rocker/r-ver:4.3.1

RUN install2.r --error \
    tidyverse \
    ggplot2 \
    caret \
    randomForest \
    xgboost \
    e1071 \
    forecast \
    cluster \
    ROCR \
    DMwR2 \
    PRROC \
    pROC

WORKDIR /app
COPY . /app

CMD ["Rscript", "main.R"]
