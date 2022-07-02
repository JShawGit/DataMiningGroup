library(caret)
library(ROCR)
evaluation <- function(predictions, labels){
  rounded <- round(predictions)
  eval <- NULL
  eval$cm <- confusionMatrix(as.factor(rounded),as.factor(labels))
  rocpd <- prediction(predictions, labels)
  eval$roc <- performance(rocpd, "tpr", "fpr")
  eval$AUROC <- performance(rocpd, measure = "auc")
  eval$AUROC <- eval$AUROC@y.values[[1]]
  eval$pr <- performance(rocpd, "prec", "rec")
  eval$AUPRC <- performance(rocpd, measure = "aucpr")
  eval$AUPRC <- eval$AUPRC@y.values[[1]]
  eval
}