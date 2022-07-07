#Since this file does not concern algorithm implementation, we used the following packages for evaluation:
library(caret)
library(ROCR)

#Return many evaluation metrics as well as ROC and PR objects to facilitate graphing those curves
evaluation <- function(predictions, labels){
  rounded <- round(predictions)
  eval <- NULL
  #Includes the metrics 1-6 from section 4.5 of the paper
  eval$cm <- confusionMatrix(as.factor(rounded),as.factor(labels))
  rocpd <- prediction(predictions, labels)
  #Plot this for ROC curve
  eval$roc <- performance(rocpd, "tpr", "fpr")
  #Metric 7 from section 4.5 of the paper
  eval$AUROC <- performance(rocpd, measure = "auc")
  eval$AUROC <- eval$AUROC@y.values[[1]]
  #Plot this for PR curve
  eval$pr <- performance(rocpd, "prec", "rec")
  #Metric 8 from section 4.5 of the paper
  eval$AUPRC <- performance(rocpd, measure = "aucpr")
  eval$AUPRC <- eval$AUPRC@y.values[[1]]
  eval
}