library(prodlim)
set.seed(0)

load('binarized_data.Rds')
age.cutoff <- mean(aggregate(df$Age,list(df$class),mean)$x)
df$Age <- as.numeric(df$Age > age.cutoff)
col_names <- names(df)
df[,col_names] <- lapply(df[,col_names] , factor)
library(gRain)
dependencies <- ~Age + Gender + class*Age*Gender +
  Genital.thrush*Gender*class +
  Polyuria*class +
  Polydipsia*class +
  sudden.weight.loss*Polyuria*class +
  weakness*class +
  Polyphagia*class +
  visual.blurring*class +
  Itching*class +
  Irritability*class +
  delayed.healing*weakness*Itching*class + #*Polyphagia +
  partial.paresis*class +
  muscle.stiffness*Age*visual.blurring*class +
  Alopecia*Gender*delayed.healing*class +
  Obesity*sudden.weight.loss*class

graph <- dag(dependencies)
cond.prob.tabs <- extractCPT(df, graph,smooth = 1)

for(i in 1:length(cond.prob.tabs)){
  cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
}

# for(i in 3:length(cond.prob.tabs)){
#   cond.prob.tabs[[i]] <- as.data.frame(ftable(cond.prob.tabs[[i]],row.vars = 1:length(dimnames(cond.prob.tabs[[i]]))))
# }
#subset(cond.prob.tabs$delayed.healing, (delayed.healing == 0 & class == 1 & weakness == 0 & Itching == 1 & Age == 1))$Freq
predict <- function(tuple){
  tuple$class <- 0
  zero.joint.prob <- 1
  for(colname in colnames(df)){
    tab.names <- colnames(cond.prob.tabs[[colname]])[1:length(colnames(cond.prob.tabs[[colname]]))-1]
    filtered.tup <- tuple[,tab.names]
    cond.prob <- cond.prob.tabs[[colname]][row.match(filtered.tup, cond.prob.tabs[[colname]][1:ncol(cond.prob.tabs[[colname]])-1]),ncol(cond.prob.tabs[[colname]])]
    zero.joint.prob <- zero.joint.prob * cond.prob
    #print(paste0("ZJP:",zero.joint.prob))
  }
  
  tuple$class <- 1
  one.joint.prob <- 1
  for(colname in colnames(df)){
    tab.names <- colnames(cond.prob.tabs[[colname]])[1:length(colnames(cond.prob.tabs[[colname]]))-1]
    filtered.tup <- tuple[,tab.names]
    cond.prob <- cond.prob.tabs[[colname]][row.match(filtered.tup, cond.prob.tabs[[colname]][1:ncol(cond.prob.tabs[[colname]])-1]),ncol(cond.prob.tabs[[colname]])]
    one.joint.prob <- one.joint.prob * cond.prob
    #print(paste0("OJP:",one.joint.prob))
  }
  
  round(one.joint.prob/(one.joint.prob+zero.joint.prob))
}

#predict(df[5,])

results <- integer(520)
# for(i in 1:520){
#   results[i] <- predict(df[i,])
# }

fold1 <- df[1:173,]
fold2 <- df[174:(174+173),]
fold3 <- df[(174+173):520,]

cond.prob.tabs <- extractCPT(rbind(fold2,fold3), graph, smooth = 1)
for(i in 1:length(cond.prob.tabs)){
  cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
}
for(i in 1:173){
  results[i] <- predict(fold1[i,])
}

cond.prob.tabs <- extractCPT(rbind(fold1,fold3), graph, smooth = 1)
for(i in 1:length(cond.prob.tabs)){
  cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
}
for(i in 1:173){
  results[i+173] <- predict(fold2[i,])
}

cond.prob.tabs <- extractCPT(rbind(fold1,fold2), graph, smooth = 1)
for(i in 1:length(cond.prob.tabs)){
  cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
}
for(i in 1:174){
  results[i+173+173] <- predict(fold3[i,])
}


print("Accuracy:")
(520-sum(abs(results - (as.numeric(df$class)-1))))/520


