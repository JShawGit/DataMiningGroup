# library(prodlim)
# library(data.table)
#set.seed(0)

load('binarized_data.Rds')
age.cutoff <- mean(aggregate(df$Age,list(df$class),mean)$x)
df$Age <- as.numeric(df$Age > age.cutoff)
col_names <- names(df)
df[,col_names] <- lapply(df[,col_names] , factor)
# library(gRain)
# dependencies <- ~Age + Gender + class*Age*Gender +
#   Genital.thrush*Gender*class +
#   Polyuria*class +
#   Polydipsia*class +
#   sudden.weight.loss*Polyuria*class +
#   weakness*class +
#   Polyphagia*class +
#   visual.blurring*class +
#   Itching*class +
#   Irritability*class +
#   delayed.healing*weakness*Itching*class + #*Polyphagia +
#   partial.paresis*class +
#   muscle.stiffness*Age*visual.blurring*class +
#   Alopecia*Gender*delayed.healing*class +
#   Obesity*sudden.weight.loss*class

gen.tabs <- function(dat,smooth=1){
  result <- NULL
  result$class <- CJ(0:1)[,1]
  colnames(result$class) <- c("class")
  result$class[,Freq := mapply((function (class) (sum(dat$class==class)+smooth)/(dim(dat)[1]+smooth*dim(result$class)[1])),class)]
  
  result$Age <- CJ(0:1,0:1)[,2:1]
  colnames(result$Age) <- c("Age","class")
  result$Age[,Freq := mapply((function (p1,p2) (sum(dat$Age==p1 & dat$class==p2)+smooth)/(sum(dat$Age==p2)+smooth*2)),Age,class)]
  
  result$Gender <- CJ(0:1,0:1)[,2:1]
  colnames(result$Gender) <- c("Gender","class")
  result$Gender[,Freq := mapply((function (p1,p2) (sum(dat$Gender==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Gender,class)]
  
  result$Genital.thrush <- CJ(0:1,0:1)[,2:1]
  colnames(result$Genital.thrush) <- c("Genital.thrush","class")
  result$Genital.thrush[,Freq := mapply((function (p1,p2) (sum(dat$Genital.thrush==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Genital.thrush,class)]
  
  result$Polyuria <- CJ(0:1,0:1)[,2:1]
  colnames(result$Polyuria) <- c("Polyuria","class")
  result$Polyuria[,Freq := mapply((function (p1,p2) (sum(dat$Polyuria==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Polyuria,class)]
  
  result$Polydipsia <- CJ(0:1,0:1)[,2:1]
  colnames(result$Polydipsia) <- c("Polydipsia","class")
  result$Polydipsia[,Freq := mapply((function (p1,p2) (sum(dat$Polydipsia==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Polydipsia,class)]
  
  result$sudden.weight.loss <- CJ(0:1,0:1)[,2:1]
  colnames(result$sudden.weight.loss) <- c("sudden.weight.loss","class")
  result$sudden.weight.loss[,Freq := mapply((function (p1,p2) (sum(dat$sudden.weight.loss==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),sudden.weight.loss,class)]
  
  result$weakness <- CJ(0:1,0:1)[,2:1]
  colnames(result$weakness) <- c("weakness","class")
  result$weakness[,Freq := mapply((function (p1,p2) (sum(dat$weakness==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),weakness,class)]
  
  result$Polyphagia <- CJ(0:1,0:1)[,2:1]
  colnames(result$Polyphagia) <- c("Polyphagia","class")
  result$Polyphagia[,Freq := mapply((function (p1,p2) (sum(dat$Polyphagia==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Polyphagia,class)]
  
  result$visual.blurring <- CJ(0:1,0:1)[,2:1]
  colnames(result$visual.blurring) <- c("visual.blurring","class")
  result$visual.blurring[,Freq := mapply((function (p1,p2) (sum(dat$visual.blurring==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),visual.blurring,class)]
  
  result$Itching <- CJ(0:1,0:1)[,2:1]
  colnames(result$Itching) <- c("Itching","class")
  result$Itching[,Freq := mapply((function (p1,p2) (sum(dat$Itching==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Itching,class)]
  
  result$Irritability <- CJ(0:1,0:1)[,2:1]
  colnames(result$Irritability) <- c("Irritability","class")
  result$Irritability[,Freq := mapply((function (p1,p2) (sum(dat$Irritability==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Irritability,class)]
  
  result$delayed.healing <- CJ(0:1,0:1)[,2:1]
  colnames(result$delayed.healing) <- c("delayed.healing","class")
  result$delayed.healing[,Freq := mapply((function (p1,p2) (sum(dat$delayed.healing==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),delayed.healing,class)]
  
  result$partial.paresis <- CJ(0:1,0:1)[,2:1]
  colnames(result$partial.paresis) <- c("partial.paresis","class")
  result$partial.paresis[,Freq := mapply((function (p1,p2) (sum(dat$partial.paresis==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),partial.paresis,class)]
  
  result$muscle.stiffness <- CJ(0:1,0:1)[,2:1]
  colnames(result$muscle.stiffness) <- c("muscle.stiffness","class")
  result$muscle.stiffness[,Freq := mapply((function (p1,p2) (sum(dat$muscle.stiffness==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),muscle.stiffness,class)]
  
  result$Alopecia <- CJ(0:1,0:1)[,2:1]
  colnames(result$Alopecia) <- c("Alopecia","class")
  result$Alopecia[,Freq := mapply((function (p1,p2) (sum(dat$Alopecia==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Alopecia,class)]
  
  result$Obesity <- CJ(0:1,0:1)[,2:1]
  colnames(result$Obesity) <- c("Obesity","class")
  result$Obesity[,Freq := mapply((function (p1,p2) (sum(dat$Obesity==p1 & dat$class==p2)+smooth)/(sum(dat$class==p2)+smooth*2)),Obesity,class)]
  
  for(i in 1:length(result)){
    result[[i]] <- as.data.frame(result[[i]])
  }
  
  result
}

# graph <- dag(dependencies)
# cond.prob.tabs <- extractCPT(df, graph,smooth = 1)
# 
# for(i in 1:length(cond.prob.tabs)){
#   cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
# }

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
  #one.joint.prob/(one.joint.prob+zero.joint.prob)
}

#predict(df[5,])

results <- integer(520)
# for(i in 1:520){
#   results[i] <- predict(df[i,])
# }

fold1 <- df[1:173,]
fold2 <- df[174:(174+173),]
fold3 <- df[(174+173):520,]

# cond.prob.tabs <- extractCPT(rbind(fold2,fold3), graph, smooth = 1)
# for(i in 1:length(cond.prob.tabs)){
#   cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
# }
cond.prob.tabs <- gen.tabs(rbind(fold2,fold3))
# for(i in 1:length(cond.prob.tabs)){
#   cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
# }
for(i in 1:173){
  results[i] <- predict(fold1[i,])
}

cond.prob.tabs <- gen.tabs(rbind(fold1,fold3))
# for(i in 1:length(cond.prob.tabs)){
#   cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
# }
for(i in 1:173){
  results[i+173] <- predict(fold2[i,])
}

cond.prob.tabs <- gen.tabs(rbind(fold1,fold2))
# for(i in 1:length(cond.prob.tabs)){
#   cond.prob.tabs[[i]] <- as.data.frame(cond.prob.tabs[[i]])
# }
for(i in 1:174){
  results[i+173+173] <- predict(fold3[i,])
}


print("Accuracy:")
(520-sum(abs(results - (as.numeric(df$class)-1))))/520
