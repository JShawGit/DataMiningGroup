# ---------------------------------------------------------------------------- #
# Jessica Shaw Group Project
# ============================================================================ #



# ---------------------------------------------------------------------------- #
# Function to read in the data for the project
get_dataset <- function(filename="diabetes_data_upload.csv") {
  
  # get data from file and specify column names
  data = read.csv(filename)
  colnames(data) <- c(
    "Age",
    "Gender",
    "Polyuria",
    "Polydipsia",
    "Sudden.Weight.Loss",
    "Weakness",
    "Polyphagia",
    "Genital.Thrush",
    "Visual.Blurring",
    "Itching",
    "Irritability",
    "Delayed.Healing",
    "Partial.Paresis",
    "Muscle.Stiffness",
    "Alopecia",
    "Obesity",
    "Class"             
  )
  
  # change specific attributes to binary values
  data$Class  <- ifelse(data$Class=="Positive", 1, 0)
  data$Gender <- ifelse(data$Gender=="Female",  1, 0)
  
  # change yes/no attributes to binary values
  toChange = names(data)[-c(1, 2, 17)]
  for (i in 1:length(toChange)) {
    data[,toChange[i]] <- ifelse(data[toChange[i]]=="Yes",1,0)
  }
  
  # return a data frame
  return (data)
}
# ============================================================================ #



# ---------------------------------------------------------------------------- #
# Decision tree function that returns the root node
# X and Y should be dataframes, classes should be a list
DecisionTree <- function(X, Y) {
  ##############################
  # Is a root node with children
  Tree <- function(x, y) {
    root = Create_Tree(x, y)
    tree = list(
      rootNode = root
    )
    return ((structure(tree, class = "Tree")))
  } 
  ##############################
  
  
  
  #######################
  # Creates a Node object
  Node <- function(label=NULL, isLeaf=FALSE, condition=NULL,
                   leftChild=NULL, rightChild=NULL) {
    node = list(
      label=label,  
      isLeaf=isLeaf,
      leftChild=leftChild, 
      rightChild=rightChild,
      condition=condition
    )
    return (structure(node, class = "Node"))
  } 
  #######################
  
  
  
  #######################################################
  # The function to find the best split of the attributes
  Best_Split <- function(x, y) {
    # returns the column index in x to split on and the condition
    # the condition is "N >= condition"
    
    # Gini index
    Gini <- function(attribute, labels) {
      xy = cbind(attribute, labels)
      tab       = table(xy)
      rowSums   = rowSums(tab)
      probs     = tab / rowSums
      probs2    = probs^2
      giniRows  = 1 - rowSums(probs2)
      giniTotal = sum(
        (rowSums*giniRows) / sum(tab)
      )
      return (giniTotal)
    }
    
    # find all gini indexes
    size.x = ncol(x)
    ginivals = c()
    ginisplits = c()
    for (i in 1:size.x) {
      attr  = x[i]
      uniq  = unique(attr[,1])
      uniql = length(uniq)
      
      # continuous attributes
      if (uniql > 2) {
        minsplit = -1
        ming     = 999.0
        
        # cycle through each possible split
        for (i in 2:(uniql-1)){
          
          # split the attribute array by value
          spl = attr
          spl[spl >= uniq[i]] <- 1
          spl[spl < uniq[i]]  <- 0
          g = Gini(spl, y)
          
          if (g < ming) {
            ming = g
            minsplit = uniq[i]
          }
          
        }
        
        # add the minimum split/value
        ginisplits = c(ginisplits, minsplit)
        ginivals   = c(ginivals, ming)
      }
      
      # binary attributes
      else {
        g = Gini(attr, y)
        ginisplits = c(ginisplits, 1)
        ginivals = c(ginivals, g)
      }
      
    }
    
    # get minimum list-index
    minIndex = which.min(ginivals)
    minSplit = ginisplits[minIndex]
    
    # index of attribute, split ">=" value
    return (c(minIndex, minSplit))
  } 
  #######################################################
  
  
  
  ##########################################
  # Returns whether the data should be split
  Can_Split <- function(x, y, N=3) {
    
    # if the data has no attributes to split
    if (ncol(x) <= 1){
      print("No attributes to split")
      return (FALSE)
    }
    
    # if the number of x items < N
    if (nrow(x) < N) {
      print(paste("Not enough rows",nrow(x)))
      return (FALSE)
    }
    
    # if all labels are equal
    if (as.numeric(length(unique(y[,1]))) <= 1) {
      print("All equal labels")
      return (FALSE)
    }
    
    # if all attributes are equal
    if (length(unique(as.list(x))) == 1)  {
      print("Equal Attributes")
      return (FALSE)
    }
    
    return (TRUE)
  } 
  ############################################
  
  
  
  ######################################################
  # Classifies a given data frame by most frequent label
  Classify <- function(y) {
    return (as.numeric(tail(names(sort(table(y[,1]))), 1)))
  } 
  ######################################################
  
  
  
  #################################
  # This creates a tree recursively
  Create_Tree <- function(x, y, classes) {
    root = Node()
    
    # if we can split this node further
    if (Can_Split(x, y)){
      # print("-------------------------------")
      # print("Creating new node")
      
      # find where to split the data
      split = Best_Split(x, y)
      root$label <- names(x)[split[1]]
      root$condition <- split[2] # val >= condition
      
      
      # split the dataset
      i_L = which(x[split[1]] <  split[2])
      i_R = which(x[split[1]] >= split[2])
      
      # data without split attribute
      x_2 = x
      x_2[split[1]] <- NULL
      
      # new x datasets
      x_L = x_2[i_L,,drop=F]
      x_R = x_2[i_R,,drop=F]
      
      # new y datasets
      y_L = y[i_L,,drop=F]
      y_R = y[i_R,,drop=F]
      
      # create the root's children
      root$leftChild  <- Create_Tree(x_L, y_L)
      root$rightChild <- Create_Tree(x_R, y_R)
    }
    
    
    # else create the leaf node
    else {
      # print("===============================")
      # print("Creating new leaf")
      root$isLeaf <- TRUE
      root$label  <- Classify(y)
      print(root$label)
    }
    
    return (root)
  } 
  #################################
  
  
  
  # Returns a tree when the function is called
  return (Tree(X, Y))
}
# ============================================================================ #



# ---------------------------------------------------------------------------- #
# This takes a tree to predict one or more values in a data frame
Predict <- function(TREE, X) {
  
  # want to find results from the tree's root
  root = TREE$root
  results = c()
  
  # loop though each data entry
  for (row in 1:nrow(X)){
    
    # the current node in the search
    currNode = root
    
    # while not a leaf, find splits
    while (isFALSE(currNode$isLeaf)) {
      cond = currNode$condition
      
      # if value of attribute >= condition, go right
      if (X[row, currNode$label] >= cond) {
        currNode = currNode$rightChild
      }
      
      # else go left
      else {
        currNode = currNode$leftChild
      }
    }
    
    # add the predicted label
    results <- c(results, currNode$label)
  }
  
  return (results)
}
# ============================================================================ #



# ---------------------------------------------------------------------------- #
# Gets the accuracy of a prediction and labels
Get_Accuracy <- function(TREE, X, Y) {
  
  # get prediction as list
  preds = Predict(TREE, X)
  
  # metrics
  total   = nrow(X)
  correct = 0
  
  # loop through
  for (row in 1:total) {
    if (Y[row] == preds[row]) {
      correct = correct + 1
    }
  }
  
  # return ratio
  return (correct / total)
}
# ============================================================================ #



# ---------------------------------------------------------------------------- #
# Actual Program

# get data
data = get_dataset()
X = data[1:ncol(data)-1]
Y = data["Class"]

# split sets
size = nrow(X)
mid  = as.integer(size*.75)
train = 1:mid
test = (mid+1):size

X_train = X[train,,drop=F]
Y_train = Y[train,,drop=F]

X_test = X[test,,drop=F]
Y_test = Y[test,,drop=F]

# fit tree
tree = DecisionTree(X_train, Y_train)

# get amount correct
Get_Accuracy(tree, X_test, Y_test[,1])

