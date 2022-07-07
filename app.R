library(shinydashboard)
library(shinyWidgets)
library(ggcorrplot)
library(gridExtra)
library(dplyr)
library(shiny)
library(grid)
library(DT)



# Function to read in the data for the project ---------------------------------
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
# ==============================================================================



# The Decision Tree ------------------------------------------------------------
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

# ==============================================================================


# Get data ---------------------------------------------------------------------
data = get_dataset()
X = data[1:ncol(data)-1]
Y = data["Class"]
tree = DecisionTree(X, Y)
# ==============================================================================



# Define UI here ---------------------------------------------------------------
ui <- fluidPage( 
  
  # FluidPage options
  title="Diabetes Prediction Application",
  style="background-color:#16161a;padding:20px;",
  
  # Page title
  #titlePanel("Diabetes Prediction", windowTitle="Diabetes Prediction"),
  
  # First row has: Prediction and the Graph #
  fluidRow(
    style = paste0("margin:10px;display:flex;flex-direction:row;"),
    
    # Prediction in/output (17 values in total normally)
    column(6,  align="center", 
          style = paste0("background-color:#45464d;color:white;font-size:12px;",
                        "border-style:groove;border-radius:20px;",
                        "margin:10px;display:flex;flex-direction:column;"),
          
      tags$b(h2("Diagnosis Prediction")),
           
      fluidRow(
        style = paste0("margin:10px;display:flex;flex-direction:row;"),
        
        column(3, align="left",
          numericInput("Age", width=80, tags$b("Age"), value=40),
          radioButtons("Genital.Thrush", tags$b("Genital Thrush"), 
                       choices=list("True"=1, "False"=0), selected=0),
          radioButtons("Obesity", tags$b("Obesity"),
                       choices=list("True"=1, "False"=0), selected=1),
          radioButtons("Polyuria", tags$b("Polyuria"), 
                       choices=list("True"=1, "False"=0), selected=0),
        ),
        
        column(3, align="left",
          radioButtons("Alopecia", width=80, tags$b("Alopecia"), 
            choices=list("True"=1, "False"=0), selected=1),
          radioButtons("Irritability", tags$b("Irritability"), 
                       choices=list("True"=1, "False"=0), selected=0),
          radioButtons("Partial.Paresis", tags$b("Partial Paresis"), 
                       choices=list("True"=1, "False"=0), selected=0),
          radioButtons("Sudden.Weight.Loss", tags$b("Sudden Weight Loss"), 
                       choices=list("True"=1, "False"=0), selected=0),
        ),
        
        column(3, align="left",
          radioButtons("Delayed.Healing", width=80, tags$b("Delayed Healing"), 
            choices=list("True"=1, "False"=0), selected=1),
          radioButtons("Itching", tags$b("Itching"), 
                       choices=list("True"=1, "False"=0), selected=1),
          radioButtons("Polydipsia", tags$b("Polydipsia"), 
                       choices=list("True"=1, "False"=0), selected=1),
          radioButtons("Visual.Blurring", tags$b("Visual Blurring"), 
                       choices=list("True"=1, "False"=0), selected=0),
        ),
        
        column(3, align="left",
         radioButtons("Gender", width=80, tags$b("Gender"),
                      choices=list("Female"=1, "Male"=0), selected=0),
         radioButtons("Muscle.Stiffness", tags$b("Muscle Stiffness"), 
                      choices=list("True"=1, "False"=0), selected=1),
         radioButtons("Polyphagia", tags$b("Polyphagia"), 
                      choices=list("True"=1, "False"=0), selected=0),
         radioButtons("Weakness", tags$b("Weakness"), 
                      choices=list("True"=1, "False"=0), selected=1)
        )
      ),
      
      
      fluidRow(htmlOutput("predict"))
           
    ),
    
    # The graph output
    column(6, align="center", 
           style = paste0("background-color:white;color:black;font-size:12px;",
                          'border-style:groove;border-radius:20px;',
                          "margin:10px;display:flex;flex-direction:column;"),
      tags$b(h2("Diagnosis Correlation")),
      plotOutput("graph"),
      selectInput("X", label = tags$b("Graph correlator"), 
                  choices = colnames(data)[-17])
    )
    
  ), # ------------------------------------ #
  
  
  # Second row has: Data grid and Graph options #
  fluidRow(
    style = paste0("margin:10px;display:flex;flex-direction:row;"),
      
      # The dataframe table
      column(6, align="center", 
        style = paste0("background-color:#b5b6bd;color:black;",
                      'border-style:groove;border-radius:20px;margin:10px;',
                      "display:flex;flex-direction:column;"),
        tags$b(h2("Available Data")),
        dataTableOutput("plot")
      ),
      
      # The correlation graph output
      column(6, align="center", 
        style = paste0("background-color:white;color:black;",
                      'border-style:groove;border-radius:20px;padding:10px;',
                      "margin:10px;display:flex;flex-direction:column;"),
        tags$b(h2("Variable Relationships")),
        plotOutput("corr")
      )
      
  ) # ------------------------------------ #

) # ============================================================================



# Define server logic here -----------------------------------------------------
server <- function(input, output) {
  
  
  # The plot to draw
  output$graph <- renderPlot({
    x = data[input$X][,1]
    y = as.factor(data["Class"][,1])
    cdplot(
      y~x, 
      xlab=input$X, 
      ylab="Diabetic Diagnosis", 
      main="Probability of Diagnosis",
      yaxlabels=c("Negative", "Positive")
    )
  })
  
  
  # Correlation matrix
  output$corr <- renderPlot({
    dt = data
    colnames(data) <- c(names(data)[-17], "Diagnosis")
    model.matrix(~0+., data=data) %>%
      cor(use="pairwise.complete.obs") %>%
      ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
  })
  
  
  # The dataframe to show
  output$plot <- DT::renderDataTable(
    {
      dt = data
      colnames(dt) <- c(names(data)[-17], "Diagnosis")
      dt
    },
    options=list(scrollX=TRUE, scrollY=TRUE, pageLength = 8)
  )
  
  
  # The prediction to show
  output$predict <- renderUI({
    n = names(data)[-17]
    x = c(
        input$Age, input$Gender, input$Polyuria, input$Polydipsia, 
        input$Sudden.Weight.Loss, input$Weakness, input$Polyphagia, 
        input$Genital.Thrush, input$Visual.Blurring, input$Itching, 
        input$Irritability, input$Delayed.Healing, input$Partial.Paresis, 
        input$Muscle.Stiffness, input$Alopecia, input$Obesity
      )
    df = as.data.frame(do.call(rbind,list(x)))
    colnames(df) <- n
    res = Predict(tree, df)
    text = "Empty"
    color = "#000000"
    if (res == 0) {
      text = "<p>Diabetic prediction: <b>NEGATIVE</b></p>"
      color = "#6cd476"
    }
    else {
      text = "<p>Diabetic prediction: <b>POSITIVE</b></p>"
      color = "#db7b70"
    }
    HTML(paste0(
      '<center style="background-color:', color, ';color:white;display:flex;',
      'flex-direction:column;justify-content:center;',
      'border-style:groove;border-radius:50px;',
      'height:50px;width:300px;"',
      '<i>', text, '</i>',
      '</center>'
    ))
    
    
  })
  
} # ============================================================================



# Run app
shinyApp(ui=ui, server=server)

