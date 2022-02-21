library(ProjetML1) # import an rmse function !



CV_blocks = function(yourData, no_folds, model, params){
  # Copy below here -------------------------
  
  # Template. A remplacer : (yourData, no_folds, model, params)
  K = no_folds
  data = yourData
  folds <- cut(seq(1,nrow(data)),breaks=K,labels=FALSE)
  fold_scores = c()
  start_time = Sys.time()
  for(i in 1:K){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    model.train(formula, trainData, params)
    Y_test = testData$Y
    Y_predict = model.predict(subset(testData, select = -c(Y)))
    fold_scores = c(fold_scores, rmse(Y_test, Y_predict))
  }
  CV_exec_time = Sys.time()-start_time
  CV_score = mean(fold_scores)
  return(CV_score)
  
  # Copy until here---------------------
  # return(fold_scores)
}