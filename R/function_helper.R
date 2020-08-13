

##########################################################################################
#' Evaluate a xgboost model through cross validation
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param data_DM an xgboost pointer
#' @param searchGrid a matrix
#'
#' @import magrittr
#' @import stats
#' @import xgboost
#'
#' @return ErrorsHyperparameters
#'
xgboost_cv <- function(data_DM, searchGrid){

  set.seed(searchGrid[1, "seed"])
  currentObjective <- searchGrid[1, "objective"]
  currentEval_metric <- searchGrid[1, "eval_metric"]

  # run grid search
  system.time(
    ErrorsHyperparameters <- apply(searchGrid, 1, function(parameterList){

      #Extract Parameters to test
      currentEta <- parameterList[["eta"]]
      currentMinChild <- parameterList[["min_child"]]
      currentDepth <- parameterList[["max_depth"]]
      currentGamma <- parameterList[["gamma"]]
      currentSubsampleRate <- parameterList[["subsample"]]
      currentColsampleRate <- parameterList[["colsample_bytree"]]
      currentLambda <- parameterList[["lambda"]]
      currentAlpha <- parameterList[["alpha"]]

      currentnIter <- parameterList[["nIter"]]

      xgboostModelCV <- xgboost::xgb.cv(data =  data_DM,
                                        nrounds = currentnIter,
                                        nfold = 5,
                                        showsd = TRUE,

                                        "eta" = currentEta,

                                        "min_child_weight" = currentMinChild,
                                        "max.depth" = currentDepth,

                                        "gamma" = currentGamma,

                                        "subsample" = currentSubsampleRate,
                                        "colsample_bytree" = currentColsampleRate,

                                        "lambda" = currentLambda,
                                        "alpha " = currentAlpha,

                                        metrics = currentEval_metric,
                                        eval_metric = currentEval_metric,
                                        objective = currentObjective, #"reg:linear",
                                        booster = "gbtree",
                                        early_stopping_rounds = 10,
                                        print_every_n = 10,
                                        verbose = 0
      )

      xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)

      if(currentEval_metric == "rmse"){
        train_eval <- tail(xvalidationScores$test_rmse_mean, 1)
        test_eval <- tail(xvalidationScores$train_rmse_mean,1)
      } #end if rmse

      if(currentEval_metric == "auc"){
        train_eval <- tail(xvalidationScores$test_auc_mean, 1)
        test_eval <- tail(xvalidationScores$train_auc_mean,1)
      } #end if auc

      bestIter <- xgboostModelCV$best_iteration
      sum_eval <- (train_eval + test_eval)
      output <- return(c(train_eval, test_eval, sum_eval, bestIter,
                         currentEta, currentMinChild, currentDepth,
                         currentGamma, currentSubsampleRate, currentColsampleRate,
                         currentLambda, currentAlpha))
    }) # end xgboost gridsearchcv
  ) # end sys.time

  {
    # xvalidationScores
    ErrorsHyperparameters <- as.data.frame(cbind(t(ErrorsHyperparameters), NA, NA))
    colnames(ErrorsHyperparameters) <- c("train_eval", "test_eval", "sum_eval", "bestIter",
                                         "eta", "min_child", "max_depth",
                                         "gamma", "subsample", "colsample_bytree",
                                         "lambda", "alpha",
                                         "eval_metric", "objective")

    if(currentEval_metric == "rmse"){
      ErrorsHyperparameters <- ErrorsHyperparameters[order(ErrorsHyperparameters[,"sum_eval"]),]
    } # end rmse

    if(currentEval_metric == "auc"){
      ErrorsHyperparameters <- ErrorsHyperparameters[order(ErrorsHyperparameters[,"sum_eval"], decreasing = T),]
    } # end auc

    ErrorsHyperparameters[, "objective"] <- currentObjective
    ErrorsHyperparameters[, "eval_metric"] <- currentEval_metric

  } # end xvalidationscores

  return(ErrorsHyperparameters)

} # end function xgboost_cv





##########################################################################################
#' Construct a xgboost model based on cross validation
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param dataset_stim a matrix
#' @param outcome_var an array
#' @param ErrorsHyperparameters a matrix
#' @param classif_type a character string
#' @param seed a numeric value
#' @param k a numeric value
#'
#' @import magrittr
#' @import stats
#' @import caret
#' @import xgboost
#' @importFrom pROC roc multiclass.roc auc
#'
#' @return xgb
#'
prediction_model <- function(dataset_stim, outcome_var,
                             ErrorsHyperparameters, classif_type,
                             seed, k){

  # split train and test
  inTrain <- caret::createDataPartition(y = outcome_var ,
                                        p = as.numeric(levels(ErrorsHyperparameters[1, "subsample"])),
                                        times = k, list = T)

  xgb <- vector(mode = "list", length = k)
  model <- vector(mode = "list", length = k)
  xgb_roc_obj <- vector(mode = "list", length = k)
  pred_score <- rep(NA, times = k)

  for(x1 in c(1:k)){

    # data to train the model
    training_matrix <- data.matrix(dataset_stim[inTrain[[x1]], ])
    Y_train <- outcome_var[inTrain[[x1]] ]
    training_DM <- xgboost::xgb.DMatrix(data = training_matrix, label = Y_train)

    # subset the rest to test
    testing_matrix <- data.matrix(dataset_stim[-inTrain[[x1]], ])
    Y_testing <- outcome_var[-inTrain[[x1]] ]
    testing_DM <- xgboost::xgb.DMatrix(data = testing_matrix, label = Y_testing)

    # set xgbosst pars
    default_param <- list(eta = as.numeric(levels(ErrorsHyperparameters[1, "eta"]))[ErrorsHyperparameters[1,"eta"]],

                          min_child_weight = as.numeric(levels(ErrorsHyperparameters[1, "min_child"]))[ErrorsHyperparameters[1,"min_child"]],
                          max.depth = as.numeric(levels(ErrorsHyperparameters[1, "max_depth"]))[ErrorsHyperparameters[1,"max_depth"]],

                          gamma = as.numeric(levels(ErrorsHyperparameters[1, "gamma"]))[ErrorsHyperparameters[1,"gamma"]],

                          subsample = as.numeric(levels(ErrorsHyperparameters[1, "subsample"]))[ErrorsHyperparameters[1,"subsample"]],
                          colsample_bytree = as.numeric(levels(ErrorsHyperparameters[1, "colsample_bytree"]))[ErrorsHyperparameters[1,"colsample_bytree"]],

                          lambda = as.numeric(levels(ErrorsHyperparameters[1, "lambda"]))[ErrorsHyperparameters[1,"lambda"]],
                          alpha = as.numeric(levels(ErrorsHyperparameters[1, "alpha"]))[ErrorsHyperparameters[1,"alpha"]],

                          #eval_metric = levels(ErrorsHyperparameters[1, "eval_metric"])[ErrorsHyperparameters[1,"eval_metric"]],
                          objective = levels(ErrorsHyperparameters[1, "objective"])[ErrorsHyperparameters[1,"objective"]],
                          booster = "gbtree",
                          nthread = 4,
                          seed = seed
    )


    # train the model using the best iteration found by cross validation
    suppressWarnings(
      xgb[[x1]] <- xgboost::xgb.train(data = training_DM,
                                      params = default_param,
                                      eval_metric = "merror",
                                      nrounds = as.numeric(levels(ErrorsHyperparameters[1,"bestIter"]))[ErrorsHyperparameters[1,"bestIter"]],
                                      print_every_n = 10,
                                      verbose = 0
                                      # num_class = 12,
                                      # save_name = paste0("xgboost.model_",filterPars[["outcome"]][x1], "_", filterPars[["stimulationValues"]][x3])
      ) # end xgb.train
    ) # end suppresswarnings


    # predict values in test set
    xgb_preds <- stats::predict(xgb[[x1]], testing_matrix)

    if(classif_type == "binary"){
      suppressMessages(xgb_roc_obj[[x1]] <- pROC::roc(Y_testing, round(xgb_preds)) )
      pred_score[x1] <- pROC::auc(xgb_roc_obj[[x1]])
    }
    if(classif_type == "multi"){
      suppressMessages(xgb_roc_obj[[x1]] <- pROC::multiclass.roc(Y_testing, xgb_preds) )
      pred_score[x1] <- pROC::auc(xgb_roc_obj[[x1]])
    }
    if(classif_type == "regression"){
      suppressMessages(pred_score[x1] <- caret::RMSE(Y_testing, xgb_preds) )
    }

  } # end for x1

  pred_score_mean <- mean(pred_score)

  if(classif_type == "binary" || classif_type == "multi"){
    cat("XGB mean AUC ", pred_score_mean, "\n")
    best_k <- which(pred_score == max(pred_score))[1]
    cat("XGB max AUC ", pred_score[best_k])
  }

  if(classif_type == "regression"){
    cat("XGB RMSE ", pred_score_mean, "\n")
    best_k <- which(pred_score == min(pred_score))[1]
    cat("XGB min RMSE ", pred_score[best_k])
  }
  message("\n", "\n")

  return(xgb[[best_k]])

} # end function prediction model





##########################################################################################
#' SSE and findElbow function are adapted from FlowSOM R package
#' Sofie Van Gassen et al., Cytometry Part A, 2015
#' https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.22625
#' http://bioconductor.org/packages/release/bioc/html/FlowSOM.html
#########################################################################################
#'
#' @param data a array of numeric values
#'
#' @import stats
#'
#' @return optimal
#'
findElbow <- function (data)
{
  n <- length(data)
  data <- as.data.frame(cbind(1:n, data))
  colnames(data) <- c("X", "Y")
  min_r <- Inf
  optimal <- 1
  for (i in 2:(n - 1)) {
    f1 <- stats::lm(Y ~ X, data[1:(i - 1), ])
    f2 <- stats::lm(Y ~ X, data[i:n, ])
    r <- sum(abs(c(f1$residuals, f2$residuals)))
    if (r < min_r) {
      min_r <- r
      optimal <- i
    }
  }
  return(optimal)
}






##################################################################







##########################################################################################
#' Find optimal xgboost parameters through hypertuning
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param data_DM an xgboost pointer
#' @param searchGrid a matrix
#'
#' @import magrittr
#' @import stats
#' @import caret
#' @import randomForest
#'
rf_hypertuning <- function(data_DM, searchGrid){

  message("\n", "Hypertuning in progress...")

  # set parameters
  customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                    class = rep("numeric", 2),
                                    label = c("mtry", "ntree"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
  }
  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, newdata)
  }
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, newdata, type = "prob")
  }
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes

  customRF

  # tuning mtry parameters
  control <- trainControl(method="cv", number=5) #, repeats=3)
  mtry = 1

  if(ncol(data_DM) < 20){
    mtry=seq(2, ncol(data_DM), 1)
  } else {
    mtry=seq(2, 20, 2)

    if(ncol(data_DM) < 45){
      mtry=c(mtry, seq(25, ncol(data_DM), 5))
    } else {
      mtry=c(mtry, seq(25, 45, 5))

      if(ncol(data_DM) < 90){
        mtry=c(mtry, seq(50, ncol(data_DM), 10))
      } else {
        mtry=c(mtry, seq(50, 90, 10))

        if(ncol(data_DM) < 200){
          mtry=c(mtry, seq(100, ncol(data_DM), 20))
        } else {
          mtry=c(mtry, seq(100, 200, 10))


          if(ncol(data_DM) > 250){
            mtry=c(mtry, seq(250, ncol(data_DM), 50))
          } else {
            mtry=c(mtry, seq(250, 500, 50))

          } # end 250+
        } #end 200
      } # end 90
    } # end 45
  } # end 20

  mtry
  tunegrid <- expand.grid(.mtry=mtry, .ntree=c(500)) #,1000, 1500, 2000, 2500))

  set.seed(seed)
  rf_model_mtry <- train(x=data_DM,
                         y=as.factor(Y_data),
                         method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

  rf_model_mtry
  plot(rf_model_mtry$results$mtry, rf_model_mtry$results$Accuracy)

  ErrorsHyperparameters <- rf_model_mtry$results[which(rf_model_mtry$results$Accuracy == max(rf_model_mtry$results$Accuracy)),]
  ErrorsHyperparameters

  # tuning ntree parameters
  control <- trainControl(method="cv", number=3) #, repeats=3)
  tunegrid <- expand.grid(.mtry=ErrorsHyperparameters$mtry, .ntree=c(500, 750, 1000, 1250, 1500))
  set.seed(seed)
  rf_model_ntree <- train(x=data_DM,
                          y=as.factor(Y_data),
                          method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

  rf_model_ntree
  plot(rf_model_ntree$results$ntree, rf_model_ntree$results$Accuracy)

  ErrorsHyperparameters <- rf_model_ntree$results[which(rf_model_ntree$results$Accuracy == max(rf_model_ntree$results$Accuracy)),]
  ErrorsHyperparameters


  message(paste0("Best eta (initial) : ", best_eta))

  message("Last cross-validation result :")
  print(head(ErrorsHyperparameters[1,]))


  return(ErrorsHyperparameters)

} # End xgboost_hypertuning








