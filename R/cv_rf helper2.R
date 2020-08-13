##########################################################################################
#' train cv rf model
#'
#' Patrice Vallin, easyrf, Apr 2020
#'
#########################################################################################
#'
#' @param data_DM a data matrix of numeric value
#' @param Y_data an array of numeric value
#' @param metric a character string refering to the name of the metric being autotuned
#' @param control a character string
#' @param searchGrid a matrix
#'
rf_cv <- function(data_DM, Y_data, seed, metric, control, searchGrid){

  # run grid search
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

  # train model
  set.seed(seed)
  custom <- caret::train(x=data_DM,
                         y=as.factor(Y_data),
                         method=customRF,
                         metric=metric,
                         tuneGrid=searchGrid,
                         trControl=control)

  output <- return(custom)

} # end function rf_cv


