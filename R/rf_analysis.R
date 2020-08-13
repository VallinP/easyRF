##########################################################################################
#' Analyse the dataset with randomForest
#'
#' Patrice Vallin, easyRF, Apr 2020
#' .
#' SSE and findElbow function are adapted from FlowSOM R package
#' Sofie Van Gassen et al., Cytometry Part A, 2015
#' https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.22625
#' http://bioconductor.org/packages/release/bioc/html/FlowSOM.html
#########################################################################################
#'
#' @param dataFile a character string specifying the location of the project dataset
#'
#' @import magrittr
#' @import stats
#' @import randomForest
#' @import caret
#' @import rpart
#' @import dplyr
#'
rf_analysis <- function(dataFile){

  message("\n", "\n", rep("#", times = 20))
  message("# Dataset analysis #")
  message(rep("#", times = 20), "\n")


  ###################################################
  #  Step 1 : Load data & settings
  ###################################################

  library("caret")
  library("randomForest")
  library("magrittr")

  # Select a dataset
  message("Loading dataset & parameters...", "\n")
  projectDir <- dirname(dataFile)
  filename <- basename(dataFile)
  load(paste0(projectDir,"/easyrf/rf_analysis.Rdata"))

  # Set pars
  max_feats <- rf_pars[1, "max_feats"]
  objective <- rf_pars[1, "objective"] #levels(droplevels(rf_pars[1, "objective"]))
  eval_metric <- rf_pars[1, "eval_metric"] #levels(droplevels(rf_pars[1, "eval_metric"]))
  seed <- rf_pars[1, "seed"]
  set.seed(seed)


  ################################################################################
  # Create a training and testing group object
  ################################################################################
  {
    rf_ID <-
      outcome_var <-
      rf_feats <-
      rf_feat_names <-
      rf_model <-
      rpart_model <-
      rf_model_cv <-
      rf_cv_performance <-
      elbow_point_res <-
      vector(mode = "list", length = length(filterPars[["outcome"]]))

    names(rf_ID) <-
      names(outcome_var) <-
      names(rf_feats) <-
      names(rf_feat_names) <-
      names(rf_model) <-
      names(rpart_model) <-
      names(rf_model_cv) <-
      names(rf_cv_performance) <-
      names(elbow_point_res) <-
      filterPars[["outcome"]]

  }


  ##########################################################
  # Compute the model and validate it for each filter keep
  ##########################################################
  # set loop parameters
  x1 <- 1 # variable detude
  x3 <- 1 # stimulation/biological condition
  x4 <- 1
  nLoop <- 1

  for (x1 in 1:length(filterPars[["outcome"]])) {

    # set names of lists inTrain, rf and model
    {
      rf_ID[[x1]] <-
        outcome_var[[x1]] <-
        rf_feats[[x1]] <-
        rf_feat_names[[x1]] <-
        rf_model[[x1]] <-
        rpart_model[[x1]] <-
        rf_model_cv[[x1]] <-
        rf_cv_performance[[x1]] <-
        elbow_point_res[[x1]] <-
        vector(mode = "list", length = length(filterPars[["stimulationValues"]]))

      names(rf_ID[[x1]]) <-
        names(outcome_var[[x1]]) <-
        names(rf_feats[[x1]]) <-
        names(rf_feat_names[[x1]]) <-
        names(rf_model[[x1]]) <-
        names(rpart_model[[x1]]) <-
        names(rf_model_cv[[x1]]) <-
        names(rf_cv_performance[[x1]]) <-
        names(elbow_point_res[[x1]]) <-
        filterPars[["stimulationValues"]]

    }


    for (x3 in  1:length(filterPars[["stimulationValues"]])) {

      message(rep("-", times = 30))
      message(paste0("Variable ", filterPars[["outcome"]][x1]))
      message(paste0("Biological condition ", filterPars[["stimulationValues"]][x3]))
      message(rep("-", times = 30), "\n")

      {
        outcome_var[[x1]][[x3]] <-
          rf_feats[[x1]][[x3]] <-
          rf_feat_names[[x1]][[x3]] <-
          rf_model[[x1]][[x3]] <-
          rpart_model[[x1]][[x3]] <-
          rf_model_cv[[x1]][[x3]] <-
          rf_cv_performance[[x1]][[x3]] <-
          vector(mode = "list", length = 3)

        names(outcome_var[[x1]][[x3]]) <-
          names(rf_feats[[x1]][[x3]]) <-
          names(rf_feat_names[[x1]][[x3]]) <-
          names(rf_model[[x1]][[x3]]) <-
          names(rpart_model[[x1]][[x3]]) <-
          names(rf_model_cv[[x1]][[x3]]) <-
          names(rf_cv_performance[[x1]][[x3]]) <-
          c("Model 1", "Model 2", "Model 3")

        rf_cv_performance[[x1]][[x3]][[1]] <-
          rf_cv_performance[[x1]][[x3]][[2]] <-
          rf_cv_performance[[x1]][[x3]][[3]] <-
          as.data.frame(matrix(data = NA, ncol = 6))

        if(eval_metric == "Accuracy"){col_rf_cv <- c("ntree", "mtry", "Accuracy",
                                                     "Kappa", "AccuracySD", "KappaSD")}
        if(eval_metric == "RMSE"){col_rf_cv <- c("ntree", "mtry", "RMSE",
                                                 "Rsquared", "RMSESD", "RsquaredSD")}
        colnames(rf_cv_performance[[x1]][[x3]][[1]]) <-
          colnames(rf_cv_performance[[x1]][[x3]][[2]]) <-
          colnames(rf_cv_performance[[x1]][[x3]][[3]]) <-
          col_rf_cv

      }

      ##########################################################
      # Select stimulation rows & remove dumy vars & empty cols
      ##########################################################
      {
        # Select rows matching the current stimulation value
        filter_rows <- df_all_combined$stimulation == filterPars[["stimulationValues"]][x3]
        filter_rows[is.na(filter_rows)] <- 0
        if(TRUE %in% levels(as.factor(is.na(filter_rows)))){ stop("NA values detected during the sort of one or more variables")}

        # Store primary ID (& featnames ?)
        rf_ID[[x1]][[x3]] <- df_all_combined[filter_rows == 1, filterPars[["ID"]]]

        # Remove dummy vars
        dataset_stim <- data.frame(df_all_combined[filter_rows == 1, !colnames(df_all_combined) %in% filterPars[["filter_all"]]])
        suppressWarnings(rm(filter_rows))

        # Remove empty columns (again)
        currentNcol <- dim(dataset_stim)[2]
        dataset_stim <- Filter(function(x)!all(is.na(x)), dataset_stim)
        cat(paste0(currentNcol - dim(dataset_stim)[2], " empty features removed (at this step)."), "\n")
        cat(paste0("Dataset dimension : ", dim(dataset_stim)[1], " rows, ", dim(dataset_stim)[2], " features."), "\n", "\n")

      } # End select stimulation rows and remove dummy vars


      {
        # store outcome feature
        outcome_var[[x1]][[x3]] <- dataset_stim[, filterPars[["outcome"]][x1]]
        dataset_stim <- dataset_stim[, !colnames(dataset_stim) %in% filterPars[["outcome"]][x1]]
      }


      ####################################################
      # Step 5: Run an initial rf model
      ####################################################

      x4 <- 1
      rf_feats[[x1]][[x3]][[x4]] <- ncol(dataset_stim)
      rf_feat_names[[x1]][[x3]][[x4]] <- colnames(dataset_stim)
      if(max_feats == "auto"){max_feats <- 0}

      {
        # Create a rf matrix containing all the data
        suppressWarnings(rm(list = c("data_matrix", "data_DM")))
        data_matrix <- data.matrix(dataset_stim[, rf_feat_names[[x1]][[x3]][["Model 1"]]])
        Y_data <- outcome_var[[x1]][[x3]]

        data_matrix <- na.omit(cbind(Y_data, data_matrix))
        data_matrix <- cbind(Y_data, data_matrix)
        data_DM <- as.data.frame(data_matrix[, !colnames(data_matrix) == "Y_data"])
        Y_data <- data_matrix[, "Y_data"]

        dim(data_DM)
        colnames(data_DM) == colnames(dataset_stim)
      }

      # Hypertuning
      message(paste0("Model ", x4, " : ", rf_feats[[x1]][[x3]][[x4]], " features"), "\n")

      # Hypertuning RF
      if(ncol(data_DM) > 20){
        control <- trainControl(method="cv", number=5, search = "grid")
      } else {
        control <- trainControl(method="repeatedcv", repeats=3, number=5, search = "grid")
      }
      searchGrid <- generate_searchGrid(ncol(data_DM), seed, rf_pars)
      system.time(
        rf_model_cv[[x1]][[x3]][[x4]] <- rf_cv(data_DM=data_DM,
                                             Y_data=Y_data,
                                             seed=seed,
                                             metric=eval_metric,
                                             control=control,
                                             searchGrid = searchGrid)
      )

      rf_model_cv[[x1]][[x3]][[x4]]

      rf_cv_performance[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]$results
      rf_cv_performance[[x1]][[x3]][[x4]] <- rf_cv_performance[[x1]][[x3]][[x4]][order(rf_cv_performance[[x1]][[x3]][[x4]][,eval_metric],
                                                                                       decreasing = T),]

      # Extract final model
      rf_model[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]$finalModel

      # Extract Variable importance

      # Compute feature importance matrix
      message("Extraction of the important features")
      importance <- randomForest::importance(rf_model[[x1]][[x3]][[x4]])
      varImportance <- data.frame(Variables = row.names(importance),
                                  Importance = round(importance[ ,'MeanDecreaseGini'],2))
      # Create a rank variable based on importance
      rankImportance <- varImportance %>%
        dplyr::mutate(Rank = paste0('#',dplyr::dense_rank(dplyr::desc(Importance))))
      head(importance)
      head(varImportance)

      # Order feat importance
      feat.order <- rev(order(varImportance$Importance))
      varImportance <- varImportance[feat.order,]
      feat_names1 <- as.character(varImportance[,"Variables"])
      feat_n1 <- length(feat_names1)
      head(feat_names1)

      # rpart based classification tree
      if(objective == "classification"){
        fol= formula( paste(" as.factor(Y_data) ~ ", paste(feat_names1, collapse = " + "), collapse = " ") )
        rpart_model[[x1]][[x3]][[x4]] <- rpart::rpart( fol, data=data_DM, method= "class")
      } else {
        fol= formula( paste(" Y_data ~ ", paste(feat_names1, collapse = " + "), collapse = " ") )
        rpart_model[[x1]][[x3]][[x4]] <- rpart::rpart( fol, data=data_DM, method= "anova")
      } # end if classif


      #####################################################s################
      # Step 6: Tune number of features
      #####################################################################
      message("Determining the Elbow point")
      importance_matrix <- varImportance[, "Importance"]
      names(importance_matrix) <- varImportance[, "Variables"]
      elbow_point_res[[x1]][[x3]][[x4]] <- importance_matrix

      smooth <- 0.2
      for (i in 2:(feat_n1[[x1]][[x3]][[x4]] - 1)) {
        elbow_point_res[[x1]][[x3]][[x4]][i] <- (1 - smooth) * elbow_point_res[[x1]][[x3]][[x4]][i] +
          (smooth/2) * elbow_point_res[[x1]][[x3]][[x4]][i - 1] +
          (smooth/2) * elbow_point_res[[x1]][[x3]][[x4]][i + 1]
      }

      elbow_point <- easyRF:::findElbow(elbow_point_res[[x1]][[x3]][[x4]])
      if(max_feats == 0){max_feats <- elbow_point}


      #################################################################################
      # Step 7: Run rf on eblow_point defined number of features, CV_parameters
      #################################################################################
      x4 <- 2
      rf_feats[[x1]][[x3]][[x4]] <- elbow_point
      rf_feat_names[[x1]][[x3]][[x4]] <- feat_names1[c(1:elbow_point)]
      rf_feats[[x1]][[x3]][[x4]] <- length(rf_feat_names[[x1]][[x3]][[2]])

      {
        # Create a rf matrix containing all the data
        suppressWarnings(rm(list = c("data_matrix", "data_DM")))
        data_matrix <- data.matrix(dataset_stim[, rf_feat_names[[x1]][[x3]][[x4]] ])
        Y_data <- outcome_var[[x1]][[x3]]

        data_matrix <- na.omit(cbind(Y_data, data_matrix))
        #data_matrix <- cbind(Y_data, data_matrix)
        data_DM <- as.data.frame(data_matrix[, !colnames(data_matrix) == "Y_data"])
        Y_data <- data_matrix[, "Y_data"]

        dim(data_DM)
        colnames(data_DM) == colnames(dataset_stim[, feat_names1[c(1:elbow_point)]])
      }

      # Hypertuning
      message(paste0("Model ", x4, " : ", rf_feats[[x1]][[x3]][[x4]], " features"), "\n")

      # Hypertuning RF
      if(ncol(data_DM) > 20){
        control <- trainControl(method="cv", number=5, search = "grid")
      } else {
        control <- trainControl(method="repeatedcv", repeats=3, number=5, search = "grid")
      }
      searchGrid <- generate_searchGrid(ncol(data_DM), seed, rf_pars)
      system.time(
        rf_model_cv[[x1]][[x3]][[x4]] <- rf_cv(data_DM=data_DM,
                                               Y_data=Y_data,
                                               seed=seed,
                                               metric=eval_metric,
                                               control=control,
                                               searchGrid = searchGrid)
      )

      rf_model_cv[[x1]][[x3]][[x4]]

      rf_cv_performance[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]$results
      rf_cv_performance[[x1]][[x3]][[x4]] <- rf_cv_performance[[x1]][[x3]][[x4]][order(rf_cv_performance[[x1]][[x3]][[x4]][,eval_metric],
                                                                                       decreasing = T),]

      # Extract final model
      rf_model[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]$finalModel

      # Extract Variable importance

      # Compute feature importance matrix
      message("Extraction of the important features")
      importance <- randomForest::importance(rf_model[[x1]][[x3]][[x4]])
      varImportance <- data.frame(Variables = row.names(importance),
                                  Importance = round(importance[ ,'MeanDecreaseGini'],2))
      # Create a rank variable based on importance
      rankImportance <- varImportance %>%
        dplyr::mutate(Rank = paste0('#',dplyr::dense_rank(dplyr::desc(Importance))))
      head(importance)
      head(varImportance)

      # Order feat importance
      feat.order <- rev(order(varImportance$Importance)) #[c(1:rf_feat_n[[x1]][[x3]][[x4]])]
      varImportance <- varImportance[feat.order,]
      feat_names2 <- as.character(varImportance[,"Variables"])
      feat_n2 <- length(feat_names2)
      head(feat_names2)

      # rpart based classification tree
      if(objective == "classification"){
        fol= formula( paste(" as.factor(Y_data) ~ ", paste(feat_names2, collapse = " + "), collapse = " ") )
        rpart_model[[x1]][[x3]][[x4]] <- rpart::rpart( fol, data=data_DM, method= "class")
      } else {
        fol= formula( paste(" Y_data ~ ", paste(feat_names2, collapse = " + "), collapse = " ") )
        rpart_model[[x1]][[x3]][[x4]] <- rpart::rpart( fol, data=data_DM, method= "anova")
      } # end if classif

      #################################################################################
      # Step 8: Run rf on max_feats defined number of features, CV_parameters
      #################################################################################
      x4 <- 3
      if(rf_feats[[x1]][[x3]][[2]] > max_feats){

        rf_feats[[x1]][[x3]][[x4]] <- max_feats
        rf_feat_names[[x1]][[x3]][[x4]] <- feat_names2[c(1:max_feats)]
        rf_feats[[x1]][[x3]][[x4]] <- length(rf_feat_names[[x1]][[x3]][[x4]])

        {
          # Create a rf matrix containing all the data
          suppressWarnings(rm(list = c("data_matrix", "data_DM")))
          data_matrix <- data.matrix(dataset_stim[, rf_feat_names[[x1]][[x3]][[x4]]])
          Y_data <- outcome_var[[x1]][[x3]]

          data_matrix <- na.omit(cbind(Y_data, data_matrix))
          #data_matrix <- cbind(Y_data, data_matrix)
          data_DM <- as.data.frame(data_matrix[, !colnames(data_matrix) == "Y_data"])
          Y_data <- data_matrix[, "Y_data"]

          dim(data_DM)
          colnames(data_DM) == colnames(dataset_stim[, feat_names2[c(1:max_feats)]])
        }

        # Hypertuning
        message(paste0("Model ", x4, " : ", rf_feats[[x1]][[x3]][[x4]], " features"), "\n")

        # Hypertuning RF
        if(ncol(data_DM) > 20){
          control <- trainControl(method="cv", number=5, search = "grid")
        } else {
          control <- trainControl(method="repeatedcv", repeats=3, number=5, search = "grid")
        }
        searchGrid <- generate_searchGrid(ncol(data_DM), seed, rf_pars)
        system.time(
          rf_model_cv[[x1]][[x3]][[x4]] <- rf_cv(data_DM=data_DM,
                                                 Y_data=Y_data,
                                                 seed=seed,
                                                 metric=eval_metric,
                                                 control=control,
                                                 searchGrid = searchGrid)
        )

        rf_model_cv[[x1]][[x3]][[x4]]

        rf_cv_performance[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]$results
        rf_cv_performance[[x1]][[x3]][[x4]] <- rf_cv_performance[[x1]][[x3]][[x4]][order(rf_cv_performance[[x1]][[x3]][[x4]][,eval_metric],
                                                                                         decreasing = T),]

        # Extract final model
        rf_model[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]$finalModel

        # Extract Variable importance

        # Compute feature importance matrix
        message("Extraction of the important features")

        importance <- randomForest::importance(rf_model[[x1]][[x3]][[x4]])
        varImportance <- data.frame(Variables = row.names(importance),
                                    Importance = round(importance[ ,'MeanDecreaseGini'],2))
        # Create a rank variable based on importance
        rankImportance <- varImportance %>%
          dplyr::mutate(Rank = paste0('#',dplyr::dense_rank(dplyr::desc(Importance))))

        # Order feat importance
        feat.order <- rev(order(varImportance$Importance)) #[c(1:rf_feat_n[[x1]][[x3]][[x4]])]
        varImportance <- varImportance[feat.order,]
        feat_names3 <- as.character(varImportance[,"Variables"])
        feat_n3 <- length(feat_names3)
        head(feat_names3)

        # rpart based classification tree
        if(objective == "classification"){
          fol= formula( paste(" as.factor(Y_data) ~ ", paste(feat_names3, collapse = " + "), collapse = " ") )
          rpart_model[[x1]][[x3]][[x4]] <- rpart::rpart( fol, data=data_DM, method= "class")
        } else {
          fol= formula( paste(" Y_data ~ ", paste(feat_names3, collapse = " + "), collapse = " ") )
          rpart_model[[x1]][[x3]][[x4]] <- rpart::rpart( fol, data=data_DM, method= "anova")
        } # end if classif

      } else {

        rf_feats[[x1]][[x3]][[3]] <- rf_feats[[x1]][[x3]][[2]]
        rf_feat_names[[x1]][[x3]][[3]] <- rf_feat_names[[x1]][[x3]][[2]]
        rf_feats[[x1]][[x3]][[3]] <- rf_feats[[x1]][[x3]][[2]]
        rf_model_cv[[x1]][[x3]][[x4]] <- rf_model_cv[[x1]][[x3]][[x4]]
        rf_model[[x1]][[x3]][[x4]] <- rf_model[[x1]][[x3]][[x4]]

      } # end if feat_n4 > max_feats

    } #end for filter x3

  } #end for filter x1


  message("\n", paste0("Saving data..."))
  suppressWarnings(
    save(list = c("dataFile", "df_all_combined", "outcome_var",
                  "filterPars", "rf_pars", "rf_ID",
                  "rf_model", "rpart_model", "rf_model_cv", "rf_feats", "rf_feat_names",
                  "rf_cv_performance", "elbow_point_res"),
         file = paste0(projectDir, "/easyRF/rf_analysis.Rdata"),
         compress = "gzip")
  )


} # end rf_analysis
