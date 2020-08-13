##########################################################################################
#' Explain rf models
#'
#' Patrice Vallin, easyrf, Apr 2020
#'
#' Plots were adapted from  https://www.kaggle.com/pradeeptripathi/prediction-of-titanic-survival-using-random-forest
#########################################################################################
#'
#' @param dataFile a character string specifying the location of the project dataset
#'
#' @import stats
#' @import ggplot2
#' @import RColorBrewer
#' @import randomForest
#' @import rpart
#' @import dplyr
#'
rf_plot <- function(dataFile){

  library("magrittr")
  library("dplyr")
  library("randomForest")

  cat('\n')
  message(rep("#", times = 29))
  message("# randomForest reports #")
  message(rep("#", times = 29))
  cat("\n")

  # Select a dataset
  message("Loading dataset & parameters...", "\n")
  projectDir <- dirname(dataFile)
  filename <- basename(dataFile)
  load(paste0(projectDir, "/easyRF/rf_analysis.Rdata"))

  x1 <- 1 # variable detude
  x3 <- 1 # stimulation/biological condition
  x4 <- 1

  # initialize pdf printer
  pdf(file = paste0(projectDir, "/easyRF/rf_plot.pdf"), width = 20, height = 6)

  for (x1 in 1:length(filterPars[["outcome"]])) {

    for (x3 in  1:length(filterPars[["stimulationValues"]])) {

      message(rep("-", times = 30))
      message(paste0("Variable ", filterPars[["outcome"]][x1]))
      message(paste0("Biological condition ", filterPars[["stimulationValues"]][x3]))
      message(rep("-", times = 30))
      message(" ")

      for(x4 in 1:3){

        feat_n <- rf_feats[[x1]][[x3]][[x4]]
        feat_names <- as.character(rf_feat_names[[x1]][[x3]][[x4]])
        currentRF_model <- rf_model[[x1]][[x3]][[x4]]
        currentRF_model_cv <- rf_model_cv[[x1]][[x3]][[x4]]
        currentRFcv_performance <- rf_cv_performance[[x1]][[x3]][[x4]]

        message(paste0("Model ", x4, " : ", feat_n, " features"))


        # dataset
        dataset_stim <- df_all_combined[
          df_all_combined[, filterPars[["ID"]] ] %in% rf_ID[[x1]][[x3]],
          as.character(feat_names)]

        colnames(dataset_stim)


        {
          # Create a rf matrix containing all the data
          suppressWarnings(rm(list = c("data_matrix", "data_DM")))
          data_matrix <- data.matrix(dataset_stim)
          Y_data <- outcome_var[[x1]][[x3]]

          data_matrix <- na.omit(cbind(Y_data, data_matrix))
          #data_matrix <- cbind(Y_data, data_matrix)
          head(data_matrix[,1:6])
          data_DM <- as.data.frame(data_matrix[, as.character(feat_names)]) #!colnames(data_matrix) == "Y_data"])
          Y_data <- data_matrix[, "Y_data"]

          dim(data_DM)
          all(colnames(data_DM) == colnames(dataset_stim))
        }

        # Plot All cv model
        #summary(currentRF_model_cv)
        cat("cv model comparison plot", "\n")
        plot1 <- plot(currentRF_model_cv,
             main=paste0(
               filterPars$outcome[x1], " : ", filterPars$stimulationValues[x3],
               " Model ", x4, " : ", feat_n, " features", "\n",
               " CV models, Accuracy")
        )
        print(plot1)
        #dotplot(custom)

        # Show model error
        cat("final rf model error plot", "\n")
        ymax = round(max(max(apply(currentRF_model$err.rate, 2, min))+.3,
                         max(apply(currentRF_model$err.rate, 2, max))),
                     2)
        plot(currentRF_model, ylim=c(0, ymax),
             main=paste0(
               filterPars$outcome[x1], " : ", filterPars$stimulationValues[x3],
               " Model ", x4, " : ", feat_n, " features", "\n",
               " final model - Error")
        )
        legend('topright', colnames(currentRF_model$err.rate), col=1:3, fill=1:3)

        # Extract Variable importance
        # summary(currentRF_model)
        names(currentRF_model$forest$ncat)
        all(names(currentRF_model$forest$ncat) == colnames(data_DM))

        # Compute feature importance matrix
        cat("Importance features plot", "\n")
        importance <- randomForest::importance(currentRF_model)

        # Plot Var Importance
        plot(importance, main = "Importance feature plot")

        varImportance <- data.frame(Variables = row.names(importance),
                                    Importance = round(importance[ ,'MeanDecreaseGini'],2))
        # Create a rank variable based on importance
        rankImportance <- varImportance %>%
          dplyr::mutate(Rank = paste0('#',dplyr::dense_rank(dplyr::desc(Importance))))

        # Use ggplot2 to visualize the relative importance of variables
        plot1 <- ggplot2::ggplot(rankImportance, ggplot2::aes(x = reorder(Variables, Importance),
                                                             y = Importance, fill = Importance)) +
          ggplot2::geom_bar(stat='identity') +
          ggplot2::geom_text(ggplot2::aes(x = Variables, y = 0.5, label = Rank),
                             hjust=0, vjust=0.55, size = 4, colour = 'red') +
          ggplot2::labs(x = 'Variables') +
          ggplot2::coord_flip() +
          ggthemes::theme_few()

        print(plot1)


        ## plot tree
        cat("rpart-based classification tree plot", "\n")
        plot(rpart_model[[x1]][[x3]][[x4]], uniform=TRUE,
             main="Classification Tree (rpart based model")      #plot decision tree
        text(rpart_model[[x1]][[x3]][[x4]], use.n=FALSE, all=TRUE, cex=.8 )

        #print(rpart_model[[x1]][[x3]][[x4]])     #print results
        #rpart::printcp(rpart_model[[x1]][[x3]][[x4]])   #display cp table
        rpart::plotcp(rpart_model[[x1]][[x3]][[x4]]) #, main = "cross-validation results plot")    #plot cross-validation results
        ######## rpart::rsq.rpart(fit) #plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
        #summary(rpart_model[[x1]][[x3]][[x4]])   #detailed results including surrogate splits

        # Final Prediction
        # Predict using the test set
        #rf_preds <- stats::predict(currentRF_model, data_DM)
        #rf_preds
        #sum(rf_preds == Y_data)/length(Y_data)
        #pROC::auc(rf_preds, Y_data)


        # Plot Elbow point
        if(x4 == 2){
          cat("Elbow point resolve plot", "\n")
          class(elbow_point_res[[x1]][[x3]][[1]])
          plot(elbow_point_res[[x1]][[x3]][[1]], main = "Elbow point")
          abline(v = feat_n, col = "red")
        } # end x4 == 2

        cat('\n')

      } # end for x4

    } # end for x3

  } # end for x1

  dev.off()

} # end rf_plot
