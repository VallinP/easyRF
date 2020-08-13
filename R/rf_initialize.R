#'#########################################################################################
#' Initialize easyRF
#'
#' Patrice Vallin, easyRF, Apr 2020.
#'########################################################################################
#'
#' @import magrittr
#' @import stats
#'
#' @export
#'
easyRF.initialize <- function(){

  library(magrittr)

  caption.value <- "Select your project directory"
  dataFile <- easyRF:::choose_dataset(caption.value)
  dir <- dirname(dataFile)

  # Attachment directory
  suppressWarnings(dir.create(paste0(dir, "/easyRF/attachments/"), recursive = TRUE))

  # Load dataset
  message("Reading dataset...")
  if(grepl(".csv", dataFile) && !suppressMessages(class(try(data.frame(read.table(dataFile, sep = ",")), silent=TRUE)) == "try-error")){
    dataset <- suppressMessages(read.csv(file=paste0(dataFile)))
  } else if(grepl(".xlsx", dataFile) && !suppressMessages(class(try(data.frame(readxl::read_xlsx(dataFile)), silent=TRUE)) == "try-error")){
    dataset <- suppressWarnings(as.data.frame(readxl::read_xlsx(dataFile)))
  } else {
    stop("Provided csv file invalid")
  }


  # rf_metadata.csv
  df <- as.data.frame(matrix(data = NA, nrow = ncol(dataset), ncol = 11))
  colnames(df) <- c("parameter", "type", "exclude", "ohe", "ID", "patient_ID",
                    "timepoint", "filter_stimulation", "filter_outcome",
                    "filter_cohort", "filter_valid")
  df[, "parameter"] <- colnames(dataset)
  rownames(df) <- NULL


  # Features Type
  message("Determining features type...")
  {
    # - Add features type (log, integer, scalar, charact)
    lapply(dataset, as.factor) %>%
      lapply(., levels) %>%
      lapply(., length) ->
      len_fact

    head(len_fact)

    null_test <- (len_fact <= 1)
    null_feat <- names(null_test[null_test == TRUE])
    df[null_test, "type"] <- "null"
    # Auto-exclude TRUE if Null type
    df[null_test, "exclude"] <- 1
    summary(null_test)

    char_test <- (sapply(dataset, is.character) & len_fact >= 3)
    char_feat <- names(char_test[char_test == TRUE])

    bin_test <- (sapply(dataset, is.character) & len_fact == 2)
    char_test2 <- (char_test == TRUE & bin_test == FALSE)
    df[char_test2, "type"] <- "char"
    # Auto-OHE TRUE if Character type
    df[char_test2, "ohe"] <- 1
    summary(char_test)

    bin_feat <- names(bin_test[bin_test == TRUE])
    df[bin_test, "type"] <- "bin"
    summary(bin_test)

    num_test <- sapply(dataset, is.numeric)
    num_feat <- names(num_test[num_test == TRUE & bin_test == FALSE])
    num_test2 <- (num_test == TRUE & bin_test == FALSE)
    df[num_test2, "type"] <- "num"
    summary(num_test)

    head(df)
  } # End features type


  # Correlation between features
  {
    message("Computing features correlation...")
    # Compute correlations
    corr_res <- stats::cor(dataset[, num_feat], use = "pairwise.complete.obs")
    head(summary(corr_res))[, c(1:6)]
    #heatmap(corr_res)

    # Find strong correlated features - TRUE if correlation > 0.8
    colMax <- function(data) sapply(data, max, na.rm = TRUE)
    corr_max <- colMax(data = as.data.frame(corr_res))
    corr_max_test <- corr_max > .8
    summary(corr_max_test)

    colMin <- function(data) sapply(data, min, na.rm = TRUE)
    corr_min <- colMin(data = as.data.frame(corr_res))
    corr_min_test <- corr_min < -.8
    summary(corr_min_test)

    corr_test <- as.logical(corr_max_test | corr_min_test)
    summary(corr_test)

    # Retrieve the correlated feat names
    corr_feat <- colnames(corr_res)[corr_test]
    corr_feat

    head(corr_res[1:6,])
    corr_test <- !( (corr_res > .8 | corr_res < - .8) & !is.na(corr_res) )


    # Extract most correlated features
    corr_res2 <- corr_res
    for(i in 1:nrow(corr_res2)){
      corr_res2[i , colnames(corr_res2)[as.logical(corr_test[,i])] ] <- " "
      for(j in 1:ncol(corr_res2)){
        if(i == j){
          corr_res2[i, j] <- "1"
        }
      }
    }

    head(corr_res2[,1:6])

    write.table(corr_res2, paste0(dir, "/easyRF/attachments/correlation_table.csv"), sep = ",")

    message(paste0(length(corr_feat), " numerical features were found to be correlated to one or more other."), "\n")

  } # end correlation

  {
    # - Percentage of missing value per features.
    NA_sum <- colSums(is.na(dataset))
    NA_pct <- 100 * sum(NA_sum) / (nrow(dataset) * ncol(dataset))

    if(sum(NA_sum) >= 1){
      NA_sum <- t(as.data.frame(NA_sum))
      colnames(NA_sum) <- colnames(dataset)
      write.table(NA_sum, file = paste0(dir,"/easyRF/attachments/NA_values_summary.csv"), sep = ",", row.names = F, col.names = T)
      message(paste0(sum(NA_sum), " NA values (over ", nrow(dataset) * ncol(dataset), ") were detected. About ", round(NA_pct, 2), " percent of the whole dataset)"))
      message("A csv file containing a summary of NA values was generated.")
      message("")

      quant <- quantile(NA_sum, 0.75, type = 1)
      if(sum(NA_sum >= as.numeric(quant)) > 100){
        quant <- quantile(NA_sum, 0.90, type = 1)
        if(sum(NA_sum >= as.numeric(quant)) > 100){
          quant <- quantile(NA_sum, 0.95, type = 1)
          }
      }

      message("Features with at least ",  quant, " NA values : ")
      cat(paste0(colnames(NA_sum)[NA_sum[NA_sum >= quant]], ", "), "\n")

    } # end NA_sum > 1

  } # End NA pct


  # Write a new metadata template
  cat("\n")
  if(!file.exists(paste0(dir, "/easyRF/attachments/rf_metadata.csv"))){
    message("No previous rf_metadata.csv file detected. Writing a new one...")
    write.table(df, file = paste0(dir,"/easyRF/attachments/rf_metadata.csv"), sep = ",", row.names = F, col.names = T)
    message("Please, complete rf_metadata.csv file before next step")
  } else {
    if(all(as.character(df[,"parameter"]) %in% read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"parameter"]) &&
       all(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"parameter"] %in% df[,"parameter"]) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"ID"])) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"patient_ID"])) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"timepoint"])) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"filter_stimulation"])) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"filter_outcome"])) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"filter_cohort"])) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_metadata.csv"))[,"filter_valid"])) ){
      message("Existing rf_metadata.csv file detected.")
    } else {
      message("Previous rf_metadata.csv file detected, but invalid. Writing a new one...")
      write.table(df, file = paste0(dir,"/easyRF/attachments/rf_metadata.csv"), sep = ",", row.names = F, col.names = T)
      message("Please, complete rf_metadata.csv file before next step")
    }
  }


  # parameters file
  df2 <- matrix(data = NA, ncol = 3, nrow = 4)
  colnames(df2) <- c("parameter", "description", paste0("value") )
  df2[, "parameter"] <- c("nIter", "objective", "seed", "max_feats")
  df2[, "description"] <- c(
    #"nIter",
    "Maximum number of boosting iterations",

    #"objective",
    "objective specifies which type of model to compute : regression or classification",

    # seed
    "integer",

    # max_feats
    "Integer >= 10"

  )


  df2[, "value"] <- c("300", "classification", "1234", "10")


  cat("\n")
  if(!file.exists(paste0(dir, "/easyRF/attachments/rf_grid_parameters.csv"))){
    message("No previous metadata.csv file detected. Writing a new one...")
    write.table(df2, file = paste0(dir,"/easyRF/attachments/rf_grid_parameters.csv"), sep = ",", row.names = F, col.names = T)
    message("Please, complete metadata.csv file before next step")
  } else {
    if(all(as.character(df2[,"parameter"]) %in% read.csv(file=paste0(dir,"/easyRF/attachments/rf_grid_parameters.csv"))[,"parameter"]) &&
       all(read.csv(file=paste0(dir,"/easyRF/attachments/rf_grid_parameters.csv"))[,"parameter"] %in% df2[,"parameter"]) &&
       !all(is.na(read.csv(file=paste0(dir,"/easyRF/attachments/rf_grid_parameters.csv"))[,"value"])) ){
      message("Existing rf_grid_parameters.csv file detected.")
    } else {
      message("Previous rf_grid_parameters file detected, but invalid. Writing a new one...")
      write.table(df2, file = paste0(dir,"/easyRF/attachments/rf_grid_parameters.csv"), sep = ",", row.names = F, col.names = T)
      message("Please, complete rf_grid_parameters.csv file before next step")
    }
  }

  cat("\n")
  message("easyRF was successfully initialized.")
  cat("\n")

} # End easyRF.initialize()
