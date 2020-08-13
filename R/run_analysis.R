#' Run rf analysis workflow
#'
#' @import magrittr
#'
#' @export
#'
run_analysis <- function(){

  caption.value <- "Select your project directory"
  dataFile <- easyRF:::choose_dataset(caption.value)

  easyRF:::import_parameters(dataFile)

  easyRF:::prepare_dataset(dataFile)

  easyRF:::rf_analysis(dataFile)

  easyRF:::rf_plot(dataFile)

  # report (prediction plots with auc,
  #rf explainer plot,
  #violin plot with pval, )

  message("\n", "\n", rep("=", times = 30), "\n",
          "# rf analysis completed #", "\n",
          rep("=", times = 30), "\n", "\n")

} #end run_analysis function
