##########################################################################################
#' Generate searchGrid for RF
#'
#' Patrice Vallin, easyrf, Apr 2020
#'
#########################################################################################
#'
#' @param ncol_DM a integer value
#' @param seed a integer value
#' @param rf_pars a list containing all rf pars
#'
generate_searchGrid <- function(ncol_DM, seed, rf_pars){

  mtry_max <- 5*floor(sqrt(ncol_DM))
  if(mtry_max > ncol_DM){mtry_max <- ncol_DM}

  if(mtry_max < 10){
    mtry <- seq(2, (ncol_DM - 1), 1)
    ntree = c(500, 1000, 1500)

  } else if(mtry_max < 20){
    mtry <- seq(2, ncol_DM, 2)
    ntree = c(500, 1000, 1500)

  } else {
    mtry <- seq(2, 20, 2)
    ntree = c(500, 1000)

  }
  while(mtry[length(mtry)] < mtry_max){
    mtry[length(mtry)+1] <- round(1.15*mtry[length(mtry)],0)
  }


  mtry
  ntree

  searchGrid <- expand.grid(mtry=mtry, ntree=ntree)
  return(searchGrid)

}
