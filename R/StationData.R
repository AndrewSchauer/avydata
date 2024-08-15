#' Take an initial look at a new dataset, looking at name, class, and unique values for each column in a new data frame.
#'
#' @param data_in A new data.frame. This should previously be loaded into the R environment.
#'
#' @return A list containing a snapshot of the new dataset. "Name" contains the name of each column, "Class" contains the class of each column, and "Unique" contains the unique values of each column. The list is called "Info" and will print when the function is executed.
#'
#' @name StationData
#' @export
library(devtools)

StationData <- function(data_in){
  meta_list <- list()

  for(i in 1:ncol(data_in)){
    meta_list[[i]] <-
      list("Name" = names(data_in)[i],
           "Class" = class(data_in[,i]),
           "Unique" = sort(unique(data_in[,i])))
  }
  for(i in 1:ncol(data_in)){
    print(meta_list[[i]])
  }
  attributes(StationData) <- list("Info" = meta_list)
}
