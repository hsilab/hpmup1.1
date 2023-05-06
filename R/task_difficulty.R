#' TaskDiff()
#'
#' @description Return the number of cognitive operators
#' @param line_of_input
#'
#' @return operator name
#' @export
#'
#' @examples
#' ExtOper(line_of_input)
TaskDiff <- function(line_of_input) {
  oper_raw<-strsplit(line_of_input, " ")

  # dot_counter is not needed for Scenario Development tab
  mat <- matrix(unlist(oper_raw), ncol=1, byrow=TRUE)
  dot_counter <- 0
  for (i in 1:nchar(mat[1,1])) {
    c <- substr(mat[1,1], i, i)
    if (c == ".")
      dot_counter <- dot_counter + 1
  }
  dot_counter

  if (dot_counter > 0) {
    # for Cogulator generated code - Starting with a dot for every row
    oper<-substr(mat[1,1], dot_counter + 1, nchar(mat[1,1]))

    oper_set_line <- nrow(oper_set)

    # To check if a cognitive operators has been found or not.
    just_flag <- 0

    for (i in 1:oper_set_line) {
      if ( (oper_set[i, 1] == "cognitive") && (oper == oper_set[i, 2]) ) {
        just_flag <- 1
        return(1)
      }
    }
    if (just_flag == 0)
    {
      return(0)
    }
  } else {
    # for Scenario Development tab - NO DOT at the beginning of the code
    oper_set_line <- nrow(oper_set)

    # To check if a cognitive operators has been found or not.
    just_flag <- 0

    for (i in 1:oper_set_line) {
      if ( (oper_set[i, 1] == "cognitive") && (oper_raw[[1]][3] == oper_set[i, 2]) ) {
        just_flag <- 1
        return(1)
      }
    }
    if (just_flag == 0)
    {
      return(0)
    }
  }
}
