#' RunMain()
#'
#' @description Run main function
#'
#' @param scenario Scenario in CSV format
#' @param skill Novice or Expert
#'
#' @return
#' @export
#'
#' @examples
#' RunMain(csv file, "Novice")
RunMain <- function(scenario, time_library, skill) {
  Reseted <- ResetAll()
  wm_Box <- Reseted[[1]]
  chunk_Lifecyle <- Reseted[[2]]
  acc_Time_1 <- Reseted[[3]]
  acc_Time_2 <- Reseted[[4]]
  num_Oper <- ResetNumOp()
  nC <- 0 # to save the number of cognitive operators for each run
  nC_all <- 0 # to save the number of cognitive operators

  # to get the difficulty of the task
  for (i in 2:nrow(scenario)) {
    nC <- TaskDiff(scenario[i, 1])
    if (nC == 1) {
      nC_all <- nC_all + 1
    }
  }

  print(nC_all)

  # main model run
  for (i in 2:nrow(scenario)) {
    oper_Name <- ExtOper(scenario[i, 1])
    oper_Time <- RetrievingOperTime(oper_Name, i, scenario, skill, time_library, nC_all)
    acc_Time_1 <- acc_Time_1 + as.numeric(oper_Time)

    num_of_chunks <- MultipleChunks(oper_Time, oper_Name, scenario[i, 1])
    chunk_process <- ExtChunk(scenario[i, 1], oper_Name, acc_Time_1, acc_Time_2, wm_Box, oper_Time, num_of_chunks, chunk_Lifecyle)
    wm_Box <- chunk_process[[1]]
    chunk_Lifecyle <- chunk_process[[2]]
    acc_Time_2 <- acc_Time_1

    num_Oper <- GetNumOper(oper_Name, num_Oper, oper_Time, skill)
  }

  TCT <- round(acc_Time_1/1000, 2)

  MC <- GetMemoryChunk(chunk_Lifecyle, acc_Time_1)
  # JP - 2/9/22 (To make 0 chunk as NAN... This should go in mem.R)
  if (MC[[2]] == 0)
    MC[[2]]<- 0
  else
    MC[[2]]<-round(MC[[2]], 2)

  # Added "floor" to throw out decimal points of perceptual operators generated due to NOVICE's repetitive perceptions
  # final_R <- list(wm_Box, TCT, MC[[1]], round(MC[[2]], 2), floor(num_Oper))
  final_R <- list(wm_Box, TCT, MC[[1]], MC[[2]], floor(num_Oper))

  return(final_R)
}

#' WM_status()
#'
#' @description Return calculated working memory storage
#' @return
#' @export
#'
#' @examples
#' WM_status()
WM_status <- function() {
  return(wm_Box)
}
