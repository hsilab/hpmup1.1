#' ExtOper()
#'
#' @description Return a pure operator name
#' @param line_of_input
#'
#' @return operator name
#' @export
#'
#' @examples
#' ExtOper(line_of_input)
ExtOper <- function(line_of_input) {
  oper_raw<-strsplit(line_of_input, " ")

  # print("ExtOper")
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

    # print(oper)
    return(oper)
  } else {
    # for Scenario Development tab - NO DOT at the beginning of the code

    # print(oper_raw[[1]][3])
    return(oper_raw[[1]][3])
  }
}

# a<-read.csv(file="D:/JP_project_test/ncpm/task_1.csv", header=TRUE)
#
# RetrievingOperTime("Look", 2, a, "Novice")

#' RetrOpTime_Motor()
#'
#' @description Retrieving the operator time (for error)
#' @param oper operator name
#' @param k line number
#'
#' @return error time for a motor operator
#' @export
#'
#' @examples
#' RetrOpTime_Motor(operator name, line number)
RetrOpTime_Motor <- function (oper, k) {

  # get the time of hand gesture
  err_Time <- 0
  err_Time <- GetErr(oper)
  return(err_Time)
}

#' RetrievingOperTime()
#'
#' @description Retrieving the operator time
#'
#' @param oper operator name
#' @param k line number
#' @param scenario Scenario in CSV format
#' @param skill Novice or Expert
#'
#' @return operator time for an operator
#' @export
#'
#' @examples
#' RetrievingOperTime(operator name, line number, scenario, skill, oper_set, nC)
RetrievingOperTime <- function (oper, k, scenario, skill, oper_set, nC) {
  oper_set_line <- nrow(oper_set)

  # bring operator time from the database
  matched_Time <- 0
  for (i in 1:oper_set_line) {
    if (oper == oper_set[i, 2])
      matched_Time <- as.numeric(oper_set[i, 3])
  }

  if (nC < 2) {
    # N-CPM : SRK-Skill level (Easy/simple task) - repetition in perception only!
    # N-CPM : Novice Cognitive
    if ( (oper == "Think" | oper == "Recall" | oper == "Store") && (skill == "Novice") ) {
      # repetition <- sample(1:2, size=1) # Novice look & searching pattern
      for (i in 1:oper_set_line) {
        if (oper == oper_set[i, 2])
          matched_Time <- as.numeric(oper_set[i, 3])
      }
      # matched_Time <- repetition * matched_Time
    }

    # N-CPM : Novice Perceptual
    if ( (oper == "Look" | oper == "Search" | oper == "Read" | oper == "Hear") && (skill == "Novice") ) {
      repetition <- sample(1:2, size=1) # Novice look & searching pattern
      for (i in 1:oper_set_line) {
        if (oper == oper_set[i, 2])
          matched_Time <- as.numeric(oper_set[i, 3])
      }
      matched_Time <- repetition * matched_Time
    }

    # N-CPM : Novice Motor
    if ( (oper == "Touch" | oper == "Tap" | oper == "Reach" | oper == "Return" | oper == "Grasp" | oper == "Raise" | oper == "Release" | oper == "Return" | oper == "Type") && (skill == "Novice") ) {
      # repetition was deactivated (commented) because the task in this category is so simple
      # repetition <- sample(2:3, size=1) # Novice look & searching pattern
      for (i in 1:oper_set_line) {
        if (oper == oper_set[i, 2])
          matched_Time <- as.numeric(oper_set[i, 3])
      }
      # matched_Time <- repetition * matched_Time
    }
  } else if (nC == 2) {
    # N-CPM : SRK-Rule level (moderate task) - repetition for all!!

    # N-CPM : Novice Cognitive
    if ( (oper == "Think" | oper == "Recall" | oper == "Store") && (skill == "Novice") ) {
      repetition <- sample(1:2, size=1) # Novice look & searching pattern
      for (i in 1:oper_set_line) {
        if (oper == oper_set[i, 2])
          matched_Time <- as.numeric(oper_set[i, 3])
      }
      matched_Time <- repetition * matched_Time
    }

    # N-CPM : Novice Perceptual
    if ( (oper == "Look" | oper == "Search" | oper == "Read" | oper == "Hear") && (skill == "Novice") ) {
      repetition <- sample(1:2, size=1) # Novice look & searching pattern
      for (i in 1:oper_set_line) {
        if (oper == oper_set[i, 2])
          matched_Time <- as.numeric(oper_set[i, 3])
      }
      matched_Time <- repetition * matched_Time
    }

    # N-CPM : Novice Motor
    if ( (oper == "Touch" | oper == "Tap" | oper == "Reach" | oper == "Return" | oper == "Grasp" | oper == "Raise" | oper == "Release" | oper == "Return" | oper == "Type") && (skill == "Novice") ) {
      repetition <- sample(1:2, size=1) # Novice look & searching pattern
      for (i in 1:oper_set_line) {
        if (oper == oper_set[i, 2])
          matched_Time <- as.numeric(oper_set[i, 3])
      }
      matched_Time <- repetition * matched_Time
    }
  } else if (nC > 2) {
      # N-CPM : SRK-Knowledge level (Hard/complex task) - more repetitions, especially for cognition!!

      # N-CPM : Novice Cognitive
      if ( (oper == "Think" | oper == "Recall" | oper == "Store" | oper == "Verify" | oper == "Decide") && (skill == "Novice") ) {
        repetition <- sample(3:6, size=1) # Novice look & searching pattern.. .Kav ref
        for (i in 1:oper_set_line) {
          if (oper == oper_set[i, 2])
            matched_Time <- as.numeric(oper_set[i, 3])
        }
        matched_Time <- repetition * matched_Time
      }

      # N-CPM : Novice Perceptual
      if ( (oper == "Look" | oper == "Search" | oper == "Read" | oper == "Hear") && (skill == "Novice") ) {
        repetition <- sample(2:3, size=1) # Novice look & searching pattern
        for (i in 1:oper_set_line) {
          if (oper == oper_set[i, 2])
            matched_Time <- as.numeric(oper_set[i, 3])
        }
        matched_Time <- repetition * matched_Time
      }

      # N-CPM : Novice Motor
      if ( (oper == "Touch" | oper == "Tap" | oper == "Reach" | oper == "Return" | oper == "Grasp" | oper == "Raise" | oper == "Release" | oper == "Return" | oper == "Type") && (skill == "Novice") ) {
        repetition <- sample(2:3, size=1) # Novice look & searching pattern
        for (i in 1:oper_set_line) {
          if (oper == oper_set[i, 2])
            matched_Time <- as.numeric(oper_set[i, 3])
        }
        matched_Time <- repetition * matched_Time
      }
  }

  # Deprecated because there is overlap between CPM-GOMS and two laws...
  # # N-CPM : Novice Motor
  # if ( (oper == "Touch" | oper == "Tap") && (skill == "Novice") ) {
  #   RT <- 0 # Hick-Hyman law (Hick, 1952)
  #   MT <- 0 # Fitts' law (Fitts, 1954)
  #   nov_Chunk <- 1.9 # (Chase & Simon, 1973)
  #
  #   nov_dat_display <- 10 # number of items in the display for novices (all items)
  #
  #   icon_width <- 3
  #   icon_distance <- 30
  #
  #   RT_nov <- 1/nov_Chunk * log2(nov_dat_display)
  #   # MT_nov <- 1/nov_Chunk * log2(2*nov_dat_display/icon_width) * 1000 # millisecond
  #
  #   matched_Time <- RT_nov # + MT_nov # for novices
  #   repetition <- sample(1:3, size=1) # Kenny & Deshmukh, 2011
  #   matched_Time <- matched_Time * repetition # Kenny & Deshmukh, 2011
  #
  # } else if ( (oper == "Touch") && (skill == "Expert") ) {
  #   # N-CPM : Expert Motor
  #   RT <- 0 # Hick-Hyman law (Hick, 1952)
  #   MT <- 0 # Fitts' law (Fitts, 1954)
  #   exp_Chunk <- 2.5 # (Chase & Simon, 1973)
  #
  #   exp_dat_display <- 2 # number of items in the display for experts (only essential information
  #   icon_width <- 3
  #   icon_distance <- 30
  #
  #   RT_exp <- 1/exp_Chunk * log2(exp_dat_display)
  #   # MT_exp <- 1/exp_Chunk * log2(2*exp_dat_display/icon_width) * 1000 # millisecond
  #
  #   matched_Time <- RT_exp # + MT_exp # for experts
  # } else if ( (oper == "Reach" | oper == "Return" | oper == "Grasp" | oper == "Raise" | oper == "Release") && (skill == "Novice") ) { # Kenny & Deshmukh, 2011
  #   repetition <- sample(1:2, size=1) # Novice look & searching pattern
  #   for (i in 1:oper_set_line) {
  #     if (oper == oper_set[i, 2])
  #       matched_Time <- as.numeric(oper_set[i, 3])
  #   }
  #   matched_Time <- repetition * matched_Time
  # }

  # calculation time for hearing and saying : each word is 400 ms
  if (oper == "Hear" | oper == "Say") {
    num_Words <- strsplit(scenario[k, 1], " ")
    num_Words <- length(num_Words[[1]]) - 1
    matched_Time <- num_Words * 400
    } else if (oper == "Type") {
      dot_counter <- 0
      for (i in 1:nchar(scenario[k,1])) {
        c <- substr(scenario[k,1], i, i)
        if (c == ".")
          dot_counter <- dot_counter + 1
      }
      dot_counter
      # 280=average type speed, 4 = number of characters in "type"
      matched_Time <- (nchar(scenario[k,1]) - 4 - dot_counter) * 280
    } else if (oper == "Tap") {
      dot_counter <- 0
      for (i in 1:nchar(scenario[k,1])) {
        c <- substr(scenario[k,1], i, i)
        if (c == ".")
          dot_counter <- dot_counter + 1
      }
      dot_counter
      # 450=average tap speed, 3 = number of characters in "type"
      matched_Time <- (nchar(scenario[k,1]) - 3 - dot_counter) * 450
  }


  # # N-CPM : Novice "Read" involves more thingkings
  # if ( (oper == "Read") && (skill == "Novice") ) {
  #   repetition <- sample(1:2, size=1) # Novice look & searching pattern
  #   # oper <- "Think"
  #   for (i in 1:oper_set_line) {
  #     if (oper == oper_set[i, 2])
  #       matched_Time <- as.numeric(oper_set[i, 3]) # Junho - 12052021
  #   }
  #   matched_Time <- repetition * matched_Time
  # }

  return(matched_Time)
}



