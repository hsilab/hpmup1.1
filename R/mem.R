
# Function: Memory chunk ==============================================================================================================
#' GetMemoryChunk()
#' @description Calculate and return memory load
#' @param chunk_Lifecyle A table with all chunk's birth and death
#' @param acc_Time_1 Entire task duration
#'
#' @return A list of chunk lifecyle and memory load
#' @export
#'
#' @examples
#' GetMemoryChunk(chunk lifecycle, total task completion time)
GetMemoryChunk <- function (chunk_Lifecyle, acc_Time_1) {
  MC <- 0
  # Fill out empty end_Time if a chunk only has start_Time
  for (i in 1:nrow(chunk_Lifecyle)) {
    if (chunk_Lifecyle[i,1] != 0 && (chunk_Lifecyle[i,3] == 0 | is.na(chunk_Lifecyle[i,3])))
      chunk_Lifecyle[i,3] <- acc_Time_1
    MC <- MC + abs( chunk_Lifecyle[i,2] - chunk_Lifecyle[i,3] )
  }
  MC <- MC/acc_Time_1
  MC_list <- list(chunk_Lifecyle, MC)
  return(MC_list)
}

#' MultipleChunks()
#'
#' @description Extract multiple chunks in one vector
#' @param oper_Time Operator time (e.g., Look is 550ms)
#' @param oper_Name Operator name
#' @param line Line number
#'
#' @return Number of chunks in one line
#' @export
#'
#' @examples
#' MultipleChunks(operator time, operator name, line number)
MultipleChunks <- function (oper_Time, oper_Name, line) {
  counter <- 0

  start <- 0
  end <- 0

  multiple_chunk_index <- 0

  # Chunk extraction
  for(i in 1:nchar(line)) {
    if(substr(line,i,i) == "<")
      start <- i
    else if(substr(line,i,i) == ">")
      end <- i
    else
      counter<-counter+1

    if(start * end != 0) {
      multiple_chunk_index <- multiple_chunk_index + 1
      start <- 0
      end <- 0
    }
  }
  return(multiple_chunk_index)
}

#' ExtChunk()
#'
#' @description Extracting and pushing a chunk (Main function)
#' @param line_of_input Line number
#' @param oper Operator name
#' @param time_current Current time
#' @param time_past Time when a previous operator was added
#' @param wm_Box A data frame for working memory storage
#' @param oper_Time Operator time
#' @param num_of_chunks Number of chunk in one line
#' @param chunk_Lifecyle A data frame with all chunk's birth and death
#'
#' @return A list of chunk lifecycle and working memory
#' @export
#'
#' @examples
#' ExtChunk(line_of_input, oper, time_current, time_past, wm_Box, oper_Time, num_of_chunks, chunk_Lifecyle)
#' ExtChunk(3, "Look", 5000, 4200, working memory, 550, 1, chunk_Lifecycle)
ExtChunk <- function (line_of_input, oper, time_current, time_past, wm_Box, oper_Time, num_of_chunks, chunk_Lifecyle) {

  #print("ExtChunk")
  #print(line_of_input)
  counter <- 0
  candidate_chunk <- 0
  multiple_chunk_counter <- 0

  start <- 0
  end <- 0

  for(i in 1:nchar(line_of_input)) {

    # Chunk extraction
    if(substr(line_of_input,i,i) == "<")
      start <- i
    else if(substr(line_of_input,i,i) == ">")
      end <- i
    else
      counter<-counter+1

    # Chunk formation
    # multiple chunk counter is used to divide the pushed time in chunk_Lifecyle
    if (start * end != 0) {
      candidate_chunk <- substr(line_of_input, start, end)
      multiple_chunk_counter <- multiple_chunk_counter + 1
    }

    # New chunk or old chunk?
    if (candidate_chunk != 0 || counter == nchar(line_of_input)) {

      other_chunk_counter <- 0
      same_chunk_counter <- 0
      empty_chunk_counter <- 0

      this_is_new_chunk <- 0
      this_is_old_chunk <- 0

      # check if the candidate is new or old(same)
      for (i in 1:nrow(wm_Box)) {
        if ( (wm_Box$Chunk_Name[i] != candidate_chunk) && (wm_Box$Chunk_Name[i] != 0) )
          other_chunk_counter <- other_chunk_counter + 1
        else if (wm_Box$Chunk_Name[i] == 0)
          empty_chunk_counter <- empty_chunk_counter + 1
        else
          same_chunk_counter <- same_chunk_counter + 1
      }

      # Push a new chunk
      if ( (same_chunk_counter == 0) && (candidate_chunk != 0)) {
        new_added_wm_Box <- PushChunk(candidate_chunk, wm_Box, time_current, oper, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle)
        wm_Box <- as.data.frame(new_added_wm_Box[1])
        chunk_Lifecyle <- as.data.frame(new_added_wm_Box[2])
      } else if (multiple_chunk_counter > 1) {
        new_added_wm_Box <- PushChunk(candidate_chunk, wm_Box, time_current, oper, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle)
        wm_Box <- as.data.frame(new_added_wm_Box[1])
        chunk_Lifecyle <- as.data.frame(new_added_wm_Box[2])
      }

      # Update chunk
      if (empty_chunk_counter != 7) {
        update_wm_Box <- UpdateChunk(candidate_chunk, oper, time_current, wm_Box, same_chunk_counter)
        wm_Box <- update_wm_Box
      }
      start<-0
      end<-0
      candidate_chunk<-0
    }
  }
  a <- wm_Box
  b <- chunk_Lifecyle
  c <- list(a, b)
  return(c)
}

#' PushChunk()
#'
#' @description Push a new chunk
#' @param new_chunk Chunk name
#' @param wm_Box A data frame for working memory storage
#' @param time_current Current time
#' @param oper Operator name
#' @param oper_Time Operator time
#' @param num_of_chunks Number of chunk in one line
#' @param time_past Time when a previous operator was added
#' @param multiple_chunk_counter A number to make a difference if there are more than two chunks in one line ???
#' @param chunk_Lifecyle A data frame with all chunk's birth and death
#'
#' @return A list of chunk lifecycle and working memory
#' @export
#'
#' @examples
#' PuchChunk(new_chunk, wm_Box, time_current, oper, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle)
PushChunk <- function (new_chunk, wm_Box, time_current, oper, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle) {

  # count the number of empty chunks (or occupied chunks)
  count_zero <- 0
  lifecycle <- data.frame()

  for (i in 1:nrow(wm_Box)) {
    if (wm_Box[i,2] == 0)
      count_zero <- count_zero + 1
  }

  # current number of chunk information
  empty_rows = count_zero
  occupied_rows = 7- count_zero

  # Decaying and make 7th row as empty
  if (occupied_rows == 7) {
    decay_Process <- DecayChunk(wm_Box, time_current, chunk_Lifecyle)
    wm_Box <- decay_Process[[1]]
    chunk_Lifecyle <- decay_Process[[2]]
  }

  # Set the row number for the chunk should be located
  if (occupied_rows == 7)
    assgined_Chunk_Num = 7
  else
    assgined_Chunk_Num = 7 - count_zero + 1

  wm_Box[assgined_Chunk_Num, 2] <- new_chunk # chunk_name
  wm_Box[assgined_Chunk_Num, 3] <- GetStackDepth(wm_Box$Chunk_Name) # stack_depth
  wm_Box[assgined_Chunk_Num, 4] <- time_current # pushed_time (FIXED)
  wm_Box[assgined_Chunk_Num, 5] <- (time_current + 50) -  wm_Box[assgined_Chunk_Num, 4]  # elapsed_time (NOT FIXED)
  wm_Box[assgined_Chunk_Num, 6] <- GetRehearsal(oper, wm_Box[assgined_Chunk_Num, 2], wm_Box[assgined_Chunk_Num, 6], 0, new_chunk) # rehearsal
  wm_Box[assgined_Chunk_Num, 7] <- GetActivation(wm_Box[assgined_Chunk_Num, 3], wm_Box[assgined_Chunk_Num, 5], wm_Box[assgined_Chunk_Num, 6]) # activation
  wm_Box[assgined_Chunk_Num, 8] <- GetProbRecall(wm_Box[assgined_Chunk_Num, 7]) # prob_recall

  # Record the lifecycle information
  lifecycle <- RecordChunk(wm_Box, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle)

  # Combine wm_Box and chunk_Lifecycle
  combined_result <- list(wm_Box, lifecycle)

  return(combined_result)
}

#' RecordChunk()
#'
#' @description Record the lifecycle information
#' @param wm_Box A data frame for working memory storage
#' @param oper_Time Operator time
#' @param num_of_chunks Number of chunk in one line
#' @param time_past Time when a previous operator was added
#' @param multiple_chunk_counter A number to make a difference if there are more than two chunks in one line ???
#' @param chunk_Lifecyle A data frame with all chunk's birth and death
#'
#' @return chunk lifecycle
#' @export
#'
#' @examples
#' RecordChunk(wm_Box, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle)
RecordChunk <- function (wm_Box, oper_Time, num_of_chunks, time_past, multiple_chunk_counter, chunk_Lifecyle) {
  # check the row number for newly added chunk
  count_zero_wm <- 0

  for (i in 1:nrow(wm_Box)) {
    if (wm_Box[i,2] == 0)
      count_zero_wm <- count_zero_wm + 1
  }
  new_chunk_index <- 7 - count_zero_wm

  # check the row of the chunk_Lifecycle where the new chunk should be added
  count_zero_cl <- 0

  for (i in 1:nrow(chunk_Lifecyle)) {
    if (chunk_Lifecyle[i,1] == 0)
      count_zero_cl <- count_zero_cl + 1
  }
  row_for_new_chunk <- nrow(chunk_Lifecyle) - count_zero_cl + 1

  # Record the information on chunk_Lifecycle : chunk name
  chunk_Lifecyle[row_for_new_chunk, 1] <- wm_Box[new_chunk_index, 2]

  # Record the information on chunk_Lifecycle : chunk pushed Time. Each chunk in one row will be evenly distributed in the oper_Time
  if (multiple_chunk_counter == 1 && num_of_chunks > 1)
    chunk_Lifecyle[row_for_new_chunk, 2] <- time_past + 50
  else if (num_of_chunks == 1)
    chunk_Lifecyle[row_for_new_chunk, 2] <- wm_Box[new_chunk_index, 4]
  else if (multiple_chunk_counter > 1)
    chunk_Lifecyle[row_for_new_chunk, 2] <- time_past + (oper_Time/num_of_chunks) * (multiple_chunk_counter-1)

  return(chunk_Lifecyle)
}

#' DecayChunk()
#'
#' @description Make a chunk be eliminated
#' @param wm_Box A data frame for working memory storage
#' @param time_current Current time
#' @param chunk_Lifecyle A data frame with all chunk's birth and death
#'
#' @return A list of chunk lifecycle and working memory
#' @export
#'
#' @examples
#' DecayChunk(wm_Box, time_current, chunk_Lifecyle)
DecayChunk <- function (wm_Box, time_current, chunk_Lifecyle) {
  # check the lowest activation and its row number
  lowest_act <- min(wm_Box[,7])
  for (i in 1:nrow(wm_Box)) {
    if (wm_Box[i,7] == lowest_act)
      row_number_lowest_act <- i
  }
  # Find that chunk and record the decayed time into chunk_Lifecyle
  for (i in 1:nrow(chunk_Lifecyle)) {
    if(chunk_Lifecyle[i, 1] == wm_Box[row_number_lowest_act, 2])
      chunk_Lifecyle[i, 3] <- time_current
  }
  # Remove the chunk from wm_Box (Overwrite the row with next rows)
  k <- 0
  for (i in 1:nrow(wm_Box)) {
    if(i > row_number_lowest_act) {
      for (j in 1:ncol(wm_Box)) {
        wm_Box[row_number_lowest_act + k, j] <- wm_Box[i, j]
      }
      k <- k + 1
    }
  }
  # Fill out the 7th chunk with 0
  for (i in 1:ncol(wm_Box)) {
    wm_Box[7, i] <- 0
  }
  # Combine wm_Box and chunk_Lifecycle
  combined_result <- list(wm_Box, chunk_Lifecyle)
  return(combined_result)
}

#' UpdateChunk()
#'
#' @description Update time, rehearsal, activation, and recall prob of old chunks, not the brand new chunk
#' @param candidate_chunk Chunk name
#' @param oper_name Operator name
#' @param time_current Current time
#' @param wm_Box A data frame for working memory storage
#' @param same_chunk_counter A variable to see if two chunks are same or not
#'
#' @return working memory storage
#' @export
#'
#' @examples
#' UpdateChunk(candidate_chunk, oper_name, time_current, wm_Box, same_chunk_counter)
UpdateChunk <- function (candidate_chunk, oper_name, time_current, wm_Box, same_chunk_counter) {

  for (i in 1:nrow(wm_Box)) {
    # Update for the other chunks
    if ( (wm_Box$Chunk_Name[i] != candidate_chunk) && (wm_Box$Chunk_Name[i] != 0) && (time_current != wm_Box$Chunk_Arrival_Time[i])) {
      # wm_Box[i, 5] <- time_current - wm_Box[i, 4] - 50
      wm_Box[i, 5] <- time_current - wm_Box[i, 4]
      wm_Box[i, 7] <- GetActivation(wm_Box[i, 3], wm_Box[i, 5], wm_Box[i, 6]) # activation update
      wm_Box[i, 8] <- GetProbRecall(wm_Box[i, 7]) # prob_recall update
    }
    # Rehearsal : Update for the same chunk except for the first push
    else if ( (wm_Box$Chunk_Name[i] == candidate_chunk) && (same_chunk_counter != 0) ) {
      # wm_Box[i, 5] <- time_current - wm_Box[i, 4] - 50
      wm_Box[i, 5] <- time_current - wm_Box[i, 4]
      updated_rehearsal <- GetRehearsal(oper_name, candidate_chunk, wm_Box[i, 6], same_chunk_counter, candidate_chunk)
      wm_Box[i, 6] <- updated_rehearsal
      wm_Box[i, 7] <- GetActivation(wm_Box[i, 3], wm_Box[i, 5], updated_rehearsal) # activation update
      wm_Box[i, 8] <- GetProbRecall(wm_Box[i, 7]) # prob_recall update
    }
  }
  return(wm_Box)
}

# Function: Activation =========================================================================================================================================
#' GetActivation()
#'
#' @description Calculation for activation
#' @param stack_Depth The floor number of a chunk in working memory storage (wm_Box)
#' @param last_Time Current time
#' @param rehearsals Number of rehearsals
#'
#' @return Calculated activation value
#' @export
#'
#' @examples
#' GetActivation(stack_Depth, last_Time, rehearsals)
GetActivation <- function (stack_Depth, last_Time, rehearsals) {
  m1 = log(rehearsals/sqrt(last_Time/1000))
  m2 = m1 + 1/stack_Depth - 1
  return(m2)
}

# Function: rehearsal =========================================================================================================================================
#' GetRehearsal()
#'
#' @description Calculate the number of rehearsals
#' @param oper Operator name
#' @param chk_nam_list A list of chunk names in working memory storage
#' @param current_rehearsal The number of rehearsals
#' @param same A variable to check if the new chunk is the same with one of chunks in working memory storage
#' @param chunk_Name Chunk name
#'
#' @return Number of rehearsals
#' @export
#'
#' @examples
#' GetRehearsal(oper, chk_nam_list, current_rehearsal, same, chunk_Name)
GetRehearsal <- function (oper, chk_nam_list, current_rehearsal, same, chunk_Name) {

  # Expert - This logic is to identify chunk whether it is in LTM
  if ( !is.na(chunk_Name) ) {
    result_text <- c()
    separation_space <- 1
    word_start <- 1
    word_end <- 0

    char_pointer <- " "

    for (i in 1:nchar(chunk_Name)) {
      char_pointer <- substr(chunk_Name, i, i)
      if (char_pointer == "<")
        word_start <- word_start + 1
      else if (char_pointer == " " || char_pointer == ">") {
        word_end <- i - 1
        result_text[separation_space] <- substr(chunk_Name, word_start, word_end)
        separation_space <- separation_space + 1
        word_start <- i + 1
      }
    }

    temp_LTM <- GetLTM()
    for (i in 1:length(result_text)) {
      for (j in 1:length(temp_LTM)) {
        if (result_text[i] == temp_LTM[j])
          return (10)
      }
    }
  }
  # Novice - logic from Cogulator
  if ( (oper == "Recall" || oper == "Hear" || oper == "Say") && same > 0)
    return(current_rehearsal + 1)
  else if (oper == "Recall" && same == 0)
    return(10)
  else if (oper == "Store")
    return (current_rehearsal + 1)
  else
    return (3)
}

# Function: Storage for experts' long-term memory
#' GetLTM()
#'
#' @description An expert's long-term memory
#' @return A vector with LTM of an expert
#' @export
#'
#' @examples
#' GetLTM()
GetLTM <- function() {
  exp_LTM <- c()
  exp_LTM[1] <- "Road"
  exp_LTM[2] <- "Drive"
  exp_LTM[3] <- "Traffic"
  exp_LTM[4] <- "Alpha"
  exp_LTM[5] <- "Road"
  exp_LTM[6] <- "Drive"
  exp_LTM[7] <- "Traffic"
  exp_LTM[8] <- "Alpha"
  return(exp_LTM)
}

# Function: prob of recall ====================================================================================================================================
#' GetProbRecall()
#'
#' @description Calculation on recallability using activation
#' @param m_value Activation
#'
#' @return Recall probability
#' @export
#'
#' @examples
#' GetProbRecall(activation)
GetProbRecall <- function (m_value) {
  tau <- -1 # threshold
  # s <- 0.2 # noise for experts
  s <- 0.8 # noise for novices

  p <- 1/(1+exp((tau-m_value)/s))
  return(p)
}


# Function: number of operator ================================================================================================================================
#' GetNumOper()
#'
#' @description Return the number of operators (Calculate repetition for novices)
#'
#' @param oper_Name Operator name
#' @param num_Oper Number of operators
#' @param oper_Time Operator time (duration)
#' @param skill Novice or Expert
#'
#' @return Number of operator
#' @export
#'
#' @examples
#' GetNumOper(oper_Name, num_Oper, oper_Time, skill)
GetNumOper <- function (oper_Name, num_Oper, oper_Time, skill) {
  for (i in 1:nrow(oper_set)) {
    if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "see") && (skill == "Novice") ) {
      total_P <- oper_Time/(as.numeric(oper_set[i,3]))
      num_Oper$Perceptual <- num_Oper$Perceptual + total_P
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "see") ) {
      num_Oper$Perceptual <- num_Oper$Perceptual + 1
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "speech") ) {
      num_Oper$Perceptual <- num_Oper$Perceptual + 1
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "cognitive") && (skill == "Novice") ) {
      total_C <- oper_Time/(as.numeric(oper_set[i,3]))
      # print(total_C)
      num_Oper$Cognitive <- num_Oper$Cognitive + total_C
      # print(num_Oper$Cognitive)
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "cognitive") ) {
      num_Oper$Cognitive <- num_Oper$Cognitive + 1
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "hands") && (skill == "Novice") ) {
      # total_M <- oper_Time/(as.numeric(oper_set[i,3]))
      # num_Oper$Motor <- num_Oper$Motor + total_M
      if (oper_Name != "Say") {
        total_M <- oper_Time/(as.numeric(oper_set[i,3]))
        num_Oper$Motor <- num_Oper$Motor + total_M
      }
      else if (oper_Name == "Say") {
        num_Oper$Motor <- num_Oper$Motor + 1
      }
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "hands") ) {
      num_Oper$Motor <- num_Oper$Motor + 1
    }
    else if ( (oper_Name == oper_set[i,2]) && (oper_set[i, 1] == "Foot") ) {
      num_Oper$Motor <- num_Oper$Motor + 1
    }
  }
  return(num_Oper)
}

#' GetLOI()
#'
#' @description Line of input generator
#' @param op_Name Operator name
#' @param err_chunk_made Error chunk
#'
#' @return Line
#' @export
#'
#' @examples
#' GetLOI(op_Name, err_chunk_made)
GetLOI <- function (op_Name, err_chunk_made) {
  made_line <- paste(op_Name, err_chunk_made)
  made_line
  return(made_line)
}

#' GetStackDepth()
#'
#' @description Return the depth of chunk that was added into the stack
#' @param chk_name chunk name
#'
#' @return
#' @export
#'
#' @examples
#' GetStackDepth(chunk name)
GetStackDepth <- function (chk_name) {
  stack_flag <- 0

  for (i in 1:length(chk_name)) {
    if (chk_name[i] == 0)
      stack_flag <- stack_flag + 1
  }
  stack_depth <- 7 - stack_flag
  return(stack_depth)
}
