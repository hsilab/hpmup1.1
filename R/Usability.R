
############################################ ############################################ ################################
############################################ ############################################ ################################
############################################ LEARNIBILITY ################################################################
############################################ ############################################ ################################
############################################ ############################################ ################################

#' CalcLearnability
#'
#' @param config device configuration
#' @param resil_p physical resilience
#' @param resil_m mental resilience
#' @param wl_p physical workload
#' @param wl_m mental workload
#' @param thre_min minimum threshold
#' @param thre_max maximum threshold
#' @param tct_cycle tct cycle
#' @param SDCQ subjective device calibration quality
#' @param pcps percent change of pupil size
#' @param br blink rate
#' @param perf task performance overall
#' @param nC number of cognitive operators
#' @param nP number of perceptual operators
#' @param nM number of motor operators
#' @param training number of required training trials
#' @param tct_cycle_exp_calced task performance one cycle calculated from the model
#' @param mem_chunk_calced memory chunk calculated from the model
#' @param output_control need to determine between physical and virtual
#' @param task_type CRT or SHAP
#' @param user_input Ask whether to use calculated value or manually added value
#' @param Effort The amount of effort
#' @param FI First impression
#'
#' @return
#' @export
#'
#' @examples skipped
CalcLearnability <- function(output_control, task_type, config, thre_min, thre_max, resil_p, resil_m, wl_p, wl_m, tct_cycle_exp_calced, mem_chunk_calced, SDCQ, FI, # f_impression, f_amount,
                             pcps, br, perf, tct_cycle, nC, nP, nM, training, user_input, Effort) { # , py_a, py_b, py_c, py_d, py_e, py_f, py_g, py_h)

  # DC-p11: 20, 35, 0.67, negative, 0.43, yes, yes, moderate, low...
  # PR-p27: 15, 25, 0.83, positive, 0.33, yes, yes, high, moderate...
  # CC-p17: 16, 23, 0.92, positive, 0.40, yes, yes, moderate, moderate...
  # JP - 02102023
  A <- CalcA(config, SDCQ, FI)

  Index_X <- CalcSlopeCurve(resil_p, resil_m, wl_p, wl_m, SDCQ)
  Learnability <- FindX(A, Index_X, thre_max)

  print("1. Learnability: ")
  print(Learnability)

  ErrorRate <- CalcError(Learnability, output_control)
  print("2. Error Rate: ")
  print(ErrorRate)

  perf_expert <- 120/tct_cycle_exp_calced   # this 120 should also be customized as an input AND just for CRT

  Efficiency <- CalcEfficiency(ErrorRate, perf_expert, task_type)
  print("3. Efficiency: ")
  print(Efficiency)

  print("4. Memory chunk: ")
  print(mem_chunk_calced)

  # JP - 02102023
  satisfaction <- CalcSatisfaction(Learnability, Efficiency, thre_min, thre_max, resil_p, resil_m, wl_p, wl_m, output_control, Effort)
  print("5. Satisfaction: ")
  print(satisfaction)


  if (config == 1) {
    config_DC <- 1
    config_PR <- 0
    config_CC <- 0
  } else if (config == 2) {
    config_DC <- 0
    config_PR <- 1
    config_CC <- 0
  } else {
    config_DC <- 0
    config_PR <- 0
    config_CC <- 1
  }

  # DC-p11: 0.3970572, 4.5, 8, 8.136, 8, 2, 7, 6.4, 1.7, 6, 1, 1, -1
  # PR-p27: 0.04815, 7, 7, 3.202, 2, 1, 5, 4.2, 0, 5, 2, -1, 1
  # CC-p17: 0.086025884, 3.5, 10, 3.913, 2, 1, 5, 4.2, 0, 4, 1, -1, -1
  # if (user_input == 1) { # This means to use Learnability, instead of user input
  #   cwl <- GetPredict(pcps, br, perf, tct_cycle, nC, nP, nM, tct_cycle_exp_calced, mem_chunk_calced, Learnability, config, task_type)
  #   print("6. CWL: ")
  #   print(cwl)
  # } else { # This means to the user input (the number of training trials)
  #   cwl <- GetPredict(pcps, br, perf, tct_cycle, nC, nP, nM, tct_cycle_exp_calced, mem_chunk_calced, training, config, task_type)
  #   print("6. CWL: ")
  #   print(cwl)
  # }
  print("6. CWL: ")

  if (user_input == 1) { # This means to use Learnability, instead of user input
    cwl <- CalcCWL(pcps, br, perf, tct_cycle, nC, nP, nM, tct_cycle_exp_calced, mem_chunk_calced, Learnability, config_CC, config_DC, config_PR)
  } else {
    cwl <- CalcCWL(pcps, br, perf, tct_cycle, nC, nP, nM, tct_cycle_exp_calced, mem_chunk_calced, training, config_CC, config_DC, config_PR)
    # cwl <- CalcCWL(0.195177, 18, 16, 2.853, 2, 1, 5, 4.2, 0, 8, 0, 0, 1)
  }
  print(cwl)

  CalcedUSA <- list(Learnability, ErrorRate, Efficiency, mem_chunk_calced, satisfaction, cwl)
  return(CalcedUSA)
}

#' FindX
#'
#' @param A tct for the first training trial
#' @param Index_X index
#' @param thre_max maximum threshold
#'
#' @return the trial number which is lower that the thre_max (moving average of three trials)
#' @export
#'
#' @examples skipped
FindX <- function(A, Index_X, thre_max) {

  flag <- 3
  time_each_trial <- c()
  moving_average <- c()

  # have time for each trial
  for (i in 1:30) {
    L <- A*i^Index_X
    time_each_trial[i] <- L
  }
  # get the moving average
  for (i in 1:28) {
    moving_average[i] <- (time_each_trial[i] + time_each_trial[i+1] + time_each_trial[i+2])/3
    print(moving_average[i])
  }
  # get the trial number when it becomes lower than the thre max
  for (i in 1:28) {
    if(moving_average[i] > thre_max) {
      flag <- flag+1
    }
  }

  return(flag)
}

#' CalcSlopeCurve
#'
#' @param resil_p physical resilience
#' @param resil_m mental resilience
#' @param wl_p physical workload
#' @param SDCQ Subjective device calibration quality
#' @param wl_m mental workload
#'
#' @return
#' @export
#'
#' @examples skipped
CalcSlopeCurve <- function(resil_p, resil_m, wl_p, wl_m, SDCQ) {

  # interpolated_slope <- -0.2
  # interpolated_interceptor <- 0.95

  # weight of the slope of the linear equation in the numerator of the index of X
  interpolated_slope <- 0
  if (resil_p > 0 & resil_m > 0) {
    interpolated_slope <- -0.2
  } else if (resil_p > 0 & resil_m < 0) {
    interpolated_slope <- -0.3
  } else if (resil_p < 0 & resil_m > 0) {
    interpolated_slope <- -0.35
  } else {
    interpolated_slope <- -0.35
  }
  print("interpolated slope: ")
  print(interpolated_slope)

  # weight of interceptor of the linear equation in the numerator of the index of X
  interpolated_interceptor <- 0
  if (wl_p > 0 & wl_m > 0) { # both physical and mental workload are extremely high
    interpolated_interceptor <- 0.92
  } else if (wl_p > 0 & wl_m < 0) { # both physical and mental workload are extremely low
    interpolated_interceptor <- 0.96
  } else if (wl_p == 0 & wl_m == 0) { # both physical and mental workload are in normal
    interpolated_interceptor <- 0.925
  } else if (wl_p > 0 & wl_m == 0) { # only physical workload is extremely high
    interpolated_interceptor <- 1
  } else if (wl_p < 0 & wl_m == 0) { # only physical workload is extremely low
    interpolated_interceptor <- 1
  } else if (wl_p == 0 & wl_m > 0) { # only mental workload is extremely high
    interpolated_interceptor <- 0.8
  } else {# wl_p == 0 & wl_m < 0 # only mental workload is extremely low
    interpolated_interceptor <- 0.93
  }
  print("interpolated interceptor: ")
  print(interpolated_interceptor)

  # index of X
  print("slope final: ")
  print(interpolated_slope * SDCQ + interpolated_interceptor)
  index_X <- log(interpolated_slope * SDCQ + interpolated_interceptor) / log(2)

  print("index_X: ")
  print(index_X)
  return(index_X)
}

#' CalcA
#'
#' @param SDCQ Subjective device calibration quality
#' @param config device configuration
#' @param FI first impression
#'
#' @return
#' @export
#'
#' @examples skipped
CalcA <- function(config, SDCQ, FI) {

  # JP - 02102023
  Theoretial_A <- 0

  if (config == 1) {
    Theoretial_A <- 54
  } else {
    Theoretial_A <- 36
  }

  conf_bias <- 0
  if (FI > 0) {
    FI <- FI + 0.65  # Positive
  } else if (FI < 0) {
    FI <- 1 - abs(FI)  # Negative
  } else {
    FI <- FI  # Neutral
  }

  print("SDCQ:")
  print(SDCQ)
  print("FI:")
  print(FI)
  print("Updated A:")
  print(Theoretial_A * FI / SDCQ)

  Updated_A <- Theoretial_A * FI / SDCQ

  return (Updated_A)

  # lot <- Att
  #
  # # the t_final is just for A (different from the logic in Satisfaction)
  # if (Att < 0.5) {
  #   lot <- 1-(0.5-Att)*2
  # } else if (Att > 0.75) {
  #
  #   lot <- (Att - 0.5)*2
  #
  # } else {
  #   lot <- Att - 0.5 + 0.65
  # }
  #
  # A <- theta * lot / cq
  # print("A: ")
  # print(A)
  # return(A)
}

############################################ ############################################ ################################
############################################ ############################################ ################################
############################################ ERRORS ######################################################################
############################################ ############################################ ################################
############################################ ############################################ ################################

#' CalcError
#'
#' @param L Learnability
#' @param output_control physical or virtual
#'
#' @return
#' @export
#'
#' @examples skipped
CalcError <- function (L, output_control) {
  if (output_control == 1)
    norm_L <- 1-L/30 # this '30' is a very assumption that the number of maximum training trials with PHYSICAL device is 30...
  else
    norm_L <- 1-L/20 # this '30' is a very assumption that the number of maximum training trials in VIRTUAL environment is 20...

  ErrorRate <- min( 1/(1-exp(1))*(exp(norm_L)-exp(1)), 1)

  return(ErrorRate)
}

############################################ ############################################ ################################
############################################ ############################################ ################################
############################################ Efficiency ##################################################################
############################################ ############################################ ################################
############################################ ############################################ ################################

#' CalcEfficiency
#'
#' @param E error
#' @param perf_expert expert's performance
#' @param task_type CRT or SHAP
#'
#' @return
#' @export
#'
#' @examples skipped
CalcEfficiency <- function (E, perf_expert, task_type) {

  print("expert performance: ")
  print(perf_expert)

  if (task_type == 1) # CRT
    effi <- 120/ (perf_expert*(1-E)) # JP - 02102023. This is because efficiency is tct_cycle: This is for CRT.
  else
    effi < max(perf_expert*(1-E), 0)

  print("Effi: ")
  print(effi)

  return ( effi )

  ### Currently, the result of the scenario file in the data folder "DC-CRT"... is different from the result in the EXCEL.
  ### Thus, there is a slight difference in Efficiency... which will also propagate the difference in Satisfaction.
}

############################################ ############################################ ################################
############################################ ############################################ ################################
############################################ Satisfaction ################################################################
############################################ ############################################ ################################
############################################ ############################################ ################################

#' CalcSatisfaction
#'
#' @param L learnability
#' @param Effi efficiency
#' @param thre_min minimum threshold
#' @param thre_max maximum threshold
#' @param resil_p slope for p
#' @param resil_m slope for m
#' @param wl_p intercept for p
#' @param wl_m interecept for m
#' @param output_control physical or virtual
#' @param Effort the amount of effort
#'
#' @return
#' @export
#'
#' @examples skipped
CalcSatisfaction <- function(L, Effi, thre_min, thre_max, resil_p, resil_m, wl_p, wl_m, output_control, Effort) {

  # JP - 02102023
  perceived_perf <- 120 / Effi # This is to convert the tct_cyled efficiency to pins (performance)

  if (output_control == 1) # PHYSICAL
    expectation <- 120/(thre_min + (thre_max-thre_min)/(2*8.4)*L)*3 # multiplication of 3 is for moving three pins, 8.53 is for PHYSICAL device
  else # VIRTUAL
    expectation <- 120/(thre_min + (thre_max-thre_min)/(2*3.85)*L)*3 # multiplication of 3 is for moving three pins, 3.85 is for VIRTUAL environment

  # JP - 02102023
  gap <- perceived_perf - expectation

  if (output_control == 1) # PHYSICAL
    dis_belief <- 1-(abs(gap) - 1)/(20-1) # This 20 is same for both PHYSICAL and VIRTUAL (also in EXCEL)
  else # VIRTUAL
    dis_belief <- min(1-(abs(gap) - 1)/(20-1), 1) # This 20 is same for both PHYSICAL and VIRTUAL (also in EXCEL)

  print("percevied_perf: ")
  print(perceived_perf)
  print("expectation: ")
  print(expectation)
  print("gap: ")
  print(gap)
  print("disconfirmation of belief: ")
  print(dis_belief)

  # JP - 02102023
  satis <- dis_belief * (1 - Effort) + 0.15 # 1 - Effort for Ease of Use

  # # This is for the satisfaction when the performance is high enough to shift the curve in Kano model
  # if (abs(gap) < 3) {
  #   if (resil_p > 0)
  #     std_gap <- std_gap - 0.2
  #   if (resil_m > 0)
  #     std_gap <- std_gap - 0.1
  #   if (wl_p > 0)
  #     std_gap <- std_gap - 0.2
  #   if (wl_p < 0)
  #     std_gap <- std_gap - 0.15
  #   if (wl_m > 0)
  #     std_gap <- std_gap - 0.1
  #   if (wl_m < 0)
  #     std_gap <- std_gap - 0.2
  # }

  return ( satis )

  # if (Att < 0.5) {
  #   return ( 1-(0.5-Att)*2 )
  # } else if (Att >= 0.75) {
  #   Att <- (Att - 0.5)*2
  #   real <- Effi
  #   expectation <- 120/(thre_min + (thre_max-thre_min)/(2*8.53)*L)
  #   return ( (Att + 0.65) * real / expectation )
  # } else {
  #   return (Att - 0.5 + 0.65)
  # }

  # if (Att < 0) {
  #   return ( 1-Att )
  #
  # } else if (Att > 0.5) {
  #
  #   real <- Effi
  #   expectation <- 120/(thre_min + (thre_max-thre_min)/(2*8.53)*L)
  #   return ( (Att + 0.65)*real/expectation )
  #
  # } else {
  #
  #   return ( Att + 0.65 )
  #
  # }
}

############################################ ############################################ ################################
############################################ ############################################ ################################
############################################ CWL #########################################################################
############################################ ############################################ ################################
############################################ ############################################ ################################

#' CalcCWL
#'
#' @param pcps later...
#' @param br later...
#' @param perf later...
#' @param tct_cycle later...
#' @param nC later...
#' @param nP later...
#' @param nM later...
#' @param tct_cycle_exp_calced later...
#' @param mem_chunk_calced later...
#' @param training later...
#' @param config_DC later...
#' @param config_PR later...
#' @param config_CC later...
#'
#' @return summation of a and b
#' @export
#'
#' @examples skipped
CalcCWL <- function(pcps, br, perf, tct_cycle, nC, nP, nM, tct_cycle_exp_calced, mem_chunk_calced, training, config_CC, config_DC, config_PR) {

  use_virtualenv("C:/Users/junho.park/AppData/Local/Programs/Python/Python39/v_cwl")
  print("--- venv loading is ok ---")

  source_python("inst/python/rRet_test.py")
  print("--- source_python loading is ok ---")

  draft <- predictSingle(pcps, br, perf, tct_cycle, nC, nP, nM, tct_cycle_exp_calced, mem_chunk_calced, training, config_CC, config_DC, config_PR)
  print("--- predictSingle is ok ---")

  if (draft == 1)
    CWL <- "High"
  else
    CWL <- "Low"

  return(CWL)
}
