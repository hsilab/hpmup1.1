#### data: operator set
# oper_set <- read.csv(file="./inst/extdata/cpmr_test.csv", header=FALSE)
# oper_set <- read.csv(file="./data/cpmr_test.csv", header=FALSE)
# oper_set <- read.csv(file="./data/inst/extdata/cpmr_test.csv", header=FALSE)
# oper_set <- read.csv(file="./extdata/cpmr_test.csv", header=FALSE) # Junho - 09252021

#### Skill
skill <- "Novice"
# skill <- "Expert"

# operater set
a<-c()
b<-c()
c<-c()

oper_set <- data.frame(a,b,c)

oper_set <- rbind(oper_set, c(0, 0, 0))

colnames(oper_set)[1]<-"Category_1"
colnames(oper_set)[2]<-"Category_2"
colnames(oper_set)[3]<-"TCT"

head(oper_set)

oper_set <- rbind(oper_set , c("see","Look",as.numeric(550)))
oper_set <- rbind(oper_set , c("see","Perceptual_processor",as.numeric(100)))
oper_set <- rbind(oper_set , c("see","Proofread",as.numeric(330)))
oper_set <- rbind(oper_set , c("see","Read",as.numeric(260)))
oper_set <- rbind(oper_set , c("see","Search",as.numeric(1250)))
oper_set <- rbind(oper_set , c("see","Saccade",as.numeric(30)))
oper_set <- rbind(oper_set , c("see","Hear",as.numeric(400)))
oper_set <- rbind(oper_set , c("cognitive","Attend",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Cognitive_processor",as.numeric(70)))
oper_set <- rbind(oper_set , c("cognitive","Initiate",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Ignore",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Mental",as.numeric(1250)))
oper_set <- rbind(oper_set , c("cognitive","Recall",as.numeric(550)))
oper_set <- rbind(oper_set , c("cognitive","Store",as.numeric(50)))
oper_set <- rbind(oper_set , c("cognitive","Think",as.numeric(1250)))
oper_set <- rbind(oper_set , c("cognitive","Verify",as.numeric(1250)))
oper_set <- rbind(oper_set , c("cognitive","Decide",as.numeric(50)))
oper_set <- rbind(oper_set , c("hands","Click",as.numeric(320)))
oper_set <- rbind(oper_set , c("hands","Drag",as.numeric(230)))
oper_set <- rbind(oper_set , c("hands","Grasp",as.numeric(750)))
oper_set <- rbind(oper_set , c("hands","Hands",as.numeric(450)))
oper_set <- rbind(oper_set , c("hands","Keystroke",as.numeric(280)))
oper_set <- rbind(oper_set , c("hands","Motor_processor",as.numeric(70)))
oper_set <- rbind(oper_set , c("hands","Point",as.numeric(950)))
oper_set <- rbind(oper_set , c("hands","Swipe",as.numeric(170)))
oper_set <- rbind(oper_set , c("hands","Tap",as.numeric(450)))
oper_set <- rbind(oper_set , c("hands","Touch",as.numeric(490)))
oper_set <- rbind(oper_set , c("hands","Turn",as.numeric(800)))
oper_set <- rbind(oper_set , c("hands","Type",as.numeric(280)))
oper_set <- rbind(oper_set , c("hands","Write",as.numeric(2000)))
oper_set <- rbind(oper_set , c("hands","Say",as.numeric(400)))
oper_set <- rbind(oper_set , c("system","Wait",as.numeric(1000)))
oper_set <- rbind(oper_set , c("hands","Reach",as.numeric(795.6)))
oper_set <- rbind(oper_set , c("hands","Flexion",as.numeric(209.5)))
oper_set <- rbind(oper_set , c("hands","Extension",as.numeric(201.4)))
oper_set <- rbind(oper_set , c("hands","Turn_MTM",as.numeric(306)))
oper_set <- rbind(oper_set , c("hands","Move_MTM",as.numeric(795.6)))
oper_set <- rbind(oper_set , c("hands","Open_MTM",as.numeric(520)))
oper_set <- rbind(oper_set , c("hands","GLM_drag",as.numeric(546)))
oper_set <- rbind(oper_set , c("hands","Flick",as.numeric(298.5)))
oper_set <- rbind(oper_set , c("hands","GLM_Tap",as.numeric(179)))
oper_set <- rbind(oper_set , c("hands","double_tap",as.numeric(358)))
oper_set <- rbind(oper_set , c("hands","Zoomin",as.numeric(506)))
oper_set <- rbind(oper_set , c("hands","Zoomout",as.numeric(506)))
oper_set <- rbind(oper_set , c("hands","GLM_point",as.numeric(340)))
oper_set <- rbind(oper_set , c("hands","Tap_dup",as.numeric(450)))
oper_set <- rbind(oper_set , c("Foot","Kick",as.numeric(400)))
oper_set <- rbind(oper_set , c("hands","Raise",as.numeric(1223)))
oper_set <- rbind(oper_set , c("hands","Return",as.numeric(795.6)))
oper_set <- rbind(oper_set , c("hands","Release",as.numeric(750)))
oper_set <- oper_set[-1,]

head(oper_set)



# Glossary
a<-c()
b<-c()
c<-c()
d<-c()

glossaryGUI <- data.frame(a, b, c, d)

glossaryGUI <- rbind(glossaryGUI, c(0, 0, 0, 0))

colnames(glossaryGUI)[1]<-"Name"
colnames(glossaryGUI)[2]<-"Definition"
colnames(glossaryGUI)[3]<-"Reference"
colnames(glossaryGUI)[4]<-"Task completion time (ms)"

glossaryGUI

glossaryGUI <- rbind(glossaryGUI, c("Look","Look at an item at a known position","Kieras, 1997; John & Gray, 1995; Estes, 2017",550))
glossaryGUI <- rbind(glossaryGUI, c("Read","Time to read a single word","Kieras, 1997; Estes, 2017",260))
glossaryGUI <- rbind(glossaryGUI, c("Search","Search for an item at an unknown position","Kieras, 1997; Estes, 2017",1250))
glossaryGUI <- rbind(glossaryGUI, c("Saccade","A single rapid eye movement","Card et al., 1986",230))
glossaryGUI <- rbind(glossaryGUI, c("Hear","Listen to someone speaking. Label should be the text of the speech","Kieras, 1997; John & Gray, 1995; Estes, 2017",400))
glossaryGUI <- rbind(glossaryGUI, c("Attend","Shifting of attention to stimuli","John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Initiate","Initiate motor process","John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Ignore","Removes item from working memory","Kieras, 1997; John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Mental","Generic operator for thinking","Card et al., 1980",1350))
glossaryGUI <- rbind(glossaryGUI, c("Recall","Retreive information from long term memory or working memory","Kieras, 1997; John & Gray, 1995; Estes, 2017",550))
glossaryGUI <- rbind(glossaryGUI, c("Store","Place item in working memory","Kieras, 1997; John & Gray, 1995; Estes, 2017",50))
glossaryGUI <- rbind(glossaryGUI, c("Think","Generic operator for thinking","Kieras, 1997; John & Gray, 1995; Estes, 2017",1250))
glossaryGUI <- rbind(glossaryGUI, c("Verify","Generic operator for thinking","Kieras, 1997; John & Gray, 1995; Estes, 2017",1250))
glossaryGUI <- rbind(glossaryGUI, c("Click","Press of a mouse button","Kieras, 1997; John & Gray, 1995; Estes, 2017",320))
glossaryGUI <- rbind(glossaryGUI, c("Drag","Drag an item across a screen, associated with touchscreen devices","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",546))
glossaryGUI <- rbind(glossaryGUI, c("Grasp","Act of reaching with the hand and grasping an object","Kieras, 1997; Estes, 2017",750))
glossaryGUI <- rbind(glossaryGUI, c("Hands","Move hands to position","Kieras, 1997; John & Gray, 1995; Estes, 2017",450))
glossaryGUI <- rbind(glossaryGUI, c("Keystroke","Press a single keyboard key","Kieras, 1997; John & Gray, 1995; Estes, 2017",280))
glossaryGUI <- rbind(glossaryGUI, c("Point","Move cursor via mouse","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",340))
glossaryGUI <- rbind(glossaryGUI, c("Swipe","One swipe gesture","Kieras, 1997; Estes, 2017",170))
glossaryGUI <- rbind(glossaryGUI, c("Tap","Touch a series of virtual buttons","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",179))
glossaryGUI <- rbind(glossaryGUI, c("Touch","Press a virtual button","Kieras, 1997; Estes, 2017",490))
glossaryGUI <- rbind(glossaryGUI, c("Turn","One turn of a knob or dial","Kieras, 1997; Estes, 2017",800))
glossaryGUI <- rbind(glossaryGUI, c("Type","Press a series of keyboad keys","Kieras, 1997; John & Gray, 1995; Estes, 2017",280))
glossaryGUI <- rbind(glossaryGUI, c("Write","Time to write a single word (handwriting)","Kieras, 1997; Estes, 2017",2000))
glossaryGUI <- rbind(glossaryGUI, c("Say","Speech. Label should be the text of speech","Kieras, 1997; John & Gray, 1995; Estes, 2017",400))
glossaryGUI <- rbind(glossaryGUI, c("Wait","User waiting for system. Modify time by adding x seconds at end of line","Kieras, 1997; John & Gray, 1995; Estes, 2017",1000))
glossaryGUI <- rbind(glossaryGUI, c("Reach","Move a hand to a display","Maynard et al., 1948",234))
glossaryGUI <- rbind(glossaryGUI, c("Flick","Flick a screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",298.5))
glossaryGUI <- rbind(glossaryGUI, c("double_Tap","Double tap a screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",358))
glossaryGUI <- rbind(glossaryGUI, c("Zoom in","Zoom in on the screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",506))
glossaryGUI <- rbind(glossaryGUI, c("Zoom out","Zoom out from the screen","Kim & Myung, 2016; Nystrom, 2018; Park & Zahabi, 2021",506))
glossaryGUI <- rbind(glossaryGUI, c("Point_Finger","Point finger at the screen","None",340))
glossaryGUI <- glossaryGUI[-1,]

head(glossaryGUI)


# data frame: working memory
a<-c()
b<-c()
c<-c()
d<-c()
e<-c()
f<-c()
g<-c()
h<-c()

wm_Box <- data.frame(a, b, c, d, e, f, g, h)

wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))
wm_Box <- rbind(wm_Box, c(0, 0, 0, 0, 0, 0, 0, 0))

# colnames(wm_Box)[1]<-"chunk_num"
# colnames(wm_Box)[2]<-"chunk_name"
# colnames(wm_Box)[3]<-"stack_depth"
# colnames(wm_Box)[4]<-"pushed_time(Global)"
# colnames(wm_Box)[5]<-"elapsed_time(Local)"
# colnames(wm_Box)[6]<-"rehearsal"
# colnames(wm_Box)[7]<-"activation"
# colnames(wm_Box)[8]<-"prob_recall"
colnames(wm_Box)[1]<-"Chunk_Number"
colnames(wm_Box)[2]<-"Chunk_Name"
colnames(wm_Box)[3]<-"Stack_Depth"
colnames(wm_Box)[4]<-"Chunk_Arrival_Time"
colnames(wm_Box)[5]<-"Chunk_Elapsed_Time"
colnames(wm_Box)[6]<-"Rehearsal"
colnames(wm_Box)[7]<-"Activation"
colnames(wm_Box)[8]<-"Probability_of_Recall"
wm_Box

# data frame: chunk lifecyle
cl_1 <- c()
cl_2 <- c()
cl_3 <- c()
cl_4 <- c()
cl_5 <- c()

chunk_Lifecyle <- data.frame(cl_1, cl_2, cl_3, cl_4, cl_5)
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))
chunk_Lifecyle <- rbind(chunk_Lifecyle, c(0,0,0,0,0))

colnames(chunk_Lifecyle)[1]<-"Chunk_name"
colnames(chunk_Lifecyle)[2]<-"Time_pushed"
colnames(chunk_Lifecyle)[3]<-"Time_decayed"
colnames(chunk_Lifecyle)[4]<-"Time_end"
colnames(chunk_Lifecyle)[5]<-"Time_total"
chunk_Lifecyle

# data frame: number of operators
num_Oper <- c()
no_1 <- c()
no_2 <- c()
no_3 <- c()

num_Oper <- data.frame(no_1, no_2, no_3)
num_Oper <- rbind(num_Oper, c(0,0,0))

colnames(num_Oper)[1]<-"Perceptual"
colnames(num_Oper)[2]<-"Cognitive" # It was congitive...
colnames(num_Oper)[3]<-"Motor"

#' Reset all variables
#'
#' @return Empty wm_Box, chunk_Lifecycle, acc_Time_1, acc_Time_2
#' @export
#'
#' @examples
#' ResetAll()
ResetAll <- function() {
  # reset wm_Box
  for (i in 1:7) {
    for (j in 1:8) {
      wm_Box[i,j] <- 0
    }
    wm_Box[i,1] <- i # assigning chunk number in each row
    wm_Box[i,6] <- 3 # all rehearsals are reset as 3
  }
  # reset chunk_Lifecyle
  for (i in 1:nrow(chunk_Lifecyle)) {
    for (j in 1:ncol(chunk_Lifecyle)) {
      chunk_Lifecyle[i,j] <- 0
    }
  }
  acc_Time_1 <- 0
  acc_Time_2 <- 0

  reseted <- list(wm_Box, chunk_Lifecyle, acc_Time_1, acc_Time_2)
  return(reseted)
}

#' Reset number of operators
#'
#' @return Empty num_Oper
#' @export
#'
#' @examples
#' ResetNumOp()
ResetNumOp <- function() {
  num_Oper[1,1] <- 0
  num_Oper[1,2] <- 0
  num_Oper[1,3] <- 0
  return (num_Oper)
}

#' Set scenario
#'
#' @param user_scn If a user send a scenario (CSV), then add it into the model's inner variable
#'
#' @return task_1 The scenario file that will be used in the model
#' @export
#'
#' @examples
#' DefineScenario(csv)
DefineScenario <- function(user_scn) {
  task_1 <- user_scn
  return(task_1)
}
