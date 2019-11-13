#' @keywords internal
marksForQuestion=function(str,sec=NULL,ter=NULL) {
  if (str=="ASTAR") return (4)
  else if (str=="MCMC") return (5)
  else if (str=="FB") return (4)
  else if (str=="VIT") return (3)
  else if (str=="AB") return (3)
  else if (str=="SCH") return (4)
  else if (str=="MB") return (2)
  else if (str=="ANN") return (2)
  else if (str=="CNN") return (2)
  else if (str=="TEXT") return(getTextQuestionMarks(sec,ter))
  else
    stop("Invalid Question")
}

#' createExam
#'
#' This function will generate the latex script and associated images for a complete exam. It will be used
#' to generate the real exam, and can be used by you to generate practice exams.
#'
#' The real exam will contain 5-8 questions, selected so as to get a spread of questions from different
#' sections of the course and to not include pairs of question that are quite similar.
#'
#' The set of questions include 9 algorithmic questions and 9 text questions. You can set seeds individually
#' for each algorithmic question, and these seeds determine the values generated for the question. If
#' you set NA the question is not included on the exam. You can specify the number of text questions and
#' the seed used to sample that number from the 9 available.
#'
#' You will need to compile the latex script to generate a pdf. Make sure you keep the generated images in the
#' same folder as the tex file which compiling it. The images generated are the ab.jpg and ffnn.jpg. These
#' are generated for the alpha-beta pruning and feed forward neural networks questions respectively.
#'
#' Note that this function sinks output streams to write to file. If it is cancelled during processing, you
#' may find your output is no longer directed to console. Use the sink function to redirect output streams back
#' to console.
#' @param filename The name of the file to save the latex script in.
#' @param ASTAR The random seed for the A* question. Pass NA to not include the question.
#' @param MCMC The random seed for the Bayesian network/MCMC sampling question. Pass NA to not include the question.
#' @param FB The random seed for the HMM forward-backward algorithm question. Pass NA to not include the question.
#' @param VIT The random seed for the HMM Viterbi algorithm question. Pass NA to not include the question.
#' @param AB The random seed for the alpha-beta pruning question. Pass NA to not include the question. Including this
#' question will lead to the ab.jpg image file being created.
#' @param SCH The random seed for the scheduling question. Pass NA to not include the question.
#' @param MB The random seed for the multi-armed bandits question. Pass NA to not include the question.
#' @param ANN The random seed for the feed forward neural network question. Pass NA to not include the question. Including this
#' question will lead to the ffnn.jpg image file being created.
#' @param CNN The random seed for the CNN question. Pass NA to not include the question.
#' @param TEXT The random seed for selecting the text questions. Pass NA to not include any text questions.
#' @param NUMTEXT The number of text questions to include if the TEXT parameter is not NA. Should be 1 to 9.
#' @param seedToExamRef Used when generating the real exam. You should keep the default value when generating
#' practice exams.
#' @return NULL
#' @export
createExam=function(
  filename,
  ASTAR=sample(10000,1),
  MCMC=sample(10000,1),
  FB=sample(10000,1),
  VIT=sample(10000,1),
  AB=sample(10000,1),
  SCH=sample(10000,1),
  MB=sample(10000,1),
  ANN=sample(10000,1),
  CNN=sample(10000,1),
  TEXT=sample(10000,1),
  NUMTEXT=9,
  seedToExamRef=list(doRef=F)
) {
  sink(file=filename,type="output")

  examRef="This is a student generated exam."
  if (seedToExamRef$doRef) {
    f=function(i)ifelse(is.na(i),0,seedToExamRef$f(i))
    examRef=paste(f(ASTAR),f(MCMC),f(FB),f(VIT),f(AB),f(SCH),f(MB),f(ANN),f(TEXT),f(NUMTEXT),sep="-")
  }

  questions=sum(
    ifelse(is.na(ASTAR),0,1),
    ifelse(is.na(MCMC),0,1),
    ifelse(is.na(FB),0,1),
    ifelse(is.na(VIT),0,1),
    ifelse(is.na(AB),0,1),
    ifelse(is.na(SCH),0,1),
    ifelse(is.na(MB),0,1),
    ifelse(is.na(ANN),0,1),
    ifelse(is.na(CNN),0,1),
    ifelse(is.na(TEXT),0,NUMTEXT)
  )
  marks=sum(c(
    ifelse(is.na(ASTAR),0,marksForQuestion("ASTAR")),
    ifelse(is.na(MCMC),0,marksForQuestion("MCMC")),
    ifelse(is.na(FB),0,marksForQuestion("FB")),
    ifelse(is.na(VIT),0,marksForQuestion("VIT")),
    ifelse(is.na(AB),0,marksForQuestion("AB")),
    ifelse(is.na(SCH),0,marksForQuestion("SCH")),
    ifelse(is.na(MB),0,marksForQuestion("MB")),
    ifelse(is.na(ANN),0,marksForQuestion("ANN")),
    ifelse(is.na(CNN),0,marksForQuestion("CNN")),
    ifelse(is.na(TEXT),0,marksForQuestion("TEXT",TEXT,NUMTEXT))
  ))
  three=marks/2
  four=floor(2*(three+(marks-three)/3))/2
  five=floor(2*(three+(2*(marks-three)/3)))/2

  cat("\\documentclass{article}\n",
      "\\usepackage[utf8]{inputenc}\n",
      "\\usepackage{graphicx}\n",
      "\\graphicspath{ {images/} }\n",
      "\\begin{document}\n\n",
      "\\title{ARTIFICIAL INTELLIGENCE EXAM}\n",
      "\\author{Course Code: 1DL340}\n",
      "\\date{Ref. ",examRef,"}\n",
      "\\maketitle\n\n",
      "This exam has ",questions," questions for a total of ",marks," marks. Grade boundaries are:\n\n",
      "\\begin{center}\n",
      "3 - ",formatC(three,format='f',digits=1),"\n\n",
      "4 - ",formatC(four,format='f',digits=1),"\n\n",
      "5 - ",formatC(five,format='f',digits=1),"\n\n",
      "\\end{center}\n\n",
      "In exceptional circumstances these boundaries may be adjusted at the discretion of the examiner. This would be done on an exam-wide basis, NOT for individual students.\n\n",
      "You are permitted to make use of a calculator and language dictionary in this exam.\n"
  )

  if (!is.na(ASTAR))
    makeQuestionAStar(ASTAR,answer=F,latex=T)
  if (!is.na(MCMC))
    makeQuestionMCMC(MCMC,answer=F,latex=T)
  if (!is.na(FB))
    makeQuestionHMM_FB(FB,answer=F,latex=T)
  if (!is.na(VIT))
    makeQuestionHMM_Vit(VIT,answer=F,latex=T)
  if (!is.na(AB)) {
    ab="ab.jpg"
    makeQuestionAlphaBeta(AB,answer=F,latex=T,imageFilename=ab)
    cat("\n\\begin{figure}[h!]\n",
        "\\includegraphics[width=\\textwidth]{",ab,"}\n",
        "\\end{figure}\n",sep="")
  }
  if (!is.na(SCH))
    makeQuestionSchedule(SCH,answer=F,latex=T)
  if (!is.na(MB))
    makeQuestionMultiArmedBandit(MB,answer=F,latex=T)
  if (!is.na(ANN)) {
    ffnn="ffnn.jpg"
    makeQuestionFFNN(ANN,answer=F,latex=T,imageFilename=ffnn)
    cat("\n\\begin{figure}[h!]\n",
        "\\includegraphics[width=\\textwidth]{",ffnn,"}\n",
        "\\end{figure}\n",sep="")
  }
  if (!is.na(CNN))
    makeQuestionCNN(CNN,answer=F,latex=T)
  if (!is.na(TEXT))
    makeTextQuestions(TEXT,answer=F,latex=T,NUMTEXT)

  cat("\n\\end{document}\n")
  sink(NULL)
}
