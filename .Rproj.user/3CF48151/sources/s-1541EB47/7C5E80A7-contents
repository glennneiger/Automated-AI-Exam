#' @keywords internal
makeHMM=function(){
  rnds=runif(20,.06,.94)
  out=list(Tr=round(matrix(c(rnds[1],1-rnds[1],rnds[2],1-rnds[2]),nrow=2,byrow=T),1),
       Em=round(matrix(c(rnds[3],1-rnds[3],rnds[4],1-rnds[4]),nrow=2,byrow=T),1),
       In=c(.5,.5),
       Obs=sample(c(T,F),2,T))
  rownames(out$Tr)=c("state(t)=False","state(t)=True")
  colnames(out$Tr)=c("state(t+1)=False","state(t+1)=True")
  rownames(out$Em)=c("state=False","state=True")
  colnames(out$Em)=c("emission=False","emission=True")
  names(out$In)=c("state(0)=False","state(0)=True")
  names(out$Obs)=c("emission(1)","emission(2)")
  return (out)
}

#' @keywords internal
solveFB=function(hmm){
  f0=hmm$In
  ob1=ifelse(hmm$Obs[1],2,1)
  f11=(f0[1]*hmm$Tr[1,1]+f0[2]*hmm$Tr[2,1])*hmm$Em[1,ob1]
  f12=(f0[1]*hmm$Tr[1,2]+f0[2]*hmm$Tr[2,2])*hmm$Em[2,ob1]
  f1=c(f11,f12)
  ob2=ifelse(hmm$Obs[2],2,1)
  f21=(f1[1]*hmm$Tr[1,1]+f1[2]*hmm$Tr[2,1])*hmm$Em[1,ob2]
  f22=(f1[1]*hmm$Tr[1,2]+f1[2]*hmm$Tr[2,2])*hmm$Em[2,ob2]
  f2=c(f21,f22)
  names(f0)=NULL
  names(f1)=NULL
  names(f2)=NULL
  print("Forward Values:")
  print(f0)
  print(f1)
  print(f2)

  b2=c(1,1)
  b11=(b2[1]*hmm$Tr[1,1]*hmm$Em[1,ob2])+(b2[2]*hmm$Tr[1,2]*hmm$Em[2,ob2])
  b12=(b2[1]*hmm$Tr[2,1]*hmm$Em[1,ob2])+(b2[2]*hmm$Tr[2,2]*hmm$Em[2,ob2])
  b1=c(b11,b12)
  b01=(b1[1]*hmm$Tr[1,1]*hmm$Em[1,ob1])+(b1[2]*hmm$Tr[1,2]*hmm$Em[2,ob1])
  b02=(b1[1]*hmm$Tr[2,1]*hmm$Em[1,ob1])+(b1[2]*hmm$Tr[2,2]*hmm$Em[2,ob1])
  b0=c(b01,b02)
  names(b0)=NULL
  names(b1)=NULL
  names(b2)=NULL
  print("Backward Values:")
  print(b0)
  print(b1)
  print(b2)

  print("State Probabilities:")
  print((b0*f0)/sum(b0*f0))
  print((b1*f1)/sum(b1*f1))
  print((b2*f2)/sum(b2*f2))
}
#' @keywords internal
solveVit=function(hmm){
  p0=hmm$In
  ob1=ifelse(hmm$Obs[1],2,1)
  p11_options=c(p0[1]*hmm$Tr[1,1]*hmm$Em[1,ob1],p0[2]*hmm$Tr[2,1]*hmm$Em[1,ob1])
  names(p11_options)=NULL
  p11=max(p11_options)
  p11_from=which.max(p11_options)
  p12_options=c(p0[1]*hmm$Tr[1,2]*hmm$Em[2,ob1],p0[2]*hmm$Tr[2,2]*hmm$Em[2,ob1])
  names(p12_options)=NULL
  p12=max(p12_options)
  p12_from=which.max(p12_options)
  p1=c(p11,p12)

  print("Step 1: Path Node Probabilities")
  print(p1)
  print("Step 1: Path Step Origins")
  print(c(ifelse(p11_from==1,"FALSE","TRUE"),ifelse(p12_from==1,"FALSE","TRUE")))

  ob2=ifelse(hmm$Obs[2],2,1)
  p21_options=c(p1[1]*hmm$Tr[1,1]*hmm$Em[1,ob2],p1[2]*hmm$Tr[2,1]*hmm$Em[1,ob2])
  names(p21_options)=NULL
  p21=max(p21_options)
  p21_from=which.max(p21_options)
  p22_options=c(p1[1]*hmm$Tr[1,2]*hmm$Em[2,ob2],p1[2]*hmm$Tr[2,2]*hmm$Em[2,ob2])
  names(p22_options)=NULL
  p22=max(p22_options)
  p22_from=which.max(p22_options)
  p2=c(p21,p22)

  print("Step 2:Path Node Probabilities")
  print(p2)
  print("Step 2: Path Step Origins")
  print(c(ifelse(p21_from==1,"FALSE","TRUE"),ifelse(p22_from==1,"FALSE","TRUE")))

  p2_node=which.max(p2)
  p1_node=ifelse(p2_node==1,p21_from,p22_from)
  p0_node=ifelse(p1_node==1,p11_from,p12_from)
  print(paste("Most probable path:",ifelse(p0_node==1,"FALSE","TRUE"),ifelse(p1_node==1,"FALSE","TRUE"),ifelse(p2_node==1,"FALSE","TRUE")))
  print(paste("Probabality:",max(p2)))
}
#' makeQuestionHMM_FB
#'
#' Make the HMM forward-backward algorithm question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionHMM_FB=function(seed=NA,answer=T,latex=F){
  if(!is.na(seed))
    set.seed(seed)
  hmm=makeHMM()
  if (latex){
    str="hmmfb"
    cat("\\clearpage\n")
    cat("\\section{Hidden Markov Models: Forward-Backward Algorithm}\n")
    cat("\nTables~\\ref{",str,"1} to~\\ref{",str,"4} provide the transition matrix, emission matrix, initial state and a sequence of observations for a hidden Markov model. ",sep="")
    cat("Use the forward-backward algorithm to calculate the probability distributions for the state of the system at times 0, 1 and 2 given the observations. TRUE=1, FALSE=0. Show all working. (4 marks)\n\n")
    latexHMM(hmm,str)
  }
  else
    consoleHMM(hmm)
  if (answer)
    solveFB(hmm)
}
#' @keywords internal
consoleHMM=function(hmm){
  print("Transition Matrix:")
  print(hmm$Tr)
  print("Emission Matrix:")
  print(hmm$Em)
  print("Initial State:")
  print(hmm$In)
  print("Observations:")
  print(hmm$Obs)
}
#' @keywords internal
latexHMM=function(hmm,str){

  cat("\\begin{table}[h!]\n")
  cat("\\caption{Transition Matrix}\n")
  cat("\\label{",str,"1}\n",sep="")
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{ |c||c|c| } \n")
  cat("\\hline\n")
  cat(" $S_{t-1}$ & $S_t$=0 & $S_t$=1\\\\\n")
  cat("\\hline\n")
  cat(" 0 & ",hmm$Tr[1,1]," & ",hmm$Tr[1,2],"\\\\\n",sep="")
  cat(" 1 & ",hmm$Tr[2,1]," & ",hmm$Tr[2,2],"\\\\\n",sep="")
  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  cat("\\end{table}\n")

  cat("\\begin{table}[h!]\n")
  cat("\\caption{Emission Matrix}\n")
  cat("\\label{",str,"2}\n",sep="")
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{ |c||c|c| } \n")
  cat("\\hline\n")
  cat(" $S$ & $E=0$ & $E=1$\\\\\n")
  cat("\\hline\n")
  cat(" 0 & ",hmm$Em[1,1]," & ",hmm$Em[1,2],"\\\\\n",sep="")
  cat(" 1 & ",hmm$Em[2,1]," & ",hmm$Em[2,2],"\\\\\n",sep="")
  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  cat("\\end{table}\n")

  cat("\\begin{table}[h!]\n")
  cat("\\caption{Initial State}\n")
  cat("\\label{",str,"3}\n",sep="")
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{ |c|c| } \n")
  cat("\\hline\n")
  cat(" $S=0$ & $S=1$\\\\\n")
  cat("\\hline\n")
  cat(hmm$In[1]," & ",hmm$In[2],"\\\\\n",sep="")
  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  cat("\\end{table}\n")

  cat("\\begin{table}[h!]\n")
  cat("\\caption{Observations}\n")
  cat("\\label{",str,"4}\n",sep="")
  cat("\\begin{center}\n")
  cat("\\begin{tabular}{ |c|c| } \n")
  cat("\\hline\n")
  cat(" Time=1 & Time=2\\\\\n")
  cat("\\hline\n")
  cat(hmm$Obs[1]," & ",hmm$Obs[2],"\\\\\n",sep="")
  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{center}\n")
  cat("\\end{table}\n")
}

#' @keywords internal
testSolveHMM_FB=function(seed=NA){
  if(!is.na(seed))
    set.seed(seed)
  hmm=makeHMM()
  solveFB(hmm)
}
#' makeQuestionHMM_Vit
#'
#' Make the HMM Viterbi algorithm question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionHMM_Vit=function(seed=NA,answer=T,latex=F){
  if(!is.na(seed))
    set.seed(seed)
  hmm=makeHMM()
  if (latex) {
    str="hmmvit"
    cat("\\clearpage\n")
    cat("\\section{Hidden Markov Models: Viterbi Algorithm}\n")
    cat("\nTables~\\ref{",str,"1} to~\\ref{",str,"4} provide the transition matrix, emission matrix, initial state and a sequence of observations for a hidden Markov model. ",sep="")
    cat("Use the Viterbi algorithm to calculate the most probable path and its probability.  TRUE=1, FALSE=0. Show all working. (3 marks)\n\n")
    latexHMM(hmm,str)
  }
  else {
    consoleHMM(hmm)
  }
  if (answer)
    solveVit(hmm)
}
#' @keywords internal
testSolveHMM_Vit=function(seed=NA){
  if(!is.na(seed))
    set.seed(seed)
  hmm=makeHMM()
  solveVit(hmm)
}

# "Single Calculation Error: 3.5"
# "Forward and Backward Correct: 3"
# "Forward and Smoothing Correct, Backward Partially Correct: 2.5"
# "Forward and Smoothing Correct: 1.5"
# "Forward Correct, Backward Partially Correct: 1.5"
# "Forward Correct: 1"
# "Smoothing Correct: .5"
#
# "Knowing the b values for time 2 is insufficient to consider the backward partially correct."

