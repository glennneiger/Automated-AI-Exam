#' @keywords internal
makeProbTables=function() {
  rnds=sample(c(.05,.1,.15,.2,.25,.3,.35,.4,.45,.55,.6,.65,.7,.75,.8,.85,.9,.95),20,T)
  out=list(
  P_A=matrix(c(rnds[1],1-rnds[1]),nrow=1),
  P_B_given_A=matrix(c(rnds[2],1-rnds[2],rnds[3],1-rnds[3]),nrow=2,byrow=T),
  P_C_given_A=matrix(c(rnds[4],1-rnds[4],rnds[5],1-rnds[5]),nrow=2,byrow=T),
  P_D_given_BC=matrix(c(rnds[6],1-rnds[6],rnds[7],1-rnds[7],rnds[8],1-rnds[8],rnds[9],1-rnds[9]),nrow=4,byrow=T),
  P_E_given_C=matrix(c(rnds[10],1-rnds[10],rnds[11],1-rnds[11]),nrow=2,byrow=T)
  )
  colnames(out$P_A)=c("A=F","A=T")
  colnames(out$P_B_given_A)=c("B=F","B=T")
  colnames(out$P_C_given_A)=c("C=F","C=T")
  colnames(out$P_D_given_BC)=c("D=F","D=T")
  colnames(out$P_E_given_C)=c("E=F","E=T")
  rownames(out$P_A)=c("NA")
  rownames(out$P_B_given_A)=c("A=F","A=T")
  rownames(out$P_C_given_A)=c("A=F","A=T")
  rownames(out$P_D_given_BC)=c("B=F,C=F","B=F,C=T","B=T,C=F","B=T,C=T")
  rownames(out$P_E_given_C)=c("C=F","C=T")
  return (out)

}
#' @keywords internal
getEvidence=function(){
  nodes=sample(1:5,2)
  evidence=sample(c(T,F),2,T)
  data.frame(node=nodes,value=evidence)
}

#' @keywords internal
getProbability_=function(node,values,probs){
  vIndex=ifelse(values[node],2,1)
  if (node==1) {
    return (probs[[1]][1,vIndex])
  }
  else if (node==2) {
    aIndex=ifelse(values[1],2,1)
    return (probs[[2]][aIndex,vIndex])
  }
  else if (node==3) {
    aIndex=ifelse(values[1],2,1)
    return (probs[[3]][aIndex,vIndex])
  }
  else if (node==4) {
    bIndex=ifelse(values[2],2,1)
    cIndex=ifelse(values[3],2,1)
    rIndex=2*(bIndex-1)+cIndex
    return (probs[[4]][rIndex,vIndex])
  }
  else if (node==5) {
    cIndex=ifelse(values[3],2,1)
    return (probs[[5]][cIndex,vIndex])
  }
  else {
    stop("WTF")
  }
}
#' @keywords internal
getProbability=function(values,probs){
  prod(sapply(1:5,getProbability_,values,probs))
}
#' @keywords internal
testSolveMCMC=function(probs,evidence,rnds,initial){
  samples=data.frame(A=rep(NA,3),B=rep(NA,3),C=rep(NA,3),D=rep(NA,3),E=rep(NA,3))
  for (i in 1:2) {
    row=evidence[i,]
    samples[1:3,row[1,1]]=row[1,2]
  }
  unknown=which(is.na(samples[1,]))
  samples[1,unknown]=initial
  print(paste("Initial Sample:",paste(samples[1,],collapse=",")))
  rIndex=1
  for (turn in 1:2) {
    values=samples[turn,]
    for (i in unknown) {
      current=samples[turn,i]
      cand=!current
      curProb=getProbability(values,probs)
      values[i]=cand
      candProb=getProbability(values,probs)
      update=F
      if (candProb>=curProb) {
        print(paste("Updating: CandProb",candProb,"vs CurProb",curProb))
        update=T
      }
      else {
        if (rnds[rIndex]<=candProb/curProb) {
          print(paste("Updating: CandProb",candProb,"vs CurProb",curProb,"so ratio is",round(candProb/curProb,3),"and rnd",rIndex,"=",rnds[rIndex]))
          update=T
        }
        else {
          print(paste("Not Updating: CandProb",candProb,"vs CurProb",curProb,"so ratio is",round(candProb/curProb,3),"and rnd",rIndex,"=",rnds[rIndex]))
        }
        rIndex=rIndex+1
      }
      if (!update) {
        values[i]=current
      }
    }
    samples[turn+1,unknown]=values[unknown]
  }
  print(samples)
}
#' makeQuestionMCMC
#'
#' Make the Bayesian network/MCMC sampling question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionMCMC=function(seed=NA,answer=T,latex=F){
  if(!is.na(seed))
    set.seed(seed)
  tables=makeProbTables()
  evidence=getEvidence()
  rnds=round(runif(6),3)
  initial=sample(c(T,F),3,T)
  if (latex){
    cat("\\clearpage\n")
    cat("\\section{MCMC and Directed Graphical Models}\n")
    cat("\nTables~\\ref{MCMC1} to~\\ref{MCMC5} provide the conditional probability distributions for a directed graphical model.\n")
    cat("\nA. Use this information to draw the graph of the associated directed graphical model. (1 mark)\n")
    cat("\nB. Table~\\ref{MCMC6} provides observed values for some of the nodes. Given these, ")
    cat("the initial values provided in Table~\\ref{MCMC7} and the random numbers provided below, use the Metropolis within Gibbs MCMC sampling algorithm to generate two ")
    cat("complete samples of the variables. Assume that the candidate function gives the opposite of the current value. At each step, explain ")
    cat("what value you are considering, what the current and candidate values are, and why you updated it or did not update it. (4 marks)\n")

    cat("\nRandom numbers:",paste(rnds,collapse=","),"\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{P(A)}\n")
    cat("\\label{MCMC1}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c||c|c| } \n")
    cat("\\hline\n")
    cat(" - & A=F & A=T\\\\\n")
    cat("\\hline\n")
    cat(" & ",tables[[1]][1]," & ",tables[[1]][2],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{P(B$|$A)}\n")
    cat("\\label{MCMC2}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c||c|c| } \n")
    cat("\\hline\n")
    cat(" A & B=F & B=T\\\\\n")
    cat("\\hline\n")
    cat(" A=F & ",tables[[2]][1,1]," & ",tables[[2]][1,2],"\\\\\n",sep="")
    cat(" A=T & ",tables[[2]][2,1]," & ",tables[[2]][2,2],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{P(C$|$A)}\n")
    cat("\\label{MCMC3}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c||c|c| } \n")
    cat("\\hline\n")
    cat(" A & C=F & C=T\\\\\n")
    cat("\\hline\n")
    cat(" A=F & ",tables[[3]][1,1]," & ",tables[[3]][1,2],"\\\\\n",sep="")
    cat(" A=T & ",tables[[3]][2,1]," & ",tables[[3]][2,2],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{P(D$|$B,C)}\n")
    cat("\\label{MCMC4}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c||c|c| } \n")
    cat("\\hline\n")
    cat(" B & C & D=F & D=T\\\\\n")
    cat("\\hline\n")
    cat(" B=F & C=F & ",tables[[4]][1,1]," & ",tables[[4]][1,2],"\\\\\n",sep="")
    cat(" B=F & C=T & ",tables[[4]][2,1]," & ",tables[[4]][2,2],"\\\\\n",sep="")
    cat(" B=T & C=F & ",tables[[4]][3,1]," & ",tables[[4]][3,2],"\\\\\n",sep="")
    cat(" B=T & C=T & ",tables[[4]][4,1]," & ",tables[[4]][4,2],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{P(E$|$C)}\n")
    cat("\\label{MCMC5}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c||c|c| } \n")
    cat("\\hline\n")
    cat(" C & E=F & E=T\\\\\n")
    cat("\\hline\n")
    cat(" C=F & ",tables[[5]][1,1]," & ",tables[[5]][1,2],"\\\\\n",sep="")
    cat(" C=T & ",tables[[5]][2,1]," & ",tables[[5]][2,2],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    nodes=

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Observed Values}\n")
    cat("\\label{MCMC6}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c| } \n")
    cat("\\hline\n")
    cat(" Node & Value \\\\\n")
    cat("\\hline\n")
    cat(LETTERS[evidence[1,1]]," & ",evidence[1,2],"\\\\\n",sep="")
    cat(LETTERS[evidence[2,1]]," & ",evidence[2,2],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    unknown=(1:5)[-evidence$node]
    cat("\\begin{table}[h!]\n")
    cat("\\caption{Initial Values}\n")
    cat("\\label{MCMC7}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c| } \n")
    cat("\\hline\n")
    cat(" Node  & Value \\\\\n")
    cat("\\hline\n")
    cat(LETTERS[unknown[1]]," & ",initial[1],"\\\\\n",sep="")
    cat(LETTERS[unknown[2]]," & ",initial[2],"\\\\\n",sep="")
    cat(LETTERS[unknown[3]]," & ",initial[3],"\\\\\n",sep="")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

  }
  else {
    for (table in tables)
      print(table)
    print(evidence)
    print(paste("Random numbers:",paste(rnds,collapse=",")))
    print(paste("Initial sampling values for unknown nodes (in unknown node order):",paste(initial,collapse=",")))
  }
  if (answer) {
    testSolveMCMC(tables,evidence,rnds,initial)
  }
}
