#' makeQuestionMultiArmedBandit
#'
#' Make the multi-armed bandits question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionMultiArmedBandit=function(seed=NA,answer=T,latex=F) {
  if (!is.na(seed))
    set.seed(seed)

  # Make some dirichlet parameters:
  dirs=lapply(1:3,function(i)c(6+rbinom(1,6,.5),1+rbinom(1,6,.5)))
  # Generate some samples from each:
  sams=lapply(dirs,function(d)round(rbeta(5,d[2],d[1]),2))

  if (latex) {
    cat("\\clearpage\n")
    cat("\\section{Multi-Armed Bandit Optimization}\n\n")

    cat("Image we are testing click through rates on three different web layouts. ")
    cat("At the current point, the Dirichlet (beta) distributions associated with each layout have the parameters in Table~\\ref{MABO1}.")
    cat("\\begin{table}[h!]\n")
    cat("\\caption{Dirichlet (Beta) Parameters for Layout}\n")
    cat("\\label{MABO1}\n",sep="")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c|c| } \n")
    cat("\\hline\n")
    cat(" Layout & Parameter 1 & Parameter 2 \\\\\n")
    cat("\\hline\n")
    cat("A & ",dirs[[1]][1]," & ",dirs[[1]][2],"\\\\\n")
    cat("B & ",dirs[[2]][1]," & ",dirs[[2]][2],"\\\\\n")
    cat("C & ",dirs[[3]][1]," & ",dirs[[3]][2],"\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\nThe first value is associated with not clicking through, the second clicking through.\n")

    cat("\nA new person views the site. We generate samples from the distributions to determine which layout is used. These samples are given in Table~\\ref{MABO2}.\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Samples from Layout Dirichlet (Beta) Distributions}\n")
    cat("\\label{MABO2}\n",sep="")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c|c|c|c|c| } \n")
    cat("\\hline\n")
    cat(" Layout & Sample 1 & Sample 2 & Sample 3 & Sample 4 & Sample 5 \\\\\n")
    cat("\\hline\n")
    cat("A & ",sams[[1]][1]," & ",sams[[1]][2]," & ",sams[[1]][3]," & ",sams[[1]][4]," & ",sams[[1]][5],"\\\\\n")
    cat("B & ",sams[[2]][1]," & ",sams[[2]][2]," & ",sams[[2]][3]," & ",sams[[2]][4]," & ",sams[[2]][5],"\\\\\n")
    cat("C & ",sams[[3]][1]," & ",sams[[3]][2]," & ",sams[[3]][3]," & ",sams[[3]][4]," & ",sams[[3]][5],"\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    # Makes purchase
    cat("\n\nWhen shown the website with the chosen layout, the person makes a purchase ('clicks through'). ")

    # Question
    cat("Give the new parameters of the three distributions after this event. (2 Marks)\n")
  }
  else {
    cat("We are testing click through rates on three different web layouts.")
    cat("At the current point, the dirichlet (beta) distributions associated with each layout have the parameters:\n")
    cat("A.",dirs[[1]][1],dirs[[1]][2],"\n")
    cat("B.",dirs[[2]][1],dirs[[2]][2],"\n")
    cat("C.",dirs[[3]][1],dirs[[3]][2],"\n")
    cat("The first value is associated with not clicking through, the second clicking through.\n")

    cat("\nA new person views the site. We generate samples from the distributions to determine which layout is used. The samples are:\n")
    cat("A.",paste(sams[[1]],collapse=","),"\n")
    cat("B.",paste(sams[[2]],collapse=","),"\n")
    cat("C.",paste(sams[[3]],collapse=","),"\n")

    # Makes purchase
    cat("\nWhen shown the website with the chosen layout, the person makes a purchase ('clicks through').\n")

    # Question
    cat("\nQUESTION: What are the new parameters of the three distributions after this event?\n")
  }
  if (answer) {
    readline("Press Enter to see solution...")

    # Solution
    means=sapply(sams,mean)
    used=which.max(means)
    dirs[[used]][2]=dirs[[used]][2]+1
    cat("\nANSWER:\n")
    cat("The sample means are:\n")
    cat("A.",means[1],"\n")
    cat("B.",means[2],"\n")
    cat("C.",means[3],"\n")
    cat("So we use layout ",used,".\nSince the person clicked through, the updated parameters are:\n",sep="")
    cat("A.",dirs[[1]][1],dirs[[1]][2],"\n")
    cat("B.",dirs[[2]][1],dirs[[2]][2],"\n")
    cat("C.",dirs[[3]][1],dirs[[3]][2],"\n")
  }
}
#' @keywords internal
testSolveMultiArmedBandits=function(seed=NA) {
  if (!is.na(seed))
      set.seed(seed)
  createMultiArmedBanditQuestion()
}
