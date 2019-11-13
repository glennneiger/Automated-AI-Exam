#' @keywords internal
setupCNN=function(){
  weights1=matrix(sample(-4:4,4,T),nrow=2)
  weights2=matrix(sample(-4:4,4,T),nrow=2)
  inputs=matrix(sample(-5:5,9,T),nrow=3)
  list(weights1=weights1,weights2=weights2,inputs=inputs)
}
#' @keywords internal
convolveCNN=function(inputs,weights,n){
  output=matrix(rep(NA,4),nrow=2)
  #cat("\nCalculations for Output Matrix",n,":\n")
  for (i in 1:2) {
    for (j in 1:2) {
    #cat("  Element:",i,j,"\n")
    #cat("    (",weights[1,1],"*",inputs[i,j],") + (",weights[1,2],"*",inputs[i,j+1],") + (",weights[2,1],"*",inputs[i+1,j],") + (",weights[2,2],"*",inputs[i+1,j+1],")\n",sep="")
      output[i,j]=max(0,
                      weights[1,1]*inputs[i,j]+
                        weights[1,2]*inputs[i,j+1]+
                        weights[2,1]*inputs[i+1,j]+
                        weights[2,2]*inputs[i+1,j+1]
      )
    }
  }
  return (output)
}
#' @keywords internal
solveCNN=function(info){
  weights1=info$weights1
  weights2=info$weights2
  inputs=info$inputs

  cat("The output of the layer will be two 2x2 matrices, one for each filter.\n")

  m1=convolveCNN(inputs,weights1,1)
  m2=convolveCNN(inputs,weights2,2)

  cat("\nThe values of the first matrix are:\n")
  print(m1)

  cat("\nThe values of the second matrix are:\n")
  print(m2)
}
#' makeQuestionCNN
#'
#' Make the CNN question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionCNN=function(seed=NA,answer=T,latex=F){
  if (!is.na(seed))
    set.seed(seed)

  info=setupCNN()

  if (latex) {
    cat("\\clearpage\n")
    cat("\\section{Convolution layers in CNNs}\n\n")

    cat("Tables~\\ref{CNN1} to~\\ref{CNN3} provide an input matrix and two filter matrices for a convolutional layer in a CNN. ",
        "Assuming no padding, that stride is [1,1], and that all activation functions are rectifiers, calculate the output of this layer. (2 marks)\n",
        sep="")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Input Matrix}\n")
    cat("\\label{CNN1}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c|c| } \n")
    cat("\\hline\n")
    cat(info$inputs[1,1]," & ",info$inputs[1,2]," & ",info$inputs[1,3],"\\\\\n")
    cat(info$inputs[2,1]," & ",info$inputs[2,2]," & ",info$inputs[2,3],"\\\\\n")
    cat(info$inputs[3,1]," & ",info$inputs[3,2]," & ",info$inputs[3,3],"\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Filter 1}\n")
    cat("\\label{CNN2}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c| } \n")
    cat("\\hline\n")
    cat(info$weights1[1,1]," & ",info$weights1[1,2],"\\\\\n")
    cat(info$weights1[2,1]," & ",info$weights1[2,2],"\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Filter 1}\n")
    cat("\\label{CNN3}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c| } \n")
    cat("\\hline\n")
    cat(info$weights2[1,1]," & ",info$weights2[1,2],"\\\\\n")
    cat(info$weights2[2,1]," & ",info$weights2[2,2],"\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")
  }
  else {
    cat("Consider the following input matrix:\n")
    cat("Input Matrix:\n")
    print(info$inputs)
    cat("And the following two convolution filers:\n")
    cat("Filter 1:\n")
    print(info$weights1)
    cat("Filter 2:\n")
    print(info$weights2)
    cat("Assuming that all activation functions are rectifiers, calculate the output of this layer. (2 marks)\n")
  }
  if (answer) {
    readline("Press Enter to see the answer...")
    solveCNN(info)
  }
}

