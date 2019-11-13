#' @keywords internal
setupANN=function(){
  weights1=matrix(sample(-9:9,6,T),nrow=3)
  weights2=matrix(sample(-9:9,3,T),nrow=3)
  inputs=sample(-10:10,2)
  plot.new()
  plot.window(xlim=c(0,4),ylim=c(0,4))
  segments(c(1,1,1,1,1,1),c(1,2,3,1,2,3),
           c(2,2,2,2,2,2),c(1,1,1,2,2,2))
  segments(c(2,2,2),c(1,2,3),c(3,3,3),c(2,2,2))

  points(c(1,1,1),c(1,2,3),cex=3,pch=c(16,16,15),col=c("light blue","light blue","light blue"))
  points(c(2,2,2),c(1,2,3),cex=3,pch=c(16,16,15),col=c("green","green","green"))
  points(c(3),c(2),cex=3,pch=16,col=c("red"))

  text(1.2,.75,weights1[1,1])
  text(1.4,1.2,weights1[1,2])
  text(1.2,1.6,weights1[2,1])
  text(1.4,1.84,weights1[2,2])
  text(1.2,2.35,weights1[3,1])
  text(1.2,3.1,weights1[3,2])

  text(2.2,0.9,weights2[1,1])
  text(2.2,1.84,weights2[2,1])
  text(2.2,3.1,weights2[3,1])
  title("Basic Regression Feed-Forward Neural Network")

  list(weights1=weights1,weights2=weights2,inputs=inputs)
}
#' @keywords internal
solveANN=function(info){
  weights1=info$weights1
  weights2=info$weights2
  inputs=info$inputs

  cat("The value of the first non-bias hidden node (counting downwards) is:\n")
  h1=max(0,inputs[1]*weights1[2,2]+inputs[2]*weights1[1,2]+weights1[3,2])
  cat("max ( 0 , [ ",inputs[1]," x ",weights1[2,2]," + ",inputs[2]," x ",weights1[1,2]," + ",weights1[3,2]," ] ) = ",h1,"\n",sep="")

  cat("The value of the second non-bias hidden node (counting downwards) is:\n")
  h2=max(0,inputs[1]*weights1[2,1]+inputs[2]*weights1[1,1]+weights1[3,1])
  cat("max ( 0 , [ ",inputs[1]," x ",weights1[2,1]," + ",inputs[2]," x ",weights1[1,1]," + ",weights1[3,1]," ] ) = ",h2,"\n",sep="")

  cat("The value of the output node is:\n")
  o=h1*weights2[2,1]+h2*weights2[1,1]+weights2[3,1]
  cat(h1," x ",weights2[2,1]," + ",h2," x ",weights2[1,1]," + ",weights2[3,1]," = ",o,"\n",sep="")
}
#' makeQuestionFFNN
#'
#' Make the feed-forward neural network question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @param imageFilename The filename of the image to be saved when outputing latex script.
#'  This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionFFNN=function(seed=NA,answer=T,latex=F,imageFilename=NULL){
  if (!is.na(seed))
    set.seed(seed)

  if (!is.null(imageFilename))
    jpeg(imageFilename)
  info=setupANN()
  if (!is.null(imageFilename))
    dev.off()

  if (latex) {
    cat("\\clearpage\n")
    cat("\\section{Basic Feed-Forward ANNs}\n\n")

    cat("Examine the neural network given in the diagram labelled 'Basic Regression Feed-Forward Neural Network'. ",
        "In this diagram, square nodes represent biases, blue nodes the input layer, green nodes a hidden layer, and red nodes the output layer. ",
        "The first round blue input node is associated with feature X1, and the second with feature X2 (counting downwards). ",
        "Assuming that all activation functions are rectifiers (i.e. the hidden nodes are ReLU units), and the output is a basic linear regression function, calculate the output of this network if it was given an input of X1 = ",info$inputs[1]," and X2 = ",info$inputs[2],". Show all working. (2 Marks)\n",
        sep="")
  }
  else {
    cat("Consider the network in the diagram. ")
    cat("Square nodes represent biases, blue nodes the input layer, green nodes a hidden layer, and red nodes the output layer. ")
    cat("X1 is the first round blue node, and X2 the second (counting downwards).\n")
    cat("Assuming that all activation functions are rectifiers (i.e. the hidden nodes are ReLU units), and the output is a basic linear regression function, calculate the output of this network if it was given an input of X1 = ",info$inputs[1]," and X2 = ",info$inputs[2],". Show all working.\n")
  }
  if (answer) {
    readline("Press Enter to see the answer...")
    solveANN(info)
  }
}
