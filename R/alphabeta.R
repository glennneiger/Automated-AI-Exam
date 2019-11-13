#' @keywords internal
abNode=function(depth,max,wFrom,wTo,index=1){
  mid=wFrom+(wTo-wFrom)/2
  out=NULL
  if (depth==0)
    out=list(index=NA,nextIndex=index,
             max=max,value=sample(-25:25,1),
             alpha=NA,beta=NA,depth=depth,width=mid)
  else {
    nextIndex=index+1
    A=abNode(depth-1,!max,wFrom,mid,nextIndex)
    nextIndex=A$nextIndex
    B=abNode(depth-1,!max,mid,wTo,nextIndex)
    nextIndex=B$nextIndex
    out=list(
      index=index,
      nextIndex=nextIndex,
      A=A,
      B=B,
      max=max,
      value=NA,
      alpha=NA,
      beta=NA,
      depth=depth,
      width=mid
      )
  }
  class(out)="abn"
  return(out)
}

#' @keywords internal
plot.abn=function(obj,from=NULL) {
  if (!is.null(from))
    segments(obj$width,obj$depth,from$width,from$depth)
  if (!is.null(obj$A))
    plot(obj$A,obj)
  if (!is.null(obj$B))
    plot(obj$B,obj)
  if (is.na(obj$value)) {
    points(obj$width,obj$depth,col=ifelse(obj$max,"red","light blue"),
           pch=ifelse(obj$max,15,16),cex=3)
    text(obj$width,obj$depth,obj$index)
  }
  else
    text(obj$width,obj$depth-.3,obj$value,col=ifelse(obj$max,"red","blue"))
}
#' @keywords internal
alphaBetaPruning=function(depth=4,firstMax=T) {
  out=list(
    root=abNode(depth,firstMax,0,2^depth),
    depth=depth,
    firstMax=firstMax,
    width=2^depth
  )
  class(out)="abp"
  return(out)
}

#' @keywords internal
solveABP=function(obj,comments) {
  solveABN(obj$root,comments=comments)
}
#' @keywords internal
announceABN=function(obj,alpha,beta,pass=FALSE) {
  if (pass)
    cat("Alpha-beta values passed on to node ",obj$index,".\n",sep="")
  else
    cat("Alpha-beta values updated for node ",obj$index,".\n",sep="")
  cat("Node: ",obj$index,"  alpha=",alpha,", beta=",beta,sep="")
  readline("")
}
#' @keywords internal
pruneABP=function(to,from,comments,pruned=NULL){
  col=ifelse(is.null(pruned),"orange","red3")
  size=.4
  midWidth=(from$width+to$width)/2
  midDepth=(from$depth+to$depth)/2
  segments(midWidth-size,midDepth-size,midWidth+size,midDepth+size,col=col,lwd=3)
  segments(midWidth-size,midDepth+size,midWidth+size,midDepth-size,col=col,lwd=3)
  if (!is.null(pruned)) {
    midWidth=(pruned$width+to$width)/2
    midDepth=(pruned$depth+to$depth)/2
    segments(midWidth-size,midDepth-size,midWidth+size,midDepth+size,col=col,lwd=3)
  }
  if (comments){
    readline("")
  }
}
#' @keywords internal
solveABN=function(obj,alpha=-Inf,beta=Inf,from=NULL,comments=T) {
  if (!is.na(obj$value)) {
    if (comments){
      cat("The algorithm has reached the assigned value (",obj$value,
          ") so it is returned.",sep="")
      readline("")
    }
    return(obj$value)
  }
  if (comments) {
    announceABN(obj,alpha,beta,T)
  }
  if (obj$max) {
    isA=T
    for (child in list(obj$A,obj$B)) {
      v=solveABN(child,alpha,beta,obj,comments)
      if (v>alpha) {
        alpha=v
        if (comments) {
          announceABN(obj,alpha,beta)
        }
      }
      else {
        if (comments){
          cat("Alpha-beta values are not updated for node ",obj$index,".",sep="")
          readline("")
        }
      }
      if (alpha>=beta) {
        if (isA){
          if (comments){
            cat("Alpha-beta interval collapses - a prune is possible.\n")
          }
          pruneABP(obj,from,comments,obj$B)
        }
        else {
          if (comments){
            cat("Alpha-beta interval collapses. But since this occurred on the last child of node ",obj$index,
              ", it does not help reduce the work done by the algorithm. This normally just means the branch has been ruled out as being involving the optimal path of play. We mark it in orange.",sep="")
          }
          pruneABP(obj,from,comments)
        }
        return (alpha)
      }
      isA=!isA
    }
    if (comments){
      cat("Node ",obj$index," is a max node, so the alpha value (",alpha,
        ") is returned.",sep="")
      readline("")
    }
    return (alpha)
  }
  else {
    isA=T
    for (child in list(obj$A,obj$B)) {
      v=solveABN(child,alpha,beta,obj,comments)
      if (v<beta){
        beta=v
        if (comments) {
          announceABN(obj,alpha,beta)
        }
      }
      else {
        if (comments){
          cat("Alpha-beta values are not updated for node ",obj$index,".",sep="")
          readline("")
        }
      }
      if (alpha>=beta) {
        if (isA){
          if (comments){
            cat("Alpha-beta interval collapses - a prune is possible.\n")
          }
          pruneABP(obj,from,comments,obj$B)
        }
        else {
          if (comments){
            cat("Alpha-beta interval collapses. But since this occurred on the last child of node ",obj$index,
              ", it does not help reduce the work done by the algorithm. This normally just means the branch has been ruled out as being involving the optimal path of play. We mark it in orange.",sep="")
          }
          pruneABP(obj,from,comments)
        }
        return (beta)
      }
      isA=!isA
    }
    if (comments){
      cat("Node ",obj$index," is a min node, so the beta value (",beta,
        ") is returned.",sep="")
      readline("")
    }
    return (beta)
  }
}
#' @keywords internal
plot.abp=function(obj) {
  plot.new()
  plot.window(xlim=c(-0.5,obj$width+.5),ylim=c(-0.5,obj$depth+.5))
  plot(obj$root)
  title("Game tree for alpha-beta pruning.")
  legend(0,obj$depth+.5,c("MAX","MIN"),col=c("red","light blue"),pch=c(15,16),cex=1)
}
#' makeQuestionAlphaBeta
#'
#' Make the alpha-beta pruning question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @param comments Should comments be outputed to console.
#' @param imageFilename The filename of the image to be saved when outputing latex script.
#'  This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionAlphaBeta=function(seed=NA,answer=T,latex=F,comments=T,imageFilename=NULL) {
  if (!is.na(seed))
    set.seed(seed)
  model=alphaBetaPruning()

  if (!is.null(imageFilename))
    jpeg(imageFilename)
  plot(model)
  if (!is.null(imageFilename))
    dev.off()

  if (latex) {
    cat("\\clearpage\n")
    cat("\\section{Alpha-Beta Pruning}\n\n")
    cat(
      "Examine the game tree included in this exam. Note that the values in the nodes are node indices, not mini-max values. Perform alpha-beta pruning on this game tree. ",
      "You should show all working, where this means all alpha-beta values. ",
      "You can write these values on the diagram, or alternatively on a separate sheet of paper. ",
      "In both cases, provide a way of identifying the sequence of updates to alpha-beta values associated with nodes (we suggest you just cross out old values and write new values sequentially downwards). ",
      "If you write on a separate sheet of paper, use the node indices in the diagram as a way of identifying which node particular alpha-beta values are associated with. ",
      "Show where pruning occurs, by indicating which branches will not be evaluated. ",
      "Finally, provide the result (value at end state) of the game assuming optimal play. (3 marks)\n"
    )
  }
  else {
    cat(
      "Examine the game tree in the plot window. Note that the values in the nodes are node indices, not mini-max values. Perform alpha-beta pruning on this game tree. ",
      "You should show all working, where this means all alpha-beta values. ",
      "Show where pruning occurs, by indicating which branches will not be evaluated. ",
      "Finally, provide the result (value at end state) of the game assuming optimal play.\n",sep="")
  }
  if (answer) {
    cat(
      "Note that in the exam, you will be expected to record all alpha-beta values and how they change, normally on the plot. ",
      "Since this is difficult to do in a small plot, we output these values and their changes in text in the console. ",
      "You will also need to mark branches that are pruned. We use crosses to mark the branch where the alpha-beta interval collapses, and single lines to mark the branches that are pruned as a result. ",
      "Where no branches are pruned, the cross is in orange. You will not be required to mark the orange crosses in the exam. ",
      "They are included here for pedogogical reasons.\n")
    res=solveABP(model,comments)
    title(sub=paste("The result of this game will be",res))
  }
}
#' @keywords internal
testSolveAlphaBeta=function(seed=NA) {
  if (!is.na(seed))
    set.seed(seed)
  model=alphaBetaPruning()
  plot(model)
  cat("Note that in the exam, you will be expected to record all alpha-beta values and how they change, normally on the plot. Since this is difficult to do in a small plot, we output these values and their changes in text in the console. You will also need to mark branches that are pruned. We use crosses to mark the branch where the alpha-beta interval collapses, and single lines to mark the branches that are pruned as a result. Where no branches are pruned, the cross is in orange. You will not be required to mark the orange crosses in the exam. They are included here for pedogogical reasons.")
  readline()
  res=solveABP(model)
  title(paste("The result of this game will be",res))
}
