#' @keywords internal
generateAStarEdges=function(nodes=5) {
  edges = matrix(rep(0,(nodes+2)^2),nrow=nodes+2)
  rownames(edges)=c("Start",LETTERS[1:nodes],"Goal")
  colnames(edges)=c("Start",LETTERS[1:nodes],"Goal")
  for (i in 1:(nrow(edges)-1)) {
    if (i==(nrow(edges)-1))
      edges[i,i+1]=1
    else {
      other=sample((i+1):min(i+4,ncol(edges)),sample(0:2,1))
      edges[i,other]=1
      if (i!=nrow(edges))
        edges[i,i+1]=1
    }
  }
  p=matrix(sample(2:7,length(edges),T),nrow=nrow(edges))
  edges=edges*p
  return (edges)
}
#' @keywords internal
makeHeuristc=function(edges){
  nodes=nrow(edges)
  out=rep(0,nodes)
  minH=min(edges[which(edges>0)])
  for (i in (nodes-1):1) {
    neighs=which(edges[i,]>0)
    out[i]=min(out[neighs])+minH
  }
  return (out)
}
#' @keywords internal
makeBasicHeuristic=function(edges){
  nodes=nrow(edges)
  out=rep(0,nodes)
  minH=1
  for (i in (nodes-1):1) {
    neighs=which(edges[i,]>0)
    out[i]=min(out[neighs])+minH
  }
  return (out)
}

#' @keywords internal
makeNode=function(index,f,h,route) {
  list(index=index,f=f,h=h,route=route)
}
#' @keywords internal
whichInFrontier=function(frontier,index) {
  which(sapply(frontier,function(n)n$index==index))
}
#' @keywords internal
inFrontier=function(frontier,index) {
  if (length(frontier)==0)
    return (FALSE)
  else
    return (any(whichInFrontier(frontier,index)))
}
#' @keywords internal
bestFrontier=function(frontier) {
  if (length(frontier)==0)
    stop("WTF?")
  which.min(sapply(frontier,function(n)n$f+n$h))
}
#' @keywords internal
addToFrontier=function(frontier,newNode,edges,heuristic,nextNode) {
  if (!inFrontier(frontier,newNode)) {
    frontier=append(frontier,list(
      makeNode(
        newNode,
        nextNode$f+edges[nextNode$index,newNode],
        heuristic[newNode],
        c(nextNode$route,newNode)
        )))
  }
  else {
    otherIndex=whichInFrontier(frontier,newNode)
    other=frontier[[otherIndex]]
    newNodeNode=makeNode(
      newNode,
      nextNode$f+edges[nextNode$index,newNode],
      heuristic[newNode],
      c(nextNode$route,newNode)
    )
    if (other$f+other$h>newNodeNode$f+newNodeNode$h) {
      frontier=frontier[-otherIndex]
      frontier=append(frontier,list(newNodeNode))
    }
  }
  return (frontier)
}

#' @keywords internal
testSolve=function(edges,heuristic) {
  print("Heuristic:")
  print(heuristic)
  frontier=list()
  nextNode=makeNode(1,0,heuristic[1],c(1))
  turns=0
  while (nextNode$index!=length(heuristic)) {
    turns=turns+1
    print(paste("Turn:",turns))
    print(paste("Next:",nextNode$index,"(",nextNode$f,"-",nextNode$h,"-",paste(nextNode$route,collapse=","),")"))
    neighbors=which(edges[nextNode$index,]>0)
    for (n in neighbors) {
      frontier=addToFrontier(frontier,n,edges,heuristic,nextNode)
    }
    print("Frontier:")
    for (n in frontier)
      print(paste("  ",n$index,"(",n$f,"-",n$h,"-",paste(n$route,collapse=","),")"))
    best=bestFrontier(frontier)
    nextNode=frontier[[best]]
    frontier=frontier[-best]
  }
  print(paste("Total Turn:",turns))
  print(paste("Cost:",nextNode$f))
  print(paste("Route:",paste(nextNode$route,collapse=",")))
}
#' makeQuestionAStar
#'
#' Make the A* question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param basic Should the heuristic be basic (assume all edges cost at least one). If false, a better heuristic
#' is sought.
#' @param nodes The number of nodes in the search space. The real question will have 5.
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionAStar=function(seed=NA,basic=F,nodes=5,answer=T,latex=F) {
  if (!is.na(seed))
    set.seed(seed)
  edges=generateAStarEdges(nodes)
  if (latex) {
    cat("\\clearpage\n")
    cat("\\section{A-Star}\n")
    cat("\nTable~\\ref{AStar_Edges} gives the edge values for a shortest path problem. ")
    cat("Using these and the A* algorithm, find the shortest path from the start node to the goal node. ")
    cat("Provide a valid heuristic and show all working. (4 marks)\n\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Edges}\n")
    cat("\\label{AStar_Edges}\n")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c||c|c|c|c|c|c|c| } \n")
    cat("\\hline\n")
    cat(" & ",paste(colnames(edges),collapse=" & "),"\\\\\n",sep="")
    cat("\\hline\n")
    for (i in 1:nrow(edges)) {
      cat(rownames(edges)[i]," & ",paste(edges[i,],collapse=" & "),"\\\\\n",sep="")
    }
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")
  }
  else {
    print("Edges:")
    print(edges)
  }
  if (answer) {
    heuristic=makeHeuristc(edges)
    if (basic)
      heuristic=makeBasicHeuristic(edges)
    testSolve(edges,heuristic)
  }
}
#' @keywords internal
getAnswerAStar=function(seed=NA,basic=F){
  if(!is.na(seed))
    set.seed(seed)
  edges=generateAStarEdges(nodes)
  heuristic=makeHeuristc(edges)
  if (basic)
    heuristic=makeBasicHeuristic(edges)
  testSolve(edges,heuristic)
  return (NULL)
}
