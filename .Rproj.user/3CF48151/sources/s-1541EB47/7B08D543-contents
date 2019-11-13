#' @keywords internal
makeSchedulingProblem=function(actions=7){

  start=list(
    index=1,
    name="Start",
    after=NA,
    duration=0,
    uses=c(),
    consumes=paste(0,"nails")
  )
  partialOrder=list(start)
  nailsMade=sample(c(0,-1,-1),3)
  durations=seq(5,50,5)
  for (i in 1:3) {
    time=sample(durations,1)
    use=sample(c("Saw"),sample(0:1,1))
    consume=nailsMade[i]
    nails="nails"
    if (consume==1 || consume==-1)
      nails="nail"
    nextAction=list(
      index=length(partialOrder)+1,
      name=paste("Action",i),
      after=1,
      duration=time,
      uses=use,
      consumes=paste(consume,nails)
    )
    partialOrder=append(partialOrder,list(nextAction))
  }

  nailsUsed=sample(c(rep(0,actions-5),1,1))
  for (i in 4:actions) {
    pred=sample(2:i,sample(1:3,1))
    time=sample(durations,1)
    use=sample(c("Hammer","Saw"),sample(0:2))
    consume=nailsUsed[i-3]
    # Make sure consumtion always consistent
    if (consume)
      pred=union(pred,which(nailsMade==-1)+1)
    nails="nails"
    if (consume==1 || consume==-1)
      nails="nail"
    nextAction=list(
      index=length(partialOrder)+1,
      name=paste("Action",i),
      after=pred,
      duration=time,
      uses=use,
      consumes=paste(consume,nails)
      )
    partialOrder=append(partialOrder,list(nextAction))
  }
  asPred=sapply(2:(actions+1),function(i) {
    any(sapply(partialOrder,function(act)return(i %in% act$after)))
  })

  finish=list(
    index=length(partialOrder)+1,
    name="Finish",
    after=which(!asPred)+1,
    duration=0,
    uses=c(),
    consumes=paste(0,"nails")
  )
  partialOrder=append(partialOrder,list(finish))
  return (partialOrder)
}
#' @keywords internal
unconstrainedSchedule=function(partialOrder){
  ES=rep(NA,length(partialOrder))
  LS=ES
  for (action in partialOrder){
    if (is.na(action$after[1]))
      ES[action$index]=0
    else {
      ES[action$index]=max(sapply(action$after,function(pre){
        ES[pre]+partialOrder[[pre]]$duration
      }))
    }
  }
  for (action in rev(partialOrder)) {
    if (action$index==length(partialOrder))
      LS[action$index]=ES[action$index]
    else {
      before=which(sapply(partialOrder,function(act){
        action$index %in% act$after
      }))
      LS[action$index]=min(sapply(before,function(post){
        LS[post]-action$duration
      }))
    }
  }
  slack=LS-ES
  cat("\nUnconstrained Schedule:\n")
  for (i in 1:length(partialOrder)) {
    cat(i,". ",partialOrder[[i]]$name,": ES=",ES[i],", LS=",LS[i],", Slack=",slack[i],"\n",sep="")
  }
  list(ES=ES,LS=LS,slack=slack)
}

#' @keywords internal
firstConstrainedTime_=function(duration,timeMin,freeIntervals) {
  time=0
  done=FALSE
  resources=length(freeIntervals)
  sufficientIntervals=list()
  for (resource in freeIntervals) {
    good=sapply(resource,function(interval){
      interval$to-max(interval$from,timeMin)>=duration
    })
    nextSufficientIntervals=resource[good]
    if (length(nextSufficientIntervals)==0){
      stop("Invalid schedule 5")
    }
    for (i in 1:length(nextSufficientIntervals)) {
      if (nextSufficientIntervals[[i]]$from<timeMin)
        nextSufficientIntervals[[i]]$from=timeMin
    }
    sufficientIntervals=append(sufficientIntervals,list(nextSufficientIntervals))
  }

  for (interval in sufficientIntervals[[1]]) {
    if (resources==1)
      return(interval$from)
    else {
      for (interval2 in sufficientIntervals[[2]]) {
        maxFrom=max(interval$from,interval2$from)
        minTo=min(interval$to,interval2$to)
        if (duration<=minTo-maxFrom) {
          if (resources==2)
            return (maxFrom)
          else {
            for (interval3 in sufficientIntervals[[3]]) {
              maxFrom2=max(maxFrom,interval3$from)
              minTo2=min(minTo,interval3$to)
              if (duration<=minTo2-maxFrom2) {
                  return (maxFrom2)
              }
            }
          }
        }
      }
    }
  }
  stop("Invalid Schedule 1.")
}
#' @keywords internal
firstConstrainedTime=function(duration,
                              after,nails,hammer,saw,
                              Durations,Times,Nails,Hammer,Saw) {
  timeMin=max(Times[after]+Durations[after])
  if (is.na(after[1]))
    timeMin=0
  if (nails>0) {
    if (length(Nails)==0) {
      stop("Invalid schedule 2.")
    }
    nailTimes=list()
    nailCount=0
    for (event in Nails) {
      if (event$effect==-1) {
        # A nail is made
        nailCount=nailCount+1
        if (length(nailTimes)==0 ||
            nailTimes[[length(nailTimes)]]$to!=Inf) {
          nailTimes=append(nailTimes,list(list(from=event$time,to=Inf)))
        }
      }
      else {
        # A nail is used
        nailCount=nailCount-1
        if (nailCount<0) {
          stop("Invalid schedule 3.")
        }
        else if (nailCount==0) {
          if (length(nailTimes)==0) {
            stop("Invalid schedule 4.")
          }
          nailTimes[[length(nailTimes)]]$to=event$time
        }
      }
    }
    if (hammer) {
      if (saw) {
        # Need a nail, hammer and saw
        return (firstConstrainedTime_(duration,timeMin,list(nailTimes,Hammer,Saw)))
      }
      else {
        # Need a nail and hammer
        return (firstConstrainedTime_(duration,timeMin,list(nailTimes,Hammer)))
      }
    }
    else if (saw) {
      # Need a nail and saw
      return (firstConstrainedTime_(duration,timeMin,list(nailTimes,Saw)))
    }
    else {
      # Need a nail
      return (firstConstrainedTime_(duration,timeMin,list(nailTimes)))
    }
  }
  else if (hammer) {
    if (saw) {
      # Need a hammer and saw
      return (firstConstrainedTime_(duration,timeMin,list(Hammer,Saw)))
    }
    else {
      # Need a hammer
      return (firstConstrainedTime_(duration,timeMin,list(Hammer)))
    }
  }
  else if (saw) {
    # Need a saw
    return (firstConstrainedTime_(duration,timeMin,list(Saw)))
  }
  else {
    # Needs nothing
    return (timeMin)
  }
}
#' @keywords internal
constrainedSchedule=function(partialOrder,times) {
  toDo=1:length(partialOrder)
  Times=rep(NA,length(partialOrder))
  Durations=sapply(partialOrder,function(act)act$duration)
  Nails=list() # Element gives time and effect (+1,-1) of event
  # Element gives start and stop of resource being free
  Hammer=list(list(from=0,to=Inf))
  Saw=list(list(from=0,to=Inf))
  while (length(toDo)>0) {
    predDone=sapply(partialOrder,function(action){
      is.na(action$after) || !any(is.na(Times[action$after]))
    })
    posActions=which(1:length(partialOrder)%in%toDo &
                       predDone )
    nextAction=posActions[which(times$slack[posActions]==min(times$slack[posActions]))][1]
    after=partialOrder[[nextAction]]$after
    nails=0
    if (partialOrder[[nextAction]]$consumes=="1 nail")
      nails=1
    else if  (partialOrder[[nextAction]]$consumes=="-1 nail")
      nails=-1
    saw="Saw"%in%partialOrder[[nextAction]]$uses
    hammer="Hammer"%in%partialOrder[[nextAction]]$uses
    time=firstConstrainedTime(partialOrder[[nextAction]]$duration,
                              after,nails,hammer,saw,
                              Durations,Times,Nails,Hammer,Saw)
    if (nails!=0) {
      eventTime=time
      if (nails==-1)
        eventTime=time+partialOrder[[nextAction]]$duration
      nailEvent=list(list(time=eventTime,effect=nails))
      if (length(Nails)==0)
        Nails=append(Nails,nailEvent)
      else{
        # > rather than >= so uses go after productions at same time
        nailTimes=sapply(Nails,function(event)event$time>eventTime)
        #nailTimes=sapply(Nails,function(event)event$time>=eventTime)
        if (!any(nailTimes))
          Nails=append(Nails,nailEvent)
        else {
          cutIndex=which.max(nailTimes)
          if (cutIndex==1) {
            Nails=append(nailEvent,Nails)
          }
          else {
            Nails=append(append(Nails[1:(cutIndex-1)],nailEvent),Nails[cutIndex:length(Nails)])
          }
        }
      }
    }
    if (saw) {
      sawTimes=sapply(Saw,function(event)event$to>=time)
      cutIndex=which.max(sawTimes)
      newEvent1=Saw[[cutIndex]]
      newEvent1$to=time
      newEvent2=Saw[[cutIndex]]
      newEvent2$from=time+partialOrder[[nextAction]]$duration
      newEvent=list()
      if (newEvent1$from!=newEvent1$to)
        newEvent=append(newEvent,list(newEvent1))
      if (newEvent2$from!=newEvent2$to)
        newEvent=append(newEvent,list(newEvent2))
      if (cutIndex==1) {
        if (length(Saw)==1)
          Saw=newEvent
        else
          Saw=append(newEvent,Saw[2:length(Saw)])
      }
      else if (cutIndex==length(Saw)) {
        Saw=append(Saw[1:(length(Saw)-1)],newEvent)
      }
      else {
        Saw=append(append(Saw[1:(cutIndex-1)],newEvent),Saw[(cutIndex+1):length(Saw)])
      }
    }
    if (hammer) {
      hammerTimes=sapply(Hammer,function(event)event$to>=time)
      cutIndex=which.max(hammerTimes)
      newEvent1=Hammer[[cutIndex]]
      newEvent1$to=time
      newEvent2=Hammer[[cutIndex]]
      newEvent2$from=time+partialOrder[[nextAction]]$duration
      newEvent=list()
      if (newEvent1$from!=newEvent1$to)
        newEvent=append(newEvent,list(newEvent1))
      if (newEvent2$from!=newEvent2$to)
        newEvent=append(newEvent,list(newEvent2))
      if (cutIndex==1) {
        if (length(Hammer)==1)
          Hammer=newEvent
        else
          Hammer=append(newEvent,Hammer[2:length(Hammer)])
      }
      else if (cutIndex==length(Hammer)) {
        Hammer=append(Hammer[1:(length(Hammer)-1)],newEvent)
      }
      else {
        Hammer=append(append(Hammer[1:(cutIndex-1)],newEvent),Hammer[(cutIndex+1):length(Hammer)])
      }
    }
    Times[nextAction]=time
    toDo=toDo[-which(toDo==nextAction)]
  }

  cat("\nConstrained Schedule:\n")
  cat("Action Start Times:\n")
  for (i in 1:length(Times)) {
    cat(i,". ",partialOrder[[i]]$name,": ",Times[i],"\n",sep="")
  }
  cat("Nail Consumption:\n")
  for (i in 1:length(Nails)) {
    cat(i,". Time: ",Nails[[i]]$time,", Effect: ",Nails[[i]]$effect,"\n",sep="")
  }
  cat("Saw Use:\n")
  cnt=0
  for (i in 1:length(Saw)) {
    if (cnt==0) {
      if (Saw[[i]]$from!=0) {
        cnt=cnt+1
        cat(cnt,". From: 0, To: ",Saw[[i]]$from,"\n",sep="")
      }
    }
    else {
      cat(", To: ",Saw[[i]]$from,"\n",sep="")
    }
    if (Saw[[i]]$to!=Inf) {
      cnt=cnt+1
      cat(cnt,". From: ",Saw[[i]]$to,sep="")
    }
  }
  cat("Hammer Use:\n")
  cnt=0
  for (i in 1:length(Hammer)) {
    if (cnt==0) {
      if (Hammer[[i]]$from!=0) {
        cnt=cnt+1
        cat(cnt,". From: 0, To: ",Hammer[[i]]$from,"\n",sep="")
      }
    }
    else {
      cat(", To: ",Hammer[[i]]$from,"\n",sep="")
    }
    if (Hammer[[i]]$to!=Inf) {
      cnt=cnt+1
      cat(cnt,". From: ",Hammer[[i]]$to,sep="")
    }
  }
}
#' makeQuestionSchedule
#'
#' Make the scheduling question
#' @param seed The random seed to use to generate the values. If NA, they are generated from the
#' current seed.
#' @param answer Should the answer be outputed to console
#' @param latex Should latex script be outputed. This is for use with the makeExam function.
#' @return NULL
#' @export
makeQuestionSchedule=function(seed=NA,answer=T,latex=F) {
  if(!is.na(seed))
    set.seed(seed)
  partialOrder=makeSchedulingProblem()
  if (latex) {
    cat("\\clearpage\n")
    cat("\\section{Scheduling}\n\n")
    cat("Provide a complete resource constrained schedule for the actions found in Table~\\ref{schActions}. (4 marks)\n")

    cat("\\begin{table}[h!]\n")
    cat("\\caption{Actions}\n")
    cat("\\label{schActions}\n",sep="")
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{ |c|c|c|c|c|c| } \n")
    cat("\\hline\n")
    cat(" Index & Action & Duration & Uses & Consumes & After \\\\\n")
    cat("\\hline\n")
    for (act in partialOrder) {
      cat(act$index," & ",act$name," & ",act$duration,
          " &  ",paste(act$uses,collapse=","),
          " & ",act$consumes,
          " & ",paste(act$after,collapse=","),"\\\\\n",sep="")
    }
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}\n")
  }
  else {
    cat("\nActions to schedule:\n")
    for (act in partialOrder) {
      cat(act$index,". ",act$name,"\n  Duration: ",act$duration,
          "\n  Uses: ",paste(act$uses,collapse=","),
          "\n  Consumes: ",act$consumes,
          "\n  After: ",paste(act$after,collapse=","),"\n",sep="")
    }
  }
  if (answer){
    ucTimes=unconstrainedSchedule(partialOrder)
    constrainedSchedule(partialOrder,ucTimes)
  }
}
