source('callsGenService.r')
source('nodeService.r')

startSimulation <- function(callEventInterval,nodes,delta,realTime){
  delta <- delta * 60
  callRequest <- 0
  declinedCallRequest <- 0
  numOfNodes <- sqrt(length(nodes))
  timeStampOfLastCallEvent <- 0
  for(i in 1:length(callEventInterval)){
    indexOfCallEvent <- unlist(callEventInterval[i])[1]
    timeStampOfCallEvent <- unlist(callEventInterval[i])[2]
    if(realTime){
      print(nodes)
      Sys.sleep(timeStampOfCallEvent-timeStampOfLastCallEvent)
      timeStampOfLastCallEvent <- timeStampOfCallEvent
      print(paste('Zatial ubehnuty cas: ',floor(timeStampOfLastCallEvent*100)/100, 'sek'))
    }
    if(!realTime){
      print(paste((i/length(callEventInterval))*100,' %'))
    }
    if(timeStampOfCallEvent>delta){
      break
    }
    if(names(timeStampOfCallEvent) =='startTime'){
      callRequest <- callRequest + 1
      # create connection to one other random node
      nodesIndexes <- 1:numOfNodes
      firstNode <- sample(nodesIndexes, 1)
      secondNode <- sample(nodesIndexes[nodesIndexes!=firstNode],1)
      # dvojity sample pre dvojrozmerny vektor nie je spravny pre secondNode  
      if(length(nodesIndexes) == 2){
        firstNode <- 1
        secondNode <- 2
      }
      connectionResult <- createConnection(firstNode,secondNode,nodes)
      nodes <- connectionResult$nodes
      declinedCallRequest <- declinedCallRequest +  connectionResult$declinedNum
      if(connectionResult$declinedNum>1){
        callRequest <- callRequest + connectionResult$declinedNum - 1
      }
      indexOfCorrespondingEndCall <- privateGetIndexOfEndElementByStartIndex(callEventInterval,indexOfCallEvent)
      callEventInterval[indexOfCorrespondingEndCall] <- list(append(unlist(callEventInterval[indexOfCorrespondingEndCall]),c(position1=c(connectionResult$position1[1],connectionResult$position1[2]),position2=c(connectionResult$position2[1],connectionResult$position2[2]))))
      next
    }
    if(names(timeStampOfCallEvent) == 'endTime'){
      if(length(callEventInterval) < 4){
        next
      }
      row1 <- unlist(callEventInterval[i])[3]
      column1 <- unlist(callEventInterval[i])[4]
      nodes[row1,column1] <- nodes[row1,column1] + 1
      
      if(length(callEventInterval) < 6){
        next
      }
      row2 <- unlist(callEventInterval[i])[5]
      column2 <- unlist(callEventInterval[i])[6]
      nodes[row2,column2] <- nodes[row2,column2] + 1
    }
  }
  return(list(nodes=nodes,totalNumOfCallRequests=callRequest,declinedNumOfCallRequest=declinedCallRequest, totalNumOfDirectCallRequest=callRequest))
}

# vrati pozadovany element z vektoru, ktory splna poziadavku propert index = index ...
privateGetIndexOfEndElementByStartIndex <- function(vector,index){
  eventIndex <- 0
  for(element in vector){
    eventIndex <- eventIndex + 1
    idx<- (element[[1]][1])
    name<-(names(element[2]))
    if(idx!=index){
      next
    }
    if(name!='endTime'){
      next
    }
    return(eventIndex)
  }
}




