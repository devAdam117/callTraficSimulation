# vygeneruje casovu os naplnenu casmi zaciatku hovorov
# totalTime - celkovy cas, za aky sledujeme generovanie novych hovorov
# lambda - parameter, ktory nadstavi konkretne exp. rozdelenie podla, ktoreho sa generuju data na casovu os (ako casto sa hovori nezavisle od seba tvoria)
# timeStamps=NULL - sa pri inite nevyplna, sluzi ako cumulative zaznam na zapisovanie casov na spominanej osy
# TEST 
# generateStartCallInterval(10,3) 
privateGenerateStartCallInterval <- function(totalTime,lambda, timeStamps) {
  timeStamps <- NULL
  newTimeStamp <- 0
  index <- 0 
  while(1){
    index = index +1
    if(missing(timeStamps)){
      newTimeStamp <- rexp(1,lambda)
      timeStamps <- c(list(c(index=index,startTime=newTimeStamp)))
      next
    }
    
    newTimeStamp <- newTimeStamp + rexp(1,lambda)
    if(newTimeStamp > totalTime){
      break
    }
    timeStamps <- append(timeStamps,list(c(index=index,startTime=newTimeStamp)))
  }
  return(timeStamps)
}

# k uz vygenerovanym zaciatkom callom vygeneruje ich koniec na novu casovu os s tymi istymi typmi udajov
# startTimeStamps - je output metody generateStartCallInterval
# mu - parameter, ktory nadstavi konkretne exp. rozdelenie podla, ktoreho sa generuju data na casovu os (dlzka daneho hovoru)
# TEST 
# generateEndCallInterval(generateStartCallInterval(10,3) , 1/10)
privateGenerateEndCallInterval <- function(startTimeStamps,mu){
  endTimeStamps <- c()
  for(index in 1:length(startTimeStamps)){
    newEndTimeStamp <- rexp(1,mu) +startTimeStamps[[index]][[2]]
    endTimeStamps <- append(endTimeStamps, list(c(index=index,endTime=newEndTimeStamp)))
  }
  return (endTimeStamps)
}

# usporiada intevraly zaciatkov hovorov a koncov hovorov do jedneho spolocneho casoveho intervalu
# callsStartTimeStamps - output metody generateStartCallInterval
# callsEndTimeStamps - output metody generateEndCallInterval
# TEST
# callStarts <- generateStartCallInterval(10,3)
# orderTimeStamps(callStarts,generateEndCallInterval(callStarts,1/10))
privateOrderTimeStamps <- function(callsStartTimeStamps,callsEndTimeStamps){
  return(c(callsStartTimeStamps,callsEndTimeStamps)[ order( sapply(c(callsStartTimeStamps,callsEndTimeStamps), "[", 2) ) ])
}

# vrati rovno celkovy casovy interval zaciatkov a koncov hovorov
# ( casy zaciatkov hovorov su <= delta ale konce hovorov mozu byt > delta )
# lambda - param. podla ktoreho sa budu generovat nove hovory
# mu - param. podla ktoreho sa budu generovat konce hovorov (dlzky hovorov)
# delta - dlzka pocas ktorej zaznamenavame nove hovory
# TEST
# generateTimeEventInterval(1,1,20)
generateTimeEventInterval <- function(mu,lambda,delta){
  delta <- delta * 60
  callsStartTimeStamps <- privateGenerateStartCallInterval(delta,lambda)
  callsEndTimeStamps <- privateGenerateEndCallInterval(callsStartTimeStamps,mu)
  return(privateOrderTimeStamps(callsStartTimeStamps,callsEndTimeStamps))
}
