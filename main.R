source('serviceUtils.r')
source('statsService.r')
source('validationService.r')

# c - kapacita per node
# numOfNodes - pocet nodov
# lambda - lambda parameter nastvitelny pre generovanie zaciatkov hovorov podla exp rozdelenia
# mu - mu parameter nastvitelny pre generovanie dlzky hovorov podla exp rozdelenia
# n - kolko simulacii chceme spustit
# realTime - ak je n==1 a realTime= true dostavame postupny vystup do konzoly aktualnych konekcii spolu s casovym udajom
# confidenceInterval - ak je n>1, vygeneruje interval spolahlivosti (pomocou kvantilov), so spolahlivostou confidenceInterval udanu v rozmdedzi  (0-100)  v % , aky pomer hovorov bolo zamietnutych (priamych aj nepriamych)
main <- function(c,numOfNodes,lambda,mu,delta,n,realTime,confidenceInterval){
  #validacia vstupnuch argumentov
  validateArguments('main','c,lambda,mu,delta,n,realTime,confidenceInterval','numeric', 1,c,numOfNodes,lambda,mu,delta,n,realTime,confidenceInterval)
  validateMinMaxVal('main','c,numOfNodes,lambda,mu,delta,n,realTime',1,NULL,c,numOfNodes,lambda,mu,delta,n,realTime)
  validateMinMaxVal('main','confidenceInterval', 0.01, 99.99,confidenceInterval)
  
  timeEventInterval<- generateTimeEventInterval(mu,lambda,delta)
  nodes <- initializeNodes(numOfNodes,c)
  if(n ==1){
    simulationResult <- startSimulation(timeEventInterval,nodes,delta,realTime)
    return(simulationResult)
  }
  declinedCallsPortion <- c()
  declinedTotalPortion <- c()
  for(i in 1:n){
    simulationResult <- startSimulation(timeEventInterval,nodes,delta,FALSE)
    totalCalls <- simulationResult$totalNumOfCallRequests
    declinedCalls <- simulationResult$declinedNumOfCallRequest
    declinedCallsPortion <- append(declinedCallsPortion,declinedCalls/totalCalls)
  }
  return(makeTwoSidedConfidanceInterval(declinedCallsPortion,confidenceInterval))
}


# Nastavenie argumentov na prevolavanie
# ako rychlo sa maju vytvarat nove hovori (vyssie je rychlejsie generovanie)
newCallFrequency <- 50
# ako dlho ma ptretrvavat hovor mensie je dlhsi hovor)
lengthOfCall <- 1/50
# inicializacna dostupna kapacita medzi dvoma nodmi
initCapacitytPerNode <- 100
# aky cas v minutach chceme pozorovat
observationTime <- 1
#pocet nodov, s ktorymi chceme pracvoat (v zadani 5), tu je to volitelne
numOfNodes <- 5
# kolko simulacii, chceme sledovat ak je n>1 tak nam vypise aj interval spolahlivosti, ak chceme sledovat priebeh hovorov, tak iba v pripade n==1
n <- 100
# priebeh simulacie musi byt aj realTime==TRUE, v opacnom pripade vypise len konecny stav
realTime <- FALSE
# pozadovany interval spolahlivosti
confidenceIntervalPercentage<- 95

# spustenie scriptu
main(initCapacitytPerNode,
     numOfNodes,
     newCallFrequency,
     lengthOfCall,
     observationTime,
     n,
     realTime,
     confidenceIntervalPercentage)













