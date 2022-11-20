# vrati nody spolu s volnymi konekciami k dalsim nodom, zobrazenie konekcii medzi nodmi je urobene na dolno trojuholnikovej matici
# numOfNodes - pocet nodov, s ktorymi chceme pracovat (numeric, length=1)
# connectionCapacity - kolkymi konekciami je jeden node prepojeny s hociakym dalsim nodom (numeric, length=1)
# TEST
# initializeNodes(5,20)
initializeNodes <- function(numOfNodes, connectionCapacity) {
  nodes <- matrix(connectionCapacity,numOfNodes,numOfNodes)
  nodes[upper.tri(nodes, diag =  TRUE)] <- 0
  return(nodes)
}


# Vytvori konekciu medzi dvoma pozadovanymi nodami, ak nie je mozne pouzit priamu konekciu, konekcia sa uskutocni
# cez dalsi pomocny node. Ak by aj cesta cez pomocny node bola plna, tak sa bude snazit najst iny volny node  atd. Ak by boli
# vsetky pomocne nody plne, tak sa caka (zatial hadze exception) pokial sa jeden neuvolni a cez ten sa uskutocni konekcia.
# Pri vytvoreni konekcie cez nejake nody sa odcitava kapacita z mapy a na konci uspesnej konekcia sa vrati stav aktualnych konekcii medzi nodami a aj indexy nodov, ktore boli pouzite na danu konekciu.
# firstNode - index nodu od ktore/ku ktoremu chceme nasatvit konekciu
# secondNode - index nodu od ktore/ku ktoremu chceme nasatvit konekciu
# nodesMap - output metody initializeNodes, ktory uz mohol byt modifikovany tj. zmeny v aktualnych kapacitach medzi nodmi (ine hodnoty ako na inite)
# prevSupportNodes - sa nevyplna a metoda ho sama vyuziva rekurzivnym sposobom ako indikator, ci sa v predchadzajucom kroku snazil prepojit firstNode a secondNode cez nejake pomocne nody. V pripadee ze by cesta cez vybanre 3 nody bola plna a node uz bol pred tym pouzity ako podporny, tak sa pouzije iny pomocny node (v zadani toto nebolo blizsie specifikovane a n vyhnutie sa vynimkam to bolo potrbne nejak ohendlovat...)
# TEST
# nodes1 <- initializeNodes(5,20)
# createConnection(4,2,nodes1) priama konekcia
# nodes1[4,2] <- 0
# createConnection(4,2,nodes1) nepriama konekcia
# nodes2 <- matrix(0, 20, 20)
# nodes2[4,3] <- 1
# nodes2[3,2] <- 1
# createConnection(4,2,nodes2) nepriama konekcia ale nie prva 
# nodes3 <- matrix(0, 5, 5)
# createConnection(4,2,nodes3) neprebehla ziadna konekcia vsetky su plne
createConnection <- function(firstNode,secondNode, nodesMap, prevSupportNodes=NULL){
  # s kolkymi nodami celkovo pracujeme 
  numOfNodes <- sqrt(length(nodesMap))
  numOfTriedSupportingNodes <- length(prevSupportNodes)
  # ak sa pracuje s viacej ako 2 nodmi v mape a pocet pomocnych nodov uz vycerpal vsetky moznosti konekcia nenastane 
  if(numOfTriedSupportingNodes == numOfNodes-2 && numOfTriedSupportingNodes>0){
    return(list(
      nodes=nodesMap,
      position1=c(NULL,NULL),
      position2=c(NULL,NULL),
      declinedNum = length(prevSupportNodes)
      )
    )
  }
  # ak hovor medzi dvoma nodmi uz vyskusal vsetky cesty a vsetky boli plne, tak sa hovor uplne zamieta
  # v zadani nebolo jasne napisane co sa ma stat ak konekcia medzi dvoma nodmi nie je hotova po druhom pokuse s pouzitim supporting nodes
  # tak som to aplikoval nech to skusi vzuzit vsetky existujuce supporting nodes a ak ani jeden nie je volny, tak sa hovor zamieta
  # alternativa by bola na pockanie pokial niekto skonci hovor ale to je asi o ladku vyssie nez bolo mierene povodne zadanie..
  # pokus o priamu konekciu ak to kapacita dovoluje a ak pred tym uz nebol pokus o nepriamu konekciu
  directRow <- max(firstNode,secondNode)
  directColumn <- min(firstNode,secondNode)
  
  #specialne ak su iba dva nody a nie je medzi nimi volno, tak je hovor rovno zamitnuty lebo nevie pozuit ziaden pdorporny
  if(nodesMap[directRow,directColumn] == 0 && numOfNodes==2){
    return(list(
      nodes=nodesMap,
      position1=c(NULL,NULL),
      position2=c(NULL,NULL),
      declinedNum = 1
      )
    )
    
  }
  if(nodesMap[directRow,directColumn] > 0 && is.null(prevSupportNodes) ){
    nodesMap[directRow,directColumn] <- nodesMap[directRow,directColumn] - 1
    return(list(
      nodes=nodesMap,
      position1=c(
        row=directRow, 
        column=directColumn),
      position2=c(NULL,NULL),
      declinedNum = length(prevSupportNodes)
      )
    )
  }
  #nepriama konekcia, bud nebola dostatocna kapacita na priamu konekciu alebo uz pred tym bola nepriama, tak podla zadania musi byt opat nepriama
  avSupportNodes <- 1:numOfNodes
  avSupportNodes <- avSupportNodes[avSupportNodes != firstNode & avSupportNodes != secondNode]
  #predch. nepriamu nebudeme opat uvazovat ako pomocnu
  if(!is.null(prevSupportNodes)){
    avSupportNodes<-avSupportNodes[!avSupportNodes %in% prevSupportNodes]
  }
  newSupportNode <- sample(avSupportNodes,1)
  prevSupportNodes <- append(prevSupportNodes,newSupportNode)
  firstRow <- max(firstNode,newSupportNode)
  firstColumn <- min(firstNode,newSupportNode)
  secondRow <- max(secondNode,newSupportNode)
  secondColumn <- min(secondNode,newSupportNode)
  if(nodesMap[firstRow,firstColumn] > 0 && nodesMap[secondRow,secondColumn] > 0){
    nodesMap[firstRow,firstColumn] <- nodesMap[firstRow,firstColumn] -1
    nodesMap[secondRow,secondColumn] <- nodesMap[secondRow,secondColumn] -1
    return(list(
      nodes=nodesMap,
      position1=c(
        row=firstRow, 
        column=firstColumn),
      position2=c(
        row=max(newSupportNode,secondNode), 
        column=min(newSupportNode,secondNode)),
      declinedNum = length(prevSupportNodes)
    )
    )
  }
  return(createConnection(firstNode, secondNode, nodesMap, prevSupportNodes))
}
