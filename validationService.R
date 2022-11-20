#Validacne metody, ktore sluzia pre validaciu vstupnych argumentov, ktorychkolvek inych metod...

# validacna metoda pre vstupne argumenty ostatnych metod, ak niektory argument nieco nesplna, vyhodi exception a argument treba poupravit
validateArguments <- function (methodName,argumentNames,requiredType, requiredLength, ...){
  argumentsToValidate <- list(...)
  lengthResult <- lapply(argumentsToValidate,length)[[1]] == requiredLength
  if(!all(lengthResult)){
    stop(paste('Argument/s with name',argumentNames, 'of method',methodName, 'does not have required length of', requiredLength))
  }
  typeResult <- lapply(argumentsToValidate,class)[[1]] == requiredType
  if(!all(typeResult)){
    stop(paste('Argument/s with name',argumentNames, 'of method',methodName, 'does not have required type of', requiredType))
  }
}

#porovnava hodnoty, ak je value< ako minVal vyhodi exception
validateMinMaxVal <- function(methodName,argumentName,minVal,maxVal,...){
  valuesToValidate <- list(...)
  if(!is.null(minVal)){
    if(!all(valuesToValidate >= minVal)){
      stop(paste('Some values of ',argumentName, 'of method',methodName, 'are less then minimal allowed value = ', minVal))
    }
  }
  if(!is.null(maxVal)){
    if(!all(valuesToValidate <= maxVal)){
      stop(paste('Some values of ',argumentName, 'of method',methodName, 'are greater then maximal allowed value = ', maxVal))
    }
  }
}

