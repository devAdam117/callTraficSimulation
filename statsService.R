#Vvytvori obojstranny interval spolahlivosti (cisto iba pomocou kvantilov z dat) podla pozadovanych vstupnych argumentov
# data - hociake numericke data vo vektore
# percentage - aky velky ma byt interval spolahlivosti, udavany v percentach
# TEST
# makeTwoSidedConfidanceInterval(c(1,2,2,2,3,2,4,2),95)
makeTwoSidedConfidanceInterval <- function(data, percentage){
  lowerQuantile <- (1- percentage/100)/2
  upperQuantile <- (percentage/100) +(1 - percentage/100)/2
  return (c(lowerBoud=quantile(data,lowerQuantile), upperBound=quantile(data,upperQuantile)))
}