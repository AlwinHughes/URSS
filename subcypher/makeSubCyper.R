
createSubCyph <- function(alphlength) {
  k = 0
  alphabet = 1:alphlength
  while(k < alphlength) {
    i = sample(1:alphlength,2)
    a = alphabet[i[1]]
    alphabet[i[1]] = alphabet[i[2]]
    alphabet[i[2]] = a
    k = k + 1
  }
  return(alphabet)
}

inverseSubCyper  <- function(cypher) {
  inverse = integer(length(cypher))
  for( i in 1: length(cypher)) {
    inverse[cypher[i]] = i
  }
  return(inverse)
}

composeCyper <- function(c1, c2) {
  if(length(c1) != length(c2)) {
    print("cypher lenghts do not match")
    return(NULL)
  }

  res = integer(length(c1))
  
  for(i in 1: length(c1)) {
    res[i] = c2[c1[i]]
  }

  return(res)
}
