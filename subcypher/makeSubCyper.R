upperalph = c(LETTERS, " ", ".", ",", "!", "?", "'", ")", "-", "_", "+", "=", ":", as.character(0:9))
loweralph = c(letters, " ", ".", ",", "!", "?", "(", "'", ")", "-", "_", "+", "=", ":", as.character(0:9))
fullalph = c(letters, LETTERS, " ", ".", ",", "!", "?", "(", "'", ")", ":", "\"", "-", ";", "—", "'", "’", "“", "“", "”","‘", "_", as.character(0:9), "`")

lotwfullalph = c(letters, LETTERS, " ", ".", ",", "!", "?", "(", "'", ")", ":", "-", ";", "—", "'", "’", "“", "“", "”","‘", as.character(0:9), "`")

createSubCyph <- function(alphlength) {
  k = 0
  alphabet = 1:alphlength
  while(k < alphlength *2 ) {
    i = sample(1:alphlength,2, replace=TRUE)
    a = alphabet[i[1]]
    alphabet[i[1]] = alphabet[i[2]]
    alphabet[i[2]] = a
    k = k + 1
    #print(alphabet)
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

encrypt = function(cypher, alphabet, message) { # 

  if(length(cypher) != length(alphabet)) {
    print("error cyper and alphabet length not equal")
    return()
  }

  messagevec = strsplit(message, '')[[1]]

  for( i in 1:length(messagevec)) {
    if(match(messagevec[i], alphabet)) {
      print("message contains letters not in alphabet")
      return()
    }
  }

  cyphertext = applycypher(cypher, alphabet, strsplit(message, '')[[1]])

  return(paste(cyphertext, collapse=''))
}

applycypher <- function(cypher, alphabet, message) {
  n = length(cypher)

  inv = inverseSubCyper(cypher)
  cyphertext = character(length=length(message))
  for(i in 1:length(message)) {
    cyphertext[i] = alphabet[cypher[match(message[i], alphabet)]]
  }

  return(cyphertext)
}

applycyphernum  <- function(cypher, nummess) {
  inv = inverseSubCyper(cypher)

  cyphtext = character(length = length(nummess))
  for(i in 1: length(nummess)) {
    cyphtext[i] = cypher[nummess[i]]
  }
  return(cyphtext)
}


getFrequencies <- function(text, alphabet) {

  messagevec = strsplit(text, "")[[1]]
  for( i in 1:length(messagevec)) {
    if(is.na(match(messagevec[i], alphabet))) {
      print("message contains letters not in alphabet")
      print(messagevec[i])
      return()
    }
  }

  P = matrix( ncol = length(alphabet), nrow= length(alphabet), 0)

  print(P)

  currenti = match(messagevec[1], alphabet)

  for(i in 2:length(messagevec)) {
    nexti = match(messagevec[i], alphabet)
    P[currenti, nexti] = P[currenti, nexti] +1
    currenti = nexti
  }

  return(P)
}




n = 0 
getFrequenciesFromBook <- function(book, alphabet) {

  #matrix of frequencies of on letter after another
  P = matrix( ncol = length(alphabet), nrow= length(alphabet), 0)

  for(i in 1: length(book)) {

    messagevec = strsplit(book[i], "")[[1]]
    if(length(messagevec) > 1) {
      n <<-  n + length(messagevec)
      for( j in 1:length(messagevec)) {
        if(is.na(match(messagevec[j], alphabet))) {
          print("message contains letters not in alphabet")
          print(messagevec[j])
          print(book[i])
          return()
        }
      }

      currenti = match(messagevec[1], alphabet)

      for(j in 2:length(messagevec)) {
        nexti = match(messagevec[j], alphabet)
        P[currenti, nexti] = P[currenti, nexti] +1
        currenti = nexti
      }
    }

  }
  return(P)
}

convertMessageToNumeric = function(message, alphabet) {
  if(length(message) == 1){
    message = strsplit(message, "")[[1]]
  }
  return(mapply(function(x) { return(match(x,alphabet))}, message))
}


