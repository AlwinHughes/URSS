upperalph = c(LETTERS, " ", ".", ",", "!", "?", "'", ")", "-", "_", "+", "=", ":", as.character(0:9))
loweralph = c(letters, " ", ".", ",", "!", "?", "(", "'", ")", "-", "_", "+", "=", ":", as.character(0:9))
fullalph = c(letters, LETTERS, " ", ".", ",", "!", "?", "(", "'", ")", ":", "\"", "-", ";", "—", "'", "’", "“", "“", "”","‘", "_", as.character(0:9), "`")

lotwfullalph = c(letters, LETTERS, " ", ".", ",", "!", "?", "(", "'", ")", ":", "-", ";", "—", "'", "’", "“", "“", "”","‘", as.character(0:9), "`")

createSubCiph <- function(alphlength) {
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

inverseSubCipher  <- function(cipher) {
  inverse = integer(length(cipher))
  for( i in 1: length(cipher)) {
    inverse[cipher[i] ] = i
  }
  return(inverse)
}

composeCipher <- function(c1, c2) {
  if(length(c1) != length(c2)) {
    print("cipher lenghts do not match")
    return(NULL)
  }

  res = integer(length(c1))

  for(i in 1: length(c1)) {
    res[i] = c2[c1[i]]
  }

  return(res)
}

encrypt = function(cipher, alphabet, message) { # 

  if(length(cipher) != length(alphabet)) {
    print("error cipher and alphabet length not equal")
    return()
  }

  messagevec = strsplit(message, '')[[1]]

  for( i in 1:length(messagevec)) {
    if(match(messagevec[i], alphabet)) {
      print("message contains letters not in alphabet")
      return()
    }
  }

  ciphertext = applycipher(cipher, alphabet, strsplit(message, '')[[1]])

  return(paste(ciphertext, collapse=''))
}

applycipher <- function(cipher, alphabet, message) {
  n = length(cipher)

  inv = inverseSubCipher(cipher)
  ciphertext = character(length=length(message))
  for(i in 1:length(message)) {
    ciphertext[i] = alphabet[cipher[match(message[i], alphabet)]]
  }

  return(ciphertext)
}

applycipher.num  <- function(cipher, plaintext.num) {
  inv = inverseSubCipher(cipher)

  ciphtext = numeric(length = length(plaintext.num))
  for(i in 1: length(plaintext.num)) {
    ciphtext[i] = cipher[plaintext.num[i]]
  }
  return(ciphtext)
}


getFrequencies <- function(text, alphabet) {

  messagevec = strsplit(text, "")[[1]]
  for( i in 1:length(messagevec)) {
    if(is.na(match(messagevec[i], alphabet))) {
      print("message contains letters not in alphabet")
      #print(messagevec[i])
      return()
    }
  }

  P = matrix( ncol = length(alphabet), nrow= length(alphabet), 0)

  #print(P)

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
    print("splitting message")
    message = strsplit(message, "")[[1]]
  }
  return(mapply(function(x) { return(match(x,alphabet))}, message))
}



