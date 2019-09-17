lotwfullalph <- c(letters, LETTERS, " ", ".", ",", "!", "?", "'", ":", "-", ";", "—", "\"", "\n")

alph.simp = c(letters, " ", ",", ".", "'", "!", "?", "-", ":", ";", "\"")

fullalph <- c(letters, LETTERS, " ", ".", ",", "!", "?", "'", ":", "-", ";", "—", "\"", "\n", "*", "(", ")", 0:9, "=", "/")

swap.elements = function(key, ij) {
  phi = key
  phi[ij[1]] = key[ij[2]]
  phi[ij[2]] = key[ij[1]]
  return(phi)
}


gradient = function(ciphertext.num, start.key, dictionary, M, alph) {
  
  sigma = start.key
  KEY.LEN = length(start.key)

  word.list = getWordList(ciphertext.num, alph)
  #word.list = strsplit(paste(convertNumericToMessage(ciphertext.num, alph), collapse = ''), ' ')
  print("1")
  space.index = match(" ", alph)
  nl.index = match("\n", alph)

  sigma.char.not.in.word = charactersNotInWord3(word.list, dictionary)

  best.phi.word.list = list()
  best.phi = integer(length(start.key))

  char.not.in.word.vec = integer(M)
  for(i in 1:M) {
    print("-")

    for(j in 1:(KEY.LEN)) {
      print("--")
      for(k in (j+1):KEY.LEN) {
        phi.word.list = list()
        print("---")

        phi.word.list = list()
        ij = c(j,k)
        phi = sigma
        phi[ij[1]] = sigma[ij[2]]
        phi[ij[2]] = sigma[ij[1]]

        #print( convertNumericToMessage(applycipher.num(inverseSubCipher(phi), ciphertext.num),alph)[1:400])
        cat("swapped", alph[phi[ij[1]]], " and ", alph[phi[ij[2]]], "\n")

        if(any(ij == space.index) || any(ij == nl.index)) {
          #recalculate word list required
          print("chose space or \\n")
          phi.word.list = getWordList(applycipher.num(inverseSubCipher(phi),ciphertext.num), alph)
          sigma.char.not.in.word = charactersNotInWord3(phi.word.list, dictionary)
          #print(length(word.list))
        } else {
          phi.word.list = apply.swap.to.wordlist(word.list, ij)
          phi.char.not.in.word = charactersNotInWord3(phi.word.list, dictionary)
        }

        if(phi.char.not.in.word < sigma.char.not.in.word) {
          print("found new best")
          sigma.char.not.in.word = phi.char.not.in.word
          best.phi.word.list = phi.word.list
          best.phi = phi
        }
        cat("scored ", phi.char.not.in.word, "\n")
        #readline()

      }
    }
    sigma = best.phi
    word.list = best.phi.word.list
    char.not.in.word.vec[i] = sigma.char.not.in.word

  }
  return(list(sigma, char.not.in.word.vec))
}

apply.swap.to.wordlist = function(word.list, swap.chars) {
  new.word.list = list()
  for( i in 1:length(word.list)) {
    a = match(swap.chars[1], word.list[[i]])
    b = match(swap.chars[2], word.list[[i]])

    #print(a)

    w = replace(word.list[[i]], a, swap.chars[2])
    w = replace(w, b, swap.chars[1])
    new.word.list[[i]] = w
  }
  return(new.word.list)
}

charactersNotInWord3 = function(word.list, dictionary) {
  n = 0;
  for(w in word.list) {
    
    if(!isInDictionary(dictionary, paste(convertNumericToMessage(w, alph), collapse = ''))) {
      #print(w)
      n = n + length(w)
    }
  }
  return(n)
}

getWordList = function(char.vec, alph) {
  start = 1;
  ret.list = list()
  list.count = 1
  space.char = match(" ", alph)



  if(is.na(space.char) || length(space.char) != 1 || !all(!is.na(char.vec))) {
    print("alphabet does not cover message")
    return()
  }

  for(i in 1:length(char.vec)) {
    if(char.vec[i] == space.char) {

      ret.list[[list.count]] = char.vec[start:(i-1)]
      start = i +1
      list.count = list.count +1
    }
  }
  ret.list[[list.count]] = char.vec[start:length(char.vec)]
  return(ret.list)
}
