lotwfullalph <- c(letters, LETTERS, " ", ".", ",", "!", "?", "'", ":", "-", ";", "—", "\"", "\n")

alph.simp = c(letters, " ", ",", ".", "'", "!", "?", "-", ":", ";", "\"")

fullalph <- c(letters, LETTERS, " ", ".", ",", "!", "?", "'", ":", "-", ";", "—", "\"", "\n", "*", "(", ")", 0:9, "=", "/")

#char.not.in.word2 = numeric(100)

get.alpha = function(sig.char.not.in.word, phi.char.not.in.word) {
  return( min(1, exp((sig.char.not.in.word-phi.char.not.in.word)/10)))
}

breakWithDictionary = function(ciphertext.num, start.key, dictionary, M, alph) {

  not.in.word.sig.vec = numeric(M)
  sigma = start.key

  possible.plaintext = paste(applycipher.num(inverseSubCipher(sigma), ciphertext.num), collapse='')

  sigma.char.not.in.word = charactersNotInWord(possible.plaintext, dictionary)

  pq = PriorityQueue$new()

  j = 1
  while(j <= M) {

    #propose new phi
    ij = sample(1:length(start.key),2, replace=FALSE)
    phi = sigma
    phi[ij[1]] = sigma[ij[2]]
    phi[ij[2]] = sigma[ij[1]]


    possible.plaintext = paste(convertNumericToMessage(applycipher.num(inverseSubCipher(phi), ciphertext.num), alph), collapse = '')
    phi.char.not.in.word = charactersNotInWord(possible.plaintext, dictionary)

    #cat("phi chars not in words : ", phi.char.not.in.word, "\n")
    #cat("sig chars not in words : ", sigma.char.not.in.word, "\n")
    
    #pi.phi = get.pi(-phi.char.not.in.word)
    #cat("pi sigma ", pi.sigma, " pi phi ", pi.phi, "\n")

    alpha = get.alpha(sigma.char.not.in.word, phi.char.not.in.word) 

    cat("alpha: ", alpha, "\n")
    if(rbinom(1,1,alpha)) {
      print("accept")

      if(is.na(match(phi.char.not.in.word, pq$priorities))) {
         pq$push(phi, phi.char.not.in.word)
      }

      if(pq$size() > 10) {
        pq$pop()
      }

      sigma = phi
      pi.sigma = pi.phi
      sigma.char.not.in.word = phi.char.not.in.word

    } else {
      print("reject")
    }
      possible.plaintext2 = applycipher.num(inverseSubCipher(sigma), ciphertext.num)

      possible.words2 = paste(convertNumericToMessage(possible.plaintext2, alph), collapse = '')

      #word.count = 0
    #not.word.count = 0

      print(sigma.char.not.in.word)
      not.in.word.sig.vec[j] <- sigma.char.not.in.word

    j = j + 1
    
  }

  return(list(pq, not.in.word.sig.vec))
}

charactersNotInWord = function(text, dictionary) {

  possible.words = strsplit(text, ' ')[[1]]
  #print(possible.words)
  char.not.in.word = 0
  for(w in possible.words) {
    if(w == '') {
      next
    }

    #print(w)
    l = length(strsplit(w, '')[[1]])
    i = 1
    w.split = strsplit(tolower(w), '')[[1]]
    #print(w.split)

    #while(is.na(match(w.split[[i]], letters)) && i < l) {
    #  i = i + 1
    #}

    if(w.split[l] %in% c(".", "\"", "'", ",")) {
      w = substr(w, 1, l-1)
    }
    #while(w.split[[l]] %in% c(".", "\"", "'", ",") && l > i) {
    #  l = l -1
    #}

    if(l == i) {
      next
    }

    #cat("i: ", i, " l: ", l, "\n")
    #w2 = substring(w,i,l -1)
    #print(w)
    #print(length(w.split))
    if(isInDictionary(dictionary, tolower(w))) {
    #if(tolower(w) %in% dictionary) {
      #cat(w , " word\n")
    } else {
      char.not.in.word = char.not.in.word  + nchar(w)
      #not.word.count = not.word.count +1
      #cat(w, " not\n")
    }
  }
  return(char.not.in.word);
}

charactersNotInWord2 = function(possible.word.list, dictionary) {
  char.not.in.word = 0
  for(w in possible.words.list) {
    if(w == '') {
      next
    }

    l = length(strsplit(w, '')[[1]])
    i = 1
    w.split = strsplit(tolower(w), '')[[1]]

    if(w.split[l] %in% c(".", "\"", "'", ",")) {
      w = substr(w, 1, l-1)
    }

    if(l == i) {
      next
    }

    if(isInDictionary(dictionary, tolower(w))) {
    } else {
      char.not.in.word = char.not.in.word  + nchar(w)
    }
  }
  return(char.not.in.word);
}
