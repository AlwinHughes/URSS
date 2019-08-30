lotwfullalph <- c(letters, LETTERS, " ", ".", ",", "!", "?", "'", ":", "-", ";", "—", "\"", "\n")

alph.simp = c(letters, " ", ",", ".", "'", "!", "?", "-", ":", ";", "\"")

convertMessageToNumeric = function(message, alphabet) {
  if(length(message) == 1){
    message = strsplit(message, "")[[1]]
  }
  return(mapply(function(x) { return(match(x,alphabet))}, message))
}

convertNumericToMessage = function(nummessage, alphabet) {
  mapply( function(x) { return(alphabet[x])}, nummessage)
}


getFrequenciesNum <- function(nummessage, alphabet) {

  M = matrix( ncol = length(alphabet), nrow= length(alphabet), 0)

  rownames(M) <- alphabet
  colnames(M) <- alphabet

  currenti = nummessage[1]

  for(i in 2:length(nummessage)) {
    nexti = nummessage[i]
    M[currenti, nexti] = M[currenti, nexti] +1
    currenti = nexti
  }

  return(M)
}

getBeta = function(message.num, alphabet) {
  beta = numeric(length(alphabet))

  for(i in 1:length(message.num)) {
    if(is.na(message.num[i])) {
      print(message.num[i])
    }
    beta[message.num[i]] = beta[message.num[i]] + 1
  }


  beta = mapply(function(x) {
    if(x == 0) {
      return(-14)
    }
    return(log(x/length(message.num)))
  }, beta )

  return(beta)
}

getP.log.frombook = function(book, alph) {
  m <- matrix(0, nrow=length(alph), ncol=length(alph))
  rownames(m) <- alph
  colnames(m) <- alph
  for (line in readLines(book, encoding="UTF-8")) {
    if (line=="" | line == " " | line == "\r") next
    line <- trim.line(line, alph)
    m["\n", substr(line, 1, 1)] <- m["\n", substr(line, 1, 1)] + 1
    for (i in 1:(nchar(line)-1)) {
      current <- substr(line, i, i)
      then <- substr(line, i+1, i+1)
      if (!then %in% alph) print(then)
      m[current, then] <- m[current, then] + 1
    }
    m[substr(line, nchar(line), nchar(line)), "\n"] <- m[substr(line, nchar(line), nchar(line)), "\n"] + 1
  }
  m = m/rowSums(m)
  m = log(m)
  m[m==-Inf] = -12
  m[m==NaN]= -12
  return(m)

}

getB.log.frombook = function(book, alph) {

  m <- matrix(0, nrow=length(alph), ncol=length(alph))
  rownames(m) <- alph
  colnames(m) <- alph
  for (line in readLines(book, encoding="UTF-8")) {
    if (line=="" | line == " " | line == "\r") next
    line <- trim.line(line, alph)
    m["\n", substr(line, 1, 1)] <- m["\n", substr(line, 1, 1)] + 1
    for (i in 1:(nchar(line)-1)) {
      current <- substr(line, i, i)
      then <- substr(line, i+1, i+1)
      if (!then %in% alph) print(then)
      m[current, then] <- m[current, then] + 1
    }
    m[substr(line, nchar(line), nchar(line)), "\n"] <- m[substr(line, nchar(line), nchar(line)), "\n"] + 1
  }

  return(log(rowSums(m)/ sum(m)))
}


trim.line <- function(line, alph) {
  while (nchar(line)>0 & !(substr(line, 1, 1) %in% alph)) {
    line <- substr(line, 2, nchar(line))
  }
  while (nchar(line)>0 & !(substr(line, nchar(line), nchar(line)) %in% alph)) {
    line <- substr(line, 1, nchar(line)-1)
  }
  return(line)
}



getP.log = function(book.num, alphabet) {
  P.log = matrix(ncol = length(alphabet), nrow =length(alphabet), 0)
  

  for(i in 2:length(book.num)) {
    P.log[book.num[i-1], book.num[i]] = P.log[book.num[i-1], book.num[i]] +1
  }



  P.log = apply(P.log, 1, function(x) {
    if(sum(x) == 0) { 
      print("all zero")
      return(numeric(length(alphabet)))
    }
    return(x/sum(x))
  })



  
  P.log = apply(P.log, c(1,2), function(x) {
    if(x == 0) {
      return(-12)
    }
    return(log(x))
  })


  return(P.log)
}


transposition <- function(sigma, ij=NA) {
  if (sum(is.na(ij))>0) ij <- sample(sigma, 2)
  i <- min(ij)
  j <- max(ij)
  if (i==1) start <- c()
  else start <- 1:(i-1)
  if (j==length(sigma)) end <- c()
  else end <- (j+1):length(sigma)
  if (abs(i-j)<=1) middle <- c()
  else middle <- (i+1):(j-1)
  return(list(sigma[c(start, j, middle, i, end)], i, j))
}



breakCipher = function(ciphertext.num, beta, P, alphabet, M = 100) {

  k = length(alphabet)

  accepts = 0

  sigma = 1:k 
  msig = matrix(ncol=k, nrow=k)
  mphi = matrix(ncol=k, nrow=k)

  msig = getFrequenciesNum(ciphertext.num, alphabet)

  pq = PriorityQueue$new()
  pq$push(sigma, 0)

  sigma.l = 0

  betasig = beta[ciphertext.num[1]]

  phi = sigma
  for( l in 1:M) {
    # --my version --

      #propose phi
      phi = sigma
      ij = sample(1:length(alphabet),2)
     
      #make phi by swaping parts of sigma
      phi[ij[1]] = sigma[ij[2]]
      phi[ij[2]] = sigma[ij[1]]



    #--reece's way--
    #  t  = transposition(sigma)
    #  phi = t[[1]]
    #  ij = c(t[[2]], t[[3]])

    mphi = constructMphi(msig,ij)

    betaphi = beta[match(ciphertext.num[1], phi)]

    temp = liklihood(sigma.l, msig, mphi, P, betasig, betaphi)
    alpha = temp[1]
    phi.l = temp[2]

    if(rbinom(1,1,alpha)) {
      accepts = accepts + 1
      #push if something with same liklihood doesn't already exist
      if(is.na(match(-phi.l, pq$priorities))) {
        pq$push(phi, -phi.l)
      }

      #pop if too long
      if(pq$size() > 10) {
        pq$pop()
      }

      #swap phi and sigma ready to start again
      sigma = phi
      msig = mphi
      betasig = betaphi
      sigma.l = phi.l

    }

  }
  print(accepts)
  return(pq)
}


constructMphi = function(msig, ij) {
  matphi = msig
  matphi[rev(ij),] = matphi[ij,]
  matphi[,rev(ij)] = matphi[,ij]
  return(matphi)
}

liklihood = function(sigma.l, msig, mphi, P, betasig, betaphi) {
  l = betaphi - betasig + sum((mphi - msig) * P)
  return( c(min(c(1, exp(l))), l + sigma.l))
}

writeResult = function(pq, ciphertext.num, alph) {
  a = character(length(pq$data))

  for(i in 1:length(pq$data)) {
    a[i] = paste(convertNumericToMessage(applycipher.num(inverseSubCipher(pq$data[[i]]), ciphertext.num), alph), collapse = '')
  }

  write(paste(a, collapse = "\n\n "), "res.txt")
}

