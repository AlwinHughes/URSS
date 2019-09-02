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
  #print(accepts)
  return(list(accepts, pq))
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

