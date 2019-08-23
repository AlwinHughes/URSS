lotwfullalph = c(letters, LETTERS, " ", ".", ",", "!", "?", "(", "'", ")", ":", "-", ";", "—", "'", "’", "“", "“", "”","‘", as.character(0:9), "`")

alph.simp = c(letters, " ", ",", ".", "'", "!", "?", "-", ":", ";")

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

getP.log = function(booknvec,alphabet) {
  P.log = matrix(ncol = length(alphabet), nrow =length(alphabet), 0)
  n = length(booknvec);


  for(i in 2:length(booknvec)) {
    P.log[booknvec[i-1], booknvec[i]] = P.log[booknvec[i-1], booknvec[i]] +1
  }

  P.log = apply(P.log, c(1,2), function(x) {
    if(x == 0) {
      return(-14)
    }
    return(log(x/n))
  })


  return(P.log)
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

  for( l in 1:M) {

    #propose phi
    phi = sigma
    ij = sample(1:length(alphabet),2, replace=TRUE)
    a = sigma[ij[1]]
    phi[ij[1]] = phi[ij[2]]
    phi[ij[2]] = a


    #construct mphi
    mphi = constructMphi(msig,ij)

    #find betaphi
    betaphi = beta[match(ciphertext.num[1], phi)]
    #print(betasig)
    #print(betaphi)

    #compute alpha and new liklihood
    temp = liklihood(sigma.l, msig, mphi, P, betasig, betaphi)
    alpha = exp(temp[1])
    phi.l = temp[2]

    #print(exp(alpha))
    #generate
    U = runif(1)
    if(U < alpha) {
      accepts = accepts + 1
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

      #print("-----")
    }

  }
  #print(accepts)
  return(pq)
}


constructMphi = function(msig, ij) {
  matphi = msig
  i = ij[1]
  j = ij[2]
  ri = matphi[i,]
  rj = matphi[j,]
  ci = matphi[,i]
  cj = matphi[,j]
  matphi[i,] = rj
  matphi[j,] = ri
  matphi[,i] = cj
  matphi[,j] = ci
  return(matphi)
}

liklihood = function(sigma.l, msig, mphi, P, betasig, betaphi) {
  beta.phi = 0
  beta.sigma = 0
  l = beta.phi - beta.sigma + sum((mphi - msig) * P)
  return( c(min(c(1, l)), l + sigma.l))
}

