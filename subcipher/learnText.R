lotwfullalph <- c(letters, LETTERS, " ", ".", ",", "!", "?", "'", ":", "-", ";", "—", "\"", "\n")

convertMessageToNumeric = function(message, alphabet) {
  if(length(message) == 1){
    message = strsplit(message, "")[[1]]
  }
  return(mapply(function(x) { return(match(x,alphabet))}, message))
}

convertNumericToMessage = function(nummessage, alphabet) {
  mapply( function(x) { return(alphabet[x])}, nummessage)
}

loadBookToNum = function(path, alph) {
  lines = readLines(path)
  book = strsplit(paste(lines, collapse='\n'), '')[[1]]

  ret = numeric(length(book))
  for(i in 1:length(book)) {
    ret[i] = match(book[i],alph)
  }
  return(ret)

  
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


getQ.log = function(book.num, alphabet) {
  n = length(alphabet)
  Q.log = array(dim=c(n,n,n))
  

  for(i in 3:length(book.num)) {
    .log[book.num[i-2],book.num[i-1], book.num[i]] = P.log[book.num[i-2], book.num[i-1], book.num[i]] +1
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


