
runExperiment = function(keys.number, attempts.num, alph, logP, beta, plaintext.num, M, m.comment="", func = identity) {
  n = length(alph)

  #keys = mongo(db="R", collection="keys") 
  #attempts = mongo(db="R", collection="attempts")



  m.names = c("accept rate", "liklehood", "a-z diffs","all diff")

  out.accept.rate = numeric(attempts.num * keys.number)
  out.likelihood = numeric(attempts.num * keys.number)
  out.az.diff = numeric(attempts.num * keys.number)
  out.all.diff = numeric(attempts.num * keys.number)
  out.readable = logical(attempts.num * keys.number)
  out.key = matrix(ncol=n, nrow=attempts.num * keys.number)

  for(i in 1:keys.number) {

    key = createSubCiph(n)
    ciphertext.num = applycipher.num(key, plaintext.num)

    
    for(j in 1:attempts.num) {

      res = breakCipher(ciphertext.num, beta, logP, alph, M, func)

      l = 1 #length(res[[2]]$data)

      suggested.key = res[[2]]$data[[10]]

      #cat("decription:\n", paste(convertNumericToMessage(applycipher.num(inverseSubCipher(suggested.key),ciphertext.num), alph), collapse=''), "\n")

      readable = FALSE #as.logical(tolower(readline("Is this readable? [y/n]\n"))=="y")

      out.accept.rate[j+ (i-1)*attempts.num] = res[[1]]/M
      out.likelihood[j+ (i-1)*attempts.num] = res[[2]]$priorities[[10]]
      out.az.diff[j+ (i-1)*attempts.num] = 26 - sum(suggested.key[1:26] == key[1:26])
      out.all.diff[j+ (i-1)*attempts.num] = n - sum(suggested.key == key)
      out.readable[j+ (i-1)*attempts.num] = readable
      out.key[j + (i-1)*attempts.num,] = suggested.key

    }
    #print(apply(function(x) { return(I(list(x)))}, 1, out.key) )
    



#    key.doc = {}
#   key.doc$key = list(key)
#  key.doc$attempts = attempts.num
#    key.doc$textlength = length(plaintext.num)
#   key.doc$alphlength = length(alph)
#   key.doc$comment = m.comment
#    key.doc$iterations = M

 #   mphi = getFrequenciesNum(ciphertext.num, alph)
 #
 #   mphi[1:n,] = mphi[key,]
 #   mphi[,1:n] = mphi[,key]
 #
 #   key.doc$likelihood = likelihood(
 #     0, matrix(ncol=n, nrow=n, 0), 
 #     getFrequenciesNum(applycipher.num(inverseSubCipher(key), ciphertext.num), alph),
 #     logP, beta[ciphertext.num[1]],
 #     beta[match(applycipher.num(inverseSubCipher(key), ciphertext.num[1]), key)]
 #     )[[2]]
 #
 #   k = insertAndReturn(keys, key.doc)
 #
 #   attempts.doc$keyid = k$id
 #
 #   attempts$insert(attempts.doc)
 #
 #   print(k)
 #   print(attempts.doc)
 #
  }
    attempts.doc = data.frame(acceptance =out.accept.rate, likelihood = out.likelihood, azddiff = out.az.diff, alldiff = out.all.diff, readable = out.readable)
    attempts.doc$key = apply(out.key, 1, function(x) {return(list(x)) })
    return(list(data.frame(alph.length=length(alph), text.length=length(plaintext.num), iterations=M) ,attempts.doc))
}

createBoxPlot = function(query = '{}', projection = '{}') {
  attemptsconn = mongo(db="R", collection="attempts")
  boxplot(attemptsconn$find(query,projection))
}

powers.res = list()
ratio = 10^(1/3)
a = ratio
iterations = c(1000, 1000*a, 1000*a*a, 1000 * a * a , 10000, 10000 * a, 10000 * a * a)
powers = c(0.25, 0.333333, 0.5, 1, 2, 3, 4)

largeExperiment = function() {
  res = list()
  for(p in 1:length(powers)) {
    res[[p]] = list()
    for( i in 1:length(iterations)) {
      cat("starting power: ", powers[p], " iteration ", iterations[i]);
      res[[p]][[i]] = runExperiment(200, 1, fullalph, P.log4, beta4, p20, iterations[i], "part of a large experiment", function(x) { return (x^powers[p])})
    }
  }
  return(res)
}



