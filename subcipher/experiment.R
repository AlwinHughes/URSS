
runExperiment = function(keys.number, attempts.num, alph, logP, beta, plaintext.num, M, m.comment="") {
  n = length(alph)

  keys = mongo(db="R", collection="keys")
  attempts = mongo(db="R", collection="attempts")



  m.names = c("accept rate", "liklehood", "a-z diffs","all diff")

  for(i in 1:keys.number) {

    key = createSubCiph(n)
    ciphertext.num = applycipher.num(key, plaintext.num)

    out.accept.rate = numeric(attempts.num)
    out.liklehood = numeric(attempts.num)
    out.az.diff = numeric(attempts.num)
    out.all.diff = numeric(attempts.num)
    out.readable = logical(attempts.num)
    out.key = matrix(ncol=n, nrow=attempts.num)
    
    for(j in 1:attempts.num) {

      res = breakCipher(ciphertext.num, beta, logP, alph, M)

      l = 1 #length(res[[2]]$data)

      suggested.key = res[[2]]$data[[10]]

      cat("decription:\n", paste(convertNumericToMessage(applycipher.num(inverseSubCipher(suggested.key),ciphertext.num), alph), collapse=''), "\n")

      readable = as.logical(tolower(readline("Is this readable? [y/n]\n"))=="y")

      out.accept.rate[j] = res[[1]]/M
      out.liklehood[j] = res[[2]]$priorities[[10]]
      out.az.diff[j] = 26 - sum(suggested.key[1:26] == key[1:26])
      out.all.diff[j] = n - sum(suggested.key == key)
      out.readable[j] = readable
      out.key[j,] = suggested.key

    }
    #print(apply(function(x) { return(I(list(x)))}, 1, out.key) )
    
    attempts.doc = data.frame(acceptance =out.accept.rate, liklehood = out.liklehood, azddiff = out.az.diff, alldiff = out.all.diff, readable = out.readable)
    attempts.doc$key = apply(out.key, 1, function(x) {return(list(x)) })



    print("test")
    key.doc = {}
    key.doc$key = list(key)
    key.doc$attempts = attempts.num
    key.doc$textlength = length(plaintext.num)
    key.doc$alphlength = length(alph)
    key.doc$comment = m.comment
    key.doc$iterations = M

    print("test2")
    k = insertAndReturn(keys, key.doc)
    print(k)

    attempts.doc$keyid = k$id

    attempts$insert(attempts.doc)

  }


}


