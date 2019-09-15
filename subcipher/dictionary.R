
isInDictionary = function(dictionary, word) {
  word = tolower(word)
  if(! isLowerCaseWord(word)) {
    #print("not made of lowercase")
    return(FALSE)
  }

  n = length(dictionary)
  position = floor(n/2)
  span = floor(n/2)
  last.pos = 0;

  while(span >= 0.5) {
    #cat("span ", span, " pos: ", position, "\n")
    if(dictionary[position] == word) {
      return(TRUE)
    }
    span = span/2
    if(dictionary[position] < word) {
      position = min(position + ceiling(span), n)
    } else {
      position = max(1, position - ceiling(span))
    }
  }
  #print("returned")
  return(FALSE)
}

isLowerCaseWord = function(word) {
  word = strsplit(word,'')[[1]]
  for(l in word) {
    if(is.na(match(l, letters))) {
      return(FALSE)
    }
  }
  return(TRUE)
}

getNumOfWords = function(text) {
  
}
