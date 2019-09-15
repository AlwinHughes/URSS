insertAndReturn = function(conn, x) {
  print("test3")
  x$creationID = 91
  print("test4")
  
  conn$insert(x)
  print("test5")
  item = conn$find('{"creationID": 91}', fields='{"creationID":0}')
  print(item)
  print(conn$update('{"creationID": 91}', '{ "$unset" :{"creationID": ""} }', multiple=T))
  colnames(item)[colnames(item) == "_id"] = "id"
  print("test6")
  return(item)
}

getAttemptsFromIds = function(k.ids, attempts) {

  for(i in attempts) {
    
  }
}


