P = t(matrix(ncol = 4, nrow = 4, c(0.3, 0.7, 0,0,   0,0,1,0,   0,0,0,1,  0.2,0,0.3,0.5)))

powerOfMatrix = function(mat, power) {
  ret = mat
  for(i in 2: power) {
    ret = mat %*% ret
  }
  return(ret)
}

simulateMC = function(P, start, n) {
  current = start
  chain = integer(n)
  for(i in 1:n) {
    u = runif(1)
    if(u < P[current,1]) {
      nxt = 1
    } else if ( P[current,1] < u && u < P[current, 2] + P[current,1] ) {
      nxt = 2
    } else if (P[current,1] + P[current,2] < u && u < P[current,1] + P[current,2] + P[current, 3]) {
      nxt = 3
    } else {
      nxt = 4
    }
    chain[i] = nxt
    current = nxt
  }
  return(chain)
}
