n <- 100
p <- 15
s <- 100
origin <- rep(0,p)
d <- rep(0,s)

for (i in 1:s) {
  
  raw <- matrix(data = runif(n = n*p, min = 0, max = 1), ncol = p, nrow = n)
  
  d[i] <- min(
    apply(raw, 1, function(row) sum((row - origin)^2))
  )
  
}

cat(median(d))