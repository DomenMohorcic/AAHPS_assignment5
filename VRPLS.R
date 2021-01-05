###############################################################################
#                                                                             #
#               ASSIGNMENT 5 - Vehicle routing problem                        #
#                                                                             #
###############################################################################

# read the data
read_data <- function(name) {
  conn = file(paste("problems/Problem", toString(name), ".txt", sep=""), open="r")
  text = readLines(conn)
  close(conn)
  
  tmp <- strsplit(text[1], ",")
  num.sites <- as.integer(tmp[[1]][1])
  num.carry <- as.integer(tmp[[1]][2])
  
  data.sites <- data.frame(matrix(ncol = 6, nrow = num.sites))
  colnames(data.sites) <- c("id", "x", "y", "organic", "plastic", "paper")
  for(i in 1:num.sites) {
    tmp <- unlist(strsplit(text[1+i], ","))
    data.sites[i,] <- as.double(tmp)
  }
  
  data.roads <- data.frame()
  for(i in 1:(length(text)-(1+num.sites))) {
    tmp <- unlist(strsplit(text[1+num.sites+i], ","))
    df <- data.frame(start = as.integer(tmp[1]), end = as.integer(tmp[2]), length = as.double(tmp[3]), carry = as.double(tmp[5]))
    data.roads <- rbind(data.roads, df)
    if(as.integer(tmp[4]) == 0) {
      df <- data.frame(start = as.integer(tmp[2]), end = as.integer(tmp[1]), length = as.double(tmp[3]), carry = as.double(tmp[5]))
      data.roads <- rbind(data.roads, df)
    }
  }
  return(list("num.sites" = num.sites, "num.carry"= num.carry, "data.sites" = data.sites, "data.roads" = data.roads))
}


# calculate fitness function
fitness <- function(x, data.cpy) { # x: list of routes (list of lists)
  cost.all <- 0 # total cost of specific garbage collection
  
  for(i in 1:length(x)) {
    carry <- 0 # how much current truck is carrying
    route <- x[[i]] # get current route
    len <- 0 # current trip length
    time <- 0 # current trip time
    cost <- 10 # every trip costs 10
    for(j in 2:length(route)) {
      # calculate which road to take from j-1 to j
      subroads <- data.roads[data.roads$start == route[j-1],] # find roads that start with j-1
      possible <- subroads[subroads$end == route[j],] # find roads that end with j
      found <- FALSE
      if(length(possible$start) > 0) {
        possible <- possible[order(possible$length),] # sort remaining roads by their length
        for(k in 1:length(possible$start)) {
          if(possible[k,]$carry >= carry) { # take shortest road that can carry us
            found <- TRUE
            break
          }
        }
      }
      if(found) {
        len <- len + possible[k,]$length # add road length to trip length
      } else {
        len <- len + penalty.length # penalize under-carrying and non-existant road
      }
      
      # calculate time spent servicing
      if(data.cpy$garbage[route[j]] > 0 && route[j] != 1) {
        time <- time + 0.2 # servicing is 12min
      }
      if(route[j] == 1) {
        time <- time + 0.5 # emptying at depot is 30min
      }
      
      # new carrying capacity (if already at 0 following does nothing)
      carry <- carry + data.cpy$garbage[route[j]]
      data.cpy$garbage[route[j]] <- 0 # remove all garbage from site
    }
    cost <- cost + 0.1*len # travel cost is 0.1/km
    time <- time + len/50 # travel speed is 50km/h
    if(time > 8) { # time cost is 10/h and 20/h for overtime
      cost <- cost + 80 + (time-8)*20
    } else {
      cost <- cost + time*10
    }
    
    if(carry > num.carry) {
      cost <- cost*penalty.carry # penalize carrying overload
    }
    
    cost.all <- cost.all + cost # update global cost of specific garbage collection
  }
  
  if(sum(data.cpy$garbage) > 0) { # not all garbage is collected
    cost.all <- cost.all * penalty.garbage*sum(data.cpy$garbage)
  }
  return(cost.all)
}


"
x: 1 2 3 1 4 5 1 -> two trips

combine two trips together
split trip into two trips

add a site to the trip (random one randomly somewhere)
remove a site from a trip (random one)

switch two sites in a trip (random two, not necessearly two consecutive)
"
neighborhood <- function(x) {
  n <- c()
  idx <- 1
  
  # drop a number
  for(i in 2:(length(x)-1)) {
    tf.v <- rep(TRUE, length(x))
    tf.v[i] <- FALSE
    n.v <- x[tf.v]
    if(checkValid(n.v)) {
      n[[idx]] <- n.v
      idx <- idx + 1
    }
  }
  
  # add a number
  for(i in 1:(length(x)-1)) {
    for(k in 1:num.sites) {
      n.v <- c(x[1:i], k, x[(i+1):length(x)])
      if(checkValid(n.v)) {
        n[[idx]] <- n.v
        idx <- idx + 1
      }
    }
  }
  
  # swap a number
  for(i in 2:(length(x)-1)) {
    num <- x[i]
    for(k in 1:num.sites) {
      if(k != num) {
        n.v <- x
        n.v[i] <- k
        if(checkValid(n.v)) {
          n[[idx]] <- n.v
          idx <- idx + 1
        }
      }
    }
  }
  
  return(n)
}

# check if a neighborhood is valid
checkValid <- function(t) {
  for(i in 2:length(t)) {
    if(t[i-1] == t[i]) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# transform into matrix (readable by the fitness function)
toMatrix <- function(n) {
  mat <- c()
  idx <- 1
  cut <- 2
  for (i in 2:length(n)) {
    if (n[i] == 1) {
      mat[[idx]] <- c(1, n[cut:i])
      idx <- idx + 1
      cut <- i + 1
    }
  }
  return(mat)
}

# LS Algorithm
AlgorithmLS <- function(s0, data.cpy) {
  sm <- s0
  
  while(TRUE) {
    n <- neighborhood(s0)
    f <- c()
    
    # calculate all fitness values for neighborhood
    for (i in 1:length(n)) {
      f[i] <- fitness(toMatrix(n[[i]]), data.cpy)
    }
    
    # find best (min) fitness function for neighborhood
    best.idx <- which.min(f)
    
    # current best fitness
    fc <- fitness(toMatrix(sm), data.cpy)
    
    if (f[best.idx] < fc) {
      sm = n[[best.idx]]
      s0 = n[[best.idx]]
    } else {
      break
    }
  }
  
  return(sm)
}

# SA Algorithm
AlgorithmSA <- function(s0, lambda, t, data.cpy) {
  s <- s0
  sm <- s0
  
  while (t >= 1) {
    n <- neighborhood(s)
    rand <- sample(1:length(n), 1)
    sc <- n[[rand]]
    
    fm <- fitness(toMatrix(sm), data.cpy)
    fc <- fitness(toMatrix(sc), data.cpy)
    f <- fitness(toMatrix(s), data.cpy)
    
    if (fc < fm) {
      sm <- sc
    }
    
    if (fc < f) {
      s <- sc
    } else {
      prob <- exp(-(fc - f) / t)
      #print(prob)
      t <- t * lambda
      if(runif(1, 0, 1) < prob) {
        #print("Chosen worse")
        s <- sc
      }
    }
  }
  
  # end with LS
  sm <- AlgorithmLS(sm, data.cpy)
  
  return(sm)
}

rd <- read_data(1)
num.sites <- rd$num.sites
num.carry <- rd$num.carry
data.sites <- rd$data.sites
data.roads <- rd$data.roads

penalty.length <- sum(data.roads$length)*100
penalty.carry <- 2
penalty.garbage <- 2


# basic LS algorithm
best.organic <- c()
f.organic <- Inf
best.plasitc <- c()
f.plastic <- Inf
best.paper <- c()
f.paper <- Inf
for(i in 1:10) {
  data.cpy <- data.frame(data.sites$id, data.sites$organic)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.organic <- AlgorithmLS(s0, data.cpy)
  f.res.organic <- fitness(toMatrix(res.organic), data.cpy)
  if(f.res.organic < f.organic) {
    f.organic <- f.res.organic
    best.organic <- res.organic
  }
  
  data.cpy <- data.frame(data.sites$id, data.sites$plastic)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.plastic <- AlgorithmLS(s0, data.cpy)
  f.res.plastic <- fitness(toMatrix(res.plastic), data.cpy)
  if(f.res.plastic < f.plastic) {
    f.plastic <- f.res.plastic
    best.plastic <- res.plastic
  }
  
  data.cpy <- data.frame(data.sites$id, data.sites$paper)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.paper <- AlgorithmLS(s0, data.cpy)
  f.res.paper <- fitness(toMatrix(res.paper), data.cpy)
  if(f.res.paper < f.paper) {
    f.paper <- f.res.paper
    best.paper <- res.paper
  }
  
  print(i)
}


# Simulated Annealing LS
best.organic <- c()
f.organic <- Inf
best.plasitc <- c()
f.plastic <- Inf
best.paper <- c()
f.paper <- Inf
for(i in 1:10) {
  data.cpy <- data.frame(data.sites$id, data.sites$organic)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.organic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  f.res.organic <- fitness(toMatrix(res.organic), data.cpy)
  if(f.res.organic < f.organic) {
    f.organic <- f.res.organic
    best.organic <- res.organic
  }
  
  data.cpy <- data.frame(data.sites$id, data.sites$plastic)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.plastic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  f.res.plastic <- fitness(toMatrix(res.plastic), data.cpy)
  if(f.res.plastic < f.plastic) {
    f.plastic <- f.res.plastic
    best.plastic <- res.plastic
  }
  
  data.cpy <- data.frame(data.sites$id, data.sites$paper)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.paper <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  f.res.paper <- fitness(toMatrix(res.paper), data.cpy)
  if(f.res.paper < f.paper) {
    f.paper <- f.res.paper
    best.paper <- res.paper
  }
  
  print(i)
}


###############################################################################
#                                                                             #
#                             Solving all problems                            #
#                                                                             #
###############################################################################
for(i in 1:10) {
  rd <- read_data(i)
  num.sites <- rd$num.sites
  num.carry <- rd$num.carry
  data.sites <- rd$data.sites
  data.roads <- rd$data.roads
  
  best.organic <- c()
  f.organic <- Inf
  best.plasitc <- c()
  f.plastic <- Inf
  best.paper <- c()
  f.paper <- Inf
  for(j in 1:10) {
    
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$organic)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.organic <- AlgorithmLS(s0, data.cpy)
    f.res.organic <- fitness(toMatrix(res.organic), data.cpy)
    if(f.res.organic < f.organic) {
      f.organic <- f.res.organic
      best.organic <- res.organic
    }
    time.taken <- Sys.time() - start.time
    print(paste(toString(i), "organic: ", toString(time.taken)))
    
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$plastic)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.plastic <- AlgorithmLS(s0, data.cpy)
    f.res.plastic <- fitness(toMatrix(res.plastic), data.cpy)
    if(f.res.plastic < f.plastic) {
      f.plastic <- f.res.plastic
      best.plastic <- res.plastic
    }
    time.taken <- Sys.time() - start.time
    print(paste(toString(i), "plastic: ", toString(time.taken)))
    
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$paper)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.paper <- AlgorithmLS(s0, data.cpy)
    f.res.paper <- fitness(toMatrix(res.paper), data.cpy)
    if(f.res.paper < f.paper) {
      f.paper <- f.res.paper
      best.paper <- res.paper
    }
    time.taken <- Sys.time() - start.time
    print(paste(toString(i), "paper: ", toString(time.taken)))
  }
  
  print(paste("Solution for problem ", i))
  print("Organic:")
  print(best.organic)
  print(f.organic)
  print("Plastic:")
  print(best.plastic)
  print(f.plastic)
  print("Paper:")
  print(best.paper)
  print(f.paper)
  print("Cost:")
  print(f.organic+f.plastic+f.paper)
}
