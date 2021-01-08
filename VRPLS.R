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
  roads.length <- c()
  roads.carry <- c()
  for(i in 1:(length(text)-(1+num.sites))) {
    tmp <- unlist(strsplit(text[1+num.sites+i], ","))
    start <- as.integer(tmp[1])
    end <- as.integer(tmp[2])
    length <- as.double(tmp[3])
    one.way <- as.integer(tmp[4])
    carry <- as.double(tmp[5])
    
    road.name.str <- toString(c(start, end))
    road.name.rev <- toString(c(end, start))
    
    if(!is.null(roads.length[[road.name.str]])) { # road between start and end already exists
      if(carry %in% roads.carry[[road.name.str]]) { # do we have same carrying capacity?
        idx.str <- which(roads.carry[[road.name.str]] == carry)
        if(length < roads.length[[road.name.str]][idx.str]) { # is this road shorter?
          roads.length[[road.name.str]][idx.str] <- length
          if(one.way == 0) {
            idx.rev <- which(roads.carry[[road.name.rev]] == carry)
            roads.length[[road.name.rev]][idx.rev] <- length
          }
        }
      } else { # not same carrying capacity, add as normal
        roads.length[[road.name.str]] <- c(roads.length[[road.name.str]], length)
        roads.carry[[road.name.str]] <- c(roads.carry[[road.name.str]], carry)
        if(one.way == 0) {
          roads.length[[road.name.rev]] <- c(roads.length[[road.name.rev]], length)
          roads.carry[[road.name.rev]] <- c(roads.carry[[road.name.rev]], carry)
        }
      }
    } else { # road does not exist
      roads.length[[road.name.str]] <- length
      roads.carry[[road.name.str]] <- carry
      if(one.way == 0) {
        roads.length[[road.name.rev]] <- length
        roads.carry[[road.name.rev]] <- carry
      }
    }
    
    
    df <- data.frame(start = as.integer(tmp[1]), end = as.integer(tmp[2]), length = as.double(tmp[3]), carry = as.double(tmp[5]))
    data.roads <- rbind(data.roads, df)
    if(as.integer(tmp[4]) == 0) {
      df <- data.frame(start = as.integer(tmp[2]), end = as.integer(tmp[1]), length = as.double(tmp[3]), carry = as.double(tmp[5]))
      data.roads <- rbind(data.roads, df)
    }
  }
  
  for(i in 1:length(roads.length)) { # sort roads
    idx <- order(roads.length[[i]])
    roads.length[[i]] <- roads.length[[i]][idx]
    roads.carry[[i]] <- roads.carry[[i]][idx]
    
    online.max <- roads.carry[[i]][1] # remove worse
    idx.surviving <- c(1)
    for(j in 2:length(idx)) {
      if(online.max < roads.carry[[i]][j]) {
        online.max <- roads.carry[[i]][j]
        idx.surviving <- c(idx.surviving, j)
      }
    }
    roads.length[[i]] <- roads.length[[i]][idx.surviving]
    roads.carry[[i]] <- roads.carry[[i]][idx.surviving]
  }
  
  return(list("num.sites" = num.sites, "num.carry"= num.carry,
              "data.sites" = data.sites, "data.roads" = data.roads,
              "roads.length" = roads.length, "roads.carry" = roads.carry))
}


# calculate fitness function !!WRONG!!
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
      possible <-  data.roads[which(data.roads$start == route[j-1] & data.roads$end == route[j]),]
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


# fitness with memoization
cost <- function(x, data.garbage, memo) {
  name <- toString(x)
  if(!is.null(memo[[name]])) {
    return(memo[[name]])
  }
  
  cost.all <- 0
  start <- 2
  
  idx.1 <- which(x == 1)
  for(i in 2:length(idx.1)) {
    if(!is.null(memo[[toString(x[1:idx.1[i]])]])) {
      from.memo <- memo[[toString(x[1:idx.1[i]])]]
      cost.all <- from.memo$cost
      data.garbage <- from.memo$garbage
      start <- idx.1[i]+1
      break
    }
  }
  
  carry <- 0 # how much current truck is carrying
  len <- 0 # current trip length
  time <- 0 # current trip time
  cost <- 10
  
  for(i in start:length(x)) {
    # which road to take
    road.name <- toString(c(x[i-1], x[i]))
    possible.road <- roads.length[[road.name]]
    possible.carry <- carry <= roads.carry[[road.name]]
    idx <- which.max(possible.carry)
    if(possible.carry[idx]) {
      len <- len + possible.road[idx]
    } else {
      # invalid road
    }
    
    # garbage to collect
    if(data.garbage[x[i]] > 0) {
      carry.next <- carry + data.garbage[x[i]]
      if(carry.next <= num.carry) { # pick up trash
        carry <- carry.next
        data.garbage[x[i]] <- 0
        time <- time + 0.2
      }
    }
    
    # at end
    if(x[i] == 1) {
      time <- time + 0.5 + len/50
      if(time > 8) {
        time <- c(8, time-8)
      } else {
        time <- c(time, 0)
      }
      cost <- cost + len*0.1 + sum(time*c(10, 20))
      cost.all <- cost.all + cost
      
      sofar.name <- toString(x[1:i])
      to.memo <- list("cost" = cost.all, "garbage" = data.garbage)
      memo[[sofar.name]] <- to.memo
      
      carry <- 0
      len <- 0
      time <- 0
      cost <- 10
    }
  }
  return(list("cost" = cost.all, "memo" = memo))
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
  fc <- fitness(toMatrix(sm), data.cpy)
  
  while(TRUE) {
    n <- neighborhood(s0)
    f <- c()
    
    # calculate all fitness values for neighborhood
    for (i in 1:length(n)) {
      f[i] <- fitness(toMatrix(n[[i]]), data.cpy)
    }
    
    # find best (min) fitness function for neighborhood
    best.idx <- which.min(f)
    
    if (f[best.idx] < fc) {
      sm <- n[[best.idx]]
      s0 <- n[[best.idx]]
      fc <- f[best.idx]
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

rd <- read_data(8)
num.sites <- rd$num.sites
num.carry <- rd$num.carry
data.sites <- rd$data.sites
data.roads <- rd$data.roads
roads.length <- rd$roads.length
roads.carry <- rd$roads.carry

length(roads.carry)
nrow(data.roads)

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
    res.organic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
    f.res.organic <- fitness(toMatrix(res.organic), data.cpy)
    if(f.res.organic < f.organic) {
      f.organic <- f.res.organic
      best.organic <- res.organic
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$plastic)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.plastic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
    f.res.plastic <- fitness(toMatrix(res.plastic), data.cpy)
    if(f.res.plastic < f.plastic) {
      f.plastic <- f.res.plastic
      best.plastic <- res.plastic
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$paper)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.paper <- AlgorithmSA(s0, 0.95, 100, data.cpy)
    f.res.paper <- fitness(toMatrix(res.paper), data.cpy)
    if(f.res.paper < f.paper) {
      f.paper <- f.res.paper
      best.paper <- res.paper
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
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


# solving organic
for(i in 1:3) {
  rd <- read_data(i)
  num.sites <- rd$num.sites
  num.carry <- rd$num.carry
  data.sites <- rd$data.sites
  data.roads <- rd$data.roads
  penalty.length <- sum(data.roads$length)*100
  penalty.carry <- 2
  penalty.garbage <- 2
  
  best.organic <- c()
  f.organic <- Inf
  for(j in 1:10) {
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$organic)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.organic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
    f.res.organic <- fitness(toMatrix(res.organic), data.cpy)
    if(f.res.organic < f.organic) {
      f.organic <- f.res.organic
      best.organic <- res.organic
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
  }
  
  print(paste("Solution for problem ", i))
  print("Organic:")
  print(best.organic)
  print(f.organic)
}

# solving plastic
for(i in 1:3) {
  rd <- read_data(i)
  num.sites <- rd$num.sites
  num.carry <- rd$num.carry
  data.sites <- rd$data.sites
  data.roads <- rd$data.roads
  penalty.length <- sum(data.roads$length)*100
  penalty.carry <- 2
  penalty.garbage <- 2
  
  best.plasitc <- c()
  f.plastic <- Inf
  for(j in 1:10) {
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$plastic)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.plastic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
    f.res.plastic <- fitness(toMatrix(res.plastic), data.cpy)
    if(f.res.plastic < f.plastic) {
      f.plastic <- f.res.plastic
      best.plastic <- res.plastic
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
  }
  
  print(paste("Solution for problem ", i))
  print("Plastic:")
  print(best.plastic)
  print(f.plastic)
}

# solving paper
for(i in 1:3) {
  rd <- read_data(i)
  num.sites <- rd$num.sites
  num.carry <- rd$num.carry
  data.sites <- rd$data.sites
  data.roads <- rd$data.roads
  penalty.length <- sum(data.roads$length)*100
  penalty.carry <- 2
  penalty.garbage <- 2
  
  best.paper <- c()
  f.paper <- Inf
  for(j in 1:10) {
    start.time <- Sys.time()
    data.cpy <- data.frame(data.sites$id, data.sites$paper)
    colnames(data.cpy) <- c("id", "garbage")
    s0 <- c(1, sample(2:num.sites), 1)
    res.paper <- AlgorithmSA(s0, 0.95, 100, data.cpy)
    f.res.paper <- fitness(toMatrix(res.paper), data.cpy)
    if(f.res.paper < f.paper) {
      f.paper <- f.res.paper
      best.paper <- res.paper
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
  }
  
  print(paste("Solution for problem ", i))
  print("Paper:")
  print(best.paper)
  print(f.paper)
}


# PARALLEL


library(parallel)
test <- function(data.cpy) {
  s0 <- c(1, sample(2:num.sites), 1)
  res <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  res
}
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
  
  split = detectCores()
  cl = makeCluster(split)
  data.cpy <- data.frame(data.sites$id, data.sites$organic)
  colnames(data.cpy) <- c("id", "garbage")
  result <- parLapply(cl = cl, X = rep(1, split), fun = test, data.cpy = data.cpy)
  stopCluster(cl)
    
  start.time <- Sys.time()
  data.cpy <- data.frame(data.sites$id, data.sites$organic)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.organic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  f.res.organic <- fitness(toMatrix(res.organic), data.cpy)
  if(f.res.organic < f.organic) {
    f.organic <- f.res.organic
    best.organic <- res.organic
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  start.time <- Sys.time()
  data.cpy <- data.frame(data.sites$id, data.sites$plastic)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.plastic <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  f.res.plastic <- fitness(toMatrix(res.plastic), data.cpy)
  if(f.res.plastic < f.plastic) {
    f.plastic <- f.res.plastic
    best.plastic <- res.plastic
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  start.time <- Sys.time()
  data.cpy <- data.frame(data.sites$id, data.sites$paper)
  colnames(data.cpy) <- c("id", "garbage")
  s0 <- c(1, sample(2:num.sites), 1)
  res.paper <- AlgorithmSA(s0, 0.95, 100, data.cpy)
  f.res.paper <- fitness(toMatrix(res.paper), data.cpy)
  if(f.res.paper < f.paper) {
    f.paper <- f.res.paper
    best.paper <- res.paper
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  
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




rd <- read_data(8)
num.sites <- rd$num.sites
num.carry <- rd$num.carry
data.sites <- rd$data.sites
data.roads <- rd$data.roads

penalty.length <- sum(data.roads$length)*100
penalty.carry <- 2
penalty.garbage <- 2
library(rbenchmark)
benchmark("select" = {
    route <- sample.int(num.sites, 2)
    subroads <- data.roads[data.roads$start == route[1],]
    possible <- subroads[subroads$end == route[2],]
  },
  "which" = {
    route <- sample.int(num.sites, 2)
    possible <- data.roads[which(data.roads$start == route[1] & data.roads$end == route[2]),]
  },
  "subset" = {
    route <- sample.int(num.sites, 2)
    possible <- subset(data.roads, start == route[1] & end == route[2])
  },
  "dict" = {
    route.name <- toString(sample.int(num.sites, 2))
    possible <- roads.length[[route.name]]
  },
  replications = 100000,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self"))
