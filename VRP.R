readData <- function(name) {
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
        idx <- which(roads.carry[[road.name.str]] == carry)
        if(length < roads.length[[road.name.str]][idx]) { # is this road shorter?
          roads.length[[road.name.str]][idx] <- length
        }
      } else { # not same carrying capacity, add as normal
        roads.length[[road.name.str]] <- c(roads.length[[road.name.str]], length)
        roads.carry[[road.name.str]] <- c(roads.carry[[road.name.str]], carry)
      }
    } else { # road does not exist
      roads.length[[road.name.str]] <- length
      roads.carry[[road.name.str]] <- carry
    }
    
    if(one.way == 0) {
      if(!is.null(roads.length[[road.name.rev]])) { # road between start and end already exists
        if(carry %in% roads.carry[[road.name.rev]]) { # do we have same carrying capacity?
          idx <- which(roads.carry[[road.name.rev]] == carry)
          if(length < roads.length[[road.name.rev]][idx]) { # is this road shorter?
            roads.length[[road.name.rev]][idx] <- length
          }
        } else { # not same carrying capacity, add as normal
          roads.length[[road.name.rev]] <- c(roads.length[[road.name.rev]], length)
          roads.carry[[road.name.rev]] <- c(roads.carry[[road.name.rev]], carry)
        }
      } else { # road does not exist
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
    if(2 <= length(idx)) {
      for(j in 2:length(idx)) {
        if(online.max < roads.carry[[i]][j]) {
          online.max <- roads.carry[[i]][j]
          idx.surviving <- c(idx.surviving, j)
        }
      }
    }
    roads.length[[i]] <- roads.length[[i]][idx.surviving]
    roads.carry[[i]] <- roads.carry[[i]][idx.surviving]
  }
  
  return(list("num.sites" = num.sites, "num.carry"= num.carry,
              "data.sites" = data.sites, "data.roads" = data.roads,
              "roads.length" = roads.length, "roads.carry" = roads.carry))
}

allPairsShortestPath <- function(roads, w) {
  C <- matrix(Inf, nrow = num.sites, ncol = num.sites)
  for(i in 1:nrow(roads)) {
    # check if we can carry our weigth on this road!
    if (roads[i, "carry"] >= w) {
      # check if this one is better than the previous!
      if (C[roads[i,"start"], roads[i, "end"]] > roads[i,"length"]) {
        C[roads[i,"start"], roads[i, "end"]] <- roads[i,"length"]
      }
    }
  }
  D <- matrix(0, nrow = num.sites, ncol = num.sites)
  P <- matrix(0, nrow = num.sites, ncol = num.sites)
  
  for(i in 1:nrow(C)) {
    for(j in 1:nrow(C)) {
      D[i, j] <- C[i, j];
      P[i, j] <- -1;
    }
    D[i, i] <- 0
  }
  
  for(k in 1:nrow(C)) {
    for(i in 1:nrow(C)) {
      for(j in 1:nrow(C)) {
        if(D[i, k] + D[k, j] < D[i, j]) {
          D[i, j] = D[i, k] + D[k, j];
          P[i, j] = k;
        }
      }
    }
  }
  return(list("D" = D, "P" = P))
}

costVector <- function(x, data.garbage) {
  cost.all <- 0
  carry <- 0 # how much current truck is carrying
  len <- 0 # current trip length
  time <- 0 # current trip time
  cost.trip <- 10
  
  totalTripLen <- 0
  
  for(i in 2:length(x)) {
    # which road to take
    road.name <- toString(c(x[i-1], x[i]))
    possible.road <- roads.length[[road.name]]
    possible.carry <- carry <= roads.carry[[road.name]]
    if(length(possible.road) > 0) {
      idx <- which.max(possible.carry)
      if(possible.carry[idx]) {
        len <- len + possible.road[idx]
        totalTripLen <- totalTripLen + possible.road[idx]
      } else {
        # invalid road
        #len <- len + penalty.length
        return(road.name)
      }
    } else {
      # invalid road
      #len <- len + penalty.length
      return(road.name)
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
        cost.trip <- cost.trip + 80 + (time-8)*20
      } else {
        cost.trip <- cost.trip + time*10
      }
      cost.trip <- cost.trip + len*0.1
      cost.all <- cost.all + cost.trip
      
      carry <- 0
      len <- 0
      time <- 0
      cost.trip <- 10
    }
  }
  
  if(sum(data.garbage) > 0) {
    # not everything collected
    print(data.garbage)
    return(Inf)
    cost.all <- cost.all + penalty.garbage*sum(data.garbage)
  }
  
  #print("totalTripLen")
  #print(totalTripLen)
  return(cost.all)
}

costPerm <- function(perm, data.garbage) {
  vector.form <- c(1)
  cost.all <- 0
  carry <- 0
  len <- 0
  time <- 0
  cost.trip <- 10
  
  totalTripLen <- 0
  
  prev <- 1
  for(i in 1:length(perm)) {
    node <- perm[i] # kje pobiramo smeti
    
    idx <- which.max(roads.unique >= carry) # indeks za matriko P in D
    path <- findPath(prev, node, idx) # iscemo najkrajso pot
    if(length(path) > 0) {
      for(j in 1:length(path)) { # simuliraj pot
        if(carry + data.garbage[path[j]] <= num.carry & data.garbage[path[j]] > 0) {
          # na poti smo nekaj pobrali pa ne bi smeli
          # memo[[name]] <<- Inf
          return(list("cost" = Inf, "path" = NULL))
        }
        
        if(path[j] == 1) {
          len <- len + all.D[[idx]][prev, 1]
          totalTripLen <- totalTripLen + all.D[[idx]][prev, 1]
          time <- time + 0.5 + len/50
          if(time > 8) {
            cost.trip <- cost.trip + 80 + (time-8)*20
          } else {
            cost.trip <- cost.trip + time*10
          }
          
          cost.trip <- cost.trip + len*0.1
          cost.all <- cost.all + cost.trip
          
          vector.form <- c(vector.form, path[1:j])
          #print(vector.form)
          
          carry <- 0
          len <- 0
          time <- 0
          cost.trip <- 10
          prev <- 1
        }
      }
    }
    
    if(carry + data.garbage[node] >= num.carry) { # stop at depot in the way (forced)
      path <- findPath(prev, 1, idx)
      if(length(path) > 0) {
        for(j in 1:length(path)) { # simuliraj pot
          if(carry + data.garbage[path[j]] <= num.carry & data.garbage[path[j]] > 0) {
            # na poti smo nekaj pobrali pa ne bi smeli
            # memo[[name]] <<- Inf
            return(list("cost" = Inf, "path" = NULL))
          }
        }
      }
      
      len <- len + all.D[[idx]][prev, 1]
      totalTripLen <- totalTripLen + all.D[[idx]][prev, 1]
      time <- time + 0.5 + len/50
      
      if(time > 8) {
        cost.trip <- cost.trip + 80 + (time-8)*20
      } else {
        cost.trip <- cost.trip + time*10
      }
      
      cost.trip <- cost.trip + len*0.1
      cost.all <- cost.all + cost.trip
      
      vector.form <- c(vector.form, path, 1)
      #print(vector.form)
      
      carry <- 0
      len <- 0
      time <- 0
      cost.trip <- 10
      prev <- 1
    }
    
    idx <- which.max(roads.unique >= carry)
    path <- findPath(prev, node, idx)
    if(length(path) > 0) {
      for(j in 1:length(path)) { # simuliraj pot
        if(carry + data.garbage[path[j]] <= num.carry & data.garbage[path[j]] > 0) {
          # na poti smo nekaj pobrali pa ne bi smeli
          # memo[[name]] <<- Inf
          return(list("cost" = Inf, "path" = NULL))
        }
      }
    }
    vector.form <- c(vector.form, path, node)
    #print(vector.form)
    
    idx <- which.max(roads.unique >= carry)
    len <- len + all.D[[idx]][prev, node]
    totalTripLen <- totalTripLen + all.D[[idx]][prev, node]
    carry <- carry + data.garbage[node]
    data.garbage[node] <- 0
    time <- time + 0.2
    prev <- node
  }
  
  idx <- which.max(roads.unique >= carry)
  path <- findPath(prev, 1, idx)
  vector.form <- c(vector.form, path, 1)
  #print("Perm solution")
  #print(vector.form)
  
  len <- len + all.D[[idx]][prev, 1]
  totalTripLen <- totalTripLen + all.D[[idx]][prev, 1]
  time <- time + 0.5 + len/50
  
  if(time > 8) {
    cost.trip <- cost.trip + 80 + (time-8)*20
  } else {
    cost.trip <- cost.trip + time*10
  }
  
  cost.trip <- cost.trip + len*0.1
  cost.all <- cost.all + cost.trip
  
  #print("totalTripLen")
  #print(totalTripLen)
  
  # memo[[name]] <<- cost.all
  return(list("cost" = cost.all, "path" = vector.form))
}

findPath <- function(start, end, idx) {
  tmp <- c()
  if(all.P[[idx]][start, end] == -1) {
    return(c())
  } else {
    path.1 <- findPath(start, all.P[[idx]][start, end], idx)
    path.1 <- c(path.1, all.P[[idx]][start, end])
    path.2 <- findPath(all.P[[idx]][start, end], end, idx)
    path <- c(path.1, path.2)
    return(path)
  }
}

greedySolution <- function(data.garbage) {
  s0 <- c(1)
  perm.form <- c()
  
  while(sum(data.garbage) > 0) {
    node <- 1
    carry <- 0
    tmp <- c(node)
    idx <- which.max(roads.unique >= carry)
    d <- all.D[[idx]][node,]
    d[which((data.garbage+carry) > num.carry | data.garbage == 0)] <- Inf
    closest <- order(d)
    while(TRUE) {
      for(i in 1:length(closest)) {
        if(closest[i] != node) {
          closest.idx <- closest[i]
          break
        }
      }
      
      path <- findPath(node, closest.idx, idx)
      if(1 %in% path) {
        break
      }
      
      perm.form <- c(perm.form, closest.idx)
      carry <- carry + data.garbage[closest.idx]
      data.garbage[closest.idx] <- 0
      tmp <- c(tmp, path, closest.idx)
      node <- closest.idx
      
      idx <- which.max(roads.unique >= carry)
      d <- all.D[[idx]][node,]
      d[which((data.garbage+carry) > num.carry | data.garbage == 0)] <- Inf
      closest <- order(d)
      if(d[closest[1]] == Inf) {
        break
      }
    }
    
    path <- findPath(node, 1, idx)
    
    s0 <- c(s0, tmp[-1], path, 1)
  }
  return(list("s0" = s0, "perm" = perm.form))
}

toMatrix <- function(x) {
  mat <- c()
  idx <- 1
  cut <- 2
  for (i in 2:length(x)) {
    if (x[i] == 1) {
      mat[[idx]] <- c(1, x[cut:i])
      idx <- idx + 1
      cut <- i + 1
    }
  }
  return(mat)
}

toPerm <- function(x) {
  return(x[which(x != 1)])
}

neighborhoodPerm <- function(perm) {
  n <- c()
  idx <- 1
  
  # tole je dejansko ful dobr neighborhood!
  # sam se pogledat treba, kere ven meceva (neveljavne)
  # memoizacijo se mogoce splaca delat ... sploh za Inf vrednosti
  # da se jih ne gre skos racunat ...
  "for (i in 1:(length(perm)-1)) {
    for (j in (i + 1):length(perm)) {
      n.v <- perm
      tmp <- n.v[i]
      n.v[i] <- n.v[j]
      n.v[j] <- tmp
      n[[idx]] <- n.v
      idx <- idx + 1
    }
  }"
  
  for (i in 1:(length(perm)-1)) {
    for (j in (i + 1):length(perm)) {
      n.v <- perm
      tmp <- n.v[i]
      n.v[i] <- n.v[j]
      n.v[j] <- tmp
      n[[idx]] <- n.v
      idx <- idx + 1
    }
  }
  
  "for (i in 2:(length(perm)-1)) {
    n.v <- perm
    tmp <- n.v[i]
    n.v[i] <- n.v[i-1]
    n.v[i-1] <- tmp
    n[[idx]] <- n.v
    idx <- idx + 1
  }
  
  for (i in 3:(length(perm)-1)) {
    n.v <- perm
    tmp <- n.v[i]
    n.v[i] <- n.v[i-2]
    n.v[i-2] <- tmp
    n[[idx]] <- n.v
    idx <- idx + 1
  }"
  
  return(n)
}

AlgorithmLS <- function(s0, data.garbage, percent) {
  sm <- s0
  fc <- costPerm(sm, data.garbage)$cost
  
  while(TRUE) {
    n <- neighborhoodPerm(s0)
    
    ####  probabilistic LS  ##############
    rand <- sample(1:length(n))
    n <- n[rand[1:round(length(n) * percent)]]
    ######################################
    
    f <- c()
    
    # calculate all fitness values for neighborhood
    for (i in 1:length(n)) {
      f[i] <- costPerm(n[[i]], data.garbage)$cost
    }

    print(length(f))
    
    # find best (min) fitness function for neighborhood
    best.idx <- which.min(f)
    
    if (f[best.idx] < fc) {
      sm <- n[[best.idx]]
      s0 <- n[[best.idx]]
      fc <- f[best.idx]
      print(fc)
    } else {
      break
    }
  }
  return(sm)
}

AlgorithmSA <- function(s0, lambda, t, data.garbage, percent) {
  s <- s0
  sm <- s0
  
  while (t >= 1) {
    n <- neighborhoodPerm(s)
    rand <- sample(1:length(n), 1)
    sc <- n[[rand]]
    
    fm <- costPerm(sm, data.garbage)$cost
    fc <- costPerm(sc, data.garbage)$cost
    f <- costPerm(s, data.garbage)$cost
    
    if (fc < fm) {
      sm <- sc
    }
    
    if (fc < f) {
      s <- sc
    } else {
      prob <- exp(-(fc - f) / t)
      #print(prob)
      t <- t * lambda
      print(t)
      if(runif(1, 0, 1) < prob) {
        #print("Chosen worse")
        s <- sc
      }
    }
  }
  
  # end with LS
  sm <- AlgorithmLS(sm, data.garbage, percent)
  
  return(sm)
}

writeSolution <- function(name, folder, sol.organic, sol.plastic, sol.paper, cost) {
  mtx.organic <- toMatrix(sol.organic)
  mtx.plastic <- toMatrix(sol.plastic)
  mtx.paper <- toMatrix(sol.paper)
  
  # write path
  file.create(paste(folder, toString(name), ".txt", sep=""))
  conn = file(paste(folder, toString(name), ".txt", sep=""), open="w")
  
  for(i in 1:length(mtx.organic)) {
    write(paste(c(1, mtx.organic[[i]]), collapse=','), conn)
  }
  for(i in 1:length(mtx.plastic)) {
    write(paste(c(2, mtx.plastic[[i]]), collapse=','), conn)
  }
  for(i in 1:length(mtx.paper)) {
    write(paste(c(3, mtx.paper[[i]]), collapse=','), conn)
  }
  close(conn)
  
  # write cost
  file.create(paste(folder, toString(name), ".cost.txt", sep=""))
  conn = file(paste(folder, toString(name), ".cost.txt", sep=""), open="w")
  write(paste(cost, collapse=','), conn)
  close(conn)
}

num.file <- 2
num.percent <- 1

d <- readData(num.file)
num.sites <- d$num.sites
num.carry <- d$num.carry
data.sites <- d$data.sites
data.roads <- d$data.roads
roads.length <- d$roads.length
roads.carry <- d$roads.carry
roads.unique <- unique(data.roads$carry)
roads.unique <- roads.unique[order(roads.unique)]
length(roads.unique)

penalty.length <- sum(data.roads$length)*100
penalty.carry <- 2
penalty.garbage <- 2

all.D <- c()
all.P <- c()
for (i in 1:length(roads.unique)) {
  a <- allPairsShortestPath(data.roads, roads.unique[i])
  all.D[[i]] <- a$D
  all.P[[i]] <- a$P
  print(i/length(roads.unique))
}

# writeSolution(4, x.organic, x.plastic, x.paper)

###############################################################################
#                                                                             #
#                             SA ALGORITHMS                                   #
#                                                                             #
###############################################################################

num.file <- 7
num.percent <- 0.02

data.garbage <- data.sites$organic
x <- greedySolution(data.garbage)
found <- AlgorithmSA(x$perm, 0.95, 20000, data.garbage, num.percent)
x.organic <- costPerm(found, data.garbage)$cost
path.organic <- costPerm(found, data.garbage)$path

data.garbage <- data.sites$plastic
x <- greedySolution(data.garbage)
found <- AlgorithmSA(x$perm, 0.95, 20000, data.garbage, num.percent)
x.plastic <- costPerm(found, data.garbage)$cost
path.plastic <- costPerm(found, data.garbage)$path

data.garbage <- data.sites$paper
x <- greedySolution(data.garbage)
found <- AlgorithmSA(x$perm, 0.95, 20000, data.garbage, num.percent)
x.paper <- costPerm(found, data.garbage)$cost
path.paper <- costPerm(found, data.garbage)$path

print("Final cost")
final <- x.organic + x.plastic + x.paper
print(final)

writeSolution(num.file, "solutionsLS/Solution", 
              path.organic, path.plastic, path.paper, final)

print(path.organic)
print(path.plastic)
print(path.paper)


###############################################################################
#                                                                             #
#                              GREEDY SOLUTION                                #
#                                                                             #
###############################################################################

num.file <- 8

data.garbage <- data.sites$organic
x <- greedySolution(data.garbage)
x.organic <- costVector(x$s0, data.garbage)
path.organic <- x$s0

data.garbage <- data.sites$plastic
x <- greedySolution(data.garbage)
x.plastic <- costVector(x$s0, data.garbage)
path.plastic <- x$s0

data.garbage <- data.sites$paper
x <- greedySolution(data.garbage)
x.paper <- costVector(x$s0, data.garbage)
path.paper <- x$s0

print("Final cost")
final <- x.organic + x.plastic + x.paper
print(final)

writeSolution(num.file, "solutionsGreedy/Solution", 
              path.organic, path.plastic, path.paper, final)

print(path.organic)
print(path.plastic)
print(path.paper)
