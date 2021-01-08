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
    if(2 < length(idx)) {
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

allPairsShortestPath <- function(roads) {
  C <- matrix(Inf, nrow = num.sites, ncol = num.sites)
  for(i in 1:nrow(roads)) {
    C[roads[i,"start"], roads[i, "end"]] <- roads[i,"length"]
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

cost <- function(x, data.garbage) {
  name <- toString(x)
  if(!is.null(memo[[name]])) {
    return(memo[[name]]$cost)
  }
  
  cost.all <- 0
  carry <- 0 # how much current truck is carrying
  len <- 0 # current trip length
  time <- 0 # current trip time
  cost.trip <- 10
  
  for(i in 2:length(x)) {
    # which road to take
    road.name <- toString(c(x[i-1], x[i]))
    possible.road <- roads.length[[road.name]]
    possible.carry <- carry <= roads.carry[[road.name]]
    if(length(possible.road) > 0) {
      idx <- which.max(possible.carry)
      if(possible.carry[idx]) {
        len <- len + possible.road[idx]
      } else {
        # invalid road
        len <- len + penalty.length
      }
    } else {
      # invalid road
      len <- len + penalty.length
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
      
      #sofar.name <- toString(x[1:i])
      #to.memo <- list("cost" = cost.all, "garbage" = data.garbage)
      #memo[[sofar.name]] <<- to.memo
      
      carry <- 0
      len <- 0
      time <- 0
      cost.trip <- 10
    }
  }
  
  if(sum(data.garbage) > 0) {
    # not everything collected
    return(Inf)
    cost.all <- cost.all + penalty.garbage*sum(data.garbage)
  }
  to.memo <- list("cost" = cost.all, "garbage" = data.garbage)
  memo[[sofar.name]] <<- to.memo
  
  return(cost.all)
}

findPath <- function(start, end, P) {
  tmp <- c()
  if(P[start, end] == -1) {
    return(c())
  } else {
    path.1 <- findPath(start, P[start, end], P)
    path.1 <- c(path.1, P[start, end])
    path.2 <- findPath(P[start, end], end, P)
    path <- c(path.1, path.2)
    return(path)
  }
}

initialSolution <- function(data.garbage, D, P) {
  s0 <- c(1)
  
  while(length(unique(s0)) < num.sites) {
    node <- 1
    carry <- 0
    tmp <- c(node)
    d <- D[node,]
    d[which((data.garbage+carry) > num.carry | data.garbage == 0)] <- Inf
    closest <- order(d)
    while(TRUE) {
      for(i in 1:length(closest)) {
        if(closest[i] != node) {
          closest.idx <- closest[i]
          break
        }
      }
      
      path <- findPath(node, closest.idx, P)
      
      carry <- carry + data.garbage[closest.idx]
      data.garbage[closest.idx] <- 0
      tmp <- c(tmp, path, closest.idx)
      node <- closest.idx
      
      d <- D[node,]
      d[which((data.garbage+carry) > num.carry | data.garbage == 0)] <- Inf
      closest <- order(d)
      if(d[closest[1]] == Inf) {
        break
      }
    }
    
    path <- findPath(node, 1, P)
    
    s0 <- c(s0, tmp[-1], path, 1)
  }
  return(s0)
}

d <- readData(1)
num.sites <- d$num.sites
num.carry <- d$num.carry
data.sites <- d$data.sites
data.roads <- d$data.roads
roads.length <- d$roads.length
roads.carry <- d$roads.carry

a <- allPairsShortestPath(data.roads)
D <- a$D
P <- a$P

data.garbage <- data.sites$organic
x <- initialSolution(data.garbage, D, P)
cost(x, data.garbage)

data.garbage <- data.sites$plastic
x <- initialSolution(data.garbage, D, P)
cost(x, data.garbage)

data.garbage <- data.sites$paper
x <- initialSolution(data.garbage, D, P)
cost(x, data.garbage)
