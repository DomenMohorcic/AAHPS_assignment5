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
    df <- data.frame(start = as.integer(tmp[1]), end = as.integer(tmp[2]), length = as.double(tmp[3]), carry = as.double(tmp[5]))
    data.roads <- rbind(data.roads, df)
    if(as.integer(tmp[4]) == 0) {
      df <- data.frame(start = as.integer(tmp[2]), end = as.integer(tmp[1]), length = as.double(tmp[3]), carry = as.double(tmp[5]))
      data.roads <- rbind(data.roads, df)
    }
  }
  
  return(list("num.sites" = num.sites, "num.carry"= num.carry,
              "data.sites" = data.sites, "data.roads" = data.roads))
}

allPairsShortestPath <- function(roads) {
  C <- matrix(Inf, nrow = num.sites, ncol = num.sites)
  for(i in 1:nrow(roads)) {
    C[roads[i,"start"], roads[i, "end"]] <- roads[i,"length"]
  }
  D <- matrix(0, nrow = num.sites, ncol = num.sites)
  P <- matrix(0, nrow = num.sites, ncol = num.sites)
  
  for(i in 1:nrow(C)) {
    for(j in 1:ncol(C)) {
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

# construct a path from idx to next
#possible <- data.roads[which(data.roads$start == tmp[idx] & data.roads$carry >= carry),]
#neighbors <- unique(possible$end)
"closest <- order(D[tmp[idx],])
    closest.idx <- 0
    for(i in 2:length(closest)) {
      if(!closest[i] %in% tmp & data.garbage[closest[i]] > 0) {
        closest.idx <- closest[i]
        break
      }
    }
    if(data.garbage[closest.idx] + carry <= num.carry) { # closest can be collected
      carry <- carry + data.garbage[closest.idx]
      data.garbage[closest.idx] <- 0
      tmp <- c(tmp, closest.idx)
      idx <- idx + 1
    } else { # closest cannot be collected
      found <- FALSE
      for(j in i:length(closest)) {
        if(data.garbage[closest[j]] + carry <= num.carry) {
          carry <- carry + data.garbage[closest[j]]
          data.garbage[closest[j]] <- 0
          found <- TRUE
          
          tmp <- c(tmp, closest[j])
          idx <- idx + 1
          break
        }
      }
      
      if(!found) { # no full neighbor that can be collected
        # go back
        tmp <- c(tmp, rev(tmp)[-1]) # same way
        s0 <- c(s0, tmp[-1])
        tmp <- c(1)
        idx <- 1
      }
    }"


initialSolution <- function(data.garbage, D) {
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
      
      carry <- carry + data.garbage[closest.idx]
      data.garbage[closest.idx] <- 0
      tmp <- c(tmp, closest.idx)
      node <- closest.idx
      
      d <- D[node,]
      d[which((data.garbage+carry) > num.carry | data.garbage == 0)] <- Inf
      closest <- order(d)
      if(d[closest[1]] == Inf) {
        break
      }
    }
    s0 <- c(s0, tmp[-1], 1)
  }
  return(s0)
}

d <- readData(1)
num.sites <- d$num.sites
num.carry <- d$num.carry
data.sites <- d$data.sites
data.roads <- d$data.roads

a <- allPairsShortestPath(data.roads)
D <- a$D
P <- a$P
data.garbage <- data.sites$organic

initialSolution(data.garbage, D)
