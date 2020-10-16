find_communities <- function(modularity_matrix, guidance_matrix, 
                             numCommunities, lockNumCommunities, mu){
  
  numVertices <- nrow(modularity_matrix)
  
  #Initialize the simulated annealing parameters and result tracking
  maxSimAnnealingIterations <- 50
  initialTemp <- 10000
  tempDecreaseRate <- 0.985
  randGenerator <- NA
  
  #Randomly initialize the community structure and the corresponding objective function
  bestCommunitiesSoFar <- rep(NA,numVertices)
  for(i in 1:numVertices){
    bestCommunitiesSoFar[i] <- sample(1:numCommunities,1)
  }
  
  bestObjFnValueSoFar <- computeUserGuidedModularity(modularity_matrix, guidance_matrix,
                                                     bestCommunitiesSoFar, mu)
  
  for(simAnnealingIterationNum in 0:maxSimAnnealingIterations){
    #Start where we left off previously
    communities <- bestCommunitiesSoFar
    objFnValue <- bestObjFnValueSoFar
    
    #Run simulated annealing
    step <- 0
    temp <- initialTemp
    while(temp > 1E-8){
      #Choose a random vertex and change its community
      vertexToChange <- sample(1:numVertices,1)
      numCommunitiesToSample <- -1
      if(lockNumCommunities==TRUE){
        numCommunitiesToSample <- numCommunities
      } else{
        #Compute the number of communities
        currentNumCommunities <- max(communities)+1
        numCommunitiesToSample <- currentNumCommunities+1
      }
      newCommunity <- sample(1:numCommunitiesToSample,1)
      if(newCommunity==communities[vertexToChange]){
        newCommunity <- (newCommunity+1) %% numCommunitiesToSample
      }
      
      newCommunities <- communities
      newCommunities[vertexToChange] <- newCommunity
      
      #Compute the new objective function
      newObjFnValue <- computeUserGuidedModularity(modularity_matrix, guidance_matrix,
                                                   newCommunities, mu)
      
      #Test whether we should move to it
      if(newObjFnValue<objFnValue){
        #Move to the new community state
        if(lockNumCommunities==TRUE){
          communities <- newCommunities
        }
      }
        #Otherwise, move to it with some probability
        probOfMoving <- exp(-(newObjFnValue-objFnValue)/temp)
        
        if(runif(1,min=0,max=1)<probOfMoving){
          #Move to the new community state
          if(lockNumCommunities==TRUE){
            communities <- newCommunites
          }
          objFnValue <- newObjFnValue
        }
        
        #Increment to the next step count and decrease temperature
        temp <- temp*tempDecreaseRate
        step <- step+1
    }
    if(objFnValue<bestObjFnValueSoFar){
      bestObjFnValueSoFar <- objFnValue
      bestCommunitiesSoFar <- communities
    }
  }
}

computeModularityMatrix <- function(adjacency_matrix, vertexDegrees, m){
  twoM <- 2*m
  numRows <- nrow(adjacency_matrix)
  numCols <- ncol(adjacency_matrix)
  
  modularityMatrix <- matrix(0,nrow=numRows,ncol=numCols)
  
  for (i in 1:numRows){
    d_i <- vertexDegrees[i]
    for(j in 1:numCols){
      #Compute the variables of the equation
      A_ij <- adjacency_matrix[i,j]
      d_j <- vertexDegrees[j]
      
      #Compute the null model
      nullProbability <- (d_i*d_j)/twoM
      
      #Compute the modularity
      M_ij <- A_ij - nullProbability
      modularity_matrix[i,j] <- M_ij
    }
  }
  return(modularity_matrix)
}

computeUserGuidedModularity <- function(modularity_matrix, guidance_matrix, 
                                        communities, mu){
  sum <- 0
  numRows <- nrow(modularity_matrix)
  numCols <- ncol(modularity_matrix)
  
  for(i in 1:numRows){
    for(j in 1:numCols){
      if(i==j){
        next
      }
      #Only contribute to the value if delta(C_i, C_j) == 1
      if(communities[i] != communities[j]){
        next
      }
      M_ij <- modularity_matrix[i,j]
      deltaU_ij <- guidance_matrix[i,j]
      
      sum <- sum+M_ij+(mu*deltaU_ij) #We're adding instead of subtracting, and will negate later
    }
  }
  return(-1*sum) #Negate the sum, since we're adding instead of subtracting 
}

computeUserGuidanceMatrix <- function(adjacency_matrix, vertexDegrees, m, functionalLabels){
  numRows <- nrow(modularity_matrix)
  numCols <- ncol(modularity_matrix)
  
  guidance_matrix <- matrix(NA,nrow=numRows,ncol=numCols)
  
  for(i in 1:numRows){
    d_i <-vertexDegrees[i]
    for(j in 1:numCols){
      #Set alpha1 and alpha2 to be 1 for now
      alpha1 <- 1
      alpha2 <- 1
      
      #Compute the variables of the equation
      A_ij <- adjacency_matrix[i,j]
      d_j <- vertexDegrees[j]
      f_i <- functionalLabels[i]
      f_j <- functionalLabels[j]
      
      #If no guidance is available, the entry is zero
      if(is.na(f_i) | is.na(f_j)){
        guidance_matrix[i,j] <- 0
        next
      }
      u_ij <- alpha1*computeSmoothness(A_ij,f_i,f_j,d_i,d_j)
      ubar_ij <- -alpha2*computeSmoothness(A_ij,f_i,f_j,d_i,d_j)
      
      #Compute deltaU_ij = u_ij - ubar_ij
      deltaU_ij <- u_ij - ubar_ij
      
      guidance_matrix[i,j] <- deltaU_ij
    }
  }
  return(guidance_matrix)
}

computeUserGuidedModularityMatrix <- function(adjacency_matrix, vertexDegrees, m,
                                              mu, functionalLabels){
  twoM <- 2*m
  numRows <- nrow(adjacency_matrix)
  numCols <- ncol(adjacency_matrix)
  
  modularity_matrix <- matrix(0,nrow=numRows,ncol=numCols)
  
  for(i in 1:numRows){
    d_i <- vertexDegrees[i]
    
    for(j in 1:numCols){
      #Set alpha1 and alpha2 to be 1 for now
      alpha1 <- 1
      alpha2 <- 1
      
      #Compute the variables of the equation
      A_ij <- adjacency_matrix[i,j]
      d_j <- vertex_degrees[j]
      f_i <- functionalLabels[i]
      f_j <- functionalLabels[j]
      u_ij <- alpha1*computeSmoothness(A_ij,f_i,f_j,d_i,d_j)
      ubar_ij <- -alpha2*computeSmoothness(A_ij,f_i,f_j,d_i,d_j)
      
      if(is.na(u_ij) | is.na(ubar_ij)){
        print("u_ij or ubar_ij is NA")
      }
      
      #Compute the null model
      nullProbability <- (d_i*d_j)/twoM
      
      #Compute the guidance portion
      guidanceModifer <- u_ij-ubar_ij
      
      #Compute the modularity
      M_ij <- A_ij - (nullProbability-(u*guidanceModifer))
      modularity_matrix[i,j] <- M_ij
    }
  }
  return(modularity_matrix)
}

computeSmoothness <- function(A_ij, f_i, f_j, d_i, d_j){
  normalizedFi <- NA
  if(d_i==0){
    normalizedFi <- 0*f_i
  } else{
    normalizedFi <- f_i/sqrt(d_i)
  }
  
  normalizedFj <- NA
  if(d_j==0){
    normalizedFj <- 0*f_i
  } else{
    normalizedFj <- f_j/sqrt(d_j)
  }
  diffFiFj <- normalizedFi-normalizedFj
  return(A_ij*c(dist(t(diffFiFj)) ^ 2))
}

  