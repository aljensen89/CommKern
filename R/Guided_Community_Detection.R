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

computeUserGuidedModularityMatrix <- function(modularity_matrix, guidance_matrix, 
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

  