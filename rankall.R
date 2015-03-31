rankall <- function(outcome, rank = "best")
{
  states <- subset(outdat, select = State)
  states <- states[!duplicated(states),]
  ##read outcome data
  options(stringsAsFactors = FALSE)
  outdat <- read.csv('outcome-of-care-measures.csv')
  
  if(outcome == "heart attack")
  {
    rankallM <- data.frame(hospital = character(), state = character())
    for(i in states)
    {
      solrow <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' & State == i, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      solrow <- solrow[order(as.numeric(solrow$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), solrow$Hospital.Name), ]
      row.names(solrow) <- 1:nrow(solrow)
      if(rank == "worst"){ranking <- nrow(solrow)}
      else if(rank == "best"){ranking <- 1}
      else{ranking <- rank}
      newrow <- solrow[ranking,]
      rankallM <- rbind(rankallM, c(newrow[,1], i))
      colnames(rankallM) <- c("hospital", "state")
    }
    rankallM <- rankallM[order(rankallM$state),]
    return(rankallM)
  }
  if(outcome == "heart failure")
  {
    rankallM <- data.frame(hospital = character(), state = character())
    for(i in states)
    {
      solrow <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' & State == i, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      solrow <- solrow[order(as.numeric(solrow$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), solrow$Hospital.Name), ]
      row.names(solrow) <- 1:nrow(solrow)
      if(rank == "worst"){ranking <- nrow(solrow)}
      else if(rank == "best"){ranking <- 1}
      else{ranking <- rank}
      newrow <- solrow[ranking,]
      rankallM <- rbind(rankallM, c(newrow[,1], i))
      colnames(rankallM) <- c("hospital", "state")
    }
    rankallM <- rankallM[order(rankallM$state),]
    return(rankallM)
  }
  if(outcome == "pneumonia")
  {
    rankallM <- data.frame(hospital = character(), state = character())
    for(i in states)
    {
      solrow <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' & State == i, select = c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      solrow <- solrow[order(as.numeric(solrow$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), solrow$Hospital.Name), ]
      row.names(solrow) <- 1:nrow(solrow)
      if(rank == "worst"){ranking <- nrow(solrow)}
      else if(rank == "best"){ranking <- 1}
      else{ranking <- rank}
      newrow <- solrow[ranking,]
      rankallM <- rbind(rankallM, c(newrow[,1], i))
      colnames(rankallM) <- c("hospital", "state")
    }
    rankallM <- rankallM[order(rankallM$state),]
    return(rankallM)
  }
  
}