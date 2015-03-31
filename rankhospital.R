rankhospital <- function(states, outcome, rank)
{
  ##read outcome data
  options(stringsAsFactors = FALSE)
  outdat <- read.csv('outcome-of-care-measures.csv')
  if(states %in% outdat$State)
  {
    if(outcome == "heart attack")
    {
      outdat <- outdat[order(outdat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outdat$Hospital.Name),]
      col1 <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available" & State == states)
      filtdat <- subset(col1, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      row.names(filtdat) <- 1:nrow(filtdat)
      if(rank == "worst"){ranking <- nrow(filtdat)}
      else if(rank == "best"){ranking <- 1}
      else{ranking <- rank}
      filtdat <- filtdat$Hospital.Name
      filtdat[ranking]
    }
    else if(outcome == "pneumonia")
    {
      outdat <- outdat[order(outdat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
      col1 <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available" & State == states)
      filtdat <- subset(col1, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      row.names(filtdat) <- 1:nrow(filtdat)
      if(rank == "worst"){ranking <- nrow(filtdat)}
      else if(rank == "best"){ranking <- 1}
      else{ranking <- rank}
      filtdat <- filtdat$Hospital.Name
      filtdat[ranking]
    }
    else if(outcome == "heart failure")
    {
      outdat <- outdat[order(outdat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      col1 <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available" & State == states)
      filtdat <- subset(col1, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      row.names(filtdat) <- 1:nrow(filtdat)
      if(rank == "worst"){ranking <- nrow(filtdat)}
      else if(rank == "best"){ranking <- 1}
      else{ranking <- rank}
      filtdat <- filtdat$Hospital.Name
      filtdat[ranking]
    }
    else{stop("invalid outcome")}
  }
  else
  {
    stop("invalid state")
  }
}