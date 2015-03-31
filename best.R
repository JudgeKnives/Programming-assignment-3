best <- function(states, outcome)
{
  ##read outcome data
  options(stringsAsFactors = FALSE)
  outdat <- read.csv('outcome-of-care-measures.csv')
  if(states %in% outdat$State)
  {
  if(outcome == "heart attack")
  {
    col1 <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available" & State == states)
    filtdat <- subset(col1, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    filtdat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character( filtdat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    besthospital <- subset(filtdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), select = Hospital.Name)
    rownames(besthospital) <- NULL
    best <- as.vector(besthospital)
    best <- unlist(best$Hospital.Name)
    return(best)
  }
  else if(outcome == "pneumonia")
  {
    col1 <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available" & State == states)
    filtdat <- subset(col1, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    filtdat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character( filtdat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    besthospital <- subset(filtdat, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), select = Hospital.Name)
    rownames(besthospital) <- NULL
    best <- as.vector(besthospital)
    best <- unlist(best$Hospital.Name)
    return(best)
  }
  else if(outcome == "heart failure")
  {
    col1 <- subset(outdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available" & State == states)
    filtdat <- subset(col1, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    filtdat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character( filtdat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    besthospital <- subset(filtdat, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), select = Hospital.Name)
    rownames(besthospital) <- NULL
    best <- as.vector(besthospital)
    best <- unlist(best$Hospital.Name)
    return(best)
  }
  else
  {
    stop("invalid outcome")
  }
  }
  else
  {
    stop("invalid state")
  }
  
}