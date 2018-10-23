best <- function(state, outcome) {
## 'best' returns a character vector with the name of the hospital that has the best (lowest) 30 day mortality rate for the specified outcome in the given state
        
        ## 'state' is a character vector of length 1 indicating the 2-character abbreviated name of the state from which to select the best hospital
        
        ## 'outcome' is a character vector indicating how the best hospital should be measured
        ## The valid outcomes are "heart attack", "heart failure" or "pneumonia"
        
        ## !!IMPORTANT!! -- Required packages will be installed; 
        ## this may take several minutes the first time this is run
        packages <- c("readr","dplyr")
        package.check <- lapply(packages, FUN = function(x) {
                if (!require(x, character.only = TRUE)) {
                        install.packages(x, dependencies = TRUE)
                        library(x, character.only = TRUE)
                }
        })
        
        ## !!IMPORTANT!! -- Remember to set the working directory to where the data is stored before running this code
        ## Read outcome data...
        outcomeData <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        validStates <- levels(outcomeData$State)
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!state %in% validStates) stop("invalid state")
        if(!outcome %in% validOutcomes) stop("invalid outcome")
        
        ## Create column name from the declared outcome; capitalise first letter and replace spaces with "."
        tmpVect1 <- strsplit(outcome, " ")[[1]]
        tmpVect2 <- paste0(toupper(substr(tmpVect1, 1, 1)), substr(tmpVect1, 2, nchar(tmpVect1)), collapse = ".")
        outColName <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.", tmpVect2)
        
        ## Clean the data...
        bestData <- outcomeData %>%
                rename(Rate = outColName) %>%
                filter(State == state, Rate != "Not Available") %>%
                select(c(Hospital.Name, Rate)) %>%
                mutate(Rate = as.numeric(as.character(.[["Rate"]]))) %>%
                arrange(Rate, Hospital.Name)
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## Extract the first hospital name
        paste0(bestData$Hospital.Name [1])
        
}