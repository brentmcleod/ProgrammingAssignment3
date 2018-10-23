rankall <- function(outcome, num = "best") {
        ## 'rankall' returns a dataframe with the name of the hospital in each state ranked at the declared num
        
        ## 'outcome' is a character vector indicating how the best hospital should be measured
        ## The valid outcomes are "heart attack", "heart failure" or "pneumonia"
        
        ## 'num' is the rank of the hospitals names that you would like returned in the dataframe
        ## The user can enter a number or the words 'best' or 'worst' to return the first or last hospital by rank
        
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
        
        ## Check that outcome and num are valid
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% validOutcomes) stop("invalid outcome")
        if(!(num == "best" | num == "worst" | is.numeric(num))) stop("invalid rank")
        
        ## Create column name from the declared outcome; capitalise first letter and replace spaces with "."
        tmpVect1 <- strsplit(outcome, " ")[[1]]
        tmpVect2 <- paste0(toupper(substr(tmpVect1, 1, 1)), substr(tmpVect1, 2, nchar(tmpVect1)), collapse = ".")
        outColName <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.", tmpVect2)
        
        ## Get list of valid states
        validStates <- levels(outcomeData$State)
        
        ## Convert "best" to number 1
        if(num == "best") num <- 1
        ## The "worst" hospital is the last hospital in each state
        
        ## Clean the data...
        ## And then return hospital name in declared state with the given rank
        rankedData <- outcomeData %>%
                rename(Rate = outColName) %>%
                filter(Rate != "Not Available") %>%
                select(c(Hospital.Name, State, Rate)) %>%
                mutate(Rate = as.numeric(as.character(.[["Rate"]]))) %>%
                arrange(State, Rate, Hospital.Name) %>%
                split(.[["State"]]) %>%
                sapply(function(x){as.character(x[if(num == "worst"){nrow(x)}else{num},1])}) %>% 
                data.frame("hospital" = ., "state" = validStates)
        
}