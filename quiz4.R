# Finding the best hospital in a state
# best.R
best = function(state, outcome){
        ## Read outcome data
        data = read.csv("outcome-of-care-measures.csv", 
                        na.strings="Not Available", 
                        stringsAsFactors = FALSE)
        outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        ## Check that state and outcome are valid
        if(!state %in% data$State) stop("invalid state")
        else if(!outcome %in% names(outcomes)) stop("invalid outcome")
        
        data = data[data$State == state,]
        data = data[, c(2, outcomes[outcome])]
        names(data) = c("hospitals", outcome)
        data = data[order(data$hospitals), ]
        data = data[order(data[outcome]), ]
        return(data[1,1])
}

best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome


############################### quiz ###############################

best("SC", "heart attack")
# [1] "MUSC MEDICAL CENTER"
best("NY", "pneumonia")
# [1] "MAIMONIDES MEDICAL CENTER"
best("AK", "pneumonia")
# [1] "YUKON KUSKOKWIM DELTA REG HOSPITAL"

####################################################################

# Ranking hospitals by outcome in a state
# rankhospital.R
rankhospital = function(state, outcome, num = "best") {
        data = read.csv("outcome-of-care-measures.csv", 
                        na.strings = "Not Available", 
                        stringsAsFactors = FALSE)
        outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        
        if(!state %in% data$State) stop("invalid state")
        else if(!outcome %in% names(outcomes)) stop("invalid outcome")
        
        data = data[data$State == state, ]
        data = data[, c(2, outcomes[outcome])]
        names(data) = c("hospitals", outcome)
        data = data[order(data$hospitals), ]
        
        
        if(num == "best") num = 1
        else if(num == "worst"){
                num = nrow(data) - sum(is.na(data[outcome]))
        }
        data = data[order(data[outcome]), ]
        
        return(data[num, 1])
}


rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"

rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
# [1] NA

############################### quiz ###############################

rankhospital("NC", "heart attack", "worst")
# [1] "WAYNE MEMORIAL HOSPITAL"

rankhospital("WA", "heart attack", 7)
# [1] "YAKIMA VALLEY MEMORIAL HOSPITAL"

rankhospital("TX", "pneumonia", 10)
# [1] "SETON SMITHVILLE REGIONAL HOSPITAL"

rankhospital("NY", "heart attack", 7)
# [1] "BELLEVUE HOSPITAL CENTER"

####################################################################

# Ranking hospitals in all states
# rankall.R
rankall = function(outcome, num = "best") {
        data = read.csv("outcome-of-care-measures.csv", 
                        na.strings = "Not Available", 
                        stringsAsFactors = FALSE)
        outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        if(!outcome %in% names(outcomes)) stop("invalid outcome")
        
        data = data[, c(2, 7, outcomes[outcome])]
        names(data) = c("hospitals","states", outcome)
        data = data[order(data$hospitals), ]
        data = data[order(data[outcome]), ] 
        data = data[order(data$states), ]
        
        split.data = split(data, data$states)
        
        results = sapply(split.data, function(x){
                if(num == "best") num = 1
                if(num == "worst"){
                        num = nrow(x[outcome]) - sum(is.na(x[outcome]))
                }
                x[num, 1]
        })
        
        
        data.frame(hospital = results, 
                   state = names(results), 
                   row.names = names(results))
}

head(rankall("heart attack", 20), 10)
#                               hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

tail(rankall("pneumonia", "worst"), 3)
#                                      hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY

tail(rankall("heart failure"), 10)
#                                                             hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY



############################### quiz ###############################
r = rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
# [1] "CASTLE MEDICAL CENTER"

r = rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
# [1] "RENOWN SOUTH MEADOWS MEDICAL CENTER"

r = rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
# [1] "BERGEN REGIONAL MEDICAL CENTER"
