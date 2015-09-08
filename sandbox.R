# Functions for the initial method. Turns out to be too slow
# IGNORE

library(multicool)

person_sequence <- function(props, pi, qseq, threshold=NULL) {
    
    
    answers <- props[pi, 2:length(props)]
    sanswers <- sapply(1:length(answers), function(x) answers[qseq[x]])
    #print(sanswers)
    if(is.null(threshold)) {
        threshold <- length(qseq)
    }
    for(qi in 1:threshold) {
        #print(qseq[1:qi] )
        
        suspects <- props
        steps <- nrow(suspects)
        for(ai in 1:qi) {
            
            suspects <- suspects[suspects[names(sanswers)[ai]] == sanswers[ai],]
            steps <- nrow(suspects)
            
            if(steps == 1) {
                #print(answers[qseq[ai]])
                #print(ai)
                return(ai)
            }
            
        }
        
    }
    return(NA)
}

max_steps <- function(props, qseq, threshold=NULL) {
    
    steps <- sapply(1:nrow(props), function(pi) person_sequence(props, pi, qseq, threshold))
    max(steps)
    
}




find_best <- function(props, max_steps=20, max_sample=10) {
    qseq_set <- props[, 2:length(props)]
    dup <- duplicated(qseq_set)
    dup_num <- sum(dup)
    if(dup_num) {
        print(paste("Unsolvable, ", dup_num, " duplicates"))
        return
    }
    else {
        print("Solvable")
    }
    qseq_length <- length(qseq_set) 
    m <- initMC(1:qseq_length)
    
    best_steps <- qseq_length
    if(best_steps > max_steps) {
        best_steps <- max_steps
    }
    
    best_strategies <- data.frame()
    
    
    #for(i in 1:factorial(qseq_length)) {
    for(i in 1:max_sample) {
        qseq <- nextPerm(m)
        steps <- max_steps(props, qseq, threshold=best_steps)
        
        if(!is.na(steps)) {
            if(steps < best_steps) {
                best_steps <- steps
                
                best_strategies <- data.frame()
            }
            
            if(steps == best_steps) {
                best_strategies <- rbind(best_strategies, qseq)
            }
        }
        
        #print(paste(i, max_sample))
        
    }
    print(paste("Best strategy: ", best_steps))
    print(paste("Number o seq: ", nrow(best_strategies)))
    best_strategies
}

