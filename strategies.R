# 
# Find winning series of questions for Guess Who game
# 
# Main function:
# find_question_sets
# 
# Example:
# props <- read.csv('guesswho.csv')
# opt_combs <- find_question_sets(props)


fields_combs <- function(nf) {
    combs <- matrix(F, ncol=nf, nrow= 2**nf)
    lastn = 2 ** nf - 1
    #lastn = 1000
    for(n in 0:lastn) {
        #if(n %% 100 == 0) {
        #    print(paste(n, "/", lastn))
        #}
        comb <- sapply(intToBits(n), function(x) as.logical(x))
        #comb <- intToBits(n)
        for(i in 1:nf) {
            combs[n + 1, i] <-  comb[[i]]
        }
    }
    combs[, 1:nf]
}

find_question_sets <- function(props) {
    qseq_set <- props[, 2:length(props)]
    combs <- data.frame(fields_combs(length(qseq_set)))
    cn <- nrow(combs)
    #print(cn)
    combs$Solvable <- FALSE
    combs$QN <- 0
    for(ci in 1:cn) {
        comb_set <- qseq_set[, simplify2array(combs[ci,1:length(qseq_set)])]
        dup <- duplicated(comb_set)
        dup_num <- sum(dup)
        if(dup_num == 0) {
            combs[ci, "Solvable"] <- TRUE
            combs[ci, "QN"] <- length(comb_set)
        }
        if(length(comb_set) == 0) {
            combs[ci, "Solvable"] <- FALSE
        }
    }
    scombs <- combs[combs$Solvable,]
    min_qn <- min(scombs$QN)
    opt_combs <- scombs[scombs$QN == min_qn,]
    colnames(opt_combs) <- c(colnames(props)[2:length(props)], "Solvable", "QN")
    opt_combs
}


# Functions for initial method, that turns out to be too slow
# IGNORE BELOW

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

library(multicool)


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




