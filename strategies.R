# 
# Find winning series of questions for Guess Who game
# 
# Main function:
# find_question_sets
# 
# Example:
# props <- read.csv('guesswho.csv')
# opt_combs <- find_question_sets(props)

# Question popularity in winning combs
# sort(sapply(opt_combs[,1:15], sum), decreasing = T)


# Create a matrix of all possible combinations of questions
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





