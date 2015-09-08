
Find optimal series of questions for the "Guess Who" game

Usage in R:

    source('strategies.R')
    props <- read.csv('guesswho.csv')
    opt_combs <- find_question_sets(props)
    
Question popularity in optimal combinations:

    q_num <- length(props) - 1
    sort(sapply(opt_combs[, 1:q_num], sum), decreasing=T)
    
List question sets:

    sapply(1: nrow(opt_combs), function(x) colnames(opt_combs[, simplify2array(opt_combs[x, 1:q_num])][x, ]))