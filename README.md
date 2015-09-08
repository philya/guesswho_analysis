
Find optimal series of questions for the "Guess Who" game

Usage in R:

    source('strategies.R')
    props <- read.csv('guesswho.csv')
    opt_combs <- find_question_sets(props)
    
Question popularity in winning combs:

    sort(sapply(opt_combs[, 1:15], sum), decreasing=T)