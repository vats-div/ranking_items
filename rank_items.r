#' returns a ranked list of items given irt parameters
#' @param mdl an irt model learned from data
#' @param m is the mean of the ability parameters, default is 0
rank_items <- function(mdl, m=0, ability=NULL) {

    itemtype <- attr(mdl, 'itemtype')
    cc <- coef(mdl)
    cc[[length(cc)]] <- NULL 
   
    if (!is.null(ability)) {
        return(rank_items_ability(cc, ability))
    }

    if (all(itemtype == 'Rasch')) {
        return(rank_items_rasch(cc, m))
    }

    if (all(itemtype == '2PL')) {
        ability <- fscores(mdl, full.scores=TRUE, method='MAP')
        return(rank_items_2PL(cc, mean(ability), var(ability)))
    }
    
    return(NULL)
}

#' rank items using the rasch model
#' simply sorts the difficult parameters
#' @param cc is the output of coef(irt_model)
rank_items_rasch <- function(cc, m) {

    diff <- lapply(cc, function(c) c[2]) %>% unlist 
    sort(abs(diff + m), index.return=TRUE) %>% use_series(ix)
}

#' rank items using the 2PL model
#' @param cc is the output of coef(mdl)
#' @param m mean of the ability parameter
#' @param v variance of the ability parameter
rank_items_2PL <- function(cc, m, v) {

    # sample from N(m, v)
    samples <- rnorm(n=1000, mean=m, sd=sqrt(v))

    return(rank_items_ability(cc, samples))

#    M <- lapply(cc, entrop, samples) %>% unlist
#    return(sort(abs(M), index.return=TRUE) %>% use_series(ix))
}

#' rank items based on oracle knowledge about the
#' learner ability parameters
#' @param mdl irt model learned using mirt
#' @param ability parameter for a group of learners
rank_items_ability <- function(cc, ability) {
     if ("GroupPars" %in% names(cc)) {
        cc$GroupPars <- NULL
    }
    
    M <- lapply(cc, entrop, ability) %>% unlist
    return(sort(-abs(M), index.return=TRUE) %>% use_series(ix))
}


#' entropy of the 2PL IRT model
#' @param cc is the output of coef(mdl)
entrop <- function(cc, a) {

    # get scale and difficulty
    s <- cc[1]
    mu <- cc[2]
    tmp <- s * a + mu
    entr <- mean(log(1 + exp(tmp)) - tmp / (1+exp(-tmp)))

    entr
}
