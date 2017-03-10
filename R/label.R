#' Transform a variable into factor
#'
#' @description
#' These functions turn a variable into a factor, with \code{NA} as an independent
#' level called 'Missing'.
#'
#' @param x a numeric or a vector which can be coerced to be a factor
#' @param levels designated levels in the result factor
#' @param cuts cut points to segment a numeric, using a right-close interval
#' @param na.level should NA be an independent level? NULL for default behavior which inducts
#' @param single.values For numeric vectors, some of the values should be as separate levels.
#'
#' @import magrittr
#' @export
label.factor <- function(x, levels = NULL) {
    if (is.null(levels)) {
        levels = x %>% factor %>% levels
        if (any(is.na(x))) levels <- c('Missing', levels)
    }

    x <- x %>% as.character
    x[is.na(x)] <- 'Missing'
    x %>% factor(levels = levels)
}

#' @describeIn label.factor
#'
#' @export
label.numeric <- function(x, cuts,
                          single.values = NULL,
                          na.level = NULL) {
    cuts <- sort(cuts)
    n_cut <- length(cuts)
    level.names <- paste0('<= ', cuts) %>% c(paste0('> ', cuts[n_cut]))

    if (any(is.null(na.level))) {
        na.level <- any(is.na(x))
    }
    if (na.level) level.names <- c('Missing', level.names)
    else {
        if(any(is.na(x))) stop('x has NA value while na.level is FALSE')
    }

    y <- vector('integer', length(x))

    for (i in n_cut:1) {
        y[x <= cuts[i]] <- i
    }
    y[x > cuts[n_cut]] <- n_cut + 1

    if (na.level) {
        y <- level.names[y + 1]
        y[is.na(x)] <- 'Missing'
    } else {
        y <- level.names[y]
    }

    if (single.values %>% is.null %>% not) {
        y[x %in% single.values] <- paste0('= ', x[x %in% single.values])
        level.names <- c(level.names, paste0('= ', single.values))
    }

    factor(y, level.names)
}
