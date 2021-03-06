#' Binning Continous Varaibles Based on Information value
#'
#' \code{bin} bins a continous variable by maximizing the infomation value.
#'
#' @param df a data frame
#' @param y name of target variable
#' @param x name of predictor
#' @param target the response variable
#' @param predictor the continous variable to bin
#' @param nbin numbers of binning
#' @param early_stop_threshold iv increasing value less than the threshold will
#' cause the binning process stopped. If not NA, \code{nbin} will be ignored.
#' @param min.node.pct the smallest sample proportion of the bins
#' @param p p value used for fisher test rejection
#' @param single.values some values will be split as a level
#'
#' @return a \code{bin} object
#'
#' \code{cuts} the cut points
#' \code{IV} the information value
#' \code{WOE} the weight of evidence tagble
#'
#' @import stats
#' @import magrittr
#' @export
bin = function(...) {
    UseMethod("bin")
}

#' @describeIn bin bin.default
#' @export
bin.default <-
function(target, predictor, nbin = 5, early_stop_threshold = NA,
         min.node.pct = 0.05, p = 0.05,
         single.values = NULL, ...) {
    if (!is.numeric(predictor)) stop('predictor must be numeric')
    target <- factor(target)
    if (levels(target) %>% length != 2)
        stop('target must be or can be coerced to a factor of two levels')
    # if early_stop_threshold is not NA, nbin will be ignored
    if (!is.na(early_stop_threshold)) nbin = Inf
    good <- levels(target)[1]
    bad  <- levels(target)[2]
    total.goods <- sum(target == good)
    total.bads  <- sum(target == bad)
    total.counts <- total.bads + total.goods
    iv <- numeric(0)

    # calculate information value for one level
    iv.level <- function(part) {
        goods <- sum(part == good)
        bads <- sum(part == bad)
        gr <- goods / total.goods
        br <- bads / total.bads
        (gr - br) * log(gr / br)
    }

    # All starts from one
    level <- rep(1, length(predictor))
    # Missing values
    if (any(is.na(predictor))) {
        level[is.na(predictor)] <- -1
        iv <- c(iv, iv.level(target[is.na(predictor)]))
        names(iv) <- c(names(iv)[-length(iv)], 'Missing')
    }
    # Single values
    if (!is.null(single.values)) {
        for (val in single.values) {
            level[predictor == val] <- -1
            iv <- c(iv, iv.level(target[predictor == val]))
            names(iv) <- c(names(iv)[-length(iv)], paste0('= ', val))
        }
    }

    # all points available for cutting
    points <- unique(predictor[level == 1])
    if (length(points) > 100)
        points = quantile(predictor[level == 1], seq(.01, .99, by = 0.01))
    points = round(sort(unique(points)), 4)
    points.level <- rep(1, length(points))

    # return maximum information value gain by add a cut point in some level
    gain.iv <- function(lv) {
        old.iv <- iv.level(target[level == lv])
        # first chech the percentage
        if (sum(level == lv) > min.node.pct * length(target)) {

            new.iv <- 0
            cut.points <- NA
            for (point in points[points.level == lv]) {
                # check the percentage
                if (min(sum(predictor <= point & level == lv),
                        sum(predictor > point & level == lv)) <= min.node.pct * length(target))
                            next
                # statistical test
                if (fisher.test(matrix(
                    c(
                        sum(target == good & level == lv & predictor <= point),
                        sum(target == bad & level == lv & predictor <= point),
                        sum(target == good & level == lv & predictor > point),
                        sum(target == bad & level == lv & predictor > point)
                    ), byrow = TRUE, ncol = 2
                ))$p > p) next

                tmp.iv <- sum(iv.level(target[level == lv & predictor <= point]),
                              iv.level(target[level == lv & predictor > point]))
                if (tmp.iv > new.iv & !is.infinite(tmp.iv)) {
                    new.iv <- tmp.iv
                    cut.points <- point
                }
            }
            return(list(iv.gain = new.iv - old.iv, cut = cut.points))
        }
        list(iv.gain = -old.iv, cut = NA)
    }

    cuts <- NULL
    # begin binning iteratively
    while (TRUE) {
        # note: missing values and single value levels are not counted as a bin
        if (length(level[level != -1] %>% unique) == nbin) break

        bin.obj <- list(iv.gain = 0, cut = NA)
        cut.lv <- -1
        for (lv in level[level != -1] %>% unique) {
            tmp.bin <- gain.iv(lv)
            if (tmp.bin$iv.gain > bin.obj$iv.gain) {
                bin.obj$iv.gain <- tmp.bin$iv.gain
                bin.obj$cut <- tmp.bin$cut
                cut.lv <- lv
            }
        }

        # no more appropriate cut point
        if (is.na(bin.obj$cut)) break

        # early stop
        if (!is.na(early_stop_threshold))
            if (bin.obj$iv.gain < early_stop_threshold)
                break

        # successfully binning, do some updating job
        cuts <- c(cuts, bin.obj$cut)
        level[predictor <= bin.obj$cut & level == cut.lv] <- cut.lv * 2
        level[level == cut.lv] <- cut.lv * 2 + 1
        points.level[points.level == cut.lv & points <= bin.obj$cut] <- cut.lv * 2
        points.level[points.level == cut.lv] <- cut.lv * 2 + 1
    }

    for (lv in unique(level[level != -1])) {
        iv <- c(iv, iv.level(target[level == lv]))
        names(iv) <- c(names(iv)[-length(iv)], paste0('Level ', lv))
    }

    if (!is.null(cuts)) {
        cuts <- cuts %>% sort
    } else {
        cuts <- Inf
    }
    x <- label.numeric(predictor, cuts, single.values = single.values)
    dat <- data.frame(y = target, x = x)
    res <- woe(dat, 'y', 'x', good)

    list(cuts = cuts, IV = sum(iv[!is.infinite(iv)]), WOE = res$WoE)
}

#' @describeIn bin bin.data.frame
#' @export
bin.data.frame = function(df, y, x, ...) {
    bin(df[, y], df[, x], ...)
}