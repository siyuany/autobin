#' WoE Calculation
#'
#' @param df data.frame
#' @param y response variable
#' @param x predictor
#' @param cutp default \code{NULL}. If not provided, \code{x} will be treated as a factor. If \code{x} is a factor, this parameter will be ignored. Otherwise, \code{x} will be cut into a factor.
#' @param good the name of good. If not provided, the first levels of \code{y} will be treated as good
#' @param na.exclude whether NA values should be removed or kept as an independent level. FALSE by default.
#' @examples
#' df = data.frame(y = sample(c(0, 1), 100, replace = TRUE), x = runif(100))
#' woe(df, 'y', 'x', cutp = c(0, 0.3, 0.6, 1), good = '0')
#' @export
woe = function(df, y, x, cutp = NULL, good = NULL, na.exclude = FALSE) {
    if (!is.data.frame(df)) stop('df is not a data.frame')
    cols = colnames(df)
    if (!y %in% cols)
        stop(sprintf("Variable '%s' does not exist in dataset", y))
    if (!x %in% cols)
        stop(sprintf("Variable '%s' does not exist in dataset", x))

    response = df[, y]
    predictor = df[, x]


    if (length(unique(response)) > 2)
        stop("response should be a vector with only two unique values")
    response = as.factor(response)

    # deal with NA values in response
    if (any(is.na(response))) {
        predictor = predictor[!is.na(response)]
        response = response[!is.na(response)]
    }

    if (!is.factor(predictor) & !is.null(cutp)) {
        predictor = cut(predictor, cutp, include.lowest = TRUE)
    } else if (!is.factor(predictor) & is.null(cutp)) {
        predictor = as.factor(predictor)
        if (nlevels(predictor) > 5)
            warning("predictor is coerced to a factor with too many levels")
    }

    if (is.null(good)) {
        levels(response) = c("Good", "Bad")
    } else {
        levels(response) = ifelse(levels(response) == as.character(good),
            "Good", "Bad")
    }

    # deal with NA values in predictors
    response_nap = response[is.na(predictor)]
    response = response[!is.na(predictor)]
    predictor = predictor[!is.na(predictor)]

    woe.tbl = tapply(response, list(predictor, response), length)
    # here NA represents 0
    woe.tbl[is.na(woe.tbl)] = 0
    woe.tbl = as.data.frame(woe.tbl)

    if (!na.exclude & length(response_nap) > 0) {
        woe.tbl <- rbind(
            Missing = data.frame(Good = sum(response_nap == "Good"),
                Bad  = sum(response_nap == "Bad")),
            woe.tbl
        )
    }

    total_good = sum(woe.tbl$Good)
    total_bad = sum(woe.tbl$Bad)

    woe.tbl$GoodPct = woe.tbl$Good / total_good
    woe.tbl$BadPct = woe.tbl$Bad / total_bad
    woe.tbl$TtlPct = (woe.tbl$Good + woe.tbl$Bad) / (total_good + total_bad)
    woe.tbl$WoE = log(woe.tbl$BadPct / woe.tbl$GoodPct)
    woe.tbl$IV = (woe.tbl$BadPct - woe.tbl$GoodPct) * woe.tbl$WoE

    iv = sum(woe.tbl$IV[!is.na(woe.tbl$IV) & !is.infinite(woe.tbl$IV)])

    res = list(WoE = woe.tbl, IV = iv)
    class(res) = c('woe', class(res))
    res
}

print.woe = function(x, ...) {
    cat("IV: ", x$IV, '\n\n')
    print(x$WoE)
}
