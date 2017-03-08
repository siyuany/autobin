#' Information Value
#'
#' Calculate the information value of a variable.
#'
#' @param target the objective variable which should be binary.
#' @param groups the predictive (categorical) variable
#'
#' @return information value
#'
#' @title Information Value
#' @description
#' This function calculates the information value of a discrete predictor on an
#' objective variable.
#' @author Siyuan Yao
#' @param target the objective variable, which should be binary, i.e., have only two values.
#' @param groups the discrete predictor.
#' @return \code{iv} a non-negative numeric.
#' @examples
#' library(ISLR)
#' data(Default)
#' with(Default, iv(default, student))
#'
#' @export

iv <-
function(target, groups) {
    if (unique(target) %>% length != 2) {
        stop('The target should be a binary variable.')
    }
    if (length(target) != length(groups)) {
        stop('Lengths of target and groups do not match.')
    }

    group.struct <- tapply(target, list(groups, target), length) %>% data.frame
    ratio.1 <- group.struct[, 1] / sum(group.struct[, 1])
    ratio.2 <- group.struct[, 2] / sum(group.struct[, 2])

    ivs <- (ratio.1 - ratio.2) * log(ratio.1 / ratio.2)
    sum(ivs[!is.infinite(ivs)])
}