#' Binning Continous Varaibles Based on Information value
#'
#' \code{bin} bins a continous variable by maximizing the infomation value.
#'
#' @param target the response variable
#' @param predictor the continous variable to bin
#' @param nbin numbers of binning
#' @param unit the basic interval of the continous variable
#' @param min.node.pct the smallest sample proportion of the bins
#' @param p not used yet
#'
#' @return a \code{bin} object
#'
#' \code{cuts} the cut points
#' \code{IV} the information value
#' \code{ivtable} an iv-table given by \code{\link[woe]{woe}}
#'
#' @export
bin <-
function(target, predictor, nbin = 5, unit = 1,
         min.node.pct = 0.05, p = 0.05) {
    if (predictor %>% is.numeric %>% not) {
        stop('predictor should be numeric')
    }

    points <- (predictor / unit) %>% round %>% multiply_by(unit) %>%
        unique %>% sort

    # .NotYetUsed(p, error = FALSE)
    # percentage test
    pct.test <- function(disc.pred) {
        tapply(disc.pred, disc.pred, length) %>% min(na.rm = TRUE) >=
            length(disc.pred) * min.node.pct
    }

    # fisher's test
    # fsh.test <- function()


    # binning just for one step further
    bin.one.step <- function(last.bin = NULL) {
        # Initialization of bin object
        if (is.null(last.bin)) {
            last.bin <- list(
                cuts = numeric(0),
                IV = 0
            )
        }

        # New bin
        new.bin <- last.bin

        # iteratively test on each point, and select the one with largest
        for (p in points) {
            cuts <- c(last.bin$cuts, p) %>% unique %>% sort()
            # print(cuts)
            disc.pred <- label.numeric(predictor, cuts)
            if (pct.test(disc.pred)) {
                iv <- iv(target, disc.pred)
                # print(iv)
                if (!is.na(iv) & iv > new.bin$IV) {
                    new.bin$cuts <- cuts
                    new.bin$IV <- iv
                }
            }
        }

        new.bin
    }


    bin.obj <- list(
        cuts = numeric(0),
        IV = 0
    )

    while (length(bin.obj$cuts) < nbin) {
        new.bin <- bin.one.step(bin.obj)
        if (new.bin$IV == bin.obj$IV)
            break
        bin.obj <- new.bin
    }

    type <- target %>% factor %>% levels
    Data <- data.frame(target = target,
                       predictor = label.numeric(predictor, bin.obj$cuts))
    ivtable <- woe(Data, 'predictor', FALSE, 'target', nbin,
                   type[1], type[2])

    bin.obj$ivtable <- ivtable
    class(bin.obj) <- c('bin', class(bin.obj))
    bin.obj
}