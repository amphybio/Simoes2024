# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2020 Alexandre Ferreira Ramos - AMPhyBio Laboratory
#
# Project:  AMPhyBio Code Library
# Version:  0.1
# Created:  06-12-2020
# Authors:  Leonardo Gama <leonardo.gama@usp.br>

spearman_confint <- function(rho, n, conf.level=0.95) {
    # Calculate two-sided confidence interval for Spearman's rank correlation
    # using Fisher transformation, with a standard error for z as estimated by
    # Fieller et al. (1957). Code based on cor.test() for Pearson's correlation.
    sigma <- 1.03/sqrt(n - 3)
    z <- atanh(rho)
    ci <-  z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2)
    ci <- tanh(ci)
    attr(ci, 'conf.level') <- conf.level
    return(ci)
}

cor_test <- function(x, y, covar, alternative=c('two.sided', 'less', 'greater'),
                     method=c('pearson', 'kendall', 'spearman'), exact=NULL, conf.level=0.95,
                     continuity=FALSE, ...) {
    # Improvement to cor.test, returning confidence level for spearman
    # correlation and allowing partial correlation with a single
    # covariate.  Also appends n, x and y to the result.
    # TODO: Allow more than one covariate.

    alternative <- match.arg(alternative)
    method <- match.arg(method)

    if (!missing(covar)) {
        # Remove samples with missing observations.
        ok <- complete.cases(x, y, covar)
        x <- x[ok]
        y <- y[ok]
        covar <- covar[ok]

        # Adjust by the covariate.
        x <- lm(x ~ covar)$residuals + mean(x)
        y <- lm(y ~ covar)$residuals + mean(y)
    }

    res <- cor.test(x, y, alternative, method, exact, conf.level, continuity, ...)
    res$x <- x
    res$y <- y
    res$n <- if (missing(covar)) sum(complete.cases(x, y)) else length(x)
    if (method == 'spearman' && alternative == 'two.sided') {
        # TODO: Add conf.level for single sided spearman test.
        res$conf.int <- spearman_confint(res$estimate, res$n, conf.level)
    }

    return(res)
}
