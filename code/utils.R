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


# Para criar os objetos que serão usados nas análises, é preciso
# selecionar as colunas desejadas, na ordem desejada. Os editores de
# planilha indicam as colunas usando índices alfabéticos ("C", "AA").
# Podemos construir um dicionário (baseado em resposta no site
# StackOverflow).

EXCEL_COLS <- expand.grid(LETTERS, c('', LETTERS))
EXCEL_COLS <- setNames(1:(26*27), paste0(EXCEL_COLS[[2]], EXCEL_COLS[[1]]))

excel_cols <- function(...) {
    args <- toupper(as.character(list(...)))
    if (length(args) == 1) {
        # Accept indexes as a comma separated list in a character string.
        args <- strsplit(trimws(args), '[[:space:]]*,[[:space:]]*')[[1]]
    }
    if (length(invalid <- grep('^[A-Z]{1,2}(:[A-Z]{1,2})?$', args, invert=TRUE, value=TRUE))) {
        stop("Invalid column index or range:  ", paste(invalid, collapse=", "))   
    }
    
    cols <- integer()
    for (col_index in strsplit(args, ':')) {
        if (length(col_index) == 1) {
            cols <- c(cols, EXCEL_COLS[col_index[[1]]])
        } else {
            # Preserve columns' alphabetical indexes in names.
            range_begin <- EXCEL_COLS[col_index[[1]]]
            range_end <- EXCEL_COLS[col_index[[2]]]
            cols <- c(cols, EXCEL_COLS[range_begin:range_end])
        }
    }
    return(cols)
}

stat_matrix <- function(data, stat_func, X=NULL, Y=NULL, ...) {

    if (is.null(X) && is.null(Y)) X <- Y <- colnames(data)
    if (is.null(X) || is.null(Y)) stop("Pass both 'X' and 'Y' or none of them.")
    symmetric <- identical(X, Y)
    m <- length(X)
    n <- length(Y)

    res <- setNames(replicate(m, list()), X)
    for (i in seq_len(m)) for (j in seq_len(n)) {
        x <- X[i]
        y <- Y[j]
        if (i > j && symmetric) {
            stat <- res[[y]][[x]]
            if (hasName(stat, 'x') && hasName(stat, 'y')) {
                tmp <- stat$x; stat$x <- stat$y; stat$y <- tmp
            }
            res[[x]][[y]] <- stat

        } else {
            res[[x]][[y]] <- stat_func(data[, x], data[, y], ...)
        }
    }
    return(res)
}

get_matrix <- function(list_matrix, name) {
    res <- sapply(
            list_matrix,
            sapply,
            function(object, name) unname(getElement(object, name)),
            name
    )
    return(t(res))
} 
