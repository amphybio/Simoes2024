# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Copyright 2020 Alexandre Ferreira Ramos - AMPhyBio Laboratory
#
# Project:  AMPhyBio Code Library
# Version:  0.1
# Created:  06-12-2020
# Updated:  05-01-2021
# Authors:  Leonardo Gama <leonardo.gama@usp.br>


### Modified corrplot ###

reorder_using_hclust <- function(corr, hclust.method) {
    hc <- hclust(as.dist(1 - corr), method=hclust.method)
    order.dendrogram(as.dendrogram(hc))
}

hc_order <- function(datafr){
    x <- hclust(dist(datafr))
    return(x$order)
}

asym_corrplot <- function(corr, ...) {
    suppressPackageStartupMessages(require(corrplot))

    if (isSymmetric(corr, tol=0) || missing('order')) #!'order' %in% names(args))
        return(corrplot(corr, ...))

    args <- list(...)
    arg_names <- names(args)
    order <- arg_names[!is.na(pmatch(arg_names, 'order'))]
    order <- match.arg(args[[order]], choices=c('original', 'hclust', 'alphabet'))

    is_square <- nrow(corr) == ncol(corr)
    if (order == 'original' || is_square && order == 'alphabet')
        return(corrplot(corr, ...))

    if (is_square) {
        # Asymmetric square matrix with hclust order
        
    } else if (order == 'hclust') {
        # Rectangular matrix with hclust order

    } else {
        # Rectangular matrix with alphabet order
    }
}


### Correlation/regression plot with points ###

library(RColorBrewer)
pal <- brewer.pal(11, 'RdBu')[2:10]
color <- colorRampPalette(pal)(201)
LABEL_LINE <- 2.5

cor_subplot <- function(x, y, corr, col.box, cex.lab, font.lab, cex.stat, ...) {
    current <- par('mfg')
    i <- current[1]
    j <- current[2]
    varI <- names(corr)[i]
    varJ <- names(corr[[1]])[j]

    # Panel labels.
    if (i == 1 || j == 1) {
        panel_label <- function(...) mtext(..., line=LABEL_LINE, las=2, cex=cex.lab, font=font.lab)
        # Topmost row.
        if (i == 1) panel_label(varJ, side=3)
        # Leftmost column.
        if (j == 1) panel_label(varI, side=2)
    }

    # If in the diagonal of a symmetric panel, stop here.
    if (varI == varJ) return()

    # Get values from the 'correlations' table passed as argument.
    corr_test <- corr[[i]][[j]]

    # Background color.
    idx <- round(100*(corr_test$estimate + 1) + 1)
    oldpar <- par(usr=c(0, 1, 0, 1))
    rect(0, 0, 1, 1, col=color[idx], border=NA)
    par(oldpar)

    # Data points and regression line.  Note that x and y are swapped.
    points(corr_test$y, corr_test$x, pch=20, col='#00000060', ...)
    abline(lm(corr_test$x ~ corr_test$y), lwd=1.5)

    par(usr=c(0, 1, 0, 1))

    # Add legend.
    rect(0, 0, 1, cex.stat*par('cxy')[2], col='white', border=NA)
    subtext <- sprintf("%.3f", corr_test$p.value)
    plot_text <- function(...) mtext(subtext, side=1, line=-1.1, cex=cex.stat, ...)
    if (corr_test$p.value < .05) {
        plot_text(font=2, col='darkred')
    } else {
        plot_text(font=1, col='gray20')
    }

    # Add plot box.
    if (!is.null(col.box)) box(bty='o', col=col.box[if (i < j) 1 else 2])        
}

cor_panel <- function(data, corr, main, legend.box, margin=c(2, 2, 2, 2), gap=0, col.box=NULL, cex=2,
                      adj.main=0.55, cex.main=1.5, line.main=3, cex.lab=0.8, font.lab=2, cex.stat=0.8, ...) {
    # Margins are set via 'oma', which is affected by 'cex'. 
    CEX_FIX <- 1.5  # 'cex' inside pairs() is 0.66
    label_width <- function(x) max(strwidth(x, units='in', cex=cex.lab*CEX_FIX, font=font.lab))

    if (identical(colnames(data), names(corr)) && identical(names(corr), names(corr[[1]]))) {
        # Pairwise correlation panel.
        horInd <- verInd <- 1:ncol(data)
        left_mar <- top_mar <- label_width(colnames(data))

        if (is.null(col.box) && !isSymmetric(get_matrix(corr, 'estimate'))) {
            col.box <- c('magenta4', 'green4')
        }
    } else if (identical(colnames(data), c(names(corr), names(corr[[1]])))) {
        # X vs Y correlation panel.
        m <- length(corr)
        n <- length(corr[[1]])
        horInd <- 1:m
        verInd <- (m+1):(m+n)
        left_mar <- label_width(names(corr))
        top_mar <- label_width(names(corr[[1]]))
    } else {
        stop("Incompatible or malformated 'data' and 'corr' arguments.")
    }

    # Function pairs.default() just accepts outer margin
    # specification via 'oma', and strwidth() just returns useful
    # information in inches or fraction of figure dimensions.
    if (length(margin) == 1) margin <- rep_len(margin, 4)
    if (!missing(main)) margin[3] <- margin[3] + line.main
    old_par <- par(oma=margin + c(2, LABEL_LINE, LABEL_LINE, 2))
    on.exit(par(old_par))
    par(omi=par('omi') + c(0, left_mar, top_mar, 0))
    oma <- par('oma')

    par(bty='n')  # no plot box (borders) by default
    suppressWarnings({
        pairs(data, corr=corr, panel=cor_subplot, diag.panel=cor_subplot, text.panel=NULL,
              horInd=horInd, verInd=verInd, oma=oma, gap=gap, cex=cex, col.box=col.box,
              cex.lab=cex.lab, font.lab=font.lab, cex.stat=cex.stat, ...)
    })

    par(oma=margin)
    if (!is.null(col.box) && !missing(legend.box)) {
        #FIXME: strange graphics bug...
        par(xpd=FALSE)                               # changes nothing
        legend('topleft', legend.box, fill=col.box)  # clipped out, shows nothing
        par(xpd=TRUE)                                # allow legend outside plot area
        legend('topleft', legend.box, fill=col.box)  # doesn't work without the first two lines!
    }
    if (!missing(main)) title(main, outer=TRUE, cex.main=cex.main, line=line.main, adj=adj.main)
}
