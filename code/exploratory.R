#!/usr/bin/env Rscript
# vim: fileencoding=utf-8

# Carrega pacotes, intalando-os se necessário.
pkgs <- c('autoimage', 'corrplot', 'magrittr', 'missMDA', 'readxl', 'VIM')
not_installed <- !pkgs %in% rownames(installed.packages())
if (any(not_installed)) install.packages(pkgs[not_installed])
library(magrittr)  # pipe operator (%>%)
source('utils.R')
source('stats.R')
source('plots.R')


### Data input ###

# 2020CorrelacionesNeuroendocrineAjustada.v2.xlsx
excel_file <- 'data/CorrelacionesCerebro.xlsx'
rds_file <- paste0(excel_file, '.rds')  # ficará com extensão ".xlsx.rds"

n_sheets <- 1
data_sheet0 <- 1

xcols <- c(
        "A,V:AK,AO,AQ:AV,AW:BE"#,BF:BR,BS:BY"  
        # Brain regions volumes V:AK; 
        # Clinical data AO,AQ:AV
        # Citokines amounts AW:BE
        # Hormones (purple heads) BF:BR
        # Neuropeptides (red heads) BS:BY
)
cols <- lapply(xcols, excel_cols)

last_WSC_row <- c(13)
last_CC_row <- c(23)

# Carrega as páginas da planilha lidos na lista 'sheets' e as colunas
# de interesse na lista 'dat'.
if (file.exists(rds_file)) {
    dat <- readRDS(rds_file)
} else {
    sheets <- list()

    # Se todas as páginas estiverem bem formatadas, podemos ler as
    # 'n_sheets' páginas de uma vez.  Se for ler uma de cada vez, no
    # entanto, é possível escolher o intervalo de linhas e colunas a
    # ser lido da página com argumentos extras.  Aqui, por precaução,
    # lemos apenas os intervalos de colunas que contêm os dados de
    # interesse para evitar problemas.
    for (i in data_sheet0:n_sheets) {
        last_row <- last_CC_row[i]
        last_col <- tail(cols[[i]], 1)
        cell_range <- readxl::cell_limits(lr=c(last_row, last_col))
        sheets[[i]] <- readxl::read_excel(excel_file, sheet=i, range=cell_range, .name_repair='minimal')
    }
    
    # Esse pacote produz tabelas no formato "tibble", de outro pacote,
    # mas como não usamos as funcionalidades extras dele é melhor
    # converter para o formato de tabela básico do R para evitar
    # surpresas...
    sheets <- lapply(sheets, as.data.frame)

    # Padroniza o nome da citocina MIP-1β
    # sheets <- lapply(sheets, function(df) {
    #    colnames(df) <- sub('^MIP.*', 'MIP-1β', colnames(df))
    #    return(df)
    # })
    
    # Criamos os objetos que serão processados apenas com os dados
    # (colunas) de interesse.
    dat <- list()
    for (i in 1:length(sheets)) {
        dat[[i]] <- sheets[[i]][, cols[[i]][-1]]
        rownames(dat[[i]]) <- sheets[[i]][, cols[[i]][1]]
    }

    # Corrige entrada incorreta, convertendo para NA.
    dat[[1]][['Tumour stage']] <- as.integer(dat[[1]][['Tumour stage']])

    # Renomeia variáveis de volume cerebral.
    colnames(dat[[1]])[1:16] <- c(
        "Amydgala_L (mL)",
        "Amydgala_R (mL)",
        "Caudate_L (mL)",
        "Caudate_R (mL)",
        "Frontal Orbital SMIR_L (mL)",
        "Frontal Orbital SMIR_R (mL)",
        "Frontal SMIT_R (mL)",
        "Insula_L (mL)",
        "Insula_R (mL)",
        "Occipital S_L (mL)",
        "Occipital SMI_L (mL)",
        "Putamen_L (mL)",
        "Temporal SMI_L (mL)",
        "Temporal SMI_R (mL)",
        "Hypothalamus_L (mL)",
        "Hypothalamus_R (mL)"
    )

    # Por fim, podemos salvar as tabelas num formato interno do R para
    # acelerar o carregamento dos dados em execuções posteriores.
    saveRDS(dat, rds_file)
}

# Discount the header row.
last_WSC_row <- last_WSC_row - 1
last_CC_row <- last_CC_row - 1

# Define some variables
n_datasets <- length(dat)
groups <- factor(ifelse(1:last_CC_row > last_WSC_row, 'CC', 'WSC'))
cor_vars <- list(list(names(dat[[1]])))

# Get sets of parameters names.
#antropo_biochem <- names(dat[[1]])
#cytokines <- setdiff(names(dat[[2]]), antropo_biochem)
#hormones <- setdiff(names(dat[[3]]), antropo_biochem)
#neuropeptides <- setdiff(names(dat[[4]]), antropo_biochem)

#cor_vars <- list(
#    list(antropo_biochem),
#    list(antropo_biochem, cytokines),
#    list(antropo_biochem, hormones),
#    list(antropo_biochem, neuropeptides),
#    list(cytokines, hormones),
#    list(cytokines, neuropeptides),
#    list(hormones, neuropeptides),

    # These will be created next.
#    list(cytokines),
#    list(hormones),
#    list(neuropeptides)
#)

# Sanity check.
#for (i in 1:length(dat)) stopifnot(setequal(colnames(dat[[i]]), unlist(cor_vars[[i]])))


# Split the dataset between groups.
dat_by_group <- list()
for (i in 1:n_datasets) {
    WSC_rows <- 1:last_WSC_row[i]
    dat_by_group[[i]] <- list(
            WSC = dat[[i]][WSC_rows, ],
            CC = dat[[i]][-WSC_rows, ]
    )
}

save.image('base-state.RData')


### Data inspection ###

if(F){
# Quantas amostras têm observações completas?
x <- do.call(Map, args=c(list, dat_by_group))  # equivalent to Python's zip()
is_complete <- lapply(c(x$WSC, x$CC), complete.cases)
remove(x)

n_complete <- sapply(is_complete, sum)
n_incomplete <- lapply(is_complete, `!`) %>% sapply(sum)
tab <- matrix(c(n_complete, n_incomplete), nrow=4, byrow=TRUE)
rownames(tab) <- c('WSC_complete', 'CC_complete', 'WSC_incomplete', 'CC_incomplete')
colnames(tab) <- data_sheet0:n_sheets

# Gráfico do número de amostras completas/incompletas em cada grupo.
png('fig/complete_cases.png')
barplot(tab[1:2, ] + tab[3:4, ], ylim=c(0, 49), col=c('pink', 'lightblue'),
        xlab="Dataset worksheet number", ylab="n", beside=TRUE)
barplot(tab[1:2, ], col=c('darkred', 'darkblue'), legend=c('WSC', 'CC'), beside=TRUE, add=TRUE)
title("Complete (dark colors) and incomplete (light colors) samples")
dev.off()
}#if(F){

# Combinações de variáveis com observações faltantes.
VIM::aggr(dat[[1]][, 17:32], numbers=TRUE, prop=FALSE, oma=c(9, 4.5, 1.5, 1)+0.1)


### Principal Component Analysis ###

library(missMDA)

ncomp_loo <- estim_ncpPCA(dat[[1]], ncp.min=0, ncp.max=17, method.cv='loo')
ncomp_gcv <- estim_ncpPCA(dat[[1]], ncp.min=0, ncp.max=17, method.cv='gcv')
ncomp_kfd <- estim_ncpPCA(dat[[1]], ncp.min=0, ncp.max=17, method.cv='Kfold')
plot(ncomp_loo$criterion/max(ncomp_loo$criterion) ~ names(ncomp_loo$criterion),
     type='l', ylim=0:1, col=2, xlab="number of dimensions", ylab="relative mean error")
lines(ncomp_gcv$criterion/max(ncomp_gcv$criterion) ~ names(ncomp_gcv$criterion), col=3)
lines(ncomp_kfd$criterion/max(ncomp_kfd$criterion) ~ names(ncomp_kfd$criterion), col=4)
legend('bottomleft', c("loo", "gcv", "Kfold"), fill=2:4)
ncomp <- 7

res.imp <- imputePCA(dat[[1]], ncp=ncomp)
#res.pca <- prcomp(res.imp$completeObs, scale=TRUE)
pca <- FactoMineR::PCA(cbind(groups, res.imp$completeObs)) # it's probably broken
plot(pca, choix='ind', habillage=1)

res.MIPCA <- MIPCA(dat[[1]], ncomp)
plot(res.MIPCA, choice='ind.proc', axes=2:3, graph='classic')


### Correlation computation ###

cor_vars <- list(list(names(dat[[1]])))

corrS <- replicate(n_datasets, list())
corrP <- corrS
for (i in 1:n_datasets) for (group in names(dat_by_group[[i]])) {
    vars <- cor_vars[[i]]
    df <- dat_by_group[[i]][[group]]
    rows <- vars[[1]]
    cols <- if (length(vars) > 1) vars[[2]] else vars[[1]]
    corrS[[i]][[group]] <- stat_matrix(df, cor_test, X=rows, Y=cols, method='spearman')
    corrP[[i]][[group]] <- stat_matrix(df, cor.test, X=rows, Y=cols)
}

# Output as table.
tables <- replicate(n_datasets, list())
fields <- c('estimate', 'p.value', 'conf.int')
headers <- c('correlation', 'p-value', 'CI lower', 'CI upper')
headers <- c(paste("Spearman's", headers), paste("Pearson's", headers))
headers <- c('Var X', 'Var Y', 'N', headers)
for (i in 1:n_datasets) {
    for (group in names(dat_by_group[[i]])) {
        tab <- data.frame()
        vars1 <- names(corrS[[i]][[group]])
        vars2 <- names(corrS[[i]][[group]][[1]])
        for (var1 in vars1) for (var2 in vars2) {
            if (var1 == var2) next
            spearman <- corrS[[i]][[group]][[var1]][[var2]]
            pearson <- corrP[[i]][[group]][[var1]][[var2]]
            row <- c(spearman[fields], pearson[fields])
            row <- c(list(var1, var2, spearman$n), unlist(row))
            tab <- rbind(tab, unname(row))
        }
        colnames(tab) <- headers
        tables[[i]][[group]] <- tab
    }
}

# Save tables to TSV files.
dir.create('out', show=FALSE)
for (i in seq_along(corrS)) for (group in names(corrS[[i]])) {
    filename <- sprintf('out/CorrResults_%d_%s.tsv', i, group)
    write.table(tables[[i]][[group]], filename, row.names=FALSE, sep='\t', quote=FALSE)
}

### Basic correlation plot ###

library(corrplot)

corrS_CC <- get_matrix(corrS[[1]]$CC, 'estimate')
corrS_WSC <- get_matrix(corrS[[1]]$WSC, 'estimate')

png('fig/corrplot-cc.png', w=800, h=800)
corrplot(corrS_CC, mar=c(0,0,2,0), tl.col='gray15',
         title="Pairwise Spearman's Correlation (CC Group)")
dev.off()

png('fig/corrplo;-wsc.png', w=800, h=800)
corrplot(corrS_WSC, mar=c(0,0,2,0), tl.col='gray15',
         title="Pairwise Spearman's Correlation (WSC Group)")
dev.off()

### Complete correlation plot ###

dir.create('fig', show=FALSE)

CA <- 'lzw'  # compression algorithm
PS <- 8      # point size
RES <- 300   # resolution in ppi
WIDTH <- 7   # inches

# Color legend.
HEIGHT <- 3.5
legend_file <- 'fig/corr-color-legend-vertical2.tif'
tiff(legend_file, WIDTH/9, height=HEIGHT, units='in', res=RES, pointsize=PS, compression=CA) #, type='cairo')
par(fig=c(0, 0.3, 0.02, 0.9), mar=c(0, 0, 0, 0))
axis.args <- list(at=seq(-1, 1, by=0.25))
autoimage::legend.scale(c(-1, 1), color, axis.args=axis.args, horizontal=FALSE)
invisible(dev.off())

# Join symmetric pairs of plots in a single one.
HEIGHT <- 7
symmetric <- which(sapply(cor_vars, length) == 1)
for (i in symmetric) {
    N <- ncol(dat[[i]])
    plot_matrix <- corrS[[i]]$CC  # CC in the top right corner
    for (m in seq_len(N)) for (n in seq_len(N)) if (m > n) {
        plot_matrix[[m]][[n]] <- corrS[[i]]$WSC[[m]][[n]]
    }
    filename <- sprintf('fig/CorrPanelPair%d.tif', i)
    width <- WIDTH*8/9
    height <- HEIGHT
    if (i >= 8) {
        width <- width * 4/7
        height <- height * 4/7
    }
    tiff(filename, width, height, units='in', res=RES, pointsize=PS, compression=CA)
    cor_panel(dat[[i]], plot_matrix, legend.box=c('CC', 'WSC'))
    dev.off()
    system2('magick', args=c(filename, legend_file, '-gravity', 'east', '+append', '-compress', CA, 'tmp'))
    file.rename('tmp', filename)
}

# Asymmetric pairs.
tmp <- 'fig/tmp.tif'
WIDTH <- 4   # two images will be appended (one cropped)

asymmetric <- which(sapply(cor_vars, length) == 2)
for (i in asymmetric) {
    filename <- sprintf('fig/CorrPanelPair%d.tif', i)
    tmp_name <- sprintf('fig/CorrPanel%d_%%d.tif', i)
    HEIGHT <- if (i <= 4) 6 else 5
    tiff(tmp_name, WIDTH, HEIGHT, units='in', res=RES, pointsize=PS, compression=CA)
    for (group in c('WSC', 'CC')) {  # CC in the right side
        cor_panel(dat_by_group[[i]][[group]], corrS[[i]][[group]], main=group, adj=0.6, line=2, cex.main=1.3)
    }
    crop <- if (i <= 4) 0.02 else 0.03
    crop <- crop + max(strwidth(names(corrS[[i]][[1]]), units='figure'))
    crop <- sprintf('%dx%d+0+0', floor((1 - crop)*WIDTH*RES), HEIGHT*RES)
    dev.off()
    system2('magick', args=c(sprintf(tmp_name, 2), '-gravity', 'east', '-crop', crop, tmp))
    system2('magick', args=c(sprintf(tmp_name, 1), tmp, legend_file, '-gravity', 'east', '+append', '-compress', CA, filename))
}
invisible(suppressWarnings(file.remove(tmp)))
