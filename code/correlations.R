#!/usr/bin/env Rscript
# vim: fileencoding=utf-8

## packages ##

suppressPackageStartupMessages(library(tidyverse))

#remotes::install_github('amphybio/main', subdir='utils/R-package')


## data ##

excel_file <- 'data/E.Simoes Cachexia Brain Correlations.xlsx'

dat <- excel_file |>
    readxl::read_excel(range='A1:AA23') |>
    as.data.frame() |>
    rename(`IL1β (pg/mL)` = `IL1𝝱 (pg/mL)`) |>  # substitute beta character by standard one
    mutate(`Tumor stage` = suppressWarnings(as.numeric(`Tumor stage`)))  # discard bad value (and cast)

dat_ctrl <- dat |>  # WSC/control
    filter(Group == 0) |>
    select(!Paciente:Group)

dat_case <- dat |>  # CC/case
    filter(Group == 1) |>
    select(!Paciente:Group)


## statistics ##

cor_ctrl <- dat_ctrl |>
    amphy::stat_matrix(amphy::cor_test, method='spearman', exact=FALSE)
cor_case <- dat_case |>
    amphy::stat_matrix(amphy::cor_test, method='spearman', exact=FALSE)

rho_ctrl <- cor_ctrl |> amphy::get_matrix('estimate')
rho_case <- cor_case |> amphy::get_matrix('estimate')

pval_ctrl <- cor_ctrl |> amphy::get_matrix('p.value')
pval_case <- cor_case |> amphy::get_matrix('p.value')

as_table <- function(mat, colname) {
    mat |>
        as.data.frame() |>
        rownames_to_column('var1') |>
        pivot_longer(-var1, names_to='var2', values_to=colname) |>
        as.data.frame()  # tidyr is converting to tibble...
}

res_ctrl <- full_join(
        x = as_table(rho_ctrl, 'correlation_rho'),
        y = as_table(pval_ctrl, 'p_value'),
        by = c('var1', 'var2')
    ) |>
    mutate(group='WSC', .before=1)

res_ctrl <- full_join(
        x = as_table(rho_ctrl, 'correlation_rho'),
        y = as_table(pval_ctrl, 'p_value')
    ) |>
    mutate(group='WSC', .before=1)

res_case <- full_join(
        x = as_table(rho_case, 'correlation_rho'),
        y = as_table(pval_case, 'p_value')
    ) |>
    mutate(group='CC', .before=1)

res <- rbind(res_ctrl, res_case) |>
    filter(var1 != var2) |>
    mutate(adjusted_p_value = p.adjust(p_value, method='hommel'))


## output ##

output_file <- 'out/spearman-correlations.xlsx'

matrices <- list(
    `WSC corr` = rho_ctrl,
    `WSC p-value` = pval_ctrl,
    `CC corr` = rho_case,
    `CC p-value` = pval_case
) |>
    lapply(as.data.frame) |>
    lapply(rownames_to_column, var="Variable")

list(Complete=res) |>
    c(matrices) |>
    writexl::write_xlsx(output_file, format_header=FALSE)
