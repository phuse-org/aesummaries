<!-- Please do not edit the README.md file as it is auto-generated. Only edit the README.Rmd file -->

# Clinical Visual Analytics for Review and Submission

<!-- badges: start -->

[![R-CMD-check](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/lint.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/lint.yaml)
[![Style](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/styler.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/styler.yaml)
<!-- badges: end -->

Application to generate/visualize Adverse Events related tables and
figures.

## Purpose

The purpose of this Application is to visualize the AE (Adverse Event)
dataset through Interactive R shiny Graphs (Volcano and Forest plot). In
this Application, we can generate the plots which will be according to
the filters that we select. Since we have many table of tables to
visualize certain statistics and measures for AE, we can simply use this
app to display plots for all the combinations

### Dependencies

The latest version of the package works with the latest versions of the
packages stated in `DESCRIPTION`.

If a previous version of the package should be used, it is recommended
to use latest version of the dependencies at the point of time when the
previous version of {cvars} was released.

## Scope

-   Build a toolbox of re-usable functions and utilities to create
    AE-specific tables and figures.
-   AE datastes which is used to create tables and figures should follow
    CDISC standards.
