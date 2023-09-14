<!-- Please do not edit the README.md file as it is auto-generated. Only edit the README.Rmd file -->

# Clinical Visual Analytics for Review and Submission

<!-- badges: start -->

[![R-CMD-check](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/lint.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/lint.yaml)
[![Style](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/styler.yaml/badge.svg)](https://github.com/pfizer-rd/spa-r-ae-summaries/actions/workflows/styler.yaml)
<!-- badges: end -->

Clinical Visual Analytics for Review and Submission

## Purpose

Development of an open-source tool and package to enable generation of
identified interactive plots for clinical review and direct inclusion in
submission for regulatory agencies. The initial scope is to develop a
package to generate interactive forest and volcano plots for adverse
event and FDA Medical Queries (FMQs) analysis outputs for inclusion in
submissions to the FDA. This work is a collaboration among the American
Statistical Association (ASA), PHUSE, and FDA.

### Dependencies

The latest version of the package works with the latest versions of the
packages stated in `DESCRIPTION`.

If a previous version of the package should be used, it is recommended
to use latest version of the dependencies at the point of time when the
previous version of {cvars} was released.

## Scope

-   Build a toolbox of re-usable functions and utilities to create
    Clinical Domain tables and figures.
-   Clinical Domain datasets which are used to create tables and figures
    should follow CDISC standards.
