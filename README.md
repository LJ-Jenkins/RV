
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RV <img src="man/figures/logo.png" align="right" height="17%" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/LJ-Jenkins/RV/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LJ-Jenkins/RV/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R Validator for the validation of data against user-defined schemas.

This package is under development and subject to change constantly.

## Installation

You can install the development version of RV like so:

``` r
# install.packages("pak")
pak::pak("LJ-Jenkins/RV")
```

## Basic Usage

``` r
library(RV)

df <- data.frame(x = 1:3, y = c(" a", "b  ", " c"))

v <- Validator(
  data = df,
  schema = list(
    type = "data.frame",
    min_nrow = 1,
    x = list(
      type = "numeric",
      max_val = 5
    ),
    y = list(
      type = "character",
      apply = "function(x) trimws(x)",
      nzchar = TRUE
    )
  )
)

# Specified data is transformed
v@data
#>   x y
#> 1 1 a
#> 2 2 b
#> 3 3 c

# Overall validity
v@valid
#> [1] TRUE

# Structured errors property
v@errors
#> $type
#> NULL
#> 
#> $min_nrow
#> NULL
#> 
#> $x
#> $x$type
#> NULL
#> 
#> $x$max_val
#> NULL
#> 
#> 
#> $y
#> $y$apply
#> NULL
#> 
#> $y$type
#> NULL
#> 
#> $y$nzchar
#> NULL

# Informative errors
Validator(
  data = list(1, a = "a", b = 10),
  schema = list(
    list(type = "character"),
    a = list(min_nchar = 2),
    b = list(min_length = 2, max_val = 5)
  ),
  error = TRUE
)
#> Error:
#> ! <RV::Validator> object is invalid:
#> - Data validation failed with the following errors:
#> ├─ [[1]]
#> │ └─ type: Is not type `character`.
#> ├─ a
#> │ └─ min_nchar: Char length(s) must be at least 2.
#> └─ b
#>   ├─ max_val: Value(s) must be at most 5.
#>   └─ min_length: Length must be at least 2.

# Extensible
s <- Schema(list(my_val_is_five = TRUE))
s@valid
#> [1] FALSE

s <- add_rule(
  s,
  name = "my_val_is_five",
  validator_fn = function(field, schema_field, ...) {
    field == 5
  },
  schema_fn = function(schema_field, ...) {
    isTRUE(schema_field) || isFALSE(schema_field)
  },
  rule_type = "validate"
)
s@valid
#> [1] FALSE

Validator(data = 5, schema = s)@valid
#> [1] FALSE
```

## Overview

RV provides three
[S7](https://cran.r-project.org/web/packages/S7/index.html) classes:
`Registry`, `Schema`, and `Validator`.

`Registry` defines rules and stores all built-in RV rule names and
definitions.

``` r
r <- Registry()

S7::prop_names(r)
#>  [1] "rule_names"          "control_rules"       "transform_rules"    
#>  [4] "validate_rules"      "str_to_fn_rules"     "str_to_fn_converter"
#>  [7] "type_names"          "type_map"            "coerce_names"       
#> [10] "coerce_map"          "schema_rules"        "cross_rule_names"   
#> [13] "cross_rules"         "validator_rules"
```

`Schema` takes a user-defined nested list schema, validates the schema,
and reorders the schema according to the order defined in the
`Registry`. By default `Schema` creates a `Registry` if one is not
passed to the function.

``` r
s <- Schema(list(type = "integer", default = 1L))

s@schema
#> $default
#> [1] 1
#> 
#> $type
#> [1] "integer"

S7::prop_names(s)
#> [1] "schema"           "errors"           "Registry"         ".schema_cache"   
#> [5] "error"            "error_print_opts" "valid"
```

`Validator` takes data and a user-defined `Schema`, and applies each
`Schema` field against the data. It does this in three passes, first
applying ‘control’ rules, then ‘transform’ rules, then ‘validate’ rules.
A list given as a schema will be passed to `Schema()` on ingest.

``` r
v <- Validator(
  data = list(a = 1, b = "Hello"),
  schema = list(
    a = list(
      type = "numeric",
      min_val = 0,
      max_val = 5
    ),
    b = list(
      type = "character",
      apply = "\\(x) paste(x, 'World!')"
    ),
    c = list(
      required = FALSE,
      type = "data.frame"
    ),
    d = list(
      default = 10L
    )
  )
)

v@data
#> $a
#> [1] 1
#> 
#> $b
#> [1] "Hello World!"
#> 
#> $d
#> [1] 10

S7::prop_names(v)
#> [1] "data"             "Schema"           "errors"           ".validator_cache"
#> [5] "error"            "valid"
```

## Vignettes

For detailed information on using RV, see the vignettes:

*[Builtin rules in
RV](https://lj-jenkins.github.io/RV/articles/RV-rules.html) *[Creating
RV Schemas](https://lj-jenkins.github.io/RV/articles/RV-schemas.html)

(In development) *Validating data with RV *Adding rules to RV

## Extending RV

To add your own rules to RV, use the `add_rule` variants:

``` r
data <- structure(1L, my_attr = "Hi")

mySchema <- Schema(list(check_my_attr = 1L))
mySchema@errors
#> $check_my_attr
#> [1] "Unknown rule: `check_my_attr`."

mySchema <- add_rule(
  obj = mySchema,
  name = "check_my_attr",
  validator_fn = function(data_field, schema_field, ...) {
    if (attr(data_field, "my_attr") != schema_field) {
      list(error = "Data doesn't match schema 'my_attr'.")
    }
  },
  schema_fn = function(schema_field, ...) {
    if (!is.character(schema_field) || length(schema_field) != 1L) {
      "Must be length 1 character"
    }
  },
  rule_type = "validate"
)

mySchema@errors
#> $check_my_attr
#> [1] "Must be length 1 character"

mySchema@schema$check_my_attr <- "Hi"
Validator(data, mySchema)@valid
#> [1] TRUE
```

## Note

RV was inspired by and modelled on Python’s
[Cerberus](https://docs.python-cerberus.org/). Error printing in RV was
modelled on [lobstr](https://lobstr.r-lib.org/)’s tree function.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/LJ-Jenkins/RV/issues).

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://lj-jenkins.github.io/RV/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
