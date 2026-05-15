# RV Builtin Rules

``` r

library(RV)
```

RV has 23 validation rules and 9 cross rules.

### Validation rules

``` r

Registry()@rule_names
#>  [1] "required"     "default"      "coerce"       "apply"        "type"        
#>  [6] "inherits"     "allowed"      "forbidden"    "unique"       "min_val"     
#> [11] "max_val"      "min_length"   "max_length"   "min_nrow"     "max_nrow"    
#> [16] "min_nchar"    "max_nchar"    "nzchar"       "regex"        "dependency"  
#> [21] "dependencies" "predicate"    "apply_last"
```

The builtin rules are categorised by their type: ‘control’, ‘transform’
and ‘validate’. When the `Validator` is run, rules are applied in three
passes according to these categories, with a special final pass for the
‘apply_last’ rule.

Rules within each category are applied in the order they appear in their
respective `Registry` property.

``` r

r <- Registry()
r@control_rules
#> [1] "required" "default"
r@transform_rules
#> [1] "coerce" "apply"
r@validate_rules
#>  [1] "type"         "inherits"     "allowed"      "forbidden"    "unique"      
#>  [6] "min_val"      "max_val"      "min_length"   "max_length"   "min_nrow"    
#> [11] "max_nrow"     "min_nchar"    "max_nchar"    "nzchar"       "regex"       
#> [16] "dependency"   "dependencies" "predicate"
```

Each validation rule has two functions associated with it:

- A function that validates the given schema value.

``` r

Schema(list(type = 1L), error = TRUE)
#> Error:
#> ! <RV::Schema> object is invalid:
#> - Schema validation failed with the following errors:
#> └─ type: Must be a function or a string.
```

- A function that uses the schema value to validate data.

``` r

Validator(
  data = 1L,
  schema = list(type = "character"),
  error = TRUE
)
#> Error:
#> ! <RV::Validator> object is invalid:
#> - Data validation failed with the following errors:
#> └─ type: Is not type `character`.
```

### Cross rules

Cross rules operate within the `Schema`, checking that the values of two
or more schema rules don’t clash.

``` r

Schema(
  list(
    min_length = 5,
    max_length = 1
  ),
  error = TRUE
)
#> Error:
#> ! <RV::Schema> object is invalid:
#> - Schema validation failed with the following errors:
#> ├─ min_length: `min_length` must be smaller than `max_length`.
#> └─ max_length: `min_length` must be smaller than `max_length`.
```

## Rule information

### Validation rules

[TABLE]

### Cross rules

[TABLE]
