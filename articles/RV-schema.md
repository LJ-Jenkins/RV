# Creating RV Schemas

``` r

library(RV)
```

## RV Schemas

RV schemas are nested list objects that are passed to the `Schema`
class. This class automatically re-orders and validates the input list
schema. The `Schema` class object is then passed to the `Validator`
class for data validation.

## Creating a schema list

The lists used for RV schemas are nested list objects with rule-named
leaf elements that determine validation behaviour. The names or
positions of the nested lists are used to match their rules to the
corresponding data elements.

``` r

list(
  type = "data.frame",
  id = list(
    type = "numeric"
  ),
  email = list(
    type = "character",
    regex = "@gmail.com$"
  ),
  list(
    min_length = 2
  )
)
```

Rules at the top level of the nested list are applied to the whole data
object. Nested list elements are matched to data elements by their name
if present or by position if there is no name present.

When matching by position, rule elements are first removed, so the first
non-rule element will always be matched against `[[1]]` of the matching
data node. See the following illustrations:

    #> <list>
    #> ├─Top level rule: "Applied to `data`."
    #> └─<list>
    #>   ├─Depth 1 rule: "Applied to `data[[1]]`."
    #>   └─<list>
    #>     └─Depth 2 rule: "Applied to `data[[1]][[1]]`."

    #> <list>
    #> ├─Top level rule: "Applied to `data`."
    #> └─x: <list>
    #>   ├─Depth 1 rule: "Applied to `data[['x']]`."
    #>   └─x: <list>
    #>     └─Depth 2 rule: "Applied to `data[['x']][['x']]`."

This behaviour continues no matter the level of nesting, so it is
possible to apply rules to deeply nested values.

    #> <list>
    #> └─<list>
    #>   └─<list>
    #>     └─<list>
    #>       └─"Applied to `data[[1]][[1]][[1]]`."

### Double hits with positional matching

When matching schema elements to data, the `Validator` first attempts to
match by name before falling back to positional matching. As data
elements are not flagged when validated (and thus can be validated
multiple times), this can cause unexpected behaviour. See the following
example where the data is matched twice:

``` r

Validator(
  data = list(x = 1L),
  schema = list(
    list(type = "integer"), # matched positionally
    x = list(type = "character") # matched by name to same element
  )
)@errors
#> [[1]]
#> [[1]]$type
#> NULL
#> 
#> 
#> $x
#> $x$type
#> [1] "Is not type `character`."
```

It is strongly encouraged to use fully named schemas/data unless you are
certain about their structure.

## Using the Schema class

The `Schema` class takes the nested list and re-orders it, transforms
certain rules from strings to functions where necessary, then validates
the schema.

### Schema re-ordering

Rules are applied in three separate passes according to their category:
‘control’, ‘transform’, and ‘validate’, with a special rule ‘apply_last’
occurring in a final pass.

For each category, the `Schema` reorders the list upon ingest according
to the corresponding orders of the name properties in the `Registry` (by
default `Schema` uses a default, uncustomised `Registry` if one is not
provided):

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

Schema(
  list(
    min_length = 2L,
    type = "integer",
    default = 10L,
    coerce = "double"
  )
)@schema
#> $default
#> [1] 10
#> 
#> $coerce
#> [1] "double"
#> 
#> $type
#> [1] "integer"
#> 
#> $min_length
#> [1] 2
```

The order of the rules within each of the `Registry` properties can be
edited to specify a different order, which can then be fed to the
`Schema`:

``` r

r <- Registry()
r@validate_rules <- c("min_length", r@validate_rules[!grepl("min_length", r@validate_rules)])

Schema(
  schema = list(
    min_length = 2L,
    type = "integer",
    default = 10L,
    coerce = "double"
  ),
  registry = r
)@schema
#> $default
#> [1] 10
#> 
#> $coerce
#> [1] "double"
#> 
#> $min_length
#> [1] 2
#> 
#> $type
#> [1] "integer"
```

For more information about the builtin rules and how they operate, or
how to add custom rules to a `Registry`, see the [builtin rules
vignette](https://lj-jenkins.github.io/RV/articles/RV-rules.md) and the
adding rules vignette. .

### String to function conversion

Certain rules can be given character strings as an input which are
turned into functions during schema validation. The rules that this
apply to can be found in the `Registry`, along with the function that
does the conversion. Both can be edited.

BEWARE: No check is made on the content of the string, so use the
builtin converter with extreme care for user inputs - it is vulnerable
to code injection. This functionality can be removed by simply making
the `@str_to_fn_rules` property an empty character.

``` r

r <- Registry()
r@str_to_fn_rules
#> [1] "apply"      "apply_last" "predicate"

r@str_to_fn_converter
#> function (str) 
#> {
#>     tryCatch(as.function(eval(str2lang(str))), error = function(cnd) {
#>         NULL
#>     })
#> }
#> <bytecode: 0x5595df3f8f78>
#> <environment: namespace:RV>

Schema(
  list(predicate = "function(x) x > 10")
)@schema
#> $predicate
#> function (x) 
#> x > 10
#> <environment: 0x5595e0976398>
```

### Schema validation

`Schema` objects validate their list input and store an `@errors`
property that highlights validation errors.

For each schema node it validates that:

- There are no duplicate names.
- All leaf elements are named.
- Leaf elements are name with recognised rules.

``` r

Schema(
  list(
    x = list(type = "character"),
    x = list(type = "integer"),
    list("character"),
    list(my_rule = 1L)
  )
)@errors
#> $x
#> [1] "Names must be unique at the same depth."
#> 
#> $x
#> [1] "Names must be unique at the same depth."
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] "Schema leafs must be named with rules."
#> 
#> 
#> [[4]]
#> [[4]]$my_rule
#> [1] "Unknown rule: `my_rule`."
```

#### Rule validation

Each rule has an associated schema validation rule that checks the value
given. For example, the ‘predicate’ rule checks that given values are
either strings or functions. The ‘dependency’ rule checks that given
values are either a character vector (names), a numeric integerish
vector, or a non-nested list containing a mix of the two.

``` r

Schema(
  list(
    predicate = 1L,
    dependency = 1.5
  ),
  error = TRUE
)
#> Error:
#> ! <RV::Schema> object is invalid:
#> - Schema validation failed with the following errors:
#> ├─ dependency: Indices must be positive integers.
#> └─ predicate: Must be a function (or valid string).
```

#### Cross rule validation

There are also cross rules that check if the values of multiple rules
clash (if the individual rule components are themselves valid). For
example the ‘min_val_larger_than_max_val’ rule does what it says on the
tin:

``` r

Schema(
  list(
    min_val = 5,
    max_val = 1
  ),
  error = TRUE
)
#> Error:
#> ! <RV::Schema> object is invalid:
#> - Schema validation failed with the following errors:
#> ├─ min_val: `min_val` must be smaller than `max_val`.
#> └─ max_val: `min_val` must be smaller than `max_val`.
```

For full information about each rule and their validation, see
[`vignette('RV-rules')`](https://lj-jenkins.github.io/RV/articles/RV-rules.md).

## Key takeaways

- RV schemas are nested lists where the structure determines data
  matching.
- Nested lists are passed to the `Schema` class, which orders and
  validates a schema list.
- Each schema node applies to one level ‘above’ of the data.
- Rule elements are ignored when matching by position.
- Name matching takes priority over positional matching.
