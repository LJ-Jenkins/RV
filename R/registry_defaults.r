.rv_control_rules <- c("required", "default")

.rv_transform_rules <- c("coerce", "apply")

.rv_validate_rules <- c(
  "type", "inherits", "allowed", "forbidden", "unique",
  "min_val", "max_val", "min_length", "max_length", "min_nrow", "max_nrow",
  "min_nchar", "max_nchar", "nzchar", "regex", "dependency", "dependencies",
  "predicate"
)

.rv_str_to_fn_rules <- c("apply", "apply_last", "predicate")

.rv_str_to_fn_converter <- function(str) {
  # attempt to convert a string to a function.
  # If it fails - return NULL, which will fail the is.function()
  # check, leave original string in place, and trigger an error message
  # from the (schema) rule validator
  tryCatch(
    as.function(eval(str2lang(str))),
    error = function(cnd) {
      NULL
    }
  )
}

.rv_is.date <- function(x) inherits(x, c("Date", "POSIXlt", "POSIXct"))

.rv_type_map <- function() {
  list2env(list(
    logical = is.logical,
    integer = is.integer,
    double = is.double,
    numeric = is.numeric,
    character = is.character,
    factor = is.factor,
    ordered = is.ordered,
    complex = is.complex,
    raw = is.raw,
    matrix = is.matrix,
    list = is.list,
    pairlist = is.pairlist,
    data.frame = is.data.frame,
    date = .rv_is.date,
    fn = is.function,
    environment = is.environment,
    array = is.array,
    vector = is.vector,
    expression = is.expression,
    call = is.call
  ))
}

.rv_coerce_map <- function() {
  list2env(list(
    logical = as.logical,
    integer = as.integer,
    double = as.double,
    numeric = as.numeric,
    character = as.character,
    factor = as.factor,
    ordered = as.ordered,
    complex = as.complex,
    raw = as.raw,
    matrix = as.matrix,
    list = as.list,
    pairlist = as.pairlist,
    data.frame = as.data.frame,
    date = as.Date,
    fn = as.function,
    environment = as.environment,
    array = as.array,
    vector = as.vector,
    expression = as.expression,
    call = as.call
  ))
}

#-- schema rules

.rv_schema_type_rule <- function(field, .self, ...) {
  if (is.function(field)) {
    if (length(formals(field)) != 1) {
      "Function must have one argument."
    }
  } else if (is.character(field)) {
    if (!is_nz_string(field)) {
      "Must be a length 1, non-NA character string."
    } else if (field %notin% nested_prop(.self, "Registry", "type_names")) {
      paste0("`", field, "` not found in allowed types.")
    }
  } else {
    "Must be a function or a string."
  }
}

.rv_schema_coerce_rule <- function(field, .self, ...) {
  if (is.function(field)) {
    if (length(formals(field)) != 1) {
      "Function must have one argument."
    }
  } else if (is.character(field)) {
    if (!is_nz_string(field)) {
      "Must be a length 1, non-NA character string."
    } else if (field %notin% nested_prop(.self, "Registry", "coerce_names")) {
      paste0("`", field, "` not found in allowed types.")
    }
  } else {
    "Must be a function or a string."
  }
}

.rv_schema_nz_chr_rule <- function(field, ...) {
  if (!is_nz_chr(field)) {
    "Must be a character vector with no NA's or empty strings."
  }
}

.rv_schema_bool_rule <- function(field, ...) {
  if (!is_bool(field)) {
    "Must be a single, non-NA logical value."
  }
}

.rv_schema_ne_atomic_rule <- function(field, ...) {
  if (!is_ne_atomic(field)) {
    "Must be a non-empty vector."
  }
}

.rv_schema_scalar_numeric_rule <- function(field, ...) {
  if (!is_scalar_numeric(field)) {
    "Must be a single, non-NA numeric value."
  }
}

.rv_schema_positive_scalar_integerish_rule <- function(field, ...) {
  if (!is_positive_scalar_numeric(field) || !is_integerish(field)) {
    "Must be a single, positive, non-NA integerish value."
  }
}

.rv_schema_nz_string_rule <- function(field, ...) {
  if (!is_nz_string(field)) {
    "Must be a length 1, non-NA character string."
  }
}

.rv_schema_dependency_rule <- function(field, ...) {
  if (is.list(field)) {
    if (!is_list_string_scalar_positive_integerish(field)) {
      paste(
        "Each list element must be all be either",
        "a string (name) or a positive integer (index)."
      )
    }
  } else if (is.numeric(field)) {
    if (
      length(field) == 0 || any(!is.finite(field)) ||
        any(field < 1) || any(!is_integerish(field))
    ) {
      "Indices must be positive integers."
    }
  } else if (is.character(field)) {
    if (length(field) == 0 || anyNA(field) || any(!nzchar(field))) {
      "Character inputs must have no NA's or empty strings."
    }
  } else {
    paste(
      "Must be a character, numeric, or list."
    )
  }
}

.rv_schema_dependencies_rule <- function(field, ...) {
  if (!is.list(field)) {
    return("Must be a list.")
  } else {
    for (i in seq_along(field)) {
      out <- .rv_schema_dependency_rule(field[[i]], ...)
      if (!is.null(out)) {
        return(paste0("For dependency ", i, ": ", out))
      }
    }
  }
  if (anyDuplicated(field)) {
    "Duplicate dependencies."
  }
}

.rv_schema_fn_rule <- function(field, ...) {
  if (!is.function(field)) {
    "Must be a function (or valid string)."
  }
}
# accept anything (except empty, picked up by validator)
.rv_schema_default_rule <- function(field, ...) {}

.rv_schema_rules <- function() {
  list2env(list(
    type = .rv_schema_type_rule,
    inherits = .rv_schema_nz_chr_rule,
    required = .rv_schema_bool_rule,
    allowed = .rv_schema_ne_atomic_rule,
    forbidden = .rv_schema_ne_atomic_rule,
    unique = .rv_schema_bool_rule,
    min_val = .rv_schema_scalar_numeric_rule,
    max_val = .rv_schema_scalar_numeric_rule,
    min_length = .rv_schema_positive_scalar_integerish_rule,
    max_length = .rv_schema_positive_scalar_integerish_rule,
    min_nrow = .rv_schema_positive_scalar_integerish_rule,
    max_nrow = .rv_schema_positive_scalar_integerish_rule,
    min_nchar = .rv_schema_positive_scalar_integerish_rule,
    max_nchar = .rv_schema_positive_scalar_integerish_rule,
    nzchar = .rv_schema_bool_rule,
    regex = .rv_schema_nz_string_rule,
    coerce = .rv_schema_coerce_rule,
    dependency = .rv_schema_dependency_rule,
    dependencies = .rv_schema_dependencies_rule,
    predicate = .rv_schema_fn_rule,
    apply = .rv_schema_fn_rule,
    apply_last = .rv_schema_fn_rule,
    default = .rv_schema_default_rule
  ))
}

#-- cross-rules

# cross rules are checked after individual rules, so cross rules only
# apply for fields where the individual rules are valid.
# This means that cross rule functions can assume correct type/size/etc.

.rv_cross_fn_dependency_and_dependencies <- function(field, ...) {
  if (!is.null(field$dependency) && !is.null(field$dependencies)) {
    "Cannot have both `dependency` and `dependencies` rules."
  }
}

.rv_cross_rule_dependency_and_dependencies <- list(
  rules = c("dependency", "dependencies"),
  fn = .rv_cross_fn_dependency_and_dependencies
)

.rv_cross_fn_required_and_default <- function(field, ...) {
  if (field$required && !is.null(field$default)) {
    "Cannot have `required` as TRUE and a `default` value."
  }
}

.rv_cross_rule_required_and_default <- list(
  rules = c("required", "default"),
  fn = .rv_cross_fn_required_and_default
)

.rv_cross_fn_min_val_larger_than_max_val <- function(field, ...) {
  if (field$min_val > field$max_val) {
    "`min_val` must be smaller than `max_val`."
  }
}

.rv_cross_rule_min_val_larger_than_max_val <- list(
  rules = c("min_val", "max_val"),
  fn = .rv_cross_fn_min_val_larger_than_max_val
)

.rv_cross_fn_min_length_larger_than_max_length <- function(field, ...) {
  if (field$min_length > field$max_length) {
    "`min_length` must be smaller than `max_length`."
  }
}

.rv_cross_rule_min_length_larger_than_max_length <- list(
  rules = c("min_length", "max_length"),
  fn = .rv_cross_fn_min_length_larger_than_max_length
)

.rv_cross_fn_min_nrow_larger_than_max_nrow <- function(field, ...) {
  if (field$min_nrow > field$max_nrow) {
    "`min_nrow` must be smaller than `max_nrow`."
  }
}

.rv_cross_rule_min_nrow_larger_than_max_nrow <- list(
  rules = c("min_nrow", "max_nrow"),
  fn = .rv_cross_fn_min_nrow_larger_than_max_nrow
)

.rv_cross_fn_min_nchar_larger_than_max_nchar <- function(field, ...) {
  if (field$min_nchar > field$max_nchar) {
    "`min_nchar` must be smaller than `max_nchar`."
  }
}

.rv_cross_rule_min_nchar_larger_than_max_nchar <- list(
  rules = c("min_nchar", "max_nchar"),
  fn = .rv_cross_fn_min_nchar_larger_than_max_nchar
)

.rv_cross_fn_allowed_and_forbidden_overlap <- function(field, ...) {
  if (any(field$allowed %in% field$forbidden, na.rm = TRUE)) {
    "Values in `allowed` and `forbidden` must not overlap."
  }
}

.rv_cross_rule_allowed_and_forbidden_overlap <- list(
  rules = c("allowed", "forbidden"),
  fn = .rv_cross_fn_allowed_and_forbidden_overlap
)

.rv_cross_fn_allowed_type_mismatch <- function(field, ...) {
  if (field$type %notin% class(field$allowed)) {
    "Values in `allowed` must be of the type specified in `type`."
  }
}

.rv_cross_rule_allowed_type_mismatch <- list(
  rules = c("type", "allowed"),
  fn = .rv_cross_fn_allowed_type_mismatch
)

.rv_cross_fn_forbidden_type_mismatch <- function(field, ...) {
  if (field$type %notin% class(field$forbidden)) {
    "Values in `forbidden` must be of the type specified in `type`."
  }
}

.rv_cross_rule_forbidden_type_mismatch <- list(
  rules = c("type", "forbidden"),
  fn = .rv_cross_fn_forbidden_type_mismatch
)

.rv_schema_cross_rules <- function() {
  list2env(list(
    dependency_and_dependencies = .rv_cross_rule_dependency_and_dependencies,
    required_and_default = .rv_cross_rule_required_and_default,
    min_val_larger_than_max_val = .rv_cross_rule_min_val_larger_than_max_val,
    min_length_larger_than_max_length = .rv_cross_rule_min_length_larger_than_max_length,
    min_nrow_larger_than_max_nrow = .rv_cross_rule_min_nrow_larger_than_max_nrow,
    min_nchar_larger_than_max_nchar = .rv_cross_rule_min_nchar_larger_than_max_nchar,
    allowed_and_forbidden_overlap = .rv_cross_rule_allowed_and_forbidden_overlap,
    allowed_type_mismatch = .rv_cross_rule_allowed_type_mismatch,
    forbidden_type_mismatch = .rv_cross_rule_forbidden_type_mismatch
  ))
}

#-- validator rules

.rv_validator_type_rule <- function(field, schema_field, .self, ...) {
  if (is.character(schema_field)) {
    fn <- get0(
      schema_field,
      envir = deep_prop(.self, "Schema", "Registry", "type_map")
    )
    if (!fn(field)) {
      list(error = paste0("Is not type `", schema_field, "`."))
    }
  } else {
    if (!schema_field(field)) {
      list(error = "Is not expected type.")
    }
  }
}

.rv_validator_inherits_rule <- function(field, schema_field, ...) {
  if (!inherits(field, schema_field)) {
    if (length(schema_field) == 1) {
      list(error = paste0("Does not inherit from class `", schema_field, "`."))
    } else {
      list(
        error = paste0("Does not inherit from classes ", backtick(schema_field), ".")
      )
    }
  }
}

.rv_validator_required_rule <- function(field, schema_field, ...) {
  if (schema_field && is.null(field)) {
    # if required is TRUE and field is missing
    list(error = "Field not present.", continue = FALSE)
  } else if (!schema_field && is.null(field)) {
    # if required is FALSE and field is missing, skip other rules
    list(error = NULL, continue = FALSE)
  }
}

.rv_validator_allowed_rule <- function(field, schema_field, ...) {
  if (any(field %notin% schema_field, na.rm = TRUE)) {
    list(error = "Contains value(s) not in allowed set.")
  }
}

.rv_validator_forbidden_rule <- function(field, schema_field, ...) {
  if (any(field %in% schema_field, na.rm = TRUE)) {
    list(error = "Contains value(s) in forbidden set.")
  }
}

.rv_validator_unique_rule <- function(field, schema_field, ...) {
  if (schema_field && anyDuplicated(field)) {
    list(error = "Contains duplicates.")
  }
}

.rv_validator_min_val_rule <- function(field, schema_field, ...) {
  if (any(field < schema_field, na.rm = TRUE)) {
    list(error = paste0("Value(s) must be at least ", schema_field, "."))
  }
}

.rv_validator_max_val_rule <- function(field, schema_field, ...) {
  if (any(field > schema_field, na.rm = TRUE)) {
    list(error = paste0("Value(s) must be at most ", schema_field, "."))
  }
}

.rv_validator_min_length_rule <- function(field, schema_field, ...) {
  if (length(field) < schema_field) {
    list(error = paste0("Length must be at least ", schema_field, "."))
  }
}

.rv_validator_max_length_rule <- function(field, schema_field, ...) {
  if (length(field) > schema_field) {
    list(error = paste0("Length must be at most ", schema_field, "."))
  }
}

.rv_validator_min_nrow_rule <- function(field, schema_field, ...) {
  n <- nrow(field)
  if (is.null(n)) {
    return(list(error = "Type not applicable for `nrow()`."))
  } else if (n < schema_field) {
    return(list(error = paste0("Number of rows must be at least ", schema_field, ".")))
  }
  NULL
}

.rv_validator_max_nrow_rule <- function(field, schema_field, ...) {
  n <- nrow(field)
  if (is.null(n)) {
    return(list(error = "Type not applicable for `nrow()`."))
  } else if (n > schema_field) {
    return(list(error = paste0("Number of rows must be at most ", schema_field, ".")))
  }
  NULL
}

.rv_validator_min_nchar_rule <- function(field, schema_field, ...) {
  if (any(nchar(field) < schema_field, na.rm = TRUE)) {
    list(error = paste0("Char length(s) must be at least ", schema_field, "."))
  }
}

.rv_validator_max_nchar_rule <- function(field, schema_field, ...) {
  if (any(nchar(field) > schema_field, na.rm = TRUE)) {
    list(error = paste0("Char length(s) must be at most ", schema_field, "."))
  }
}

.rv_validator_nzchar_rule <- function(field, schema_field, ...) {
  if (schema_field && any(!nzchar(field), na.rm = TRUE)) {
    list(error = "Contains empty string(s).")
  }
}

.rv_validator_regex_rule <- function(field, schema_field, ...) {
  if (any(!grepl(schema_field, field), na.rm = TRUE)) {
    list(error = paste0("String(s) do not match regex pattern `", schema_field, "`."))
  }
}

.rv_validator_coerce_rule <- function(field, schema_field, .self, ...) {
  if (is.character(schema_field)) {
    schema_field <- get0(
      schema_field,
      envir = deep_prop(.self, "Schema", "Registry", "coerce_map")
    )
  }
  list(data = schema_field(field))
}

.rv_validator_dependency_rule <- function(field, schema_field, .data, ...) {
  if (!is.list(.data)) {
    list(error = "rule only operates on list-like data inputs.")
  } else if (!has_nested_element(.data, schema_field)) {
    list(error = paste0("Missing `data", paste_as_path(schema_field), "`."))
  }
}

.rv_validator_dependencies_rule <- function(field, schema_field, .data, ...) {
  if (!is.list(.data)) {
    list(error = "rule only operates on list-like data inputs.")
  } else {
    for (path in schema_field) {
      if (!has_nested_element(.data, path)) {
        return(
          list(error = paste0("Missing `data", paste_as_path(path), "`."))
        )
      }
    }
  }
}

.rv_validator_predicate_rule <- function(field, schema_field, ...) {
  res <- if (length(formals(schema_field)) == 1L) {
    schema_field(field)
  } else {
    schema_field(field, ...)
  }

  if (!is_bool(res)) {
    list(error = "Returned non-boolean.")
  } else if (!res) {
    list(error = "Does not satisfy predicate.")
  }
}

.rv_validator_apply_rule <- function(field, schema_field, ...) {
  if (length(formals(schema_field)) == 1L) {
    list(data = schema_field(field))
  } else {
    list(data = schema_field(field, ...))
  }
}

.rv_validator_default_rule <- function(field, schema_field, ...) {
  if (is.null(field)) {
    list(data = schema_field, continue = FALSE)
  }
}

.rv_validator_rules <- function() {
  list2env(list(
    type = .rv_validator_type_rule,
    inherits = .rv_validator_inherits_rule,
    required = .rv_validator_required_rule,
    allowed = .rv_validator_allowed_rule,
    forbidden = .rv_validator_forbidden_rule,
    unique = .rv_validator_unique_rule,
    min_val = .rv_validator_min_val_rule,
    max_val = .rv_validator_max_val_rule,
    min_length = .rv_validator_min_length_rule,
    max_length = .rv_validator_max_length_rule,
    min_nrow = .rv_validator_min_nrow_rule,
    max_nrow = .rv_validator_max_nrow_rule,
    min_nchar = .rv_validator_min_nchar_rule,
    max_nchar = .rv_validator_max_nchar_rule,
    nzchar = .rv_validator_nzchar_rule,
    regex = .rv_validator_regex_rule,
    coerce = .rv_validator_coerce_rule,
    dependency = .rv_validator_dependency_rule,
    dependencies = .rv_validator_dependencies_rule,
    predicate = .rv_validator_predicate_rule,
    apply = .rv_validator_apply_rule,
    apply_last = .rv_validator_apply_rule,
    default = .rv_validator_default_rule
  ))
}
