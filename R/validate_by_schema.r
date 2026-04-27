validate_by_schema <- function(
  ds, # data and schema
  rule_names,
  control_rules,
  transform_rules,
  validate_rules,
  rule_registry,
  self
) {
  # for each walk, rules that were operated/ignored due
  # to control flow will become error properties and
  # will be skipped
  ds$errors <- rapply(ds$schema, function(x) NULL, how = "replace")
  stages <- list(control_rules, transform_rules, validate_rules, "apply_last")

  for (stage in stages) {
    ds <- validate_rule_group(
      ds$data,
      ds$schema,
      ds$errors,
      rule_names,
      stage,
      rule_registry,
      ds$data,
      self
    )
  }

  list(data = ds$data, errors = ds$errors)
}

validate_rule_group <- function(
  data,
  schema,
  errors,
  rule_names,
  group_rules,
  rule_registry,
  full_data,
  self
) {
  # names of the schema
  schema_names <- methods::allNames(schema)

  # index rules and group rules
  all_rules_i <- schema_names %in% rule_names
  n_rules <- sum(all_rules_i)
  group_rules_i <- schema_names %in% group_rules
  non_group_rules_i <- all_rules_i & !group_rules_i

  # if any group rules, execute them
  if (any(group_rules_i)) {
    rules <- schema_names[group_rules_i]

    for (rule in rules) {
      if (!is.null(
        attr(schema[[rule]], "*__RV_SKIP__*")
      )) {
        next
      }

      res <- do_rule(
        data,
        schema[[rule]],
        rule,
        rule_registry,
        full_data,
        self
      )

      data <- res$data
      errors[rule] <- res["error"]
      if (!res$continue) {
        # control flow breaks remaining control rules -
        # break loop and this group won't be processed further,
        # but add attr to remaining non-control rules to skip
        # in future processing
        for (j in which(non_group_rules_i)) {
          attr(schema[[j]], "*__RV_SKIP__*") <- TRUE
        }
        break
      }
    }
  }

  data_names <- methods::allNames(data)

  # loop the schema
  for (i in seq_along(schema_names)) {
    if (all_rules_i[i]) {
      # if it's a rule of any kind, skip
      next
    }

    key <- schema_names[[i]]
    schema_field <- schema[[i]]
    error_field <- errors[[i]]

    if (nzchar(key)) {
      # if data has a matching name at this level, use
      if (key %in% data_names) {
        data_field <- data[[key]]
      } else {
        data_field <- NULL
      }
    } else {
      # else, use the index,
      # but adjust by removing the number ogf rules.
      # as schemas are auto-ordered rules first, the
      # index of the non-rule schema element will be
      # the index in the data minus the number of rules
      key <- i - n_rules
      if (key <= length(data)) {
        data_field <- data[[key]]
      } else {
        data_field <- NULL
      }
    }

    out <- validate_rule_group(
      data_field,
      schema_field,
      error_field,
      rule_names,
      group_rules,
      rule_registry,
      full_data,
      self
    )

    if (!is.null(out$data)) data[[key]] <- out$data
    if (!is.null(out$schema)) schema[[i]] <- out$schema
    if (!is.null(out$errors)) errors[[i]] <- out$errors
  }
  list(data = data, schema = schema, errors = errors)
}

do_rule <- function(
  data_value,
  schema_value,
  rule_name,
  rule_registry,
  full_data,
  self
) {
  if (!any(c("default", "required") %in% rule_name) && is.null(data_value)) {
    return(
      list(
        data = data_value,
        error = "No data for field.",
        continue = FALSE
      )
    )
  }

  fn <- rule_registry[[rule_name]]
  if (is.null(fn)) {
    return(
      list(
        data = data_value,
        error = paste0("Unknown rule: `", rule_name, "`."),
        continue = TRUE
      )
    )
  }

  res <- fn(
    data_value,
    schema_value,
    .data = full_data,
    .self = self
  )

  rule_output_check(res, data_value)
}

rule_output_check <- function(res, value) {
  if (is.null(res)) {
    list(
      data = value,
      error = NULL,
      continue = TRUE
    )
  } else if (!is.list(res)) {
    list(
      data = value,
      error = "Rule must return a list.",
      continue = TRUE
    )
  } else if (all(c("data", "error") %notin% names(res))) {
    list(
      data = value,
      error = "Rule list return must have `data` or `error` elements.",
      continue = TRUE
    )
  } else if (!is.null(res$error) && !is_nz_string(res$error)) {
    list(
      data = value,
      error = "Rule list `error` element must be a non-empty string.",
      continue = TRUE
    )
  } else if (!is.null(res$continue) && !is_bool(res$continue)) {
    list(
      data = value,
      error = "Rule list `continue` element must be a boolean.",
      continue = TRUE
    )
  } else {
    list(
      data = res$data %||% value,
      error = res$error,
      continue = res$continue %||% TRUE
    )
  }
}
