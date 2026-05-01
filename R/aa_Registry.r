#' @title
#' Create a Registry Object
#' @description
#' An S7 class that defines rules for validating data against a [Schema].
#' @param x
#' object to be tested.
#' @returns
#' An S7 `Registry` object with the following properties:
#' \describe{
#'   \item{`rule_names`}{All rule names, in order of evaluation.
#'     Derived from `control`/`transform`/`validate` rules with
#'     `apply_last` added to the end.}
#'   \item{'control_rules}{Rules that influence the control flow.
#'     These will be applied in the first pass when validating data.}
#'   \item{'transform_rules'}{Rules that transform data.
#'     These will be applied in the second pass when validating data.}
#'   \item{'validate_rules'}{Rules that validate data.
#'     These will be applied in the penultimate pass when validating
#'     (the last pass is the special `apply_last` pass - see details).}
#'   \item{`str_to_fn_rules`}{Schema rules that are allowed to have string
#'     or function values, with string values being converted to functions
#'     automatically during schema validation.}
#'   \item{`str_to_fn_converter`}{A function that converts string values to
#'     functions for the `str_to_fn_rules`.}
#'   \item{`type_names`}{A character vector of allowed type names, derived
#'     from the keys of the `type_map` environment.}
#'   \item{`type_map`}{An environment mapping type names to type definition
#'      functions.}
#'   \item{`coerce_names`}{A character vector of allowed coercion rule names,
#'     derived from the keys of the `coerce_map` environment.}
#'   \item{`coerce_map`}{An environment mapping type names to coercion
#'      functions.}
#'   \item{`rule_names`}{A character vector of all (non-cross) rule names.}
#'   \item{`schema_rules`}{An environment containing the schema rule
#'     functions.}
#'   \item{`cross_rule_names`}{A character vector of allowed schema cross
#'    rule names, derived from the keys of the `cross_rules`
#'    environment.}
#'   \item{`cross_rules`}{An environment containing the schema cross
#'     rule functions (rules that check relationships between multiple schema
#'     rule values).}
#'   \item{`validator_rules`}{An environment containing the validator rule
#'     functions.}
#' }
#' @details
#' The `Registry` class serves as a central repository for all the rules used in
#' schema and data validation for `RV` based workflows. It includes built-in
#' rules for common validation tasks and allows for the addition of custom
#' rules.
#'
#' Registry objects are automatically created in `Schema` objects
#' (which are passed to `Validator` objects), and rules can be added to the
#' registry directly within those objects by using the rule-adding generic
#' functions. It is therefore not necessary to create a `Registry` object
#' separately if one does not wish.
#'
#' However, `Schema` and `Validator` objects are automatically re-validated
#' when rules are added, so for the addition of many new rules, it is
#' beneficial to create a `Registry` object, add all the rules to it, and
#' then pass it to the other classes.
#' @seealso [Schema] and [Validator] constructors. [add_rule] for adding
#' rules to a registry.
#' @examples
#' r <- Registry()
#' s <- Schema(list(type = "integer"), registry = r)
#'
#' is.Registry(r)
#' @export
Registry <- S7::new_class(
  "Registry",
  properties = list(
    rule_names = S7::new_property(
      getter = function(self) {
        c(
          S7::prop(self, "control_rules"),
          S7::prop(self, "transform_rules"),
          S7::prop(self, "validate_rules"),
          "apply_last"
        )
      }
    ),
    control_rules = S7::class_character,
    transform_rules = S7::class_character,
    validate_rules = S7::class_character,
    str_to_fn_rules = S7::class_character,
    str_to_fn_converter = S7::new_property(
      S7::class_function,
      validator = function(value) {
        if (length(formals(value)) != 1L) {
          "must be a function that takes exactly one argument."
        }
      }
    ),
    type_names = env_names_prop("type_map"),
    type_map = env_prop("type_map"),
    coerce_names = env_names_prop("coerce_map"),
    coerce_map = env_prop("coerce_map"),
    schema_rules = env_prop("schema_rules"),
    cross_rule_names = env_names_prop("cross_rules"),
    cross_rules = env_prop("cross_rules"),
    validator_rules = env_prop("validator_rules")
  ),
  validator = function(self) {
    if (anyDuplicated(S7::prop(self, "rule_names"))) {
      # we only need to check rule_names as it is a combination of the others
      "@rule_names must not contain duplicates."
    } else if (any(
      S7::prop(self, "str_to_fn_rules") %notin% S7::prop(self, "rule_names")
    )) {
      paste(
        "@str_to_fn_rules contains names of rules not",
        "in @rule_names."
      )
    } else if (any(
      ls(S7::prop(self, "schema_rules")) %notin% S7::prop(self, "rule_names")
    )) {
      paste(
        "@schema_rules contains names of rules not",
        "in @rule_names."
      )
    } else if (any(
      S7::prop(self, "rule_names") %notin% ls(S7::prop(self, "schema_rules"))
    )) {
      paste(
        "@rule_names contains names of rules not",
        "in @schema_rules."
      )
    } else if (any(
      ls(S7::prop(self, "validator_rules")) %notin% S7::prop(self, "rule_names")
    )) {
      paste(
        "@validator_rules contains names of rules not",
        "in @rule_names."
      )
    } else if (any(
      S7::prop(self, "rule_names") %notin% ls(S7::prop(self, "validator_rules"))
    )) {
      paste(
        "@rule_names contains names of rules not",
        "in @validator_rules."
      )
    } else if (any(
      S7::prop(self, "cross_rule_names") %in% S7::prop(self, "rule_names")
    )) {
      paste(
        "@cross_rule_names must be different from",
        "@rule_names."
      )
    } else if (cross_rule_operating_names_check(
      S7::prop(self, "cross_rules"),
      S7::prop(self, "rule_names")
    )) {
      paste0(
        "@cross_rules contains names of rules to operate on that ",
        "are not in @rule_names."
      )
    }
  },
  constructor = function() {
    S7::new_object(
      S7::S7_object(),
      control_rules = .rv_control_rules,
      transform_rules = .rv_transform_rules,
      validate_rules = .rv_validate_rules,
      str_to_fn_rules = .rv_str_to_fn_rules,
      str_to_fn_converter = .rv_str_to_fn_converter,
      type_map = .rv_type_map(),
      coerce_map = .rv_coerce_map(),
      schema_rules = .rv_schema_rules(),
      cross_rules = .rv_schema_cross_rules(),
      validator_rules = .rv_validator_rules()
    )
  }
)

#' @rdname Registry
#' @export
is.Registry <- function(x) {
  S7::S7_inherits(x, Registry)
}
