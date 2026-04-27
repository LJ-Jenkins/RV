test_that("Schema apply_last rule: string or function passes, others fail", {
  expect_true(Schema(list(apply_last = function(x) x))@valid)
  expect_true(Schema(list(apply_last = r"{\(x) x}"))@valid)
  expect_true(Schema(list(apply_last = "\\(x) x"))@valid)
  expect_true(Schema(list(apply_last = "character"))@valid)

  expect_false(Schema(list(apply_last = ""))@valid)
  expect_false(Schema(list(apply_last = c(r"{\(x) x}", r"{\(x) x}")))@valid)
  expect_false(Schema(list(apply_last = NA_character_))@valid)
  expect_false(Schema(list(apply_last = c("character", "numeric")))@valid)
  expect_false(Schema(list(apply_last = 123))@valid)
  expect_error(Schema(list(apply_last = 123), error = TRUE))
})

test_that("Validator apply_last rule: error if no field", {
  v <- Validator(
    list(name = "test"),
    Schema(list(name2 = list(apply_last = "\\(x) x")))
  )
  expect_false(v@valid)
  expect_equal(v@errors$name2$apply, "No data for field.")
})

test_that("Validator apply_last rule: applies if value present", {
  v <- Validator(
    list(x = "123", y = 123, z = list(x = 1, y = 2), list(1)),
    Schema(list(
      x = list(apply_last = "\\(x) as.numeric(x)"),
      y = list(apply_last = "\\(x) as.character(x)"),
      z = list(apply_last = "\\(x) as.data.frame(x)"),
      list(list(apply_last = "\\(x) x + 9"))
    ))
  )
  expect_true(v@valid)
  expect_equal(
    v@data,
    list(
      x = 123,
      y = "123",
      z = data.frame(x = 1, y = 2),
      list(10)
    )
  )
})

test_that("Validator apply_last rule: occurs after every other rule", {
  v <- Validator(
    list(a = "1", b = 1),
    Schema(list(
      a = list(
        apply_last = "\\(x) if (is.numeric(x)) x + 9",
        coerce = "numeric"
      ),
      b = list(
        apply_last = "\\(x) if (is.numeric(x)) x + 9",
        apply = "\\(x) as.numeric(x)"
      )
    ))
  )

  expect_true(v@valid)
  expect_equal(
    v@data,
    list(
      a = 10,
      b = 10
    )
  )
})
