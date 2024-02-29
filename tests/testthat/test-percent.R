test_that("percentages", {
  perc <- seq(0, 100, 0.1)
  prop <- perc / 100
  set.seed(42)
  x <- sample(perc, 10^3, replace = TRUE)
  print_percent <- print(percent(perc), max = 0)
  print_percent2 <- print(percent())
  print_percent3 <- print(percent(200), max = NULL)
  expect_error(percent(letters))
  expect_identical(as.character(percent()), character())
  expect_identical(format(percent(numeric())), character())
  expect_identical(format(percent(1:10), symbol = " (%)"),
                   paste(format(1:10), "(%)"))
  expect_identical(print_percent,
                   percent(perc))
  expect_identical(print_percent2,
                   percent())
  expect_identical(print_percent3,
                   percent(200))
  expect_identical(100 * percent(perc),
                   100 * prop)
  expect_identical(100 / percent(perc),
                   100 / prop)
  expect_identical(100 + percent(perc),
                   100 + prop)
  expect_identical(100 - percent(perc),
                   100 - prop)
  expect_identical(percent(perc) * 100,
                   prop * 100)
  expect_identical(percent(perc) / 100,
                   prop / 100)
  expect_identical(percent(perc) + 100,
                   prop + 100)
  expect_identical(percent(perc) - 100,
                   prop - 100)
  expect_identical(percent(25) * percent(perc),
                   new_percent(0.25 * prop))
  expect_identical(percent(25) / percent(perc),
                   new_percent(0.25 / prop))
  expect_identical(percent(25) + percent(perc),
                   new_percent(0.25 + prop))
  expect_identical(percent(25) - percent(perc),
                   new_percent(0.25 - prop))
  expect_identical(
    unique(percent(x)),
    percent(unique(x))
  )
  expect_identical(
    factor(percent(x)),
    factor(percent(x), levels = percent(sort(unique(x))))
  )
  expect_identical(
    percent(x),
    percent(x)[seq_along(x)]
  )
  expect_identical(
    rep_len(percent(x), 0),
    percent(x)[0]
  )
  expect_identical(
    rep_len(percent(x), 20),
    percent(x)[1:20]
  )
  expect_identical(
    rep(percent(x), each = 3),
    percent(rep(x, each = 3))
  )
})
