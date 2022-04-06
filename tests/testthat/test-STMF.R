test_that("STMF import works", {

  username <- "guillaume.biessy78@gmail.com"
  password <- "PdsuQui5LqCvwK!"

  STMF <- get_STMF_data(username, password)
  expect_silent(STMF)
})
