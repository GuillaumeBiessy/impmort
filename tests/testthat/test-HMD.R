test_that("HMD import works", {

  username <- "guillaume.biessy78@gmail.com"
  password <- "PdsuQui5LqCvwK!"

  HMD <- c(Grenouilles = "France",
           Rosbeefs = "U.K.") |>
    get_HMD_data(username, password)
  expect_true(c("Grenouilles", "Rosbeefs") %in% HMD$Country |> all())
})
