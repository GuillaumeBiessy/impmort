username <- "guillaume.biessy78@gmail.com"
password <- "PdsuQui5LqCvwK!"

HMD <- c(Grenouilles = "France",
         Rosbeefs = "U.K.") |>
  get_HMD_data(username, password)
c("Grenouilles", "Rosbeefs") %in% HMD$Country |> all() |> is_equivalent_to(TRUE)

FRD <- get_FRD_data()
(FRD$Region |> unique() |> length() == 13) |> is_equivalent_to(TRUE)
