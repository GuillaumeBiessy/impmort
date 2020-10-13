username <- "guillaume.biessy78@gmail.com"
password <- "PdsuQui5LqCvwK!"

HMD <- list(Grenouilles = "France",
            Rosbeefs = "Royaume_Uni") %>%
  get_HMD_data(username, password)

is_equivalent_to(c("Grenouilles", "Rosbeefs") %in% HMD$Country %>% all, TRUE)

FRD <- get_FRD_data()
is_equivalent_to(FRD$Region %>% unique %>% length == 13, TRUE)
