## Main file for generating html docs
library(git2r)

# gs4_auth()
rmarkdown::render("osca_diagramme.Rmd", output_file="diagramme.html", output_dir = "./docs/")
rmarkdown::render("om.Rmd", output_file="om.html", output_dir = "./docs/")
rmarkdown::render("osca.Rmd", output_file="osca.html", output_dir = "./docs/")

repo <- repository()
commit(repo, paste0("Commit from main.R ", date()), all = T)
system("git push")
