## Main file for generating html docs
library(git2r)
repo <- repository()

#gs4_auth()
rmarkdown::render("osca_diagramme.Rmd", output_file="diagramme.html", output_dir = "./docs/")
rmarkdown::render("om.Rmd", output_file="om.html", output_dir = "./docs/")
rmarkdown::render("osca.Rmd", output_file="osca.html", output_dir = "./docs/")

commit(repo, "Commit from main")
