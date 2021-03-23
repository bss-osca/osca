## Main file for generating 
#gs4_auth()
rmarkdown::render("osca_diagramme.Rmd", output_file="diagramme.html", output_dir = "./docs/")
rmarkdown::render("om.Rmd", output_file="om.html", output_dir = "./docs/")
rmarkdown::render("osca.Rmd", output_file="osca.html", output_dir = "./docs/")
