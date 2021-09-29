# Create the README.md file from the covidVignette.Rmd file

rmarkdown::render(input = "covidVignette.Rmd", output_format = "github_document", output_file = "README.md")