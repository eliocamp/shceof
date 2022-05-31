xdg-open http://localhost:8787/
docker run --rm -p 8787:8787 -e DISABLE_AUTH=true -v $(pwd):/home/rstudio/shceof -v /home/rstudio/shceof/renv eliocamp/shceof
