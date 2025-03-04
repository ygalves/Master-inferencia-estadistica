<p align="center">

<img src="https://apps.uao.edu.co/apex/estudiantes/r/files/static/v370Y/logo_uao.png" width="15%"/>

<h2>UNIVERSIDAD AUTÓNOMA DE OCCIDENTE</strong></h2>

<h3>02/22/2025 CALI - COLOMBIA</strong></h3>

<h3><strong>MAESTRIA EN INTELIGENCIA ARTIFICIAL Y CIENCIA DE DATOS</strong></h3>

<h3><strong>INFERENCIA ESTADISTICA</strong></h3>

<h3><strong>ENTREGA: </strong> DESAFÍO 1</h3>

<li><strong>Profesor:</strong> Cristian E García</li>
<li><strong>Alumno:</strong> Yoniliman Galvis Aguirre</li>
<li><strong>Código:</strong> 22500214</li>

Este repositorio presenta el notebook de Rstudio, **Desafio1.Rmd** que contiene el código en R y la sustentacion teórica en markdwon del primer taller de la clase de inferencia estadística y **desafio1.R**  para sólo el código.
Para los usuarios que no deseen o no tengan RStudio pueden ver el notebook usando el archivo **Desafio1.html** desde cualquier browser.

Para quienes quieren ver el archivo usando RStudio y no lo han podido hacer, necesitarán de RStudio y un complemento, esta es una breve guia para usuarios de linux/ubuntu 22.04 y de esta forma puedan ver el notebook correctamente:

### Install R language in Linux Ubuntu 22.04
#### Update System
```bash
sudo apt-get update
sudo apt-get upgrade 

#### Get dependences
$ sudo apt install dirmngr gnupg apt-transport-https ca-certificates software-properties-common

#### Authenticate pack
$ sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

#### Get CRN repository
$ sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

#### Install R
$ sudo apt install r-base
```

#### When the libicu66 package error when installing R
Download the **libicu66** package from the website, then, navigate to the directory of the downloaded deb package using the following command.
```bash
#### Open Downloads to find libicu66 package
$ cd ~/Downloads

#### Install package
$ sudo dpkg -i libicu66_66.1-2ubuntu2_amd64.deb

#### Run the R installer again, and this time it will be installed on your Ubuntu 22.04 desktop.
$ sudo apt install r-base

#### Check R
$ sudo -i R
```
#### Install RStudio
```bash
#### Download the RStudio deb package on Ubuntu 22.04.
$ wget https://download1.rstudio.org/desktop/bionic/amd64/rstudio-2022.02.1-461-amd64.deb

#### RStudio installation using dpkg.
$ sudo dpkg -i rstudio-2022.02.1-461-amd64.deb
```
#### Opening .Rmd files[1](https://stackoverflow.com/questions/44298161/rmd-files-open-as-completely-empty)
*    Open R or RStudio: Start by opening your R console or RStudio.
*    Install the rmarkdown package: Run the following command in the console:
```bash
install.packages("rmarkdown")
```
*    Load the package: After installation, you can load the package using:

#### library(rmarkdown)
This will install and load the rmarkdown package, allowing you to create and work with R Markdown files.


<small>
[1](https://stackoverflow.com/questions/44298161/rmd-files-open-as-completely-empty)<small>: [Introduction to R Markdown - RStudio](https://rmarkdown.rstudio.com/articles_intro.html)</small>
