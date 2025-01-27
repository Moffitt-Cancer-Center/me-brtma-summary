# ME-BRTMA-TMA Analysis
This is an experiment folder, for performing R-based data science experiments. There is a fair amount of infrastructure here to
outline which is done below.

## Raw Data
The project is designed to be completely reproducible from a series of raw data files. These files are stored in `data-raw` and
can be built into analysis objects (in the `data` directory). You can build this automatically by issuing the following command:
`make data-raw`.

NOTE: We are using `dvc` for managing the data files (which tend to be large). Therefore, if you are checking out this project
from git you will need to retrieve the corresponding files from `dvc` as well.

## Output
The primary output of the project consists of a series of HTML pages (static or shiny web apps) that can be deployed to a web server. 
Using quarto, the web pages are all rendered and stored under the `public/` directory. The Makefile can be used (or just `quarto render`)
to build the files. However, to deploy the files we use the Makefile.

