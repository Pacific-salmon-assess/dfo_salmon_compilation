Repository for compiling, filtering, and making publicly available Pacific salmon spawner-recruitment time-series from various sources across the Northeast Pacific.

This compilation would not be possible without the hundreds of people, agencies, and organizations that have collected salmon data over the past half century, and the generosity of those that have taken time to collate and share it with us. Individual sources for each system are detailed in the [stock info](https://github.com/Pacific-salmon-assess/dfo_salmon_compilation/tree/main/data/filtered%20datasets) file. Note that ADF&G retains intellectual property rights to data collected by or for ADF&G, any dissemination of the data provided by ADF&G must credit ADF&G as the source, with a disclaimer that exonerates the department for errors or deficiencies in reproduction, subsequent analysis, or interpretation. 

Researchers are encouraged to use these data for analyses of their own. Over time we will add links to papers that have used the dataset here in the readme. If you do use these data, please drop us (Brendan Connors, Brendan.Connors(at)dfo-mpo.gc.ca; Dan Greenberg, Dan.Greenberg(at)dfo-mpo.gc.ca) a note before you start to let us know. These data are being used in a number of active projects and this can help to ensure any work undertaken is complimentary and not duplicative of work underway.

## Code
- `data_filtering.R`: code to combine all the raw data and filter through the various time-series.

- `functions.R`: all functions written for the analysis are placed in this file.

## Data Folders & Files
- [raw data](https://github.com/Pacific-salmon-assess/dfo_salmon_compilation/tree/main/data/raw%20data): contains original datasets obtained from collaborators or previously published databases, categorized by species.

- [filtered data](https://github.com/Pacific-salmon-assess/dfo_salmon_compilation/tree/main/data/filtered%20datasets): time stamped outputs from combining and filtering through the raw datasets. The final outputs include two files 'stock_info' (including stock metadata) and 'salmon_productivity_compilation' (the spawner-recruit time-series) affixed with their date of creation. Check you are using the most recent release.
