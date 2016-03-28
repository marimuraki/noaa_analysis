Source: Coursera Reproducible Research

## Introduction
## 
It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone
Up](https://jawbone.com/up). These type of devices are part of the
"quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

## Data
## 
The data for this assignment come in the form of a comma-separated-value
file compressed via the bzip2 algorithm to reduce its size. You can
download the file from the course web site:

* Dataset: [Storm
Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.
csv.bz2) [46Mb]

There is also some documentation of the database available. Here you
will find how some of the variables are constructed/defined.

* National Weather Service [Storm Data
Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc
%2Fpd01016005curr.pdf) * National Climatic Data Center Storm Events
[FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%
20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November
2011. In the earlier years of the database there are generally fewer
events recorded, most likely due to a lack of good records. More recent
years should be considered more complete.

## Review criteria
## 
1. Has either a (1) valid RPubs URL pointing to a data analysis document
for this assignment been submitted; or (2) a complete PDF file
presenting the data analysis been uploaded? 2. Is the document written
in English? 3. Does the analysis include description and justification
for any data transformations? 4. Does the document have a title that
briefly summarizes the data analysis? 5. Does the document have a
synopsis that describes and summarizes the data analysis in less than 10
sentences? 6. Is there a section titled "Data Processing" that describes
how the data were loaded into R and processed for analysis? 7. Is there
a section titled "Results" where the main results are presented? 8. Is
there at least one figure in the document that contains a plot? 9. Are
there at most 3 figures in this document? 10. Does the analysis start
from the raw data file (i.e. the original .csv.bz2 file)? 11. Does the
analysis address the question of which types of events are most harmful
to population health? 12. Does the analysis address the question of
which types of events have the greatest economic consequences? 13. Do
all the results of the analysis (i.e. figures, tables, numerical
summaries) appear to be reproducible? 14. Do the figure(s) have
descriptive captions (i.e. there is a description near the figure of
what is happening in the figure)? 15. As far as you can determine, does
it appear that the work submitted for this project is the work of the
student who submitted it?##

## Assignment
## 
The basic goal of this assignment is to explore the NOAA Storm Database
and answer some basic questions about severe weather events. You must
use the database to answer the questions below and show the code for
your entire analysis. Your analysis can consist of tables, figures, or
other summaries. You may use any R package you want to support your
analysis.

### Questions
### ###
Your data analysis must address the following questions:

1. Across the United States, which types of events (as indicated in the
𝙴𝚅𝚃𝚈𝙿𝙴 variable) are most harmful with respect to population
health? 2. Across the United States, which types of events have the
greatest economic consequences?

Consider writing your report as if it were to be read by a government or
municipal manager who might be responsible for preparing for severe
weather events and will need to prioritize resources for different types
of events. However, there is no need to make any specific
recommendations in your report.

### Requirements
### 
For this assignment you will need some specific tools

* RStudio: You will need RStudio to publish your completed analysis
document to RPubs. You can also use RStudio to edit/write your analysis.
* knitr: You will need the knitr package in order to compile your R
Markdown document and convert it to HTML

### Document Layout
### 
* Language: Your document should be written in English. * Title: Your
document should have a title that briefly summarizes your data analysis
* Synopsis: Immediately after the title, there should be a synopsis
which describes and summarizes your analysis in at most 10 complete
sentences. * There should be a section titled Data Processing which
describes (in words and code) how the data were loaded into R and
processed for analysis. In particular, your analysis must start from the
raw CSV file containing the data. You cannot do any preprocessing
outside the document. If preprocessing is time-consuming you may
consider using the 𝚌𝚊𝚌𝚑𝚎 = 𝚃𝚁𝚄𝙴 option for certain code chunks.
* There should be a section titled Results in which your results are
presented. * You may have other sections in your analysis, but Data
Processing and Results are required. * The analysis document must have
at least one figure containing a plot. * Your analysis must have no more
than three figures. Figures may have multiple plots in them (i.e. panel
plots), but there cannot be more than three figures total. * You must
show all your code for the work in your analysis document. This may make
the document a bit verbose, but that is okay. In general, you should
ensure that 𝚎𝚌𝚑𝚘 = 𝚃𝚁𝚄𝙴 for every code chunk (this is the
default setting in knitr).

### Publishing Your Analysis
### 
For this assignment you will need to publish your analysis on RPubs.com.
If you do not already have an account, then you will have to create a
new account. After you have completed writing your analysis in RStudio,
you can publish it to RPubs by doing the following:

* In RStudio, make sure your R Markdown document (.𝚁𝚖𝚍) document is
loaded in the editor * Click the 𝙺𝚗𝚒𝚝 𝙷𝚃𝙼𝙻 button in the doc
toolbar to preview your document. * In the preview window, click the
𝙿𝚞𝚋𝚕𝚒𝚜𝚑 button.

Once your document is published to RPubs, you should get a unique URL to
that document. Make a note of this URL as you will need it to submit
your assignment.

### Submitting Your Assignment
### 
In order to submit this assignment, you must copy the RPubs URL for your
completed data analysis document in to the peer assessment question.