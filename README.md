# Trackman-Shiny
Collection of code to create in-depth analytical reports based on Trackman CSV files for collegiate baseball.

How to use:

1) Take original Trackman data in csv format, and run it through preprocessing.Rmd
     - This will help correct and deal with fringe cases and bolster the depth of information by adding columns such as "Barrels", which are not directly tracked by can be implicitly derived.
2) Load the csv output by preprocessing.Rmd into app.R

3) Run app.R

In www folder, put a PNG/JPG image for your particular team/organization.


## Notes on additional paths/files

- spraychart.R contains our template from which we learned to incorporate visually appealing spray charts into our shiny application.

Code inspiration is credit to: https://github.com/sambornstein/Simple-Sabermetrics/tree/main - we expanded upon the formatting and ideas present in this code base to develop our unique Shiny app.

If bugs or errors are identified, please contact Trevor Beer (tbeerwheels@gmail.com). Thank you.
