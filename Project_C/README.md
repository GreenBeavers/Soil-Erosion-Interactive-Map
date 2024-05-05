# Shiny_Example
This repo is home to an example interactive map made with RShiny and leaflet.

The example plots [CEH Land Cover] as extracted on a 25km grid across Great Britain. As you zoom into the map, the plot on the right will update based on the observations still in view. If you click on any of the points, information about the location will appear as a popup.

![alt text][map]

Made by [Pia Benaud] it is a simplified and updated version of [SoilErosionMap] shinyapp made in support of the publication by [Benaud et al. (2020)] *National-scale geodata describe widespread accelerated soil erosion.* 

The original code drew inspiration from the Rshiny [Superzip] example.

Follow these links to learn more about [R Shiny] and how it can be integrated with [Leaflet for R], bringing the power of one of the most popular open-source JavaScript libraries for interactive maps. 

---

### About the scripts

Background.R was written to make a geopackage containing extracted landcover data for a 25 km grid.

A shiny app has 3 components:
* UI.R which defines the user interface (it's an object)
* Server.R contains the components needed to make the app - some of these are reactive
* app.R brings all this together - it is the only script you need to run the app work, simply run all of the code in script!



[map]: Map.png
[CEH Land Cover]: https://doi.org/10.5285/A22BAA7C-5809-4A02-87E0-3CF87D4E223A
[Pia Benaud]: http://geography.exeter.ac.uk/staff/index.php?web_id=Pia_Benaud
[SoilErosionMap]: https://piabenaud.shinyapps.io/SoilErosionMap
[Benaud et al. (2020)]: https://doi.org/10.1016/j.geoderma.2020.114378
[Superzip]: https://shiny.posit.co/r/gallery/interactive-visualizations/superzip-example/
[R Shiny]: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html 
[Leaflet for R]: https://rstudio.github.io/leaflet/