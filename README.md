# CovidEye
CovidEye is an web application that displays COVID-19 data for the state of Virginia. It is designed to be as user friendly and feature-rich as possible while maintaining a streamlined appearance.

![screenshot](./Screenshot.png)

## Development
The project uses R shiny as the base framework. This allows us to create an interactive and visually appealing site with minimal handwritten HTML, CSS, and JS.

To see our current development progress, view our [Trello](https://trello.com/b/uoXWHzLu/covideye-development-overview)!

If you want to contribute, please email rvanathanrowan@gmail.com.

## Building
The stable website covideye.org doesn’t always have the most up-to-date features. To run the development version yourself clone the repo and open the project with RStudio. Then simply run `shiny::runApp()`. The first time this command is run will take some time to download the data.

To update the data, run `source(update.R)`. This will be done automatically if the data is over 24 hours old.

## Copying
This project is licensed under the GNU GPL version 3.0, see [LICENSE](./LICENSE).
