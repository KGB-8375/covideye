# About Us

This project was made by a small team of undergraduate students as a submission
to the [IEEE Richmond Pandemic Challenge (2021)](https://r3.ieee.org/richmond/covid/covid-problems/).

The goal for this project was to develop a better implementation of the current
Virginia Department of Health's COVID-19 Dashboard. The improvements that we
offer with our solution include a feature for zooming in and out on the map, a
more clear color scheme with different schemes depending on the information
being displayed on the map, and apply easier-to-understand categories. The
Virginia Department of Health's COVID-19 map has the maximum category at 11k,
which removes much of the difference between all of the other counties. Our
implementation creates categories using
[Jenk's Natural Breaks Optimization](https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization)
of the cases at that date to better differentiate between case numbers. This, in
turn, creates a more distinctive choropleth for viewers. Finally, the main point
was to make something that is much easier to understand for the average viewer.
To do this, we added data insights for the viewer to see that make sense and
show trends that you can't see from just raw data. This in turn makes it less
work for the viewer and gives them a clearer picture of what is going on in each
county.

This project implementation is open-source, and is written in R. All of the 
COVID-19 related data that is used in this project comes from the 
[Virginia Department of Health](https://data.virginia.gov). The population and
geographical data is from the 2019 census. The project is available for viewers
on [GitHub](https://github.com/KGB-8375/rva-covidview).

## Credits:

Nathan Rowan (VCU)

Yaqub Mukhtar (VCU)

Duncan Fernandez (Reynolds)