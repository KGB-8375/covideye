# IEEE Richmond Pandemic Challenge (2021)
## Choropleth Data Tracking
Team drumroll please's github repository for RVA CovidView

## Description of Challenge

Despite efforts from the Virginia Department of Health (VDH), data for tracking the COVID-19 virus throughout Virginia
is difficult and cryptic for the average person. Even with visual elements such as choropleths (multicolored maps representing
case numbers), trends such as change in daily cases largely remains hidden. Furthermore, due to poor choices in color schemes,
the color difference between a couple thousand cases looks the same as several thousand cases. Our task is to create a new interface
that is more accessable, easier to understand, and makes the data more clear and easily understood by average people.

## Our Team

Our team *Drumroll Please* is made up of three undergrad students in the Henrico area:

 - Nathan Rowan (VCU)
 - Yaqub Mukhtar (VCU)
 - Duncan Fernandez (Reynolds)

## Our solution path

We chose to create an interactive website with a live feed of the data available from the Virginia Department of Health. To accomplish
this we're using a `Shiny` application written in `R`. This allows us to quickly build a website with a simple and effective interface
using many interactive maps (using `leaflet`), graphs (using `plotly`), and more all using one language (`R`). When a client connects 
to the site, all of these graphs are generated dynamically from an active R session before being sent to the webserver. This allows
for a huge amount of flexibility, offering graphs that are potentially unavailable anywhere else. The downside is this uses a 
substaintial amount of server processing power, however we are doing our best to keep the app running optimized and responsive for end
users.

### Our site can be viewed at [covideye.org](https://covideye.org)

## Copying

We chose to make this project open-sourced, licensed under the GNU General Public License version 3. For more information, see LICENSE.
