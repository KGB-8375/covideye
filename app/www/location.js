// Reports user's location for plotting on map
$(document).ready(
  function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
      Shiny.setInputValue("dashboard-map-geolocation", false);
    }

    function onSuccess (position) {
      setTimeout(
        function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.setInputValue("dashboard-map-geolocation", true);
          Shiny.setInputValue("dashboard-map-lat", coords.latitude);
          Shiny.setInputValue("dashboard-map-lng", coords.longitude);
        }, 1100
      )
    }
  }
);