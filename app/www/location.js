// Reports user's location for plotting on map
$(document).ready(function () {

  function getLocation (callback) {
    var options = {
      enableHighAccuracy: true,
      timeout: 5000,
      maximumAge: 0
    };

    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
      Shiny.onInputChange("geolocation", false);
    }

    function onSuccess (position) {
      setTimeout(function () {
        var coords = position.coords;
        var timestamp = new Date();

        console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
        Shiny.onInputChange("dashboard-map-geolocation", true);
        Shiny.onInputChange("dashboard-map-lat", coords.latitude);
        Shiny.onInputChange("dashboard-map-long", coords.longitude);
        Shiny.onInputChange("dashboard-map-accuracy", coords.accuracy);
        Shiny.onInputChange("dashboard-map-time", timestamp)

        console.log(timestamp);
                
        if (callback) {
          callback();
        }
      }, 1100)
    }
  }

  var TIMEOUT = 1000; //SPECIFY
  var started = false;
  function getLocationRepeat(){
    //first time only - no delay needed
    if (!started) {
      started = true;
      getLocation(getLocationRepeat);
      return;
    }

    setTimeout(function () {
      getLocation(getLocationRepeat);
    }, TIMEOUT);

  };

  getLocationRepeat();

});
