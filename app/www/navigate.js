// restore history based on next/back buttons
shinyjs.init = function() {
  window.onpopstate = function(event) {
    Shiny.onInputChange('navigatedTo', location.search);
  }
}

// update the URL to reflect current state
shinyjs.updateHistory = function(params) {
  var queryString = [];
  
  for(var key in params) {
    queryString.push(encodeURIComponent(key) + '=' + encodeURIComponent(params[key]));
  }
  
  queryString = '?' + queryString.join('&');
  history.pushState(null, null, queryString);
}
