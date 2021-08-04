// Matomo Code
var _paq = window._paq = window._paq || [];
/* tracker methods like "setCustomDimension" should be called before "trackPageView" */
_paq.push(['disableCookies']);
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
_paq.push(['enableHeartBeatTimer']);
(function() {
  var u="https://covideye.org/matomo/";
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '1']);
  var d=document, 
      g=d.createElement('script'), 
      s=d.getElementsByTagName('script')[0];
      g.async=true; 
      g.src=u+'matomo.js'; 
      s.parentNode.insertBefore(g,s);
})();

// Send non-identifiable shiny inputs to Matomo
$(document).on('shiny:inputchanged', function(event) {
  if (event.name !== 'dashboard-map-lat' && event.name !== 'dashboard-map-lng') {
    _paq.push(['trackEvent', 'input', 'update', event.name, event.value]);
  }
})
