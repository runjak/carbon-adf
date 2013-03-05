var log = function(message, warning){
  var icon = warning ? 'ui-icon-alert' : 'ui-icon-info';
//if(warning) icon = 'ui-icon-alert'
  var now = new Date().toTimeString().replace(/.*(\d{2}:\d{2}:\d{2}).*/, "$1"); 
  var content = "<div class='ui-widget ui-helper-hidden'>"
        + "<div class='ui-state-highlight ui-corner-all' style='margin-top: 20px; padding: 0 .7em;'>"
        + "<p><span class='ui-icon " + icon +"' style='float: left; margin-right: .3em;'></span>"
        + "<strong>" + now + ":</strong> " + message + "</p></div></div>";
  $(content).prependTo('#log').fadeIn();
};
