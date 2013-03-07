var Logger = Backbone.View.extend({
  initialize: function(){}
, log: function(message, warning){
    var icon = warning ? 'ui-icon-alert' : 'ui-icon-info';
    var now = new Date().toTimeString().replace(/.*(\d{2}:\d{2}:\d{2}).*/, "$1"); 
    var content = "<div class='ui-widget ui-helper-hidden'>"
          + "<div class='ui-state-highlight ui-corner-all' style='margin-top: 20px; padding: 0 .7em;'>"
          + "<p><span class='ui-icon " + icon +"' style='float: left; margin-right: .3em;'></span>"
          + "<strong>" + now + ":</strong> " + message + "</p></div></div>";
    $(content).prependTo(this.el).fadeIn();
    //Fading out older messages:
    var msgs = $('div.ui-widget', this.el);
    if(msgs.length > 3){
      var last = msgs.get(-1);
      $(last).fadeOut(function(){
        $(last).remove();
      });
    }
  }
, logAction: function(action){
    this.log(action.actionMessage, !action.actionSuccess);
  }
});
