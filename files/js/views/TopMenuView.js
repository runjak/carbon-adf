/***/
var TopMenuView = Backbone.View.extend({
  defaults: {
    children: []
  , logger: null
  }
, initialize: function(){
    var t = this;
    $(this.options.children).each(function(i, e){
      if(e.setTopMenu)
        e.setTopMenu(t);
      if(e.setLogger)
        e.setLogger(t.options.logger);
    });
  }
, hideTabId: function(tabId){ $(tabId, this.el).fadeOut(); }
, showTabId: function(tabId){ $(tabId, this.el).fadeIn(); }
, displayTabs: function(tabs, show){
    var t = this;
    $(tabs).each(function(i, e){
      var tid = e.getTabId();
      if(tid !== null)
        if(show){
          t.showTabId(tid);
          e.render();
        }else{
          t.hideTabId(tid);
        }
    });
  }
});
/***/
var TopMenuChild = Backbone.View.extend({
  setTopMenu: function(topMenu){
    this.options.topMenu = topMenu;
  }
, setLogger: function(logger){
    this.options.logger = logger;
  }
, getTabId: function(){
    console.log("TopMenuChild:getTabId should be overwritten by it\'s children.");
    return null;
  }
});
