/***/
var TopMenuView = Backbone.View.extend({
  defaults: {
    children: []
  , hidden: []
  , logger: null
  }
, initialize: function(){
    //Setting up the children:
    this.children = this.options.children;
    var t = this;
    $(this.options.children).each(function(i, e){
      if(e.setTopMenu)
        e.setTopMenu(t);
      if(e.setLogger)
        e.setLogger(t.options.logger);
    });
    //Gathering hidden tabs:
    var dis = $.map(t.options.hidden, function(e,i){
      return parseInt($(e.getTabId()).attr('data-tabindex'));
    });
    //Setting up the tabs:
    $(t.el).tabs({
      disabled: dis
    });
  }
, displayTabs: function(tabs, show){
    var t = this;
    $(tabs).each(function(i, e){
      var tix = $(e.getTabId()).attr('data-tabindex');
      if(show){
        $(t.el).parent().tabs('enable', tix);
        e.render();
      }else{
        $(t.el).parent().tabs('disable', tix);
      }
    });
  }
});
/***/
var TopMenuChild = Backbone.View.extend({
  setTopMenu: function(topMenu){
    this.topMenu = topMenu;
  }
, setLogger: function(logger){
    this.logger = logger;
  }
, getTabId: function(){
    console.log("TopMenuChild:getTabId should be overwritten by it\'s children.");
    return null;
  }
});
