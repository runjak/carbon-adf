/***/
var TopMenuView = Backbone.View.extend({
  defaults: {
    children: []
  , hidden: []
  , logger: null
  }
, initialize: function(){
    var t = this;
    //Setting up the children:
    t.children = t.options.children;
    $(this.options.children).each(function(i, e){
      if(e.setTopMenu)
        e.setTopMenu(t);
      if(e.setLogger)
        e.setLogger(t.options.logger);
      if(e.setRouter)
        e.setRouter(t.options.router);
    });
    //Building tOptions:
    t.tOptions = {
      activate: function(e,ui){
        t.options.router.navigate(ui.newTab.find('a').attr('href'), {trigger: true});
      }
    , disabled: $.map(t.options.hidden, function(e,i){
        $(e.getTabId()).hide();
        return parseInt($(e.getTabId()).attr('data-tabindex'));
      })
    };
    t.render();
  }
, render: function(){
    $(this.el).tabs(this.tOptions);
  }
, displayTabs: function(tabs, show){
    var dis = $.map(tabs, function(e,i){
      return parseInt($(e.getTabId()).attr('data-tabindex'));
    });
    if(show){
      this.tOptions.disabled = _.difference(this.tOptions.disabled, dis);
      $(tabs).each(function(i,e){ $(e.getTabId()).fadeIn(); });
    }else{
      this.tOptions.disabled = _.union(dis, this.tOptions.disabled);
      $(tabs).each(function(i,e){ $(e.getTabId()).fadeOut(); });
    }
    this.render();
    return this;
  }
, selectTab: function(tab){
    var tix = parseInt($(tab.getTabId()).attr('data-tabindex'));
    this.tOptions.active = tix;
    this.render();
    return this;
  }
, showTabs: function(tabs){ return this.displayTabs(tabs, true); }
, hideTabs: function(tabs){ return this.displayTabs(tabs, false); }
});
/***/
var TopMenuChild = Backbone.View.extend({
  setTopMenu: function(topMenu){
    this.topMenu = topMenu;
  }
, setLogger: function(logger){
    this.logger = logger;
  }
, setRouter: function(router){
    this.router = router;
  }
, getTabId: function(){
    console.log("TopMenuChild:getTabId should be overwritten by it\'s children.");
    return null;
  }
});
