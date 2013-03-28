TopmenuView = Backbone.View.extend({
  initialize: function(){
    var t    = this;
    t.login  = t.options.login;
    t.router = t.options.router;
    t.adminTab   = [6,'#TopmenuAdmin'];
    t.loginTab   = [3,'#TopmenuLogin'];
    t.profileTab = [4,'#TopmenuProfile']
    t.loginTabs  = [ [2,'#TopmenuCreate']
                   , [5,'#TopmenuUsers']
                   , t.profileTab];
    //Building tOptions:
    t.tabOptions = {
      activate: function(e,ui){
        t.router.navigate(ui.newTab.find('a').attr('href'), {trigger: true});
      }
    , disabled: $.map(t.loginTabs, function(e){ return e[0]; })
    };
    //Events:
    this.listenTo(this.login, "change:loggedIn", this.render);
    //First.render:
    t.render();
  }
, render: function(){
    if(this.login.get('loggedIn')){
      var active   = this.login.get("isAdmin") ? this.adminTab : this.profileTab;
      var showTabs = _.union([active], this.loginTabs);
      var options = {
        disabled: _.difference(this.tabOptions.disabled
          , $.map(showTabs, function(e){ return e[0]; }))
      };
      active = this.login.get('fromCookie') ? {} : {active: active[0]};
      this.tabOptions = $.extend(this.tabOptions, options, active);
      $.map(showTabs, function(e){ $(e[1]).show(); });
    }else{
      var disable = _.union([this.adminTab], this.loginTabs);
      var options = {
        disabled: $.map(disable, function(e){ return e[0]; })
      };
      var active = this.login.get('fromCookie') ? {} : {active: this.loginTab[0]}
      this.tabOptions = $.extend(this.tabOptions, options, active);
      $(this.loginTab[1]).show();
      $.map(disable, function(e){ $(e[1]).hide(); });
    }
    $(this.el).tabs(this.tabOptions);
  }
});
