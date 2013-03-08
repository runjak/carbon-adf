var LoginView = TopMenuChild.extend({
  initialize: function(){
    var t = this;
    $('#LogoutButton').click(function(){ t.logout(); });
    //Check if we've got a running session:
    var uid = $.cookie('userid');
    if(uid){
      var uid = /^\"UId \(Id (.*)\)\"$/.exec(uid)[1];
      $.get("/user/"+uid, function(data){
        t.gotUserData(data);
        if(t.router.routes[Backbone.history.fragment] === 'loginView')
          t.render();
      });
    }
  }
, render: function(){
    console.log('In Render');
    if(this.userData){
      console.log('Got ud!');
      this.topMenu.hideTabs([this]).showTabs([this.pView]);
      this.router.navigate('profile', {trigger: true});
    }else{
      this.topMenu.showTabs([this]).hideTabs([this.pView]).selectTab(this);
    }
  }
, events: {
    "click #LoginButton":    "login"
  , "click #RegisterButton": "register"
  , "keypress #Password":    "login_"
  }
, getInputs: function(){
    return {
      username: $('#Username').val()
    , password: $('#Password').val()
    };
  }
, login: function(){
    var t = this;
    $.post('/action/user/login', this.getInputs(), function(data){
      t.logger.logAction(data);
      if(data.actionSuccess){
        t.gotUserData(data);
        t.render();
      }
    });
  }
, login_: function(event){
    if(event.keyCode === 13)
      this.login();
  }
, register: function(){
    var t = this;
    $.post('/action/user/create', this.getInputs(), function(data){
      t.logger.logAction(data);
      if(data.actionSuccess){
        t.gotUserData(data);
        t.render();
      }
    });
  }
, logout: function(){
    var t = this;
    $.post("/action/user/logout", function(data){
      t.logger.logAction(data);
      t.loggedOut();
    });
  }
, loggedOut: function(){
    $('#LogoutButton').hide();
    this.userData = null;
    this.pView.setUserData(null);
    this.router.navigate('login', {trigger: true});
  }
, gotUserData: function(data){
    $('#LogoutButton').show();
    this.userData = data;
    this.pView.setUserData(data);
  }
, getTabId: function(){ return "#TopmenuLogin"; }
, setProfileView: function(pView){ this.pView = pView; }
});
