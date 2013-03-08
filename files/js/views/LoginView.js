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
      });
    }
  }
, render: function(){}
, events: {
    "click #LoginButton":    "login"
  , "click #RegisterButton": "register"
  , "keypress #Password":    "login_"
//, "click #LogoutButton":   "logout"
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
      if(data.actionSuccess)
        t.gotUserData(data);
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
      if(data.actionSuccess)
        t.gotUserData(data);
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
    this.pView.setUserData(null);
    this.topMenu.showTabs([this]).hideTabs([this.pView]).selectTab(this);
  }
, gotUserData: function(data){
    $('#LogoutButton').show();
    this.pView.setUserData(data);
    this.topMenu.hideTabs([this]).showTabs([this.pView]).selectTab(this.pView);
  }
, getTabId: function(){ return "#TopmenuLogin"; }
, setProfileView: function(pView){ this.pView = pView; }
});
