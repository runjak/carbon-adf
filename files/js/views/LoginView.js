var LoginView = TopMenuChild.extend({
  initialize: function(){}
, render: function(){}
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
, gotUserData: function(data){
    this.pView.setUserData(data);
    this.topMenu.hideTabs([this]).showTabs([this.pView]).selectTab(this.pView);
  }
, getTabId: function(){ return "#TopmenuLogin"; }
, setProfileView: function(pView){ this.pView = pView; }
});
