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
      t.options.logger.logAction(data);
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
      t.options.logger.logAction(data);
      if(data.actionSuccess)
        t.gotUserData(data);
    });
  }
, gotUserData: function(data){
    this.options.logger.log('gotUserData!');
    console.log(data);
    this.options.topMenu.displayTabs([this], false);
  }
, getTabId: function(){ return "#TopmenuLogin"; }
});
