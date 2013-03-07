var LoginView = Backbone.View.extend({
  initialize: function(){
  }
, events: {
    "click #LoginButton": "login"
  , "click #RegisterButton": "register"
  , "keypress #Password": "login_"
  }
, getUsername: function(){
    return $('#Username').val();
  }
, getPassword: function(){
    return $('#Password').val();
  }
, login: function(){
    var u = this.getUsername();
    var p = this.getPassword();
    log("Login called:\t" + u + ":" + p);
  }
, login_: function(event){
    if(event.keyCode === 13)
      this.login();
  }
, register: function(){
    var u = this.getUsername();
    var p = this.getPassword();
    log("Register called:\t" + u + ":" + p);
  }
});
