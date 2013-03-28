LoginView = Backbone.View.extend({
  initialize: function(){
    this.login = this.options.login;
    var login  = this.login;
    $('#LogoutButton').click(function(){ login.logout(); });
    this.listenTo(login, "change:loggedIn", this.render);
  }
, render: function(){
    var loggedIn = this.login.get('loggedIn');
    if(loggedIn){
      $('#LogoutButton').show();
    }else{
      $('#LogoutButton').hide();
    }
  }
, events: {
    "click #LoginButton":    "onLogin"
  , "click #RegisterButton": "register"
  , "keypress #Password":    "onLogin_"
  }
, getInputs: function(){
    return {
      username: $('#Username').val()
    , password: $('#Password').val()
    };
  }
, onLogin: function(){
    this.login.login(this.getInputs());
  }
, onLogin_: function(event){
    if(event.keyCode === 13)
      this.onLogin();
  }
, register: function(){
    this.login.register(this.getInputs());
  }
});
