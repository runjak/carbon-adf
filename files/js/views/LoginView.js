LoginView = Hideable.extend({
  initialize: function(){
    var t = this;
    window.App.router.on('route:loginView', function(){
      window.App.hideManager.render(t);
    });
  }
, events: {
    "click #LoginRegisterFormLogin":    "login"
  , "click #LoginRegisterFormRegister": "register"
  }
, render: function(){}
, login: function(){
    var d = this.formData();
    this.model.login(d.username, d.password).done(function(d){
      console.log(d);
    }).fail(function(f){
      console.log(f);
    });
  }
, register: function(){
    var model = this.model;
    var formD = this.formData();
    console.log('LoginView:register('+formD+')');
    model.create(formD.username, formD.password).done(function(d){
      d.id = d.userId;
      d.userId = null;
      model.set($.extend({loggedIn: true}, d));
    }).fail(function(f){
      console.log(f);
    });
  }
, formData: function(){
    var u = this.$('#LoginRegisterFormUsername');
    var p = this.$('#LoginRegisterFormPassword');
    var d = {username: u.val(), password: p.val()};
    u.val(''); p.val('');
    return d;
  }
});
