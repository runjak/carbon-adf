Login = User.extend({
  urlRoot: '/user/'
, defaults: { loggedIn: false }
, initialize: function(){
    var uid = $.cookie('userid');
    if(uid){
      var uid = /^\"UId \(Id (.*)\)\"$/.exec(uid)[1];
      this.set({id: uid});
      this.fetch({success: function(login){
        login.set({loggedIn: true});
      }});
    }
  }
, register: function(data){
    var l = this;
    $.post('/action/user/create', data, function(d){l.onLoggedIn(d);});
  }
, login: function(data){
    var l = this;
    $.post('/action/user/login', data, function(d){l.onLoggedIn(d);});
  }
, update: function(pwd, pwd_){
    if(pwd === pwd_){
      this.setPassword(pwd);
    }else{
      this.set({
        actionMessage: "Passwords mismatch, can't update."
      , actionSuccess: false
      });
    }
  }
, logout: function(){
    var l = this;
    $.post("/action/user/logout", function(d){l.onLoggedOut(d);});
  }
, delete: function(){
    var l = this;
    $.post("/action/user/delete", {username: this.get('username')}, function(d){l.onLoggedOut(d);});
  }
, onLoggedIn: function(data){
    this.set($.extend(data, {loggedIn: true}));
  }
, onLoggedOut: function(data){
    this.attributes = {};
    this.set($.extend(data, {loggedIn: false}));
  }
});
