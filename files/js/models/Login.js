Login = User.extend({
  defaults: { loggedIn:   false
            , fromCookie: true }
, initialize: function(){
    var uid = this.uidFromCookie();
    if(uid !== null){
      this.set({id: uid}).fetch({
        success: function(login){
          login.set({loggedIn: true, fromCookie: true});
        }
      });
    }
  }
, login: function(username, password){
    var login = this;
    var defer = $.Deferred();
    $.post('login/', {username: username, password: password}, function(data){
      var uid = login.uidFromCookie();
      login.set({id: uid, username: username, loggedIn: true, fromCookie: false});
      login.fetch({success: function(){
        defer.resolve(data);
      }});
    }).fail(function(data){
      defer.reject(data);
    });
    return defer;
  }
, logout: function(){
    var login = this;
    var defer = $.Deferred();
    $.delete('login/').done(function(data){
      login.attributes = {};
      login.set({loggedIn: false, fromCookie: false});
      defer.resolve(data);
    }).fail(function(data){
      defer.reject(data);
    });
    return defer.promise();
  }
, uidFromCookie: function(){
    var uid = $.cookie('userid');
    if(uid){
      return parseInt(/^\"(.*)\"$/.exec(uid)[1]);
    }
    return null;
  }
});
