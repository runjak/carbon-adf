User = Backbone.Model.extend({
  urlRoot: '/user/'
, defaults: {}
, initialize: function(){}
, delete: function(){
    var u = this;
    $.post("/action/user/delete", {username: this.get('username')}, function(d){
      u.attributes = {};
      u.set(d);
    });
  }
, setPassword: function(p){
    var u = this;
    var q = {username: this.get('username'), password: p};
    $.post("/action/user/password", q, function(d){u.set(d);});
  }
, setAdmin: function(a){
    if(a != this.get('isAdmin')){
      var u = this;
      var q = {username: this.get('username'), admin: a};
      $.post('/action/user/admin', q, function(d){u.set(d);});
    }
  }
});
