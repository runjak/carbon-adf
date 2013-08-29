User = Item.extend({
  urlRoot: 'user/'
, defaults: {}
, initialize: function(){}
, create: function(username, password, isAdmin){
    if(isAdmin !== true) isAdmin = false;
    this.set({
      'username': username
    , 'password': password
    , 'isAdmin':  isAdmin
    });
    return $.post(this.urlRoot, this.attributes);
  }
, delete: function(){
    return this.destroy({wait: true});
  }
, changeRole: function(){
    var t = this;
    var isA = this.get('isAdmin') !== true;
    var url = this.urlRoot + this.get('id');
    var promise = $.Deferred();
    $.put(url, {isAdmin: isA}).done(function(d){
     t.set(d);
     promise.resolve(d);
    }).fail(function(f){
      promise.reject(f);
    });
    return promise;
  }
, setPasswd: function(pwd){
    var t = this;
    var url = this.urlRoot + this.get('id');
    var promise = $.Deferred();
    $.put(url, {password: pwd}).done(function(d){
      t.set(d);
      promise.resolve(d);
    }).fail(function(f){
      promise.reject(f);
    });
    return promise;
  }
, findInDiscussion: function(discussion){
    var uid = this.get('id');
    return discussion.participants.elemById(uid);
  }
});
