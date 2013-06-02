SingleUserView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    var t = this;
    window.App.router.on('route:singleUserView', function(uid){
      t.setUserId(uid).always(function(){
        window.App.hideManager.render(t);
      });
    });
    window.App.login.on('change', t.render, t);
  }
, render: function(){
    var model = this.model;
    var view  = this;
    if(model === null){
      this.$el.html('<h2>Could not load requested user.</h2>Sorry…');
    }else{
      //Displaying the content:
      var privileg  = this.selfOrAdmin();
      var logoutB   = (model.get('id') === window.App.login.get('id'))
                    ? '<div class="btn logoff">Logout</div>' : '';
      var deleteB   = privileg ? '<div class="btn btn-danger delete">Delete</div>' : '';
      var headline  = '<thead><tr><th>'
                    + model.get('username')
                    + '</th><th>'
                    + logoutB
                    + deleteB
                    + '</th></tr></thead>';
      var lastLogin = '<tr><td>Last login:</td><td>'+model.get('lastLogin')+'</td></tr>';
      var creation  = '<tr><td>Created:</td><td>'+model.get('userCreation')+'</td></tr>';
      var btn       = window.App.login.get('isAdmin') ? 'class="btn btn-info changeRole"' : '';
      var isAdmin   = model.get('isAdmin') ? 'Admin' : 'User';
      var role      = '<tr><td>Role:</td><td><div '+btn+'>'+isAdmin+'</div></td></tr>';
      var changePwd = privileg ? '<tr><td>New Password:</td><td>'
                    + '<input type="password" class="newPwd" placeholder="New Password">'
                    + '<input type="password" class="confirmPwd" placeholder="Confirm Password">'
                    + '<div class="btn changePwd">Change</div>'
                    + '</td></tr>' : '';
      var table     = '<table class="table table-bordered table-striped">'
                    + headline
                    + '<tbody>'
                    + lastLogin
                    + creation
                    + role
                    + changePwd
                    + '</tbody></table>';
      var hasProf   = model.get('profile') !== null;
      var profile   = hasProf ? '<div class="profile"></div>' : '';
      this.$el.html(table+profile);
      //Displaying the profile:
      //FIXME implement
      //Logout Action:
      this.$('.logoff').click(function(){
        window.App.login.logout();
      });
      //Delete Action:
      this.$('.delete').click(function(){
        var isSelf = model.get('id') === window.App.login.get('id');
        model.delete().done(function(){
          if(isSelf){
            model.set({loggedIn: false});
          }else
            window.App.router.navigate('#/user', {trigger: true});
        });
      });
      //Change Role:
      this.$('.changeRole').click(function(){
        model.changeRole().done(function(d){
          view.render();
        });
      });
      //ChangePwd Action:
      this.$('.changePwd').click(function(){
        var newPwd = view.$('.newPwd');
        var confirmPwd = view.$('.confirmPwd');
        var i = {x: newPwd.val(), y: confirmPwd.val()};
        if(i.x === i.y){
          model.setPasswd(i.x);
        }else
          console.log('Passwords mismatch!');
        newPwd.val('');
        confirmPwd.val('');
      });
    }
  }
, setUserId: function(uid){
    //Unbinding events from the old model:
    if(this.model !== null)
      this.model.off(null, null, this);
    var p = $.Deferred();
    var t = this;
    //Listening to model changes after the model is ready:
    p.always(function(){
      if(t.model !== null)
        t.model.on('change', t.render(), t);
    });
    //Choosing what the model is:
    if(uid === window.App.login.get('id')){
      this.model = window.App.login;
    }else
      this.model = new User({id: uid});
    //Fetching the model:
    this.model.fetch().done(function(){
      p.resolve(t.model);
    }).fail(function(){
      t.model = null;
      p.reject(null);
    });
    return p;
  }
, selfOrAdmin: function(){
    var mId = this.model.get('id');
    var sId = window.App.login.get('id');
    var isA = window.App.login.get('isAdmin');
    return (sId === mId)||isA;
  }
});
