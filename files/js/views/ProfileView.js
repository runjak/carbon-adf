ProfileView = Backbone.View.extend({
  initialize: function(){
    var t      = this;
    this.login = this.options.login;
    this.login.on('change:loggedIn', function(){t.render();});
    this.dialogEl = $('#ProfileDeleteDialog');
    t.dialogEl.dialog({
      autoOpen: false
    , width: 400
    , buttons: [
        {text: 'Delete'
        ,click: function(){ t.deleteUser_(); }
        }
      , {text: 'Cancel'
        ,click: function(){ t.dialogEl.dialog('close'); }
        }
      ]
    });
  }
, render: function(){
    $('#ProfileStatistics').html('');
    if(this.login.get("loggedIn")){
      var displayFields = [
        {field: 'username',     title: 'Username:'}
      , {field: 'userCreation', title: 'Created:'}
      , {field: 'lastLogin',    title: 'Last login:'}
      , {field: 'karma',        title: 'Karma:'}];
      var ud = this.login.attributes;
      $(displayFields).each(function(i,e){
        var content = "<dt>" + e.title + "</dt>"
                    + "<dd>" + ud[e.field] + "</dd>";
        $('#ProfileStatistics').append(content);
      });
    }else{
    }
  }
, events: {
    "click #UpdatePasswordButton": "updatePassword"
  , "click #DeleteUserButton": "deleteUser"
  }
, updatePassword: function(){
    var nP = $('#NewPassword').val();
    var cP = $('#ConfirmPassword').val();
    this.login.update(nP, cP);
  }
, deleteUser: function(){ this.dialogEl.dialog('open'); }
, deleteUser_: function(){
    this.dialogEl.dialog('close');
    this.login.delete();
  }
});
