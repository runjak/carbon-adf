var ProfileView = TopMenuChild.extend({
  getTabId: function(){ return "#TopmenuProfile"; }
, initialize: function(){
    this.dialogEl = $('#ProfileDeleteDialog');
    var t = this;
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
    if(!this.userData) return;
    var displayFields = [{field: 'username', title: 'Username:'}
                        ,{field: 'userCreation', title: 'Created:'}
                        ,{field: 'lastLogin', title: 'Last login:'}
                        ,{field: 'karma', title: 'Karma:'}];
    var ud = this.userData;
    $(displayFields).each(function(i,e){
      var content = "<dt>" + e.title + "</dt>"
                  + "<dd>" + ud[e.field] + "</dd>";
      $('#ProfileStatistics').append(content);
    });
  }
, events: {
    "click #UpdatePasswordButton": "updatePassword"
  , "click #DeleteUserButton": "deleteUser"
  }
, setLoginView: function(lView){ this.lView = lView; }
, updatePassword: function(){
    var nP = $('#NewPassword').val();
    var cP = $('#ConfirmPassword').val();
    if(cP == nP){
      var logger = this.logger;
      var q = { username: this.userData.username
              , password: nP };
      $.post("/action/user/password", q, function(data){
        logger.logAction(data);
      });
    }else{
      this.logger.log("Passwords mismatch, can't update.", true);
    }
  }
, deleteUser: function(){
    this.dialogEl.dialog('open');
  }
, deleteUser_: function(){
    var t = this;
    $.post("/action/user/delete", {username: t.userData.username}, function(data){
      t.dialogEl.dialog('close');
      t.logger.logAction(data);
      t.lView.loggedOut();
    });
  }
, setUserData: function(userData){
    console.log(userData);
    this.userData = userData;
    this.render();
  }
});
