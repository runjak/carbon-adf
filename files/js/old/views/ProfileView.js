ProfileView = Backbone.View.extend({
  initialize: function(){
    this.listenTo(this.model, "change:loggedIn", this.render);
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
, statView: new UserStatisticsView({el: $('#ProfileStatistics')})
, render: function(){
    var m = this.model.get('loggedIn') ? this.model : null;
    this.statView.setModel(m);
  }
, events: {
    "click #UpdatePasswordButton": "updatePassword"
  , "click #DeleteUserButton": "deleteUser"
  }
, updatePassword: function(){
    var nP = $('#NewPassword').val();
    var cP = $('#ConfirmPassword').val();
    this.model.update(nP, cP);
  }
, deleteUser: function(){ this.dialogEl.dialog('open'); }
, deleteUser_: function(){
    this.dialogEl.dialog('close');
    this.model.delete();
  }
});
