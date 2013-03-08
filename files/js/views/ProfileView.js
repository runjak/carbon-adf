var ProfileView = TopMenuChild.extend({
  getTabId: function(){ return "#TopmenuProfile"; }
, initialize: function(){}
, render: function(){
    //FIXME implement
    this.logger.log('ProfileView:render()');
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
    }else{
      this.logger.log("Passwords mismatch, can't update.", true);
    }
  }
, deleteUser: function(){}
, setUserData: function(userData){ this.userData = userData; }
});
