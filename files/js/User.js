function initUser(){
  this.username = '';
  //Handling login/creation of users:
  if($("#OpenBrainWebsiteUser_loginBox").length > 0){
    var loginAction = function(action){
      var url = "action/user/" + action;
      var data = {
        username: $('#OpenBrainWebsiteUser_loginBox input[name=username]').val()
      , password: $('#OpenBrainWebsiteUser_loginBox input[name=password]').val()
      };
      console.log(data);
      $.post(url, data, function(data, status, jqXHR){
        console.log("Got data:");
        console.log(data);
        console.log("Got status:" + status);
      }, "json");
    };
    //Client login:
    $("#OpenBrainWebsiteUser_loginBox input.login").click(function(){loginAction("login");});
    //Creating a new user:
    $("#OpenBrainWebsiteUser_loginBox input.create").click(function(){loginAction("create");});
  }
  //Handling password change/deletion by users:
  if($("#OpenBrainWebsiteUser_controlBox").length > 0){
    var username = $("OpenBrainWebsiteUser_controlBox input[name=username]").val();
    //Deleting the user:
    $("#OpenBrainWebsiteUser_controlBox input.delete").click(function(){
      $.post('action/user/delete',{username: username}, function(data, status, jqXHR){
        console.log(data);
      }, 'json');
    });
    //Changing the password:
    $("#OpenBrainWebsiteUser_controlBox input.change").click(function(){
      var password = $('#OpenBrainWebsiteUser_controlBox input[name=password]').val();
      var confirm  = $('#OpenBrainWebsiteUser_controlBox input[name=confirm]').val();
      if(password == confirm){
        $.post('action/user/password',{username: username, password: password}, function(data, status, jqXHR){
          console.log(data);
        }, 'json');
        $('#OpenBrainWebsiteUser_controlBox input[name=password]').val('');
      }
      $('#OpenBrainWebsiteUser_controlBox input[name=confirm]').val('');
    });
  }
};
