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
    $("#OpenBrainWebsiteUser_loginBox button.login").click(function(){loginAction("login");});
    //Creating a new user:
    $("#OpenBrainWebsiteUser_loginBox button.create").click(function(){loginAction("create");});
  }
  //Handling password change/deletion by users:
  if($("#OpenBrainWebsiteUser_controlBox").length > 0){
    var username = $("#OpenBrainWebsiteUser_controlBox input[name=username]").val();
    console.log("username: " + username);
    //Logout:
    $("#OpenBrainWebsiteUser_controlBox button.logout").click(function(){
      $.post('action/user/logout', {}, function(data, status, jqXHR){
        console.log(data);
      }, 'json');
    });
    //Deleting the user:
    $("#OpenBrainWebsiteUser_controlBox button.delete").click(function(){
      var req = {'username': username};
      console.log("Sending req:");
      console.log(req);
      $.post('action/user/delete', req, function(data, status, jqXHR){
        console.log(data);
      }, 'json');
    });
    //Changing the password:
    $("#OpenBrainWebsiteUser_controlBox button.change").click(function(){
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
