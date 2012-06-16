function initUser(){
  //Handling the loginBox:
  if($("#OpenBrainWebsiteUser_loginBox").length > 0){
    var loginAction = function(action){
      var url = "action/user/" + action;
      var data = {
        username: $('#OpenBrainWebsiteUser_loginBox input[name=username]').val()
      , password: $('#OpenBrainWebsiteUser_loginBox input[name=password]').val()
      };
      console.log(data);
      $.post(url, data, function(data, status, jqXHR){
        console.log("Got data:" + data);
        console.log("Got status:" + status);
      }, "json");
    };
    $("#OpenBrainWebsiteUser_loginBox input#login").click(function(){loginAction("login");});
    $("#OpenBrainWebsiteUser_loginBox input#create").click(function(){loginAction("create");});
  }
};
