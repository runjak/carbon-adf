function initUser(){
  //Display/Hide LogIn/Out Box:
  $('li#MenuLogin > img').toggle(
    function(){ // Display LoginBox
      $('form#LoginBox').show();
    }
  , function(){ // Hide LoginBox
      $('form#LoginBox').hide();
    }
  );
  //Helper functions for the Buttons:
  var getLoginVals = function(){
    return {
      username: $('form#LoginBox input#Username').val()
    , password: $('form#LoginBox input#Password').val()
    };
  };
  //Controls in the menu:
  var bindMenuControls = function(){
    //Some buttons cause the LoginBox to be replaced:
    var replaceLoginBox = function(reply){
      if(/^FAIL/.test(reply)){
        alert(reply);
      }else{
        $('form#LoginBox').html(reply);
        bindMenuControls();
      }
    };
    //The Login Button:
    $('form#LoginBox button.Login').click(function(){
      $.post("action/user/login", getLoginVals(), function(reply){
        replaceLoginBox(reply);
      });
    });
    //The Create Button:
    $('form#LoginBox button.Create').click(function(){
      $.post("action/user/create", getLoginVals(), function(reply){
        replaceLoginBox(reply);
      });
    });
    //The Logout Button:
    $('form#LoginBox button.Logout').click(function(){
      $.post("action/user/logout", {}, function(reply){
        replaceLoginBox(reply);
      });
    });
    //The Profile Button:
    $('form#LoginBox button.Profile').click(function(){
      var uid = $('form#LoginBox button.Profile').attr('data-uid');
      document.location.href = "/user.html?display=" + uid;
    });
    //The Cancel 'Button':
    $('form#LoginBox a.Cancel').click(function(){
      $('li#MenuLogin > img').trigger('click');
    });
  };
  bindMenuControls();
};
