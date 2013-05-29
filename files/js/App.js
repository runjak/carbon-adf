$(function(){
  /*
    The app inspired by
    http://benhowdle.im/2013/03/24/patterns-for-managing-large-scale-backbone-applications/
  */
  //The singleton:
  window.App = {
    hideManager: new HideManager()
  , login:  new Login()
  , router: new Router()
  , views:  {}
  }
  //Setting the views:
  var views = window.App.views;
  views.loginView = new LoginView({
    el: $('#LoginRegisterForm')
  , model: window.App.login
  });
  views.loginState = new LoginState({
    el: $('#LoginState')
  , model: window.App.login
  });
  views.userView = new UserView({el: $('#UserView')});
  views.singleUserView = new SingleUserView({
    el: $('#SingleUserView')
  , model: null
  });
  views.topMenuView = new TopMenuView({el: $('#TopMenu')});
  //Starting the routing:
  Backbone.history.start();
  window.App.router.on('defaultRoute', function(actions){
    console.log('defaultRoute with args:\t'+JSON.stringify(actions));
  });
});
