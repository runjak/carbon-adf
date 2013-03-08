$(function(){
var router = new Router;
router.on('route:defaultRoute', function(actions){
  console.log('router with actions:\t' + actions);
});

Backbone.history.start();
//View setup:
var logger      = new Logger({el: $('#log')});
var loginView   = new LoginView({el: $('#login')});
var profileView = new ProfileView({el: $('#profile')});
loginView.setProfileView(profileView);
var topMenuView = new TopMenuView({
    el: $('#main')
  , 'logger': logger
  , children: [loginView, profileView]
  , hidden: [profileView]
  });
});
