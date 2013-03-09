$(function(){
var router = new Router;
//View setup:
var logger      = new Logger({el: $('#log')});
var loginView   = new LoginView({el: $('#login')});
var profileView = new ProfileView({el: $('#profile')});
var adminView   = new AdminView({el: $('#admin')});
loginView.setProfileView(profileView);
profileView.setLoginView(loginView);
var topMenuView = new TopMenuView({
    el: $('#main')
  , 'logger': logger
  , 'router': router
  , children: [loginView, profileView, adminView]
  , hidden: [profileView]
  });
//Router setup:
router.on('route:defaultRoute', function(actions){
  console.log('router with actions:\t' + actions);
});
router.on('route:loginView', function(){ loginView.render(); });
router.on('route:profileView', function(){ profileView.render(); });
router.on('route:adminView', function(page){
  if(page)
    adminView.pager.setPage(page);
  adminView.render();
});
Backbone.history.start();
});
