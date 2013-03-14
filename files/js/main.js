$(function(){
var router = new Router;
//Model setup:
var login = new Login();
//View setup:
var logger = new Logger({el: $('#log')});
logger.watch([login]);
var loginView   = new LoginView({el: $('#login'),     login: login});
var profileView = new ProfileView({el: $('#profile'), login: login});
var adminView   = new AdminView({el: $('#admin'),     login: login, router: router, logger: logger});
var topmenuView = new TopmenuView({
    el: $('#main')
  , login:  login
  , router: router
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
