var foo;
$(function(){
var router = new Router;
//Model setup:
var login = new Login();
var currentInformation = new CurrentInformation();
foo = currentInformation;
//View setup:
var logger = new Logger({el: $('#log')});
logger.watch([login]);
var mainView    = new MainView({el: $('#view'), currentInformation: currentInformation});
var createView  = new CreateView({el: $('#create'),   logger: logger, currentInformation: currentInformation});
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
router.on('route:mainView', function(inf){
  if(inf) currentInformation.set({id: inf});
});
router.on('route:loginView', function(){ loginView.render(); });
router.on('route:profileView', function(){ profileView.render(); });
router.on('route:adminView', function(page){
  if(page)
    adminView.pager.setPage(page);
  adminView.render();
});
Backbone.history.start();
//Tooltips:
$('body').tooltip();
});
