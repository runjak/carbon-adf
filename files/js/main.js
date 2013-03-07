$(function(){
var router = new Router;
router.on('route:defaultRoute', function(actions){
  log('router with actions:\t' + actions);
});

Backbone.history.start();

var loginView = new LoginView({el: $('#login')});
console.log(loginView);
});
