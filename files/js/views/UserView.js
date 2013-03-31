UserView = Backbone.View.extend({
  initialize: function(){
    this.stats = new UserStatisticsView({
      el: this.$('#UserStatistics')});
  }
, render: function(){
    if(!this.user) return;
  }
, setUser: function(user){
    this.user = user;
    this.stats.setModel(user);
  }
});
