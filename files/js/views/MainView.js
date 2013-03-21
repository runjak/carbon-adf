MainView = Backbone.View.extend({
  initialize: function(){
    this.cInf = this.options.currentInformation;
    this.listenTo(this.cInf, "change", this.render);
  }
, render: function(){
    //FIXME some more magic here, pls.
  }
});
