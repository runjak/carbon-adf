MainView = Backbone.View.extend({
  initialize: function(){
    this.listenTo(this.model, "change", this.render);
    this.relationView = new InformationRelationView({el: this.$('#ViewInformationRelations'), model: this.model});
  }
, render: function(){
    //FIXME some more magic here, pls.
    this.$('#ViewInformationTitle').html(this.model.get('title'));
    this.$('#ViewInformationDesc').html(this.model.get('description'));
    console.log('MainView:render()');
  }
});
