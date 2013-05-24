MainView = Backbone.View.extend({
  initialize: function(){
    this.listenTo(this.model, "change", this.render);
    this.relationView = new InformationRelationView({el: this.$('#ViewInformationRelations'), model: this.model});
    this.contentView = new InformationContentView({el: this.$('#ViewInformationContent'), model: this.model});
  }
, render: function(){
    this.$('#ViewInformationTitle').html(this.model.get('title'));
    this.$('#ViewInformationDesc').html(this.model.get('description'));
  }
});
