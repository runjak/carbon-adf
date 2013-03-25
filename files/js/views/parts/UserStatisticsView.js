UserStatisticsView = Backbone.View.extend({
  displayFields: [
      {field: 'username',     title: 'Username:'}
    , {field: 'userCreation', title: 'Created:'}
    , {field: 'lastLogin',    title: 'Last login:'}
    , {field: 'karma',        title: 'Karma:'}]
, initialize: function(){
    if(this.model) this.setModel(this.model);
  }
, render: function(){
    var $el = this.$el;
    var model = this.model;
    $el.html('');
    if(!model) return;
    var attrs = model.attributes;
    $(this.displayFields).each(function(i, e){
      if(!attrs[e.field]) return;
      $el.append("<dt>"+e.title+"</dt>"+"<dd>"+attrs[e.field]+"</dd>");
    });
  }
, setModel: function(model){
    if(this.model) this.stopListening(this.model);
    if(model) this.listenTo(model, "change", this.render);
    this.model = model;
    this.render();
  }
});
