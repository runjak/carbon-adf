InformationContentView = Backbone.View.extend({
  initialize: function(){
    marked.setOptions({
      gfm:      true
    , pedantic: false
    , sanitize: true
    });
    if(this.model)
      this.listenModel();
    this.render();
  }
, render: function(){
    var m = this.model;
    if(m.isContent()){
      this.$el.html(marked(m.get('media')));
    }
  }
, setModel: function(m){
    this.stopListening(this.model);
    this.model = m;
    this.listenModel();
  }
, listenModel: function(){
    this.listenTo(this.model, "change", this.render);
  }
});
