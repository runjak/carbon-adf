RenderInformationView = Backbone.View.extend({
  initialize: function(){
    marked.setOptions({
      gfm:      true
    , pedantic: false
    , sanitize: true
    });
    if(this.options.information)
      this.watch(this.options.information);
    this.render();
  }
, render: function(){
    var i = this.information;
    if(!i) return;
    if(i.isContent()){
      $(this.el).html(marked(i.get('media')));
    }
  }
, watch: function(information){
    var t = this;
    t.information = information;
    information.on("change:media", function(){ t.render(); });
  }
});
