ArticleRender = Hideable.extend({
  initialize: function(){
    this.setModel(this.model);
    this.renderTarget = this.$el;
  }
, render: function(){
    if(!this.model){
      this.renderTarget.html('');
    }else{
      var h = this.model.get('headline');
      var d = this.model.get('description');
      var c = this.model.get('content');
      if(!h) h = '';
      if(!d) d = '';
      if(!c) c = '';
      this.renderTarget.html('<article>'
        + '<h1>'+h+'</h1>'
        + '<summary>'+d+'</summary><hr>'
        + '<div>'+marked(c)+'</div>'
        + '</article>');
    }
  }
, setModel: function(m){
    if(this.model)
      this.model.off(null, null, this);
    this.model = m;
    if(m){
      this.model.on('change', this.render, this);
      this.render();
    }
  }
});
