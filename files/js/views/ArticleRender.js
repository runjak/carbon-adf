ArticleRender = Hideable.extend({
  initialize: function(){
    this.setModel(this.model);
    this.renderTarget = this.$el;
  }
, render: function(){
    if(!this.model){
      this.renderTarget.html('');
    }else{
      var headline    = this.model.get('headline');
      var creation    = this.model.get('creationTime');
      var deletion    = this.model.get('deletionTime');
      var description = this.model.get('description');
      var content     = this.model.get('content');
      if(!headline) headline = '';
      creation = creation ? ('created: '+creation) : '';
      deletion = deletion ? ('deleted: '+deletion) : '';
      if(!description) description = '';
      if(!content) content = '';
      this.renderTarget.html('<article>'
        + '<h1>'+headline+'</h1>'
        + '<div class="creation">'+creation+'</div>'
        + '<div class="deletion">'+deletion+'</div>'
        + '<summary>'+description+'</summary><hr>'
        + '<div>'+marked(content)+'</div>'
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
