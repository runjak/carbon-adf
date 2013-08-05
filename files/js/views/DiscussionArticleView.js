DiscussionArticleView = Backbone.View.extend({
  initialize: function(){
    this.setModel(this.model);
  }
, render: function(){
    var el = this.$('tbody').empty();
    if(this.model){
      var as = this.model.articles;
      if(as.length === 0){
        el.append('<tr><td colspan="4">There are currently no articles in this Discussion.</td></tr>');
      }else{
        as.map(function(a){
          var id          = a.get('id');
          var headline    = a.get('headline');
          var description = a.get('description');
          el.append('<tr><td>'+id+'</td>'
                   +'<td><a href="#/article/'+id+'">'+headline+'</a></td>'
                   +'<td>'+description+'</td>'
                   +'<td><button class="removeFromDiscussion" data-id="'+id+'">'
                     +'<i class="icon-trash"></i>Remove from Discussion'
                   +'</button></td></tr>');
        });
        var discussion = this.model;
        this.$('.removeFromDiscussion').click(function(){
          var id = $(this).attr('data-id');
          discussion.removeArticle(as.elemById(id));
        });
      }
    }else{
      el.append('<tr><td colspan="4">You must know some sorcery,'
               +' so that you can see this.</td></tr>');
    }
  }
, setModel: function(d){
    if(this.model){
      this.model.articles.off(null, null, this);
    }
    this.model = d;
    if(d){
      this.model.articles.on('reset add remove', this.render, this);
    }
    this.render();
  }
});
