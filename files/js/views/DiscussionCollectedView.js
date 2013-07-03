DiscussionCollectedView = Backbone.View.extend({
  initialize: function(){
    this.discussion = null;
    this.model.on('reset add remove', this.render, this);  
  }
, render: function(){
    if(!this.discussion) return;
    var el = this.$('tbody').empty();
    if(this.model.length === 0){
      el.append('<tr><td colspan="4">No articles collected, so none can be displayed.</td><tr>');
    }else{
      var das = this.discussion.get('articles');
      this.model.map(function(a){
        var id          = a.get('id');
        var headline    = a.get('headline');
        var description = a.get('description');
        if(das.elemById(id)) return;
        el.append('<tr><td>'+id+'</td>'
                 +'<td><a href="#/article/'+id+'">'+headline+'</a></td>'
                 +'<td>'+description+'</td><td>'
                 +'<button class="addToDiscussion" data-id="'+id+'">'
                   +'<i class="icon-plus"></i>Add to discussion'
                 +'</button>'
                 +'<button class="removeFromCollection" data-id="'+id+'">'
                   +'<i class="icon-trash"></i>Remove from collection'
                 +'</button></td></tr>');
      });
      if(this.$('tbody>tr').length === 0){
        el.append('<tr><td colspan="4">All collected articles are already added to the discussion.</td></tr>');
      }else{
        var model = this.model;
        var discussion = this.discussion;
        el.find('.addToDiscussion').click(function(){
          var id = $(this).attr('data-id');
          discussion.addArticle(model.elemById(id));
        });
        el.find('.removeFromCollection').click(function(){
          var id = $(this).attr('data-id');
          model.remove(model.elemById(id));
        });
      }
    }
  }
, setDiscussion: function(d){
    if(this.discussion){
      this.discussion.get('articles').off(null, null, this);
    }
    this.discussion = d;
    this.render();
    this.discussion.get('articles').on('reset add remove', this.render, this);
  }
});
