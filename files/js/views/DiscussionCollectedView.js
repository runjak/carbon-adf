DiscussionCollectedView = Backbone.View.extend({
  initialize: function(){
    this.item = null;
    this.model.on('reset add remove', this.render, this);  
  }
, render: function(){
    if(!this.item) return;
    var el = this.$('tbody').empty();
    if(this.model.length === 0){
      el.append('<tr><td colspan="4">No articles collected, so none can be displayed.</td><tr>');
    }else{
      var das = this.item.discussion.arguments;
      this.model.map(function(a){
        var id = a.get('id');
        var desc = $.extend({headline: '', summary: ''}, a.get('description'));
        if(das.elemById(id)) return;
        el.append('<tr><td>'+id+'</td>'
                 +'<td><a href="#/article/'+id+'">'+desc.headline+'</a></td>'
                 +'<td>'+desc.summary+'</td><td>'
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
        var item = this.item;
        el.find('.addToDiscussion').click(function(){
          var id = $(this).attr('data-id');
          item.set({canCommit: true}).discussion.arguments.add(model.elemById(id));
        });
        el.find('.removeFromCollection').click(function(){
          var id = $(this).attr('data-id');
          model.remove(model.elemById(id));
        });
      }
    }
  }
, setDiscussion: function(i){
    if(this.item){
      this.item.discussion.arguments.off(null, null, this);
    }
    this.item = i;
    this.render();
    this.item.discussion.arguments.on('reset add remove', this.render, this);
  }
// Note that this is an alias to comply with SingleDiscussionView,
// While this View has a different model.
, setModel: function(m){this.setDiscussion(m);}
});
