SingleDiscussionView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    var view = this;
    window.App.router.on('route:singleDiscussionView', function(did){
      view.setDiscussionId(did).always(function(){
        window.App.hideManager.render(view);
      });
    });
  }
, render: function(){
    if(this.model){
      this.$('h1').html(this.model.get('headline'));
      this.$('.creation').html('Creation: '+this.model.get('creationTime'));
      var deletion = this.model.get('deletionTime');
      deletion = deletion ? ('Deletion: ' + deletion) : '';
      this.$('.deletion').html(deletion);
      var deadline = this.model.get('deadline');
      deadline = deadline ? ('Deadline: ' + deadline) : '';
      this.$('.deadline').html(deadline);
      this.$('summary').html(this.model.get('description'));
    }else{
      this.$('h1').html('Discussion not found!');
      this.$('.creation, .deletion, .deadline').html('');
      this.$('summary').html('The requested discussion was not found on the server.');
    }
  }
, setDiscussion: function(d){
    if(this.model !== null && typeof(this.model) !== 'undefined'){
      this.model.off(null, null, this);
    }
    this.model = d;
    if(d){
      this.model.on('change:headline',     this.render, this);
      this.model.on('change:creationTime', this.render, this);
      this.model.on('change:deletionTime', this.render, this);
      this.model.on('change:deadline',     this.render, this);
      this.model.on('change:description',  this.render, this);
    }
  }
, setDiscussionId: function(did){
    var view = this;
    var d = new Discussion({id: did});
    var p = $.Deferred();
    if(this.model){
      if(did === this.model.get('id')){
        this.model.fetch();
        p.resolve();
        return p;
      }
    }
    d.fetch().done(function(){
      view.setDiscussion(d);
      p.resolve();
    }).fail(function(f){
      view.setDiscussion(null);
      p.reject(f);
    });
    return p;
  }
});
