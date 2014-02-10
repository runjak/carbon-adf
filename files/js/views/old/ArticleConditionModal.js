/**
  el: The modal .)
, model: A discussion
*/ ArticleConditionModal = Backbone.View.extend({
  initialize: function(){
    this.headline = this.$('h3');
    this.input = this.$('#ArticleCondition');
    this.setModel(this.model);
    var modal = this;
    this.$('.modal-header > button').click(function(){
      modal.showModal(false);
    });
  }
, events: {'click #ArticleConditionUpdate': 'update'}
, update: function(){
    var cid = this.model.get('collectionId');
    var aid = this.article.get('id');
    var url = 'article/'+aid+'/collection/'+cid+'/condition'
    var data = {condition: this.input.val()};
    var view = this;
    $.put(url, data).done(function(){
      view.model.fetch();
      view.showModal(false);
    }).fail(function(e){
      alert(e);
    });
  }
, display: function(article){
    if(!article || !this.model) return;
    this.article = article;
    this.headline.html('Article to update the condition for: '+article.get('headline'));
    this.input.val(article.get('condition'));
    this.showModal(true);
  }
, setModel: function(m){this.model = m;}
, showModal: function(show){
    window.App.views.singleDiscussionView.discussionGraphView.setUseKeyboard(!show);
    if(show) this.$el.modal('show');
    else     this.$el.modal('hide');
  }
});
