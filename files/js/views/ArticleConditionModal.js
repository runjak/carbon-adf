/**
  el: The modal .)
  model: A discussion
*/
ArticleConditionModal = Backbone.View.extend({
  initialize: function(){
    this.headline = this.$('h3');
    this.input    = this.$('#ArticleCondition');
    this.article  = null; // Scratchpad for Article
    this.setModel(this.model);
    var modal = this;
    this.$('.modal-header > button').click(function(){
      modal.showModal(false);
    });
  }
, events: {'click #ArticleConditionUpdate': 'update'}
, update: function(){
    if(!this.article){
      console.log('ArticleConditionModal:update() called without a valid article.');
      return;
    }
    var formula = this.input.val(), view = this;
    this.article.setFormula(formula).mySave().done(function(){
      view.showModal(false);
    }).fail(function(e){
      alert('There was an error saving the updated condition: '+JSON.stringify(e));
      console.log('Error in ArticleConditionModal:update()\n'+JSON.stringify(e));
    });
  }
, display: function(article){
    if(!article){
      console.log('ArticleConditionModal:display() called without a valid article.');
      return;
    }
    if(!this.model){
      console.log('ArticleConditionModal:display() while no model was set.');
      return;
    }
    this.article = article;
    var headline = article.get('description').headline
      , formula  = article.get('condition').formula;
    this.headline.html('Article to update the condition for: '+headline);
    this.input.val(formula);
    this.showModal(true);
  }
, setModel: function(m){this.model = m;}
, showModal: function(show){
    window.App.views.singleDiscussionView.discussionGraphView.useKeyboard = !show;
    this.$el.modal(show ? 'show' : 'hide');
  }
});
