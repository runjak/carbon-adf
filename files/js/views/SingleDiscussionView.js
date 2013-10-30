SingleDiscussionView = Hideable.extend({
  events: {'click #SingleDiscussionViewEvaluate': 'discussionEvaluate'}
, initialize: function(){
    this.HideTarget = this.$el.parent();
    this.resultsTab = this.$('#SingleDiscussionViewResultsTab');
    this.discussionArticleView = new DiscussionArticleView({
      el: this.$('#SingleDiscussionViewArticles'), model: null});
    this.discussionCollectedView = new DiscussionCollectedView({
      el:    this.$('#SingleDiscussionViewCollected')
    , model: window.App.collectedArticles
    });
    this.discussionGraphView = new DiscussionGraphView({
      el: this.$('#SingleDiscussionViewGraph'), model: null});
    this.discussionResultView = new DiscussionResultView({
      el: this.$('#SingleDiscussionViewResults'), model: null});
    this.discussionParticipantView = new DiscussionParticipantView({
      el: this.$('#SingleDiscussionViewParticipants'), model: null});
    this.relationCreationModal = new RelationCreationModal({
      el: this.$('#RelationCreationModal'), model: null});
    this.articleConditionModal = new ArticleConditionModal({
      el: this.$('#ArticleConditionModal'), model: null});
    this.discussionDlFileView = new DiscussionDlFileView({
      el: this.$('#SingleDiscussionViewFiles'), model: null}); 
    this.subViews = ['discussionArticleView'    , 'discussionCollectedView'
                    ,'discussionGraphView'      , 'discussionResultView'
                    ,'discussionParticipantView', 'relationCreationModal'
                    ,'articleConditionModal'    , 'discussionDlFileView'];
    var view = this;
    window.App.router.on('route:singleDiscussionView', function(did){
      view.setDiscussionId(did).always(function(){
        window.App.hideManager.render(view);
      });
    });
    window.App.router.on('route:singleDiscussionViewTab', function(did, tab){
      view.setDiscussionId(did).always(function(){
        window.App.hideManager.render(view);
        var tid = window.setTimeout(function(){
          view.setTab(tab);
          window.clearTimeout(tid);
        }, 200);
      });
    });
    this.$('#SingleDiscussionViewTabs a').click(function(e){
      view.tabClicked($(this).parent().data('tabname'));
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
      this.model.results.hasResults() ? this.resultsTab.show()
                                      : this.resultsTab.hide();
    }else{
      this.$('h1').html('Discussion not found!');
      this.$('.creation, .deletion, .deadline').html('');
      this.$('summary').html('The requested discussion was not found on the server.');
      this.$('#SingleDiscussionViewResultsTab').hide();
      this.resultsTab.hide();
    }
  }
, setDiscussion: function(d){
    if(this.model){
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
    var view = this;
    _.each(this.subViews, function(v){
      view[v].setModel(d);
    });
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
, setTab: function(tab){
  //this.$('#SingleDiscussionViewTabs li[data-tabname="'+tab+'"] a').tab('show');
    this.$('#SingleDiscussionViewTabs li[data-tabname="'+tab+'"] a').trigger('click');
  }
, tabClicked: function(tab){
    if(!this.model) return;
    did = this.model.get('id');
    window.App.router.navigate('discussion/'+did+'/'+tab);
  }
, discussionEvaluate: function(){
    alert('Evaluating the discussion…');
    $.get(this.model.url()+'/evaluate');
    var t = this, tid = window.setTimeout(function(){
      window.clearTimeout(tid);
      t.model.fetch().always(function(){
        t.setDiscussion(t.model);
      });
      alert('Evaluation should now be complete…');
    }, 1000);
  }
});
