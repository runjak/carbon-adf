SingleDiscussionView = Hideable.extend({
  events: {'click #SingleDiscussionViewEvaluate': 'discussionEvaluate'}
, initialize: function(){
    this.HideTarget = this.$el.parent();
    this.evalButton = this.$('#SingleDiscussionViewEvaluate');
    this.hideTabs   = this.$('#SingleDiscussionViewTabs').find('[data-tabname="collected"], [data-tabname="participants"]');
    this.discussionArticleView = new DiscussionArticleView({el: this.$('#SingleDiscussionViewArticles')});
    this.discussionCollectedView = new DiscussionCollectedView({
      el:    this.$('#SingleDiscussionViewCollected')
    , model: window.App.collectedArticles
    });
    this.discussionParticipantView = new DiscussionParticipantView({el: this.$('#SingleDiscussionViewParticipants')});
    this.discussionGraphView = new DiscussionGraphView({
      el: this.$('#SingleDiscussionViewGraph'), model: null});
    this.articleConditionModal = new ArticleConditionModal({
      el: this.$('#ArticleConditionModal'), model: null});
    this.discussionDlFileView = new DiscussionDlFileView({el: this.$('#SingleDiscussionViewFiles')}); 
    this.subViews = ['discussionArticleView','discussionCollectedView','discussionGraphView','discussionParticipantView','articleConditionModal','discussionDlFileView'];
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
      var desc = $.extend({headline: '', summary: ''}, this.model.get('description'))
        , disc = $.extend({deadline: null}, this.model.get('discussion'))
        , creation = this.model.get('creation')
        , deletion = this.model.get('deletion')
        , formatTime = this.model.stripFractionFromTime;
      deletion = deletion ? ('Deletion: ' + formatTime(deletion)) : '';
      disc.deadline = disc.deadline ? ('Deadline: ' + disc.deadline) : '';
      this.$('h1').html(desc.headline);
      this.$('.creation').html('Creation: ' + formatTime(creation));
      this.$('.deletion').html(deletion);
      this.$('.deadline').html(disc.deadline);
      this.$('summary').html(desc.summary);
      //Show/hide depending on evaluation:
      if(this.model.isEvaluated()){
        this.evalButton.text('Discussion already evaluated')
                       .addClass('btn-danger')
                       .removeClass('btn-success')
                       .attr('disabled', 'disabled');
        this.hideTabs.hide();
      }else{
        this.evalButton.text('Evaluate Discussion')
                       .addClass('btn-success')
                       .removeClass('btn-danger')
                       .removeAttr('disabled');
        this.hideTabs.show();
      }
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
      this.model.on('change:id',           this.updateRoute, this);
      this.model.on('change:headline',     this.render,      this);
      this.model.on('change:creationTime', this.render,      this);
      this.model.on('change:deletionTime', this.render,      this);
      this.model.on('change:deadline',     this.render,      this);
      this.model.on('change:description',  this.render,      this);
    }
    var view = this;
    _.each(this.subViews, function(v){
      view[v].setModel(d);
    });
  }
, setDiscussionId: function(did){
    var view = this;
    var d = new Item({id: did});
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
    this.$('#SingleDiscussionViewTabs li[data-tabname="'+tab+'"] a').trigger('click');
  }
, tabClicked: function(tab){
    if(!this.model) return;
    did = this.model.get('id');
    window.App.router.navigate('discussion/'+did+'/'+tab);
  }
, discussionEvaluate: function(){
    if(this.model.isEvaluated()) return;
    alert('Evaluating the discussion…');
    var t = this
      , tid = null
      , req = $.get(this.model.url()+'/evaluate').done(function(i){
        t.model.set(i);
        console.log(i);
        alert('Evaluation successful!');
      }).fail(function(e){
        console.log(e);
        alert('Evaluation failed. Sorry.');
      }).always(function(){
        window.clearTimeout(tid)
      });
    // We install a watchdog to resolve the evaluation request:
    // The timeout is 11 seconds, which is one more than the servers watchdog for this case.
    tid = window.setTimeout(function(){
      req.abort("Local watchdog decided we've got a timeout.");
    }, 11000);
    return req;
  }
, updateRoute: function(){
    if(!this.visible()) return;
    var oldId     = this.model._previousAttributes.id
      , currentId = this.model.get('id')
      , oldRoute  = Backbone.history.fragment
      , target    = oldRoute.replace(oldId, currentId);
    window.App.router.navigate(target, {trigger: false});
  }
}) ;
