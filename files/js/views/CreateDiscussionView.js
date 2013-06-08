CreateDiscussionView = Hideable.extend({
  initialize: function(){
    this.$('textarea').autoResize();
    this.setModel(new Discussion());
    this.datepicker = this.$('#CreateDiscussionViewDate').datepicker({format: 'yyyy-mm-dd'});
    var view = this;
    this.datepicker.on('changeDate', function(){view.syncDeadline();});
    window.App.router.on('route:createDiscussionView', function(){
      window.App.hideManager.render(view);
    });
    window.App.collectedArticles.on('reset add remove', this.renderIfVisible, this);
  }
, events: {
    "click #CreateDiscussion":                "create"
  , "keyup #CreateDiscussionViewHeadline":    "syncHeadline"
  , "keyup #CreateDiscussionViewDate":        "syncDeadline"
  , "keyup #CreateDiscussionViewTime":        "syncDeadline"
  , "keyup #CreateDiscussionViewDescription": "syncDescription"
  }
, render: function(){
    var target = this.$('#CreateDiscussionInitialArticles');
    var elems  = window.App.collectedArticles;
    if(elems.length === 0){
      target.html('<li class="nav-header">No articles collected!'
                 +' <i class="icon-arrow-right"></i> '
                 +'Can\'t add them on creation, sorry.</li>');
    }else{
      target.html('<li class="nav-header">Select articles to include them in the discussion:</li>');
      elems.reiterate(function(a){
        var aid = a.get('id');
        var headline = a.get('headline');
        target.append('<li data-aid="'+aid+'">'+headline+'</li>');
      });
    }
  }
, renderIfVisible: function(){
    if(this.visible())
      this.render();
  }
, create: function(e){
    e.preventDefault();
    var view = this;
    this.model.create().done(function(d){
      console.log('CreateDiscussionView.create() worked, dumping data:');
      console.log(d);
      view.setModel(new Discussion);
    }).fail(function(f){
      console.log('CreateDiscussionView.create() failed: '+JSON.stringify(f));
    });
  }
, syncHeadline: function(){this.model.set({headline: this.$('#CreateDiscussionViewHeadline').val()});}
, syncDescription: function(){this.model.set({description: this.$('#CreateDiscussionViewDescription').val()});}
, syncDeadline: function(){
    var date = this.$('#CreateDiscussionViewDate').val();
    var time = this.$('#CreateDiscussionViewTime').val(); // http://dev.w3.org/html5/markup/input.time.html
    if(time === '' || date === '') return;
    this.model.set({deadline: date+' '+time+':00'});
  }
, setModel: function(m){
    this.model = m;
    this.$('#CreateDiscussionViewHeadline').val(m.get('headline'));
    this.$('#CreateDiscussionViewDeadline').val(m.get('deadline'));
    this.$('#CreateDiscussionViewDescription').val(m.get('description'));
  }
});
