CreateDiscussionView = Hideable.extend({
  initialize: function(){
    this.$('textarea').autoResize();
    this.setModel(new Discussion());
    var view = this;
    this.datepicker = this.$('#CreateDiscussionViewDate').datepicker({
      format: 'yyyy-mm-dd'
    }).on('changeDate', function(){
      view.syncDeadline();
    });
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
      var view = this;
      target.html('<li class="nav-header">Select articles to include them in the discussion:</li>');
      elems.reiterate(function(a){
        var aid = a.get('id');
        var headline = a.get('headline');
        var creation = a.stripFractionFromTime(a.get('creationTime'));
        var item = '<li>'
                 + '<form class="form-inline">'
                 + '<label class="checkbox" title="'+creation+'">'
                 + '<input type="checkbox">'
                 + headline
                 + '</label>'
                 + '<a class="btn pull-right uncollect" title="Remove article from collection">'
                 + '<i class="icon-minus"></i>'
                 + '</a>'
                 + '<a class="btn pull-right" href="#/article/'+aid+'" title="View this article.">'
                 + '<i class="icon-eye-open"></i>'
                 + '</a>'
                 + '</form>'
                 + '</li>';
        target.append(item);
        target.find('li:last a.uncollect').click(function(){
          window.App.collectedArticles.remove(a);
        });
        target.find('li:last input[type="checkbox"]').click(function(){
          view.model.get('articles').toggleElem(a);
        });
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
