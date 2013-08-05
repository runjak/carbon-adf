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
    //Setting up the form target for uploads to be safe that it works:
    //This is necessary, because some browsers don't get it from the html o.O
    var f = this.$('form').get(0);
    var i = this.$('iframe').get(0);
    f.target = i.id;
  }
, events: {
    "click #CreateDiscussion":                  "create"
  , "click #CreateDiscussionViewClearDeadline": "clearDeadline"
  , "keyup #CreateDiscussionViewHeadline":      "syncHeadline"
  , "keyup #CreateDiscussionViewDate":          "syncDeadline"
  , "keyup #CreateDiscussionViewTime":          "syncDeadline"
  , "input #CreateDiscussionViewTime":          "syncDeadline"
  , "keyup #CreateDiscussionViewDescription":   "syncDescription"
  , "click #CreateDiscussionViewShowGraphView": "openGraphView"
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
      var finish = function(){};
      var file = view.$('#CreateDiscussionViewInstanceFile').val();
      if(file !== ''){
        var upload = view.$('#CreateDiscussionViewInstanceFileUpload').load(function(){
          upload.unbind('load');
          var result = upload.contents().text();
          console.log('Upload result was:\n'+result)
          view.finishCreation();
        });
        view.$('form').submit();
      }else view.finishCreation();
    }).fail(function(f){
      console.log('CreateDiscussionView.create() failed: '+JSON.stringify(f));
    });
  }
, finishCreation: function(){
    var route = '#/discussion/' + this.model.get('id');
    window.App.router.navigate(route, {trigger: true});
    this.setModel(new Discussion());
  }
, clearDeadline: function(){
    this.$('#CreateDiscussionViewDate').val('');
    this.$('#CreateDiscussionViewTime').val('');
    this.model.set({deadline: null});
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
, openGraphView: function(){
    window.App.views.discussionGraphView.setModel(this.model);
  }
});
