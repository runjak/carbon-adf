CreateDiscussionView = Hideable.extend({
  initialize: function(){
    this.$('textarea').autoResize();
    this.setModel(new Item());
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
        var headline = a.get('description') ? a.get('description').headline : '';
        var creation = a.stripFractionFromTime(a.get('creation'));
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
          view.model.discussion.arguments.toggleElem(a);
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
    this.model.discussionToAttributes().mySave().done(function(d){
      view.model.set(d);
      console.log("Discussion created!");
      console.log(d);
      var finish = function(){};
      // FIXME debug file uploads, they've changed some.
      var file = view.$('#CreateDiscussionViewInstanceFile').val();
      if(file !== ''){
        view.$('form').attr('action', 'item/'+d.id+'/fitinstance');
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
    this.setModel(new Item());
  }
, clearDeadline: function(){
    this.$('#CreateDiscussionViewDate').val('');
    this.$('#CreateDiscussionViewTime').val('');
    this.model.set({deadline: null});
  }
, syncHeadline: function(){
    var d = {headline: this.$('#CreateDiscussionViewHeadline').val()};
    this.model.modify('description', d);
  }
, syncDescription: function(){
    var d = {summary: this.$('#CreateDiscussionViewDescription').val()};
    this.model.modify('description', d);
  }
, syncDeadline: function(){
    var date = this.$('#CreateDiscussionViewDate').val();
    var time = this.$('#CreateDiscussionViewTime').val(); // http://dev.w3.org/html5/markup/input.time.html
    if(time === '' || date === '') return;
    var d = {deadline: date+' '+time+':00'};
    this.model.modify('discussion', d);
  }
, setModel: function(m){
    this.model = m;
    var description = $.extend({headline: '', summary: ''}, m.get('description'))
      , discussion  = $.extend({deadline: ''}, m.get('discussion'));
    this.$('#CreateDiscussionViewHeadline').val(description.headline);
    this.$('#CreateDiscussionViewDeadline').val(discussion.deadline);
    this.$('#CreateDiscussionViewDescription').val(description.summary);
    m.modify('discussion', {id: -1, participants: [window.App.login.get('id')]});
  }
, openGraphView: function(){ // FIXME this should be changed, possibly.
    window.App.views.discussionGraphView.setModel(this.model);
  }
});
