SingleResultView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    var view = this;
    window.App.router.on('route:singleResultView', function(rId){
      view.setResultId(rId).always(function(){
        window.App.hideManager.render(view);
      });
    });
    this.views = {
      graph: new SingleResultViewGraph({el: this.$('#SingleResultViewGraph')})
    };
  }
, setResultId: function(rId){
    var p = $.Deferred();
    if(this.model){
      if(this.model.get('id') === rId){
        this.model.fetch();
        p.resolve();
        return p;
      }
    }
    var r = new Item({id: rId}), t = this;
    r.fetch().done(function(){
      t.setResult(r);
      p.resolve();
    }).fail(function(f){
      t.setResult(null);
      p.reject(f);
    });
    return p;
  }
, setResult: function(r){
    if(this.model){
      this.model.off(null, null, this);
    }
    this.model = r;
    if(r){
      this.model.on('change:resultSet', this.render, this);
      if(this.visible())
        this.render();
    }
    _.each(_.values(this.views), function(v){
      v.setResult(this.model);
    }, this);
  }
, render: function(){
    if(this.model){
      var desc = this.model.get('description')
        , rSet = this.model.get('resultSet');
      this.$('h1').text(desc.headline);
      var c = rSet.setCreation;
      c = c ? 'Result created: '+this.model.stripFractionFromTime(c) : '';
      this.$('summary').text(c);
    }else{
      this.$('h1 summary').text('');
    }
  }
});
