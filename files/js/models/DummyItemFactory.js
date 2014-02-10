DummyItemFactory = Backbone.Model.extend({
  defaults: {i: 0, item: null}
//Reset the DummyItemFactory to produce dummies for another Item.
, reset: function(item){
    this.set({item: item, i: 0});
    return this;
  }
//Generates the next necessary item.
, nextItem: function(){
    var i = this.get('item');
    if(!i){
      console.log('DummyItemFactory:nextItem() with invalid Item.');
      return;
    }
    if(!window.App.login.get('loggedIn')){
      console.log('DummyItemFactory:nextItem() requires a login.');
      return;
    }
    var nameSet = {};
    i.discussion.arguments.each(function(a){
      var desc = a.get('description');
      if(desc && desc.headline){
        nameSet[desc.headline] = true;
      }
    });
    var found = false, name = null;
    while(!found){
      name = this.nextName();
      if(!nameSet[name])
        found = true;
    }
    var i = new Item({
      commiteMessage: 'Automatically generated dummy Item.'
    , commitAuthor: window.App.login.get('id')
    });
    return i.setDescription(name, 'Nothing much to say about a dummy Item.')
            .setArticle('Dummy content');
  }
, nextName: function(){
    var name = ''
      , i = this.get('i');
    this.set({i: i+1});
    do{
      name = String.fromCharCode(97 + i % 26) + name;
      i = Math.floor(i / 26);
    }while(i > 0);
    return name;
  }
});
