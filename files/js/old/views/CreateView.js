CreateView = Backbone.View.extend({
  initialize: function(){
    this.logger = window.App.logger;
    this.i      = new Information();
    this.iView  = new InformationContentView({
      el: $('#CreatePreview')
    , model: this.i
    });
    this.logger.watch(this.i);
    var t = this;
    t.dAddLink = t.mkDialog( $('#CreateAddLinkD')
                           , 'Add link'
                           , function(){ t.addLink(); });
    t.dAddImage = t.mkDialog( $('#CreateAddImageD')
                            , 'Add image'
                            , function(){ t.addImage(); });
    this.onChangeTitle().onChangeDescription().onChangeContent();
  }
, render: function(){}
, addLink: function(){
    //Fetching:
    var alt  = $('#CreateAddLinkAlt').val();
    var text = $('#CreateAddLinkText').val();
    var url  = $('#CreateAddLinkUrl').val();
    //Composing:
    alt = alt.length ? ' "'+alt+'"' : '';
    var t = "[" + text + "](" + url + alt + ")";
    this.insertAtCursor(t);
    //Clearing:
    $('#CreateAddLinkAlt, #CreateAddLinkText, #CreateAddLinkUrl').val('');
  }
, addImage: function(){
    //Fetching:
    var alt   = $('#CreateAddImageAlt').val();
    var title = $('#CreateAddImageTitle').val();
    var url   = $('#CreateAddImageUrl').val();
    //Composing:
    title = title.length ? ' "'+title+'"' : '';
    var t = "![" + alt + "](" + url + title + ")";
    this.insertAtCursor(t);
    //Clearing:
    $('#CreateAddImageAlt, #CreateAddImageTitle, #CreateAddImageUrl').val('');
  }
, mkDialog: function(el, t, h){
    el.dialog({ autoOpen: false, width: 400
    , buttons:  [{text: t, click: function(){
        h(); el.dialog('close');
      }}]
    });
    return function(){ el.dialog('open'); };
  }
, events: {
    "keyup #CreateTitle":       "onChangeTitle"
  , "keyup #CreateDescription": "onChangeDescription"
  , "keyup #CreateContent":     "onChangeContent"
  , "click #CreateNew":         "onSave"
  , "click #CreateAddLinkB":    "onLink"
  , "click #CreateAddImageB":   "onImage"
  , "click #CreateHelp":        "onHelp"
  }
, onChangeTitle: function(){ this.i.set({title: $('#CreateTitle').val()}); return this; }
, onChangeDescription: function(){ this.i.set({description: $('#CreateDescription').val()}); return this; }
, onChangeContent: function(){ this.i.set({media: $('#CreateContent').val()}); return this; }
, onSave: function(){
    var ok = true;
    $('#CreateTitle, #CreateDescription, #CreateContent').each(function(i, e){
      if($(e).val() === '') ok = false;
    });
    if(!ok){
      this.logger.log("You need all of title, description and content to save new informations.", true);
      return;
    }
    var t = this;
    t.i.create(function(){
      t.model.set(t.i.attributes);
      $('#CreateTitle, #CreateDescription, #CreateContent').val('');
      t.i = new Information();
      t.iView.setModel(t.i);
      t.logger.watch(t.i);
    });
  }
, onLink: function(){ this.dAddLink(); }
, onImage: function(){ this.dAddImage(); }
, onHelp: function(){
    var win = window.open('http://daringfireball.net/projects/markdown/syntax', '_blank');
    win.focus();
  }
, insertAtCursor: function(t){
    var myField = $('#CreateContent').get(0);
    //Solution from http://stackoverflow.com/questions/1621931/insert-text-on-the-current-place-of-the-cursor-in-the-browser
    var doc = myField.ownerDocument;
    //IE support
    if (doc.selection) {
      myField.focus();
      sel = doc.selection.createRange();
      sel.text = t;
    }
    //FF, hopefully others
    else if (myField.selectionStart || myField.selectionStart == '0') {
      var start = myField.selectionStart;
      var end   = myField.selectionEnd;
      var val   = myField.value;
      myField.value = val.substring(0, start) + t + val.substring(end, val.length);
    } 
    // fallback to appending it to the field
    else {
      myField.value += t;
    }
    this.onChangeContent();
  }
});
