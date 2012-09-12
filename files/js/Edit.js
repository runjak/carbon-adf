function initEdit(){
  //Break if no target is found:
  if($('#wysihtml5-textarea').length != 1)
    return;
  //Load the editor:
  var editor = new wysihtml5.Editor("wysihtml5-textarea", {
    toolbar:      "EditorToolbar"
  , parserRules:  wysihtml5ParserRules
  });
};
