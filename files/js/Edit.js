function initEdit(){
  //Break if no target is found:
  if($('#wysihtml5-textarea').length != 1)
    return;
  //Load the editor:
  var editor = new wysihtml5.Editor("wysihtml5-textarea", {
    toolbar:      "EditorToolbar"
  , parserRules:  wysihtml5ParserRules
  });
  //Function to resize the iframe:
  var resize = function(){
    var frame = $("iframe.wysihtml5-sandbox")[0];
    var body = frame.contentWindow.document.body;
    var height = $(body).height();
    $(frame).height(height);
    $("#wysihtml5-textarea").height(height);
  };
  $("a#resize").click(resize);
};
