function initEdit(){
  //Break if no target is found:
  if($('#MarkdownEditor').length != 1)
    return;
  //Setting marked options:
  marked.setOptions({
    gfm:      true
  , pedantic: false
  , sanitize: true
  });
  //Function to render MarkdownPreview:
  var render = function(){
    var input = $('#InformationContent').val();
    if(input == "")
      input = "Nothing there to preview.";
    $('#MarkdownPreview').html(marked(input));
  };
  //Preview on change of content:
  $('#InformationContent').keyup(function(){
    render();
  });
  //Initial preview:
  render();
};
