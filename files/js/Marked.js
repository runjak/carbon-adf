function initMarked(){
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
  //Looking for InformationContent to render:
  $('div.InformationContent').each(function(i, v){
    $(v).html(marked($(v).text()));
  });
  //Installing the editor if possible:
  if($('#MarkdownEditor').length == 1){  
    //Initial preview:
    render();
    //Preview on change of content:
    $('#InformationContent').keyup(function(){
      render();
    });
  }
};
