(function () {
    var converter = Markdown.getSanitizingConverter();
	var editor = new Markdown.Editor(converter);
    editor.run();
            
    var converter2 = Markdown.getSanitizingConverter();
	var editor2 = new Markdown.Editor(converter2, "-2");
    editor2.run();
})()