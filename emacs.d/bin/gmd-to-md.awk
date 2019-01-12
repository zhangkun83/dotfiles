#!/usr/bin/awk -f
# Convert github flavor markdown to basic markdown.
# markdown.pl recoganize 4 leading spaces per line instead of ```
# code blocks. So we convert ``` to <pre><code> instead.
BEGIN {
    v = 1;
}
{
    if (match($0, /^```.*/)) {
	if (v % 2) {
	    print "<pre><code>";
	} else {
	    sub(/^```/, "</code></pre>");
	    print;
	}
	v++;
    } else {
	if (v % 2 == 0) {
	    # Inside a code block, escape &, < and >
	    gsub(/&/, "\\&amp;");
	    gsub(/</, "\\&lt;");
	    gsub(/>/, "\\&gt;");
	}
	print;
    }
}
