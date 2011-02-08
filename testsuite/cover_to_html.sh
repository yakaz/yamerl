#!/bin/sh

cover_to_html () {
	module=$1

	in=cover_${module}.out
	out=cover_${module}.html
        pct=cover_${module}.percent

        percent=`cat $pct`

	headline=`head -n 1 "$in"`
	timestamp=${headline#*COVER }

	# Escape existing HTML (documentation)
	cat << EOF > "$out"
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>Testsuite coverity of "${module}"</title>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
<style type="text/css">
body {
	color: #000;
	background-color: #fff;
        font-family: sans-serif;
}
h1 {
	margin: 0px;
}
#percent {
        position: absolute;
        top: 10px;
        right: 10px;
        margin: 0px;
        font-size: 28px;
        font-weight: bold;
}
#timestamp {
	margin-top: 0px;
	margin-bottom: 3em;
	color: #777;
	font-style: italic;
	border-bottom: solid 1px #000;
}
.cvsid, .func {
	font-weight: bold;
}
.cvsid {
	color: #461b7e;
}
.cov {
	color: #007a00;
}
.notcov {
	color: #f00;
}
.comment {
	color: #777;
}
</style>
</head>
<body>
<h1>Testsuite coverity of <span style="color: #9fc550;">${module}</span></h1>
<p id="percent">$percent %</p>
<p id="timestamp">Date: ${timestamp}</p>
<pre>
EOF

	line_nb=`wc -l "$in"`
	tail -n $((${line_nb% *} - 4)) "$in" | sed								\
	-e 's,<,\&lt;,g' -e 's,>,\&gt;,g'									\
	-e 's,^\([[:space:]]*0\.\.|.*\),<span class="notcov">\1</span>,'					\
	-e 's,^\([[:space:]]*[1-9][0-9]*\.\.|.*\),<span class="cov">\1</span>,'					\
	-e 's,^\([[:space:]]*|[[:space:]][[:space:]]*%+ \$Id.*\),<span class="cvsid">\1</span>,'		\
	-e 's,^\([[:space:]]*|[[:space:]][[:space:]]*%.*\),<span class="comment">\1</span>,'			\
	-e 's,^\([[:space:]]*|[[:space:]]\{2\}[[:alpha:]][[:alpha:]]*.*\),<span class="func">\1</span>,'	\
	>> "$out"

	cat << EOF >> "$out"
</pre>
</body>
EOF

	rm $in $pct
}

for mod in $@; do
	cover_to_html $mod
done
