#!/bin/sh

if [ -z "$1" -o -z "$2" ]; then
	echo "Syntax: $0 <test_name> <module>"
	exit 1
fi

cover_to_html () {
	test_name=$1
	pretty_name=`echo $test_name | tr "_" " "`
	module=$2

	in=cover_${test_name}_${module}.out
	out=cover_${test_name}_${module}.html
	pct=cover_${test_name}_${module}.percent

	percent=`cat $pct`

	headline=`head -n 1 "$in"`
	timestamp=${headline#*COVER }

	# Escape existing HTML (documentation)
	cat << EOF > "$out"
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>Testsuite coverity / ${pretty_name} / ${module}</title>
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
<h1>Testsuite coverity: <span style="text-transform: capitalize;">${pretty_name}</span> / <span style="color: #9fc550;">${module}</span></h1>
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
	-e 's,^\([[:space:]]*|[[:space:]]\{2\}[[:alpha:]][a-zA-Z0-9_]*\)\(.*\),<span class="func">\1</span>\2,'	\
	>> "$out"

	cat << EOF >> "$out"
</pre>
</body>
EOF
}

update_index () {
	out=index.html

	timestamp=`date +'%Y-%m-%d at %H:%M:%S'`

	cat << EOF > "$out"
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>Testsuite coverity</title>
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
#timestamp {
  margin-top: 0px;
  margin-bottom: 3em;
  color: #777;
  font-style: italic;
  border-bottom: solid 1px #000;
}
table {
  border: 1px solid #999;
  border-collapse: collapse;
  background-color: #f9f9f9;;
}
th, td {
  padding: 4px;
}
th {
  border-bottom: 1px solid #999;
  background-color: #ebebeb;
}
</style>
</head>
<body>
<h1>Testsuite coverity</h1>
<p id="timestamp">Date: ${timestamp}</p>
EOF

	color_0="ff2200"
	color_5="ff3800"
	color_10="ff4e00"
	color_15="ff6400"
	color_20="ff7a00"
	color_25="ff9000"
	color_30="ffa500"
	color_35="ffbb00"
	color_40="ffd100"
	color_45="ffe700"
	color_50="fdfc00"
	color_55="ecff00"
	color_60="d6ff00"
	color_65="c0ff00"
	color_70="aaff00"
	color_75="94ff00"
	color_80="7eff00"
	color_85="69ff00"
	color_90="53ff00"
	color_95="3dff00"
	color_100="27ff00"

	for test_name in *.erl; do
		test_name=${test_name%.erl}
		pretty_name=`echo $test_name | tr "_" " "`
		cat << EOF >> "$out"
<h2 style="text-transform: capitalize;">$pretty_name</h2>
<table>
<tr>
<th style="width: 200px; text-align: left;">Module</th>
<th style="width: 100px; text-align: right;" colspan="2">Coverage</th>
</tr>
EOF

		for mod_out in cover_${test_name}_*.out; do
			name=${mod_out%.out}
			mod=${name#cover_${test_name}_}
			percent=`cat $name.percent 2> /dev/null`
			color=`echo $percent | sed -e 's/\..*$//'`
			if [ $((color % 5)) -gt 0 ]; then
				color=$((color + (5 - color % 5)))
			fi
			color=`eval echo \\${color_$color}`
			cat << EOF >> "$out"
<tr>
<td><a href="${name}.html">$mod</a></td>
<td style="text-align: right;">$percent %</td>
<td style="width: 5px; padding: 0px; background-color: #$color;"></td>
</tr>
EOF
		done

		cat << EOF >> "$out"
</table>
EOF
	done

	cat << EOF >> "$out"
</body>
</html>
EOF
}

cover_to_html $1 $2
update_index
