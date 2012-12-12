#!/bin/sh
#-
# Copyright (c) 2012 Yakaz
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

set -e

if [ -z "$1" ]; then
	echo "Syntax: $0 <test_name> [<test_name> ...]"
	exit 1
fi

scriptdir=`dirname $0`
test_coverage=$scriptdir/test_coverage.escript

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

output_test_coverage () {
    cover_data=
    if [ "$1" = "global" ]; then
        for test_name in $@; do
            if [ "$test_name" = "global" ]; then
                continue
            fi

            cover_data="$cover_data ${test_name}.coverdata"
        done

        test_name="global"
        pretty_name="Global coverage"
    else
        test_name=$1
        pretty_name=`echo $test_name | tr "_" " "`
        cover_data="${test_name}.coverdata"
    fi

    # Generate covered modules data. We ignore stdout because cover
    # writes some useless logs.
    prefix=cover_${test_name}
    $test_coverage $prefix $cover_data > /dev/null
    covered_mods=${prefix}_covered_mods

    # Prepare index block.
    index=index_${test_name}.html
    cat << EOF > "$index"
<div class="testsuite" id="${test_name}">
<h2 style="text-transform: capitalize;">$pretty_name</h2>
<table>
<tr>
<th style="width: 200px; text-align: left;">Module</th>
<th style="width: 100px; text-align: right;" colspan="2">Coverage</th>
</tr>
EOF

    # For each module of this test:
    #     o  Write its source code with cover results.
    #     o  Update index block.
    for module in `cat $covered_mods | sort`; do
        in=${prefix}_${module}.out
        pct=${prefix}_${module}.percent
        out=${prefix}_${module}.html

        percent=`cat $pct`

        color=`echo $percent | sed -e 's/\..*$//'`
        if [ $((color % 5)) -gt 0 ]; then
            color=$((color + (5 - color % 5)))
        fi
        color=`eval echo \\${color_$color}`

        headline=`head -n 1 "$in"`
        timestamp=${headline#*COVER }

        # Update source file to emphasize cover results.
        cat << EOF > "$out"
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>Testsuite coverage / ${pretty_name} / ${module}</title>
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
  padding-right: 8px;
  border-right: solid 5px #${color};
}
#timestamp {
  margin-top: 0px;
  madding-bottom: 0px;
  color: #777;
  font-style: italic;
  border-bottom: solid 1px #000;
}
#back {
  margin-top: 0px;
  margin-bottom: 3em;
  font-size: 10px;
  text-align: right;
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
<h1>Testsuite coverage: <span style="text-transform: capitalize;">${pretty_name}</span> / <span style="color: #9fc550;">${module}</span></h1>
<p id="percent">$percent %</p>
<p id="timestamp">Date: ${timestamp}</p>
<p id="back">[<a href="index.html">index</a>]</p>
<pre>
EOF

        line_nb=`wc -l "$in"`
        tail -n $((${line_nb% *} - 4)) "$in" | sed \
         -e 's,<,\&lt;,g' -e 's,>,\&gt;,g' \
         -e 's,^\([[:space:]]*0\.\.|.*\),<span class="notcov">\1</span>,' \
         -e 's,^\([[:space:]]*[1-9][0-9]*\.\.|.*\),<span class="cov">\1</span>,' \
         -e 's,^\([[:space:]]*|[[:space:]][[:space:]]*%+ \$Id.*\),<span class="cvsid">\1</span>,' \
         -e 's,^\([[:space:]]*|[[:space:]][[:space:]]*%.*\),<span class="comment">\1</span>,' \
         -e 's,^\([[:space:]]*|[[:space:]]\{2\}[[:alpha:]][a-zA-Z0-9_]*\)\(.*\),<span class="func">\1</span>\2,' \
         >> "$out"

        cat << EOF >> "$out"
</pre>
</body>
EOF

        # Update index block.
        cat << EOF >> "$index"
<tr>
<td><a href="${out}">$module</a></td>
<td style="text-align: right;">$percent %</td>
<td style="width: 5px; padding: 0px; background-color: #$color;"></td>
</tr>
EOF

        # Remove file generated by $test_coverage.
        rm -f $in $pct
    done

    # End index block.
    cat << EOF >> "$index"
</table>
</div>
EOF

    rm -f $covered_mods
}

output_index () {
    out=index.html

    timestamp=`date +'%Y-%m-%d at %H:%M:%S'`

    cat << EOF > "$out"
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<title>Testsuite coverage</title>
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
#global_col, tests_col {
  vertical-align: top;
}
#global_col {
  border-right: solid 1px #999;
}
#tests_col {
  padding-left: 30px;
}
.testsuite {
  float: left;
  margin-right: 30px;
  margin-bottom: 30px;
}
/*#single_tests {
  float: left;
  border: solid 1px #f00;
}*/
.testsuite table {
  border: 1px solid #999;
  border-collapse: collapse;
  background-color: #f9f9f9;;
}
.testsuite th, .testsuite td {
  padding: 4px;
}
.testsuite th {
  border-bottom: 1px solid #999;
  background-color: #ebebeb;
}
</style>
</head>
<body>
<h1>Testsuite coverage</h1>
<p id="timestamp">Date: ${timestamp}</p>
EOF

    cat << EOF >> "$out"
<table>
<tr>
<td id="global_col">
EOF

    # Add "global" block.
    index="index_global.html"
    cat "$index" >> "$out"
    rm -f "$index"

    cat << EOF >> "$out"
</td>
<td id="tests_col">
EOF

    # Add single test blocks.
    for test_name in $@; do
        index=index_${test_name}.html
        cat "$index" >> "$out"
        rm -f "$index"
    done

    cat << EOF >> "$out"
</td>
</tr>
</table>
EOF

    cat << EOF >> "$out"
</body>
</html>
EOF
}

for test_name in $@; do
    if [ "$test_name" != "dialyzer" ]; then
        test_names="$test_names $test_name"
    fi
done

# Global test coverage output.
output_test_coverage global $test_names

# Per-test test coverage output.
for test_name in $test_names; do
    output_test_coverage $test_name
done

# Index building.
output_index $test_names

# Clean files.
for test_name in $test_names; do
    cleanfiles=$scriptdir/data/$test_name/CLEANFILES
    if [ -s $cleanfiles ]; then
        rm -f `cat $cleanfiles`
    fi
done
