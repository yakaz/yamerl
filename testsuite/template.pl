#!/usr/bin/perl

use strict;
use warnings;
use utf8;

use File::Basename;

sub get_desc ($) {
    my ($fd) = @_;

    my $desc;

    while (my $line = <$fd>) {
        chomp $line;

        # An empty line stops description parsing.
        last if ($line =~ /^\s*$/o);

        if ($desc) {
            $desc .= "\\n".$line;
        } else {
            $desc = $line;
        }
    }

    return $desc;
}

sub get_include ($) {
    my ($fd) = @_;

    my @include = ();

    while (my $line = <$fd>) {
        chomp $line;

        # An empty line stops description parsing.
        last if ($line =~ /^\s*$/o);

        push(@include, $line);
    }

    return @include;
}

sub get_data ($) {
    my ($fd) = @_;

    my $data;

    while (my $line = <$fd>) {
        chomp $line;

        # An empty line stops data parsing.
        last if ($line =~ /^\s*$/o);

        if ($data) {
            $data .= "\\n".$line;
        } else {
            $data = $line;
        }
    }

    return $data;
}

sub get_test ($) {
    my ($fd) = @_;

    my $pattern;
    {
        # Slurp file content.
        local $/;
        $pattern = <$fd>;
    }
    chomp $pattern;

    return $pattern;
}

sub format_include ($@) {
    my ($keyword, @include) = @_;

    my $output = '';

    foreach my $inc (@include) {
        $output .= "\n-$keyword($inc).";
    }

    return $output;
}

sub format_test (%) {
    my (%test) = @_;

    return '{"'.$test{'desc'}.'", '.$test{'test'}.'}';
}

sub main {
    my ($template, $tests_dir) = @_;

    my @tests = ();

    my $module = basename($tests_dir);

    my $srcdir       = $ENV{'srcdir'};
    my $builddir     = $ENV{'builddir'};
    my $top_srcdir   = $ENV{'top_srcdir'};
    my $top_builddir = $ENV{'top_builddir'};

    my @include = ();
    my @include_lib = ();

    my @files = glob("$tests_dir/*.pattern");
    if ($ENV{'TEST_PATTERN'}) {
        my $pattern = basename($ENV{'TEST_PATTERN'});
        @files = grep { $_ =~ /$pattern/ } @files;
    }
    foreach my $file (@files) {
        my $name = basename($file, ".pattern");
        my $data = $file;
        $data =~ s/\.pattern$/.data/o;

        my %test = ('data' => $data);

        my $fd;
        open($fd, '<', $file) or die "Failed to open $file: $!\n";

        my $line;
        while ($line = <$fd>) {
            chomp $line;

            if ($line =~ /^\s*\%+\s*DESCRIPTION$/o) {
                $test{'desc'} = get_desc($fd);
            } elsif ($line =~ /^\s*\%+\s*INCLUDE$/o) {
                foreach my $inc (get_include($fd)) {
                    unless (grep(/^$inc$/, @include)) {
                        push(@include, $inc);
                    }
                }
            } elsif ($line =~ /^\s*\%+\s*INCLUDE_LIB$/o) {
                foreach my $inc (get_include($fd)) {
                    unless (grep(/^$inc$/, @include_lib)) {
                        push(@include_lib, $inc);
                    }
                }
            } elsif ($line =~ /^\s*\%+\s*DATA$/o) {
                $test{'data'} = $data = dirname($file).'/'.get_data($fd);
            } elsif ($line =~ /^\s*\%+\s*TEST$/o) {
                $test{'test'} = get_test($fd);
                last;
            } elsif ($line =~ /^\s*$/o || $line =~ /^\s*%/o) {
                # Skip empty lines and comments.
                next;
            }
        }

        close($fd);

        unless (exists $test{'desc'}) {
            $test{'desc'} = $name;
        }
        $test{'desc'} =~ s/"/\\"/go;
        $test{'test'} =~ s/\${FILENAME}/$data/g;

        my $formatted_test = format_test(%test);
        push(@tests, $formatted_test);
    }

    my $include     = format_include('include',     @include);
    my $include_lib = format_include('include_lib', @include_lib);

    my $fd;
    open($fd, '<', $template) or die "Failed to open $template: $!\n";

    my ($line, $indent);
    while ($line = <$fd>) {
        if ($line =~ /(\s*)\%+\s*DO NOT EDIT:/o) {
            $indent = $1;
            last;
        }

        $line =~ s/\${MODULE}/$module/g;
        $line =~ s/\${srcdir}/$srcdir/g;
        $line =~ s/\${builddir}/$builddir/g;
        $line =~ s/\${top_srcdir}/$top_srcdir/g;
        $line =~ s/\${top_builddir}/$top_builddir/g;
        if ($line =~ /\${include}/o) {
            if ($include) {
                $line =~ s/\${include}/$include/g;
            } else {
                $line = '';
            }
        } elsif ($line =~ /\${include_lib}/o) {
            if ($include_lib) {
                $line =~ s/\${include_lib}/$include_lib/g;
            } else {
                $line = '';
            }
        }

        print $line;
    }

    my $tests_count = scalar(@tests);
    for (my $i = 0; $i < $tests_count; ++$i) {
        # Adjust indentation.
        my $test = $tests[$i];
        $test =~ s/^/$indent/gm;

        print $test;

        print(($i < $tests_count - 1) ? ",\n" : "\n");
    }

    print $line;
    while ($line = <$fd>) {
        print $line;
    }

    close($fd);
}

main(@ARGV);
