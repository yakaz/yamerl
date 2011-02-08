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

sub format_test (%) {
    my (%test) = @_;

    return '{"'.$test{'desc'}.'", '.$test{'test'}.'}';
}

sub main {
    my ($template, $tests_dir) = @_;

    my @tests = ();

    my $module = basename($tests_dir);

    my @files = glob("$tests_dir/*.pattern");
    if ($ENV{'TEST_PATTERN'}) {
        my $pattern = basename($ENV{'TEST_PATTERN'});
        @files = grep { $_ =~ /$pattern/ } @files;
    }
    foreach my $file (@files) {
        my $name = basename($file, ".pattern");
        my $data = $file;
        $data =~ s/\.pattern$/.yaml/o;

        my %test = ('data' => $data);

        my $fd;
        open($fd, '<', $file) or die "Failed to open $file: $!\n";

        my $line;
        while ($line = <$fd>) {
            chomp $line;

            if ($line =~ /^\s*\%+\s*DESCRIPTION$/o) {
                $test{'desc'} = get_desc($fd);
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

    my $fd;
    open($fd, '<', $template) or die "Failed to open $template: $!\n";

    my ($line, $indent);
    while ($line = <$fd>) {
        if ($line =~ /(\s*)\%+\s*DO NOT EDIT:/o) {
            $indent = $1;
            last;
        }

        $line =~ s/\${MODULE}/$module/g;
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
