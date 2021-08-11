#!/usr/bin/env perl
use strict;
use warnings;

my $_4m = 4*10**6;
my $sum = 0;
my $f1 = 0;
my $f2 = 1;
my $fib = 0;

while ($fib <= $_4m) {
    $fib = $f1 + $f2;

    if ($fib % 2 == 0) {
        $sum += $fib;
    }

    $f1 = $f2;
    $f2 = $fib;
}

print "answer: $sum\n";
