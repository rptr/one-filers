#!/usr/bin/env perl
use strict;
use warnings;

my $sum = 0;
my $max = 1000;
my $num3 = 1000 / 3;
my $num5 = 1000 / 5 - 1;

for my $i (1 .. $num3) {
    $sum += $i * 3;
}
for my $i (1 .. $num5) {
    if ($i % 3 != 0) {
        $sum += $i * 5;
    }
}

print "$sum\n";
