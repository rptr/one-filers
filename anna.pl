#!/usr/bin/env perl
use strict;
use warnings;
use AnyEvent;
use AnyEvent::IRC::Connection;

my $c   = AnyEvent->condvar;
my $con = new AnyEvent::IRC::Connection;

$con->reg_cb (
    connect => sub {
        print "connect\n";
        my ($con) = @_;
        $con->send_msg (NICK => 'testbot');
    },
    irc_001 => sub {
    }

);

$con->reg_cb (registered => sub { print "I'm in!\n"; });
$con->reg_cb (disconnect => sub { print "I'm out!\n"; $c->broadcast });

$con->connect("irc.dalnet.org", 6667);
$c->wait;
$con->disconnect;
