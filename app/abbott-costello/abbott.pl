#!/usr/bin/env perl
use strict;
use warnings;
use Parse::RecDescent;

our (%man, %base);

%man = (
    first => "Who",
    second => "What",
    third => "I Don't Know",
);

%base = (
    "who"          => "first",
    "what"         => "second",
    "i don't know" => "third",
);

my $abbott = new Parse::RecDescent(<<'EOABBOTT');
Interpretation:
        ConfirmationRequest
      | NameRequest
      | BaseRequest

ConfirmationRequest:
        Preface(s?) Name /[i']s on/ Base
            { (lc $::man{$item[4]} eq lc $item[2])
                ? "Yes"
                : "No, $::man{$item[4]}'s on $item[4]"
            }
      | Preface(s?) Name /[i']s the (name of the)?/ Man /('s name )?on/ Base
            { (lc $::man{$item[6]} eq lc $item[2])
                ? "Certainly"
                : "No. \u$item[2] is on " . $::base{lc $item[2]}
            }

BaseRequest:
        Preface(s?) Name /(is)?/
            { "He's on " . $::base{lc $item[2]} }

NameRequest:
        /(What's the name of )?the/i Base "baseman"
            { $::man{$item[2]} }

Preface: ...!Name /\S*/

Name:   Name12 | /I Don't Know/i
Name12: /Who/i | /What/i
Base:   'first' | 'second' | 'third'
Man:    'man'   | 'guy'    | 'fellow'
EOABBOTT

my $line = @ARGV ? $ARGV[0] : "Who's on first?";
print $abbott->Interpretation($line), "\n";
