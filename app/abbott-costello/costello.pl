#!/usr/bin/env perl
use strict;
use warnings;
use Parse::RecDescent;

sub Parse::RecDescent::choose { $_[int rand @_]; }

our @try_again;

@try_again = (
    "So, who's on first?",
    "I want to know who's on first!",
    "What's the name of the first baseman?",
    "Let's start again. What's the name of the guy on first?",
    "Okay, then, who's on second?",
    "Well then, who's on third?",
    "What's the name of the fellow on third?",
);

my $costello = new Parse::RecDescent(<<'EOCOSTELLO');
Interpretation:
        Meaning <reject:$item[1] eq $thisparser->{prev}>
            { $thisparser->{prev} = $item[1] }
      | { choose(@::try_again) }

Meaning:
        Question
      | UnclearReferent
      | NonSequitur

Question:
        Preface Interrogative /[i']s on/ Base
            { choose (
                "Yes, what is the name of the guy on $item[4]?",
                "The $item[4] baseman?",
                "I'm asking you! $item[2]?",
                "I don't know!"
            ) }
      | Interrogative
            { choose (
                "That's right, $item[1]?",
                "What?",
                "I don't know!"
            ) }

UnclearReferent:
        "He's on" Base
            { choose (
                "Who's on $item[2]?",
                "Who is?",
                "So, what is the name of the guy on $item[2]?"
            ) }

NonSequitur:
        ( "Yes" | 'Certainly' | /that's correct/i )
            { choose(
                "$item[1], who?",
                "What?",
                @::try_again
            ) }

Interrogative: /who/i | /what/i
Base:   'first' | 'second' | 'third'
Preface: ...!Interrogative /\S*/
EOCOSTELLO

my $line = @ARGV ? $ARGV[0] : "So, who's on first?";
print $costello->Interpretation($line), "\n";
