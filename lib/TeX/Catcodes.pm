package TeX::Catcodes;

use strict;
use warnings;

use version; our $VERSION = qv '1.0.0';

use base qw(Exporter);

our %EXPORT_TAGS;

our @EXPORT_OK;

use constant;

sub install($\%) {
    my $tag_name = shift;
    my $hash_ref = shift;

    constant->import($hash_ref);

    $EXPORT_TAGS{$tag_name} = [ keys %{ $hash_ref } ];

    return;
}

my %CATCODES = (
    CATCODE_ESCAPE      =>  0,
    CATCODE_BEGIN_GROUP =>  1,
    CATCODE_END_GROUP   =>  2,
    CATCODE_MATH_SHIFT  =>  3,
    CATCODE_ALIGNMENT   =>  4,
    CATCODE_END_OF_LINE =>  5,
    CATCODE_PARAMETER   =>  6,
    CATCODE_SUPERSCRIPT =>  7,
    CATCODE_SUBSCRIPT   =>  8,
    CATCODE_IGNORED     =>  9,
    CATCODE_SPACE       => 10,
    CATCODE_LETTER      => 11,
    CATCODE_OTHER       => 12,
    CATCODE_ACTIVE      => 13,
    CATCODE_COMMENT     => 14,
    CATCODE_INVALID     => 15,
    ##
    ## EXTENSIONS
    ##
    CATCODE_CSNAME      => 16,
    CATCODE_PARAM_REF   => 17,
    CATCODE_ANONYMOUS   => 18,
);

install catcodes => %CATCODES;

our @EXPORT = @{ $EXPORT_TAGS{catcodes} };

1;

__END__

=head1 NAME

TeX::Catcodes - Constant names for TeX category codes.

=head1 AUTHOR

David M. Jones, C<< <dmj@ams.org> >>

=head1 SUPPORT

This code was developed at the American Mathematical Society (AMS) and
is being released with their permission in hopes that it might be of
interest or use to someone.  However, neither the author nor the AMS
is able to promise any additional support, including bug fixes,
enhancements, or documentation.

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2015 American Mathematical Society

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
