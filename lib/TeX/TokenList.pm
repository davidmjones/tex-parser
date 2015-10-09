package TeX::TokenList;

use strict;
use warnings;

use version; our $VERSION = qv '1.5.1';

use Carp;

use PTG::Class;

my %tokens_of :ATTR();

use overload
    q{==} => \&tokenlist_equal;

sub BUILD {
    my ($self, $ident, $arg_ref) = @_;

    if (exists $arg_ref->{tokens}) {
        my @tokens = @{ $arg_ref->{tokens} };

        $tokens_of{$ident} = \@tokens;
    } else {
        $tokens_of{$ident} = [];
    }

    return;
}

sub length {
    my $self = CORE::shift;

    return scalar @{ $tokens_of{ident $self} };
}

sub index {
    my $self  = CORE::shift;
    my $index = CORE::shift;

    return $tokens_of{ident $self}->[$index];
}

sub shift {
    my $self = CORE::shift;

    return CORE::shift @{ $tokens_of{ident $self} };
}

sub unshift {
    my $self = CORE::shift;

    my @items = @_;

    my @tokens;

    for my $item (@items) {
        if (eval { $item->isa(__PACKAGE__) }) {
            CORE::push @tokens, $item->get_tokens();
        } elsif (eval { $item->isa("TeX::Token") }) {
            CORE::push @tokens, $item;
        } else {
            croak "Can't append a ", ref($item), " to a ", __PACKAGE__;
        }
    }

    CORE::unshift @{ $tokens_of{ident $self} }, @tokens;

    return;
}

sub pop {
    my $self = CORE::shift;

    return CORE::pop @{ $tokens_of{ident $self} };
}

sub push {
    my $self = CORE::shift;

    my @items = @_;

    for my $item (@items) {
        if (eval { $item->isa(__PACKAGE__) }) {
            $self->push($item->get_tokens());
        } elsif (eval { $item->isa("TeX::Token") }) {
            CORE::push @{ $tokens_of{ident $self} }, $item;
        } else {
            croak "Can't append a ", ref($item), " to a ", __PACKAGE__;
        }
    }

    return;
}

sub get_tokens :ARRAYIFY {
    my $self = CORE::shift;

    my $tokens_r = $tokens_of{ident $self};

    return wantarray ? @{ $tokens_r } : $tokens_r;
}

sub to_string :STRINGIFY {
    my $self = CORE::shift;

    return join '', map { $_->to_string() } @{ $self };
}

sub tokenlist_equal {
    my $self = CORE::shift;

    my $other = CORE::shift;

    if (! defined $other) {
        croak("Can't compare a " . __PACKAGE__ . " to an undefined value");
    }

    return unless eval { $other->isa(__PACKAGE__) };

    return unless $self->length() == $other->length();

    my @tokens = $self->get_tokens();
    my @other  = $other->get_tokens();

    for (my $i = 0; $i < @tokens; $i++) {
        return unless $tokens[$i] == $other[$i];
    }

    return 1;
}

1;

__END__

=head1 NAME

TeX::TokenList - List of TeX::Token's.

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
