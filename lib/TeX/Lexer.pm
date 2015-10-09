package TeX::Lexer;

use strict;
use warnings;

use version; our $VERSION = qv '2.0.0';

use PTG::Class;

use Carp;

use TeX::Catcodes;

use TeX::Token qw(:factories);

my %catcodes_of          :ATTR;
my %extended_catcodes_of :ATTR;
my %catcode_stack        :ATTR;

my %input_stack_of :ATTR;
my %force_eof_of   :BOOLEAN(:name<force_eof> :default<0>);

my %char_buffer_of   :ATTR;
my %token_buffer_of  :ATTR;
my %line_no_of       :ATTR(:get<line_no> :default(-1));
my %char_no_of       :ATTR(:name<char_no> :default(-1));
my %file_name_of     :ATTR(:get<file_name>);
my %file_handle_of   :ATTR();

my %state_of :ATTR(:name<state> :default(-1));

my %end_line_char_of :ATTR(:name<end_line_char> :default<13>);

## filtering controls the processing of CATCODE_IGNORED and
## CATCODE_INVALID characters.  When true, they are filtered out of
## the token stream.  When false, they are passed through.

my %filtering_of     :BOOLEAN(:name<filtering>     :default(1));

## verbatim controls the processing of CATCODE_END_OF_LINE,
## CATCODE_SPACE and CATCODE_COMMENT characters.  When true, each such
## character is preserved and turned into a token of the corresponding
## catcode.  When false, they are processed according to TeX's usual
## rules.

my %verbatim_of      :BOOLEAN(:name<verbatim>      :default(0));
my %unicode_input_of :BOOLEAN(:name<unicode_input> :default<0>);

######################################################################
##                                                                  ##
##                            CONSTANTS                             ##
##                                                                  ##
######################################################################

use constant first_text_char => 0;
use constant last_text_char  => 255;

use constant mid_line    => 1;
use constant skip_blanks => 17;
use constant new_line    => 33;

######################################################################
##                                                                  ##
##                      DEFAULT CATCODE TABLE                       ##
##                                                                  ##
######################################################################

my @CATCODE = (CATCODE_OTHER) x (last_text_char - first_text_char + 1);

$CATCODE[0]        = CATCODE_IGNORED;        # ascii null
$CATCODE[13]       = CATCODE_END_OF_LINE;    # ascii return
$CATCODE[ord '\\'] = CATCODE_ESCAPE;
$CATCODE[ord '%']  = CATCODE_COMMENT;
$CATCODE[ord ' ']  = CATCODE_SPACE;
$CATCODE[127]      = CATCODE_INVALID;        # ascii delete

for my $letter ('a'..'z', 'A'..'Z') {
    $CATCODE[ord($letter)] = CATCODE_LETTER;
}

## At this point, the catcodes are equivalent to what is in effect
## when initex starts.  Now we add the plain TeX category codes.

$CATCODE[ord '{']   = CATCODE_BEGIN_GROUP;
$CATCODE[ord '}']   = CATCODE_END_GROUP;
$CATCODE[ord '$']   = CATCODE_MATH_SHIFT;
$CATCODE[ord '&']   = CATCODE_ALIGNMENT;
$CATCODE[ord '#']   = CATCODE_PARAMETER;
$CATCODE[ord '^']   = CATCODE_SUPERSCRIPT;
$CATCODE[ord "\cK"] = CATCODE_SUPERSCRIPT;	# uparrow
$CATCODE[ord '_']   = CATCODE_SUBSCRIPT;
$CATCODE[ord "\cA"] = CATCODE_SUBSCRIPT;        # downarrow
$CATCODE[ord "\t"]  = CATCODE_SPACE;
$CATCODE[ord '~']   = CATCODE_ACTIVE;
$CATCODE[ord "\f"]  = CATCODE_ACTIVE;

## Note that LaTeX makes some further changes to these.

######################################################################
##                                                                  ##
##                            UTILITIES                             ##
##                                                                  ##
######################################################################

sub __output_char( $ ) {
    my $char = shift;

    my $ord = ord($char);

    if ($ord > 32 && $ord < 127) {
        return $char;
    } elsif ($ord < 64) {
        return sprintf "^^%c", $ord + 64;
    } elsif ($ord < 128) {
        return sprintf "^^%c", $ord - 64;
    } else {
        return sprintf "^^%02x", $ord;
    }
}

sub __is_hex_char( $ ) {
    my $char = shift;

    return $char =~ m/0-9a-z/;
}

######################################################################
##                                                                  ##
##                           CONSTRUCTORS                           ##
##                                                                  ##
######################################################################

sub BUILD {
    my ($self, $ident, $arg_ref) = @_;

    $catcodes_of{$ident} = [ @CATCODE ];

    $extended_catcodes_of{$ident} = {};

    $token_buffer_of{$ident} = [];

    $self->initialize_state();

    if (exists $arg_ref->{end_line_char}) {
        $self->set_catcode($arg_ref->{end_line_char}, CATCODE_END_OF_LINE);
    }

    if (defined (my $file = $arg_ref->{source_file})) {
        $self->bind_to_file($file);
    } elsif (defined (my $string = $arg_ref->{source_string})) {
        $self->bind_to_string($string);
    } 

    return;
}

sub initialize_state {
    my $self = shift;

    my $ident = ident $self;

    $char_buffer_of{$ident}  = "";
    $token_buffer_of{$ident} = [];
    $line_no_of{$ident}      = -1;
    $char_no_of{$ident}      = -1;
    $file_name_of{$ident}    = "";
    undef $file_handle_of{$ident};

    $self->set_state(new_line);

    return;
}

{
    no warnings qw(redefine);

sub clone {
    my $self = shift;

    my $class = ref $self;

    my $clone = $class->new({ unicode_input => $self->reading_unicode(),
                              end_line_char => $self->get_end_line_char(),
                            });

    my $parent_ident = ident $self;
    my $clone_ident  = ident $clone;

    $catcodes_of{$clone_ident} = { @{ $catcodes_of{$parent_ident} } };
    $extended_catcodes_of{$clone_ident} = { %{ $extended_catcodes_of{$parent_ident} } };

    return $clone;
}
}

# sub filtering {
#     my $self = shift;
# 
#     return $filter_of{ident $self};
# }

######################################################################
##                                                                  ##
##                            ACCESSORS                             ##
##                                                                  ##
######################################################################

# sub set_verbatim {
#     my $self = shift;
#     my $verbatim = shift;
# 
#     $verbatim_of{ident $self} = $verbatim;
# 
#     ##* ##* ACKKK!!!!!!!
#     ##* 
#     ##* if ($verbatim) {
#     ##*     TeX::Token->disable_csname_spaces();
#     ##* } else {
#     ##*     TeX::Token->enable_csname_spaces();
#     ##* }
# 
#     return;
# }

# sub set_unicode_input {
#     my $self = shift;
#     my $boolean = shift;
# 
#     $unicode_input_of{ident $self} = $boolean;
# }

sub reading_unicode {
    my $self = shift;

    return $unicode_input_of{ident $self};
}

######################################################################
##                                                                  ##
##                          CONTEXT STACK                           ##
##                                                                  ##
######################################################################

# my %context_stack_of :ARRAY(:name<context_stack>);

my %context_line_of    :ATTR(:name<context_line>);
# my %context_pointer_of :ATTR(:name<context_pointer>);

# no warnings qw(redefine);

# sub push_context_stack {
#     my $self = shift;
# 
#     push @{ $context_stack_of{ident $self} }, [ $self->get_context_line(),
#                                                 $self->get_char_no() ];
# 
#     return;
# }
# 
# sub pop_context_stack {
#     my $self = shift;
# 
#     my ($line, $pointer) = pop @{ $context_stack_of{ident $self} };
# 
#     $self->set_context_line($line);
#     $self->set_char_no($pointer);
# 
#     return;
# }

######################################################################
##                                                                  ##
##                         STACK MANAGEMENT                         ##
##                                                                  ##
######################################################################

sub push_input {
    my $self = shift;

    my $ident = ident $self;

    push @{ $input_stack_of{$ident} }, {
        char_buffer  => $char_buffer_of{$ident},
        token_buffer => $token_buffer_of{$ident},
        line_no      => $line_no_of{$ident},
        char_no      => $char_no_of{$ident},
        file_name    => $file_name_of{$ident},
        file_handle  => $file_handle_of{$ident},
        state        => $self->get_state(),
    };

    return;
}

sub pop_input {
    my $self = shift;

    my $ident = ident $self;

    my $saved_state = pop @{ $input_stack_of{$ident} };

    $char_buffer_of{$ident}  = $saved_state->{char_buffer};
    $token_buffer_of{$ident} = $saved_state->{token_buffer};
    $line_no_of{$ident}      = $saved_state->{line_no};
    $char_no_of{$ident}      = $saved_state->{char_no};
    $file_name_of{$ident}    = $saved_state->{file_name};
    $file_handle_of{$ident}  = $saved_state->{file_handle};
    $self->set_state($saved_state->{state});

    return;
}

sub save_catcodes {
    my $self = shift;

    my $ident = ident $self;

    push @{ $catcode_stack{$ident} }, [ [ @{ $catcodes_of{$ident} } ],
                                        { %{ $extended_catcodes_of{$ident} } }];

    return;
}

sub restore_catcodes {
    my $self = shift;

    my $ident = ident $self;

    my $saved = pop @{ $catcode_stack{$ident} };

    die "Tried to pop empty catcode stack" unless defined $saved;

    my ($catcodes, $extended_catcodes) = @{ $saved };

    $catcodes_of{$ident}          = $catcodes;
    $extended_catcodes_of{$ident} = $extended_catcodes;

    return;
}

sub __initialize_catcode {
    my $self = shift;

    my $char_code = shift;

    my $ident = ident $self;

    if (chr($char_code) =~ /\p{L}/) {
        $extended_catcodes_of{$ident}->{$char_code} = CATCODE_LETTER;
    } else {
        $extended_catcodes_of{$ident}->{$char_code} = CATCODE_OTHER;
    }

    return;
}

sub get_catcode {
    my $self      = shift;
    my $char_code = shift;

    if ($char_code < first_text_char) {
        carp "Invalid character code for get_catcode";

        return;
    }

    my $ident = ident $self;

    if ($char_code > last_text_char) {
        if ($self->reading_unicode()) {
            # return CATCODE_OTHER;

            my $catcodes = $extended_catcodes_of{$ident};

            if (! exists $catcodes->{$char_code}) {
                $self->__initialize_catcode($char_code);
            }

            return $catcodes->{$char_code};
        } else {
            carp "Invalid character code for get_catcode";

            return;
        }
    }

    return $catcodes_of{ident $self}->[$char_code];
}

sub set_catcode {
    my $self      = shift;
    my $char_code = shift;
    my $catcode   = shift;

    if ($catcode < CATCODE_ESCAPE || $catcode > CATCODE_INVALID) {
        carp "Invalid category code $catcode";
        return;
    }

    if ($char_code < first_text_char) { 
        carp "Invalid character code for set_catcode";
        return;
    }

    my $ident = ident $self;

    if ($char_code > last_text_char) {
        if ($self->reading_unicode()) {
            $extended_catcodes_of{$ident}->{$char_code} = $catcode;
        } else {
            carp "Invalid character code for set_catcode";
            return;
        }
    }

    $catcodes_of{$ident}->[$char_code] = $catcode;

    return;
}

sub copy_catcodes {
    my $self  = shift;
    my $other = shift;

    my $ident = ident $self;
    my $odent = ident $other;

    $catcodes_of{$ident} = $catcodes_of{$odent};
    $extended_catcodes_of{$ident} = $extended_catcodes_of{$odent};

    return;
}

######################################################################
##                                                                  ##
##                        BUFFER MANAGEMENT                         ##
##                                                                  ##
######################################################################

sub __end_of_line_char {
    my $self = shift;

    my $char = $self->get_end_line_char();

    return "" if $char < 0;

    return chr($char);
}

sub input_ln {
    my $self = shift;

    my $fh = shift;

    chomp(my $line = <$fh>);

    # binmode \*STDERR, ":utf8"; print STDERR "input_ln: line = [$line]\n";

    $line =~ s/ +\z//;

    return $line;
}

sub __input_line {
    my $self = shift;

    my $ident = ident $self;

    my $fh = $file_handle_of{$ident}; # or croak "No file open for input";

    return unless defined $fh;

    return if eof($fh);

    $line_no_of{$ident}++;
    $char_no_of{$ident} = 0;

    my $line = $self->input_ln($fh);

    $line .= $self->__end_of_line_char();

    return $line;
}

sub flush_buffer {
    my $self = shift;

    my $remainder = $char_buffer_of{ident $self};

    $char_buffer_of{ident $self} = "";

    return $remainder;
}

sub end_of_buffer {
    my $self = shift;

    my $buffer = $char_buffer_of{ident $self};

    return ! defined $buffer || length($buffer) == 0;
}

sub insert {
    my $self = shift;
    my $text = shift;

    my $ident = ident $self;

    $char_buffer_of{$ident} = $text . $char_buffer_of{$ident};
}

sub __prepare_buffer {
    my $self = shift;

    my $ident = ident $self;

    return 1 unless $self->end_of_buffer();

    if ($self->is_force_eof()) {
        $self->set_force_eof(0);

        return;
    }

    my $next_line = $self->__input_line();

    return unless defined $next_line;

    ## $next_line is guaranteed to have at least one character ("\r")

    $char_buffer_of{$ident} = $next_line;

    $self->set_state(new_line);

    # $self->delete_context_stacks();

    $self->set_context_line($next_line);
    # $self->set_context_pointer(0);

    return 1;
}

sub __unget_raw {
    my $self = shift;

    my @chars = @_;

    my $ident = ident $self;

    $char_buffer_of{$ident} = join('', @chars, $char_buffer_of{$ident});

    return;
}

sub __get_next_raw {
    my $self = shift;

    $self->__prepare_buffer() || return;

    $char_no_of{ident $self}++;

    $char_buffer_of{ident $self} =~ s/\A (.) //smx;

    return $1;
}

sub __peek_next_raw {
    my $self = shift;

    $self->__prepare_buffer() || return;

    $char_buffer_of{ident $self} =~ m/ \A (.) /smx;

    return $1;
}

sub get_next_char {
    my $self = shift;

    my $char = $self->__get_next_raw();

    return unless defined $char;

    my $catcode = $self->get_catcode(ord($char));

    if ($catcode == CATCODE_SUPERSCRIPT) {
        if ($char eq $self->__peek_next_raw()) {
            $self->__get_next_raw();

            my $enc = $self->__get_next_raw();

            if (__is_hex_char($enc) && __is_hex_char($self->__peek_next_raw())) {
                $enc .= $self->__get_next_raw();

                $char = chr(hex($enc));
            } elsif ($enc =~ /[\x00-\x3F]/) {
                $char = chr(ord($enc) + 64);
            } elsif ($enc =~ /[\x40-\x7F]/) {
                $char = chr(ord($enc) - 64);
            } else {
                $self->__unget_raw($char, $enc);
            }
        }
    }

    return wantarray ? ($char, $self->get_catcode(ord($char))) : $char;
}

######################################################################
##                                                                  ##
##                             PARSING                              ##
##                                                                  ##
######################################################################

sub clear_char_buffer {
    my $self = shift;

    $char_buffer_of{ident $self} = "";

    return;
}

sub clear_token_buffer {
    my $self = shift;

    $token_buffer_of{ident $self} = [];

    return;
}

sub unget_tokens {
    my $self = shift;

    unshift @{ $token_buffer_of{ident $self} }, @_;

    return;
}

# sub get_next_token {
#     my $self = shift;
# 
#     my $token = $self->get_next_token_real();
# 
#     # print STDERR "get_next_token: token = '$token'\n";
# 
#     return $token;
# }

sub get_next_token {
    my $self = shift;

    my $ident = ident $self;

    if (@{ $token_buffer_of{$ident} }) {
        return shift @{ $token_buffer_of{$ident} };
    }

    while (my ($char, $catcode) = $self->get_next_char()) {
        next if $catcode == CATCODE_IGNORED && $self->is_filtering();

        if ($catcode == CATCODE_ESCAPE) {
            return $self->__get_control_name($char);
        }

        if ($catcode == CATCODE_END_OF_LINE) {
            if (my $token = $self->__do_end_of_line($char)) {
                return $token;
            }

            next;   
        }

        if ($catcode == CATCODE_SPACE) {
            if (my $token = $self->__do_space($char)) {
                return $token;
            }

            next;   
        }

        if ($catcode == CATCODE_COMMENT) {
            if (my $token = $self->__do_comment()) {
                return $token;
            }

            next;
        }

        if ($catcode == CATCODE_INVALID && $self->is_filtering()) {
            warn("Invalid character ", __output_char($char),
                 " on line ",          $self->get_line_no(), "\n");

            next;
        }

        $self->set_state(mid_line);

        return make_character_token($char, $catcode);
    }

    return;
}

*get_token = *get_next_token;

sub consume_next_token {
    my $self = shift;

    $self->get_next_token();

    return;
}

sub peek_next_token {
    my $self = shift;

    my $token = $self->get_next_token();

    if (defined($token)) {
        $self->unget_tokens($token);
    }

    return $token;
}

sub __get_control_name {
    my $self = shift;
    my $char = shift;

    my $control_name;

    if ($self->end_of_buffer()) {
        $control_name = "";

        $self->set_state(mid_line);
    } else {
        ($control_name, my $next_catcode) = $self->get_next_char();

        if ($next_catcode == CATCODE_LETTER) {
            while (1) {
                last if $self->end_of_buffer();

                my ($next_char, $next_catcode) = $self->get_next_char();

                if ($next_catcode == CATCODE_LETTER) {
                    $control_name .= $next_char;
                } else {
                    $self->__unget_raw($next_char);
                    last;
                }
            }

            $self->set_state(skip_blanks);
        } elsif ($next_catcode == CATCODE_SPACE) {
            $self->set_state(skip_blanks);
        } else {
            $self->set_state(mid_line);
        }
    }

    return make_csname_token($control_name);
}

sub __do_end_of_line {
    my $self = shift;
    my $char = shift;

    if ($self->is_verbatim()) {
        return make_character_token($char, CATCODE_END_OF_LINE);
    }

    $self->flush_buffer();

    my $state = $self->get_state();

    if ($state == new_line) {
        return make_csname_token('par');
    } elsif ($state == mid_line) {
        return $self->__do_space(' ');
    } elsif ($state == skip_blanks) { # no-op
        return;
    } else {
        die "Invalid lexer state: $state";
    }
}

sub __do_space {
    my $self = shift;
    my $char = shift;

    if ($self->is_verbatim()) {
        return make_character_token($char, CATCODE_SPACE);
    }

    my $state = $self->get_state();

    if ($state == new_line || $state == skip_blanks) {
        return;
    } elsif ($state == mid_line) {
        $self->set_state(skip_blanks);

        return make_character_token(' ', CATCODE_SPACE);
    } else {
        die "Invalid lexer state '$state'";
    }
}

sub __do_comment {
    my $self = shift;

    my $comment = $self->flush_buffer();

    if ($self->is_verbatim()) {
        return make_comment_token($comment);
    }

    return;
}

sub __open_input {
    my $self = shift;

    my $file_arg = shift;

    my $fh;

    if ( ref($file_arg) eq "GLOB" || eval { $file_arg->isa("IO::Handle") }) {
        $fh = $file_arg;
    } else {
        my $mode = $self->reading_unicode() ? "<:utf8" : "<";

        open($fh, $mode, $file_arg) or do {
            my $file_name = $self->get_file_name();

            die "Can't open $file_name for input: $!\n";
        };
    }

    $file_handle_of{ident $self} = $fh;

    return;
}

sub end_input {
    my $self = shift;

    my $ident = ident $self;

    my $fh = $file_handle_of{$ident};

    return unless defined $fh;

    close($fh);

    $file_handle_of{$ident} = undef;

    $self->clear_token_buffer();
    $self->set_force_eof(1);

    return;
}

sub tokenize {
    my $self = shift;

    my $string = shift;

    my $eol = $self->get_end_line_char();

    $self->set_end_line_char(-1);

    $self->push_input();

    $self->bind_to_string($string);

    my @tokens;

    while (my $token = $self->get_next_token()) {
        push @tokens, $token;
    }

    $self->pop_input();

    $self->set_end_line_char($eol);

    return @tokens;
}

sub str_toks {
    my $self   = shift;
    my $string = shift;

    return unless defined $string;

    my @tokens;

    for my $char (split //, $string) {
        my $token = make_character_token($char,
                                         $char eq ' ' ? CATCODE_SPACE :
                                                        CATCODE_OTHER);

        push @tokens, $token;
    }

    return @tokens;
}

## Updated PTG::Unicode::Translators before deleting this.

*stringify = \&str_toks;

######################################################################
##                                                                  ##
##                       PUBLIC ENTRY POINTS                        ##
##                                                                  ##
######################################################################

sub bind_to_file( $ ) {
    my $self = shift;
    my $file = shift;

    my $ident = ident $self;

    $self->initialize_state();

    $file_name_of{$ident} = $file;
    $line_no_of{$ident}   = 0;

    $self->__open_input($file);

    return;
}

sub bind_to_string( $ ) {
    my $self   = shift;
    my $string = shift;

    if (eval { $string->isa("TeX::TokenList") }) {
        return $self->bind_to_token_list($string);
    }

    my $ident = ident $self;

    $self->initialize_state();

    $file_name_of{$ident} = '<string>';
    $line_no_of{$ident}   = 0;

    if (! defined $string) {
        $string = "";
    } else {
        $string = "$string";
    }

    $self->__open_input(\$string);

    return;
}

sub bind_to_token_list( $ ) {
    my $self = shift;

    my $token_list = shift;

    my $ident = ident $self;

    $self->initialize_state();

    $file_name_of{$ident} = '<token list>';

    ## Note that we copy the list rather than binding directly to it.

    if (ref($token_list) eq 'ARRAY') {
        $token_buffer_of{$ident} = [ @{ $token_list } ];
    } else {
        $token_buffer_of{$ident} = [ $token_list->get_tokens() ];
    }

    return;
}

1;

__END__

=head1 NAME

TeX::Lexer - Lexer for TeX.

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
