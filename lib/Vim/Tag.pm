package Vim::Tag;

use strict;
use warnings;


our $VERSION = '0.02';


use base 'Exporter';


our %EXPORT_TAGS = (
    util => [ qw(make_tag) ],
);


our @EXPORT_OK = @{ $EXPORT_TAGS{all} = [ map { @$_ } values %EXPORT_TAGS ] };


sub make_tag ($$$) {
    my ($tag, $filename, $line) = @_;

    {
        no warnings 'once';
        return unless $::PTAGS;
    }

    # fix the filename in case caller has the /loader/0x1234567 path;
    # Devel::SearchINC will have set it correctly in %INC

    if (defined($filename) && $filename =~ m!^/loader/0x[0-9a-f]+/(.*)!) {
        $filename = $INC{$1};
    }

    printf "%s\t%s\t%s\n", $tag, $filename, $line;
}


1;


__END__

=head1 NAME

Vim::Tag - Generate perl tags for vim

=head1 SYNOPSIS

    package Foo::Bar;

    use Vim::Tag 'make_tag';
    make_tag $tag, $filename, $line;

then:

    $ ptags --use ~/code/coderepos | ptags_sort >~/.ptags

.vimrc:

    set tags+=~/.ptags

then this works in vim:

    :ta Foo::Bar
    :ta my_subroutine

bash completion:

    alias vit='vi -t'
    _ptags()
    {
        COMPREPLY=( $(grep -h ^${COMP_WORDS[COMP_CWORD]} ~/.ptags | cut -f 1) )
        return 0
    }
    complete -F _ptags vit

then you can do:

    $ vit Foo::Bar
    $ vit Foo--Bar     # easier to complete on than double-colons
    $ vit my_subroutine

=head1 DESCRIPTION

Manage tags for perl code in vim, with ideas on integrating tags with the bash
programmable completion project. See the synopsis.

=head1 EXPORTS

Nothing is exported automatically. The function can be exported using its name
or the C<:all> tag.

=over 4

=item make_tag

Takes a tag name, a filename and a line number. If the global variable
C<$::PTAGS> is set, it prints a tag line that vim can understand. The C<ptags>
program will set C<$::PTAGS> when going through the modules to give them a
chance to generate their own tags.

=back

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests through the web interface at
L<http://rt.cpan.org>.

=head1 INSTALLATION

See perlmodinstall for information and options on installing Perl modules.

=head1 AVAILABILITY

The latest version of this module is available from the Comprehensive Perl
Archive Network (CPAN). Visit <http://www.perl.com/CPAN/> to find a CPAN
site near you. Or see <http://www.perl.com/CPAN/authors/id/M/MA/MARCEL/>.

=head1 AUTHORS

Marcel GrE<uuml>nauer, C<< <marcel@cpan.org> >>

=head1 COPYRIGHT AND LICENSE

Copyright 2008 by Marcel GrE<uuml>nauer

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

