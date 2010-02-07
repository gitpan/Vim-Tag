package Vim::Tag::Null;
use 5.008;
use strict;
use warnings;
our $VERSION = '0.04';
our $null = bless {}, __PACKAGE__;
sub AUTOLOAD { $null }

1;
__END__

=head1 NAME

Vim::Tag::Null - Empty base package for fake packages to aid tag generation

=head1 SYNOPSIS

    use base 'Vim::Tag';
    my $gen = main->new;
    $gen->setup_fake_package('Foo::Bar');

=head1 DESCRIPTION

This empty package serves as the base class for fake packages that you don't
want to install - maybe they're needed only on another platform, on the
production system, need libraries that are impossible or difficult to install
etc.

You will probably not need to use this package directly. It is used when
calling C<setup_fake_package()> in L<Vim::Tag>.

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests through the web interface at
L<http://rt.cpan.org>.

=head1 INSTALLATION

See perlmodinstall for information and options on installing Perl modules.

=head1 AVAILABILITY

The latest version of this module is available from the Comprehensive Perl
Archive Network (CPAN). Visit <http://www.perl.com/CPAN/> to find a CPAN
site near you. Or see L<http://search.cpan.org/dist/Vim-Tag/>.

=head1 AUTHORS

Marcel GrE<uuml>nauer, C<< <marcel@cpan.org> >>

=head1 COPYRIGHT AND LICENSE

Copyright 2008-2010 by Marcel GrE<uuml>nauer

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
