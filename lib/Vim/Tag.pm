use 5.008;
use strict;
use warnings;

package Vim::Tag;
our $VERSION = '1.100880';
# ABSTRACT: Generate perl tags for vim
use File::Find;
use File::Find::Upwards;
use File::Slurp;
use Hash::Rename;
use UNIVERSAL::require;
use Vim::Tag::Null;
use parent qw(Class::Accessor::Constructor Getopt::Inherited);
__PACKAGE__->mk_constructor->mk_scalar_accessors(qw(tags))
  ->mk_array_accessors(qw(libs))
  ->mk_hash_accessors(qw(is_fake_package filename_for has_super_class));
use constant DEFAULTS => (tags => {});
use constant GETOPT => (qw(use=s win out|o=s));
use constant GETOPT_DEFAULTS => (verbose => 0, out => '-');

# --use: whether to 'use' the package; might gen more tags. The value is the
# path prefix under which to use() modules.
#
# --win: whether to use backslashes in file names
sub run {
    my $self = shift;
    $self->init;
    $self->do_getopt;
    $self->determine_libs;
    exit unless $self->libs;
    $self->generate_tags;
    $self->add_SUPER_tags;
    $self->add_yaml_marshall_tags;
    $self->fill_filename_placeholders;
    $self->finalize;
    $self->write_tags;
}
sub init { }

sub finalize {
    my $self = shift;

    # avoid Test::Base doing END{} processing, which, in the absence of real
    # tests, would produce annoying error messages.
    Test::Builder::plan(1) if $Test::Base::VERSION;
}

sub setup_fake_package {
    my ($self, @packages) = @_;
    for my $package (@packages) {
        (my $file = "$package.pm") =~ s!::!/!g;
        $INC{$file} = 'DUMMY';
        no strict 'refs';
        @{ $package . '::ISA' } = qw(Vim::Tag::Null);
        $self->is_fake_package($package, 1);
    }
    $self;
}

sub determine_libs {
    my $self = shift;

    # Go through libs in @INC order. I assume that custom libs will be
    # unshif()ed onto @INC so they come first - this happens with "use lib".
    # That means that the main perl libs will come last. By going through the
    # libs in reverse order, a local version of a module will take precedence
    # over a module that's installed system-wide.  This is useful if you have
    # a module both under development in your $PROJROOT as well as installed
    # system-wide; in this case you most likely want tags to point to the
    # locally installed version.
    our @libs = grep { !/^\.+$/ } grep { ref ne 'CODE' } @INC;
    {
        no warnings 'once';
        unshift @libs => @Devel::SearchINC::PATHS;
    }
    $self->libs(@libs);
}

sub generate_tags {
    my $self = shift;
    $::PTAGS = $self;
    for ($self->libs) {
        warn "Indexing $_\n";
        find(
            sub {
                if (-d && (/^(bin|t|blib|inc)$/
                        || file_find_upwards('PTAGS.SKIP'))
                  ) {
                    $self->verbose_warn(
                        "Skipping directory [$File::Find::name]\n");
                    return $File::Find::prune = 1;
                }
                return unless -f;
                if (/\.pm$/) {
                    $self->process_pm_file;
                } elsif (/\.pod$/) {
                    $self->process_pod_file;
                }
            },
            $_
        );
    }
}

sub write_tags {
    my $self = shift;
    my @libs = $self->libs;    # No method calls in sort sub

    # If you have a module both under development in your $PROJROOT as well as
    # installed system-wide, you most likely want tags to point to the
    # locally installed version. So here we sort according to lib, that
    # is, @INC, order, which is most likely the order you
    # want the modules to be found.
    my $lib_order = sub {
        my $path  = shift;
        my $order = 0;
        for my $candidate (@libs) {
            $order++;
            return $order if index($path, $candidate) == 0;
        }
        $order;
    };
    my %tags    = %{ $self->tags };
    my $outfile = $self->opt('out');
    ## no critic (ProhibitTwoArgOpen)
    open my $fh, ">$outfile" or die "can't open $outfile for writing: $!\n";
    for my $tag (sort keys %tags) {
        for my $file (
            sort { $lib_order->($a) <=> $lib_order->($b) }
            keys %{ $tags{$tag} }
          ) {
            for my $search (sort keys %{ $tags{$tag}{$file} }) {
                print $fh "$tag\t$file\t$search\n";
            }
        }
    }
    close $fh or die "can't close $outfile: $!\n";
}

sub delete_tags_by_pattern {
    my ($self, $pattern) = @_;
    my %tags = %{ $self->tags };
    for my $key (keys %tags) {
        delete $tags{$key} if $key =~ qr/$pattern/;
    }
    $self->tags(\%tags);
}

sub make_tag_aliases {
    my ($self, @rules) = @_;
    my %tags = %{ $self->tags };
    while (my ($search, $replace) = splice @rules, 0, 2) {
        for my $tag (keys %tags) {
            my $alias_tag = $tag;
            eval "\$alias_tag =~ s/$search/$replace/";
            die $@ if $@;
            next if $tag eq $alias_tag;
            $tags{$alias_tag} = $tags{$tag};
        }
    }
    $self->tags(\%tags);
}

sub verbose_warn {
    my ($self, $message, $level) = @_;
    $level ||= 1;
    return unless $self->opt('verbose') >= $level;
    warn $message;
}

sub add_tag {
    my ($self, $tag, $file, $search) = @_;

    # If you derived at the filename via caller(), you might get something
    # like /loader/0x1234567 if the file was loaded via Devel::SearchINC. But
    # it will also have set the correct filename in %INC, so we can find it
    # there.
    if (defined($file) && $file =~ m!^/loader/0x[0-9a-f]+/(.*)!) {
        $file = $INC{$1};
    }
    if ($file =~ qr/\(eval \d+\)/) {
        $self->verbose_warn("eval tag denied");
        return;
    }
    do {
        $self->verbose_warn("add_tag [$tag] file [$file] search [$search]\n",
            2);
        $self->tags->{$tag}{$file}{$search}++;
    } while $tag =~ y/:/-/;
}

sub make_package_tag {
    my ($self, %args) = @_;
    my $package = $args{tag};
    $self->filename_for($args{tag}, $args{filename})
      unless $self->exists_filename_for($args{tag});
    $self->verbose_warn(">>> package [$package]\n");
    $self->add_tag($args{tag}, $args{filename}, "?^$args{search}\\>");
}

sub get_filename {
    my $self     = shift;
    my $filename = $File::Find::name;
    $filename =~ y!/!\\! if $self->opt('win');
    $self->verbose_warn(">>> processing file [$filename]\n");
    $filename;
}

sub process_pm_file {
    my $self     = shift;
    my $text     = read_file($_);
    my $filename = $self->get_filename;
    my $package;
    while ($text =~ /^(package +(\w+(::\w+)*))\s*;/gmo) {
        my ($search, $tag) = ($1, $2);
        $self->make_package_tag(
            filename => $filename,
            search   => $search,
            tag      => $tag
        );
        $package ||= $tag;    # only remember the first package
    }

    # only include companion class tags if we could determine the package name
    if ($package) {

        # support vimrc definitions to switch between Foo.pm and Foo_TEST.pm.
        #
        # companionclass--Foo.pm      -> Foo_TEST.pm
        # companionclass--Foo_TEST.pm -> Foo.pm
        my $other_filename;
        if ($filename =~ /_TEST\.pm$/) {
            ($other_filename = $filename) =~ s/_TEST\.pm$/.pm/;
        } else {
            ($other_filename = $filename) =~ s/\.pm$/_TEST.pm/;
        }
        $self->add_tag("companionclass--$package", $other_filename, 1);
    }
    while ($text =~ /^(sub +(\w+(::\w+)*))\s*[:{\(#]/gmo) {
        my $tag = $2;
        $self->verbose_warn(">>> sub [$tag]\n");
        $self->add_tag($tag, $filename, "?^$1\\>");
    }
    while ($text =~ /^(use +constant\s+(\w+(::\w+)*))\s*=>/gmo) {
        my $tag = $2;
        $self->verbose_warn(">>> constant [$tag]\n");
        $self->add_tag($tag, $filename, "?^$1\\>");
    }

    # custom ptags: simple strings
    while ($text =~ /#\s*(ptags:\s*(\w+(::\w+)*))\s*$/gmo) {
        $self->verbose_warn(">>> custom ptag [$2]\n");
        my $tag = do {
            ## no critic (ProhibitNoStrict)
            no strict;
            no warnings;
            eval $2;
        };
        $self->add_tag($tag, $filename, "?^$1\\>");
    }

    # Custom ptags with code. The search name must be unique within file the
    # code ptag is defined in. Can't use the code as the ptags search pattern,
    # as it probably contains characters the vim regex engine considers
    # meta-characters ('[]$' etc).
    while ($text =~ /#\s*ptags-code:\s*([\w:]+)\s*(.*)/gmo) {
        my ($search, $code) = ($1, $2);   # assign in case the code uses regexes
        $self->verbose_warn(">>> ptags-code [$code]\n");
        my @tags = do {
            ## no critic (ProhibitNoStrict)
            no strict;
            no warnings;
            eval $code;
        };
        die $@ if $@;
        for my $tag (@tags) {
            do { $self->add_tag($tag, $filename, "?^$search\\>") }
              while $tag =~ y/:/-/;
        }
    }

    # custom ptags: per-file regexes
    my @re;
    while ($text =~ m!#\s*ptags:\s*/(.*)/\s*$!gm) {
        $self->verbose_warn(">>> ptags-regex [$1]\n");
        push @re => qr/$1/;
    }
    for my $re (@re) {

        # in theory we could nest this loop below the loop given above but
        # because they're iterating over the same string, funny things happen
        # when the regexes interfere with each other.
        while ($text =~ /$re/gm) {
            my $tag = $2;
            do { $self->add_tag($tag, $filename, "?^$1\\>") }
              while $tag =~ y/:/-/;
        }
    }
    if ($self->opt('use') && index($File::Find::name, $self->opt('use')) == 0) {

        # give modules a chance to output their custom ptags using $::PTAGS
        $self->verbose_warn(">>> use [$package]\n");
        {

            # localise global variables so that no matter what the module does
            # with them, they will be restored at the end of the block
            #
            # Spiffy messes up base::import(), so we save it here and restore
            # it later.
            local @INC = @INC;

            # %SIG values are undef first time around?
            no warnings 'uninitialized';
            local %SIG = %SIG;
            require base unless defined $INC{'base.pm'};
            my $real_base_import = \&base::import;
            local $SIG{__WARN__} = sub {
                my $warning = shift;
                return
                  if index($warning, 'Too late to run INIT block at') != -1;
                return
                  if $warning =~
                      qr/^cannot test anonymous subs .* Test::Class too late/;
                CORE::warn "Warnings during during [${package}->require]:\n";
                CORE::warn($warning);
            };
            $package->require;
            {
                no warnings 'redefine';
                *base::import = $real_base_import;
            }
        }

        # Also determine inheritance and make tags
        $self->verbose_warn(">>> inheritance for [$package]\n");
        no strict 'refs';
        $self->add_tag("subclass--$_", $filename, "?^use base\\>")
          for @{"${package}::ISA"};

        # Remember some data for tags we can't make now; we need the
        # information from all the files.
        $self->has_super_class($package, [ @{"${package}::ISA"} ]);
    }
}

sub process_pod_file {
    my $self     = shift;
    my $text     = read_file($_);
    my $filename = $self->get_filename;
    while ($text =~ /^(=for\s+ptags\s+package +(\w+(::\w+)*))\s*;/gmo) {
        my ($search, $tag) = ($1, $2);
        $self->make_package_tag(
            filename => $filename,
            search   => $search,
            tag      => $tag
        );
    }
}

# Add those tags that couldn't be added from looking at one file alone.
sub add_SUPER_tags {
    my $self            = shift;
    my %has_super_class = $self->has_super_class;
    while (my ($class, $super_array_ref) = each %has_super_class) {
        for my $super (@{ $super_array_ref || [] }) {
            next if $self->is_fake_package($super);
            unless ($self->exists_filename_for($super)) {
                warn sprintf
                  "class [%s]: can't get filename of superclass [%s]\n",
                  $class, $super;
                next;
            }
            $self->add_tag(
                "superclass--$class",
                $self->filename_for($super),
                "?^package $super\\>"
            );
        }
    }
}

sub add_yaml_marshall_tags {
    my $self = shift;
    return unless defined $YAML::TagClass;
    while (my ($marshall, $package) = each %$YAML::TagClass) {
        $marshall =~ s/\W/-/g;
        my $tag  = "marshall--$marshall";
        my $file = $self->filename_for($package);
        $self->add_tag($tag, $file, 1);
    }
}

sub fill_filename_placeholders {
    my $self = shift;
    my %tags = %{ $self->tags };
    while (my ($tag, $spec) = each %tags) {
        hash_rename %$spec,
          code => sub { s/^(filename_for:(.*))/ $self->filename_for($2) /e };
    }
    $self->tags(\%tags);
}
1;


__END__
=pod

=for test_synopsis 1;
__END__

=head1 NAME

Vim::Tag - Generate perl tags for vim

=head1 VERSION

version 1.100880

=head1 SYNOPSIS

    $ ptags --use ~/code/coderepos -o ~/.ptags

In C<.vimrc>:

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

Custom tag generation

    package Foo::Bar;

    $::PTAGS && $::PTAGS->add_tag($tag, $filename, $line);

=head1 DESCRIPTION

Manage tags for perl code in vim, with ideas on integrating tags with the bash
programmable completion project. See the synopsis.

You should subclass this class to use it in your C<ptags>-generating
application. It could be as simple as that:

    #!/usr/bin/env perl
    use warnings;
    use strict;
    use base qw(Vim::Tag);
    main->new->run;

And if you want just that, there's the C<ptags> program. But it is more
interesting to extend this with custom aliases and to have your modules
generate custom tags and so on. The documentation on those features is a bit
sparse at the moment, but take a look in this distribution's C<examples/>
directory.

=head1 METHODS

=head2 add_tag

Takes a tag name, a filename and a 'search' argument that can either be a line
number which caused the tag, or a vim search pattern which will jump to the
tag. It will add the tag to the C<tags> hash.

=head2 add_SUPER_tags

FIXME

=head2 add_yaml_marshall_tags

FIXME

=head2 delete_tags_by_pattern

FIXME

=head2 determine_libs

FIXME

=head2 fill_filename_placeholders

FIXME

=head2 finalize

FIXME

=head2 generate_tags

FIXME

=head2 get_filename

FIXME

=head2 make_package_tag

FIXME

=head2 make_tag_aliases

FIXME

=head2 process_pm_file

FIXME

=head2 process_pod_file

FIXME

=head2 run

FIXME

=head2 setup_fake_package

FIXME

=head2 verbose_warn

FIXME

=head2 write_tags

FIXME

=head1 INSTALLATION

See perlmodinstall for information and options on installing Perl modules.

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests through the web interface at
L<http://rt.cpan.org/Public/Dist/Display.html?Name=Vim-Tag>.

=head1 AVAILABILITY

The latest version of this module is available from the Comprehensive Perl
Archive Network (CPAN). Visit L<http://www.perl.com/CPAN/> to find a CPAN
site near you, or see
L<http://search.cpan.org/dist/Vim-Tag/>.

The development version lives at
L<http://github.com/hanekomu/Vim-Tag/>.
Instead of sending patches, please fork this project using the standard git
and github infrastructure.

=head1 AUTHOR

  Marcel Gruenauer <marcel@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2008 by Marcel Gruenauer.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

