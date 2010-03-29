
use strict;
use warnings;

 BEGIN { require 5.008; } 

use ExtUtils::MakeMaker 6.11;



my %WriteMakefileArgs = (
                       'test' => {
                                   'TESTS' => 't/*.t'
                                 },
                       'NAME' => 'Vim::Tag',
                       'DISTNAME' => 'Vim-Tag',
                       'CONFIGURE_REQUIRES' => {
                                                 'ExtUtils::MakeMaker' => '6.11'
                                               },
                       'AUTHOR' => 'Marcel Gruenauer <marcel@cpan.org>',
                       'BUILD_REQUIRES' => {},
                       'ABSTRACT' => 'Generate perl tags for vim',
                       'EXE_FILES' => [
                                        'bin/ptagdir',
                                        'bin/ptags'
                                      ],
                       'VERSION' => '1.100880',
                       'PREREQ_PM' => {
                                        'Scalar::Util' => '0',
                                        'English' => '0',
                                        'parent' => '0',
                                        'constant' => '0',
                                        'Hash::Rename' => '0',
                                        'Getopt::Inherited' => '0',
                                        'Carp' => '0',
                                        'Test::More' => '0.88',
                                        'Class::Accessor::Constructor' => '0',
                                        'File::Find::Upwards' => '0',
                                        'File::Find' => '0',
                                        'File::Temp' => '0',
                                        'UNIVERSAL::require' => '0',
                                        'File::Slurp' => '0'
                                      },
                       'LICENSE' => 'perl'
                     );


delete $WriteMakefileArgs{LICENSE}
  unless eval { ExtUtils::MakeMaker->VERSION(6.31) };

WriteMakefile(%WriteMakefileArgs);



