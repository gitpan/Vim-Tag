use inc::Module::Install;
include 'Module::AutoInstall';

name 'Vim-Tag';
all_from 'lib/Vim/Tag.pm';
perl_version '5.006';

requires 'File::Find::Upwards';
requires 'Getopt::Attribute';
requires 'UNIVERSAL::require';

recommends 'Test::Pod';
recommends 'Test::Pod::Coverage';

test_requires 'Test::More' => '0.70';
test_requires 'Test::Compile';

auto_install;
auto_include;
WriteAll;
