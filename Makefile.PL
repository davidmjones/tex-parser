use 5.008005;

use ExtUtils::MakeMaker;

WriteMakefile(
    NAME           => 'TeX::Parser',
    VERSION_FROM   => 'lib/TeX/Parser.pm', # finds $VERSION
    PREREQ_PM      => { Scalar::Util => 1.21 }, # e.g., Module::Name => 1.1
    AUTHOR         => 'David M. Jones <dmj@ams.org>',
    LICENSE        => mit,
);

__END__
