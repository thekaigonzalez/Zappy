# $Id: build.pl

my $OPTIMIZATION=false;

use File::Which;

my $zig = which("zig");

if (!$zig) {
  die "zig not found, can't build!";
}

`$zig build-exe argparse.zig --name argparse-test`;
`rm *.o`
