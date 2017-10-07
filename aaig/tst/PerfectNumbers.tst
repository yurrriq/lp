gap> START_TEST("AAIG package: PerfectNumbers.tst");
gap> LoadPackage("AAIG", false);
#I  method installed for IsPerfect matches more than one declaration
true
gap> Filtered([1..999], IsPerfect);
[ 6, 28, 496 ]
gap> STOP_TEST( "PerfectNumbers.tst", 10000 );
