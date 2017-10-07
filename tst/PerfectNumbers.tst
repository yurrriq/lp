gap> START_TEST("AAIG package: PerfectNumbers.tst");

gap> Filtered([1..999], IsPerfect);
[ 6, 28, 496 ]

gap> STOP_TEST( "AAIG package: PerfectNumbers.tst", 10000 );
