#! @Chapter PerfectNumbers

#! @Section The IsPerfect() Operation

InstallMethod( IsPerfect,
    "for a positive integer",
    [ IsInt and IsPosInt ],
    n -> Sigma(n) = 2*n );

#! @BeginExample
Filtered([1..999], IsPerfect);
#! [ 6, 28, 496 ]
#! @EndExample
