LoadPackage( "AAIG" );

TestDirectory( DirectoriesPackageLibrary("AAIG", "tst"),
               rec( exitGAP := true,
                    testOptions := rec(compareFunction := "uptowhitespace") ) );

FORCE_QUIT_GAP(1);
