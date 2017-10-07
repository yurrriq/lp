SetPackageInfo( rec(

PackageName := "AAIG",
Subtitle := "Abstract Algebra in GAP",
Version := "0.0.1",
Date := "06/10/2017", # NOTE: dd/mm/yyyy
PackageWWWHome :=
  Concatenation( "https://github.com/yurrriq/lp/tree/master/",
                 LowercaseString( ~.PackageName ) ),
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/yurrriq/lp"
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
SupportEmail := "eric@ericb.me",
Persons := [
  rec(
    LastName := "Bailey",
    FirstNames := "Eric",
    IsAuthor := true,
    IsMaintainer := true,
    Email := ~.SupportEmail,
    # WWWHome := ...,
    # PostalAddress := ...,
    # Place := ...,
    # Institution := ...
  )
],
Status := "other",
README_URL :=
  Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL :=
  Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
# TODO: AbstractHTML := ...,
PackageDoc := rec(
  BookName := "AAIG",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile := "doc/manual.pdf",
  SixFile := "doc/manual.six",
  LongTitle := "Abstract Algebra in GAP"
),
Dependencies := rec(
  GAP := "4.8.8",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),
AvailabilityTest := ReturnTrue,
TestFile := "tst/testall.g",
Autoload := false,
# Keywords := [ ... ],
# BannerString := ...
));
