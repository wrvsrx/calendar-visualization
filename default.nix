{ mkDerivation, aeson, base, bytestring, Chart, Chart-cairo
, containers, data-default, deepseq, directory, filepath, iCalendar
, lib, optparse-applicative, split, text, time, transformers
, unicode-show
}:
mkDerivation {
  pname = "calendar-visualization";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring Chart Chart-cairo containers data-default
    deepseq directory filepath iCalendar optparse-applicative split
    text time transformers unicode-show
  ];
  license = "unknown";
  mainProgram = "calendar-visualization";
}
