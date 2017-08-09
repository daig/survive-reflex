{reflex-platform ? import ./deps/reflex-platform {}}:
reflex-platform.ghcjs.callPackage (reflex-platform.cabal2nixResult ./.) {}
