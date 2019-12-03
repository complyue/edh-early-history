{ overlays ? [ ], ... }@args:
import (<nixpkgs>) (args // {
  overlays = (args.overlays or [ ]) ++ [
    (self: super:
      let
        pkgsWithEdh = super.haskellPackages.override {
          overrides = hself: hsuper: {
            edh = hself.callCabal2nix "edh" ./edh { };
          };
        };
      in {
        # edh tobe a top-level package
        edh = pkgsWithEdh.edh;
        # override the Haskell package set at standard locations
        haskellPackages = pkgsWithEdh;
        haskell = super.haskell // {
          packages = super.haskell.packages // { ghcWithEdh = pkgsWithEdh; };
        };
      })
  ];
})
