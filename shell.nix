let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  aoc = nixpkgs.haskellPackages.callPackage ./project.nix { };

in
nixpkgs.mkShell {
  inputsFrom = [ aoc.env ];

  buildInputs = with nixpkgs.haskellPackages; [
    (ghcWithPackages (pkgs: [ ]))
    ghcid
    ormolu
    fast-tags
    hlint
    # keep this line if you use bash
    nixpkgs.bashInteractive
    cabal2nix
    cabal-install
    cabal-fmt
  ];
}
