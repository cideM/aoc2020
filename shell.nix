let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.haskellPackages; [
    (ghcWithPackages (pkgs: [ ]))
    ghcid
    ormolu
    fast-tags
    hlint
    # keep this line if you use bash
    nixpkgs.bashInteractive
  ];
}
