let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.haskellPackages; [
    (ghcWithPackages (pkgs: [ ]))
    ghcid
    ormolu
    hlint
  ];
}
