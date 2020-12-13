let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.haskellPackages; [
    ormolu
    hlint
    nixpkgs.entr
    nixpkgs.lua53Packages.luacheck
    nixpkgs.clojure
    nixpkgs.clj-kondo
  ];
}
