let
  nixpkgsSrc = builtins.fetchTarball {
    # nixpkgs master as of 2020-07-17
    url = https://github.com/NixOS/nixpkgs/archive/e6d81a9b89e8dd8761654edf9dc744660a6bef0a.tar.gz;
    sha256 = "0lmw1zy00l89b0x7l5f85bvxdd2w245iqf9smyiyxvl1j03b0zyq";
  };

  my-overlay = self: super: {
    my-nodejs = self.nodejs-13_x;

    my-yarn = self.yarn.override {
      nodejs = self.my-nodejs;
    };

    my-env = self.stdenv.mkDerivation {
      pname = "road-to-react-purescript-env";
      version = "0.0.1";
      nativeBuildInputs = with self; [
        my-nodejs
        my-yarn
        purescript
        purescript-psa
        spago
        nodePackages.parcel-bundler
      ];
      buildInputs = with self; [
        zlib
      ];
      dontUnpack = true;
      dontPatch = true;
      dontConfigure = true;
      dontBuild = true;
      installPhase = "touch $out";
    };
  };

  nixpkgs = import nixpkgsSrc {
    overlays = [ my-overlay ];
  };

in

nixpkgs
