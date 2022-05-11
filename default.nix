{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

	  config.android_sdk.accept_license = true;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {

  android.applicationId = "org.bedelibry.app";
  android.displayName = "Bedelibry";
  android.resources = ./static/res;
  ios.bundleIdentifier = "org.bedelibry.app";
  ios.bundleName = "Bedelibry";

  packages = {
    frontend-lib = ./frontend-lib;
    m1-frontend = ./m1-frontend;
  };

  overrides = self: super: {
      monad-tree = self.callHackageDirect {
        pkg = "monad-tree";
        ver = "0.2.0.0";
        sha256 = "qU50YWyeM1QI3lGQwboJ0iUlC4c4YTOrv3u/aVagRlg=";
      } {};
      montague = self.callCabal2nix "montague" (pkgs.fetchFromGitHub {
        owner = "sintrastes";
        repo = "montague";
        rev = "8b01f34f487a2570d8a378e5fc83e6b4e830207c";
        sha256 = "iGt/2kJdDcNfmVe7DKKt9vlNor+raaFSLKKc/YG57fw=";
      }) {};
      meriv-core = self.callCabal2nix "meriv-core" (pkgs.fetchFromGitHub {
        owner = "sintrastes";
        repo = "meriv-core";
        rev = "81a62ab935ccae5a775b19e75eb8aabb76ed39de";
        sha256 = "OpxBga//brv8Z4iC/xCpdEW8k+4Nu9bsx3NHu7B3Zy8=";
      }) {};
      pure-prolog = self.callCabal2nix "pure-prolog" (pkgs.fetchFromGitHub {
        owner = "sintrastes";
        repo = "pure-prolog";
        rev = "d2a94bdc86b06ad9db416a1f8a4ed7a9c5636156";
        sha256 = "YyDd0gZzt2Fk9JC4mo581UIpK9GSnS3Q6/a7+Hg16Sc=";
      }) {};
    };
})
