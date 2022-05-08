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

  android.applicationId = "org.bedelibry.demos.montague";
  android.displayName = "Montague App";
  android.resources = ./static/res;
  ios.bundleIdentifier = "org.bedelibry.demos.montague";
  ios.bundleName = "Montague App";

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
        rev = "55f22d3efa2fc1565bac70f6cc1bb8cd9397f721";
        sha256 = "J5smYPhshQZwD6Qn4YIvjBQPXn1/5UDAkgyP+8QT6aI=";
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
        rev = "65e5d1605f27547996dedb0575bf612f45f8c2e0";
        sha256 = "3KLblRkKmKZM4TZjaxR9ycNOZXoNV4zoiPVEyI1vRPY=";
      }) {};
    };
})
