# montague-reflex

Montague-reflex is a simple [reflex](https://reflex-frp.org/) front-end for [Montague](https://github.com/Sintrastes/Montague). 

# Building/Installing

Montague-reflex is built with [nix](https://nixos.org/) and optionally [obelisk](https://github.com/obsidiansystems/obelisk/), so make sure those are installed first. 

After nix is installed, to build an Android APK for Montague, just run `nix-build -A android.frontend -o result-android` at the root of this repo. To instead build a web front-end, you can run `nix-build -A ghcjs.frontend -o result-frontend`.

If you have `ob` installed, a simple web server for testing out the app locally (at http://localhost:8000/ by default) can also be run by executing `ob run`.
