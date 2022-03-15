# montague-reflex

Montague-reflex is a simple [reflex](https://reflex-frp.org/) front-end for [Montague](https://github.com/Sintrastes/Montague). 

# Building/Installing

Montague-reflex is built with [obelisk](https://github.com/obsidiansystems/obelisk/) and [nix](https://nixos.org/), so make sure those are installed first. 

After those are installed, to build an Android APK for Montague, just run `nix-build -A android.frontend -o result-android` at the root of this repo. A simple web server for testing out the app locally (at http://localhost:8000/ by default) can also be run by executing `ob run`.
