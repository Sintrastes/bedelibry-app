# montague-reflex

Montague-reflex is a simple [reflex](https://reflex-frp.org/) front-end for [Montague](https://github.com/Sintrastes/Montague). 

Montague lets you specify your own schemas, defining a sub-set of English (or another natural language -- Unicode is supported too!)
 with an assignment of words to both a semantic and syntactic type. Montague then parses
 sentences (and sentence fragments) into a logical form, which can
 then be fed as input into other systems (for instance, a logic query language).

Thanks to the power of reflex and Functional Reactive Programming,
 you get instant feedback as you make changes to your schema and
 input sentences, as seen in the gif below.

<div align="center">
  <img width="250em" src="https://raw.githubusercontent.com/Sintrastes/montague-reflex/main/images/montague_screenshot.png"/>
</div>

# Building/Installing

Montague-reflex is built with [nix](https://nixos.org/) and optionally [obelisk](https://github.com/obsidiansystems/obelisk/), so make sure those are installed first. 

After nix is installed, to build an Android APK for Montague, just run `nix-build -A android.frontend -o result-android` at the root of this repo. This builds a symlink `result-android` at the root of the repo pointing to a directory with a built `android-app-debug.apk`.

To instead build a web front-end, you can run `nix-build -A ghcjs.frontend -o result-frontend`. The symlink `result-frontend` will contain a `frontend.jsexe` directory with all of the compiled `js` files needed to run the application, and a small `index.html` file that can be used to bootstrap the app. Note that the `css` files used by Montague may need to be manually added to this `index.html` file in order for everything to display properly.

If you have `ob` installed, a simple web server for testing out the app locally (at http://localhost:8000/ by default) can also be run by executing `ob run`.
