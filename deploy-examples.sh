rm -rf out || exit 0;

npm install -g elm@0.16

elm-package install --yes
elm-package install evancz/elm-html --yes
elm-package install evancz/elm-effects --yes
elm-package install evancz/start-app --yes
elm-make --yes

mkdir out
elm-make Example1.elm --output out/Example1.html --yes
elm-make Example2.elm --output out/Example2.html --yes
elm-make Example3.elm --output out/Example3.html --yes
elm-make Example3b.elm --output out/Example3b.html --yes
elm-make Example4.elm --output out/Example4.html --yes
cd out

git init
git config user.name "Travis CI"
git config user.email "jvoigtlaender@users.noreply.github.com"
git add .
git commit -m "Travis deploy examples to gh-pages"
git push --force --quiet "https://$GH_TOKEN@github.com/$TRAVIS_REPO_SLUG" master:gh-pages >/dev/null 2>&1
