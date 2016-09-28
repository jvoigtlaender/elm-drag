rm -rf out || exit 0;

elm-package install --yes
elm-package install evancz/elm-html --yes
elm-package install evancz/elm-effects --yes
elm-package install evancz/start-app --yes
sysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make --yes

mkdir out
sysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make Example1.elm --output out/Example1.html --yes
sysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make Example2.elm --output out/Example2.html --yes
sysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make Example3.elm --output out/Example3.html --yes
sysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make Example3b.elm --output out/Example3b.html --yes
sysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make Example4.elm --output out/Example4.html --yes
cd out

if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$TRAVIS_BRANCH" == "master" ]];
then
  git init --quiet;
  git config user.name "Travis CI";
  git config user.email "jvoigtlaender@users.noreply.github.com";
  git add .;
  git commit -m "Travis deploy examples to gh-pages";
  git push --force --quiet "https://$GH_TOKEN@github.com/$TRAVIS_REPO_SLUG" master:gh-pages >/dev/null 2>&1;
fi
