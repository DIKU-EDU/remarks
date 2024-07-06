#!/usr/bin/env bash

set -euo pipefail

tmpfile=$(mktemp)
trap "rm $tmpfile" INT TERM EXIT

git_log() {
  git log --author="$1" -i --format='%ad' --date=short | cut -d'-' -f1 | sort
}

git log --format='%aN' | perl -pe 's/Oleks/oleks/' | uniq | sort -u | while IFS= read -r author; do
  first_year=$(git_log "${author}" | head -n 1)
  last_year=$(git_log "${author}" | tail -n 1)
  emails=$(git log --author="${author}" -i --format='%ae' | sort | uniq | perl -pe 'chomp if eof' | perl -0pe 's/\n/, /g')
  if [ "$author" == "oleks" ]; then
    author="Oleks Shturmov"
  fi
  if [ "$author" == "Oleks Shturmov" ]; then
    role="Founder"
  elif [ "$author" == "Troels Henriksen" ]; then
    role="Maintainer"
  elif [ "$author" == "Michael Kirkedal Thomsen" ]; then
    role="Maintainer"
  else
    role="Contributor"
  fi
  echo "$first_year-$last_year-$author ($emails), $role" >> $tmpfile
done

cat $tmpfile | \
  sort -t'-' -k2rn -k1n -k3 | \
  perl -pe 's/(\d{4})-(\d{4})-/\1-\2 /'
