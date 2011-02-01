#!/bin/bash
#
# pbrisbin 2010
#
# I use this script to update cover images stored on my server from 
# their normal location in my music directory. I then add a function to 
# my mpc route to look for the cover by a defined name in the defined 
# location.
#
###

# list all files in the same directory that are not the file
try_files() {
  local file="$1"
  local dir="$(dirname "$1")"

  find "$dir" -maxdepth 1 -type f ! -name 'folder.jpg' -print
}

# read the tags of the file and come up with a sane filename, logic 
# should be the same haskell-side
mkfilename() {
  local IFS=$'\n'
  local file="$1"

  read -r artist   < <(mpc list artist any "${file/$mdir\//}")
  read -r album    < <(mpc list album  any "${file/$mdir\//}")

  # the exact same method is used to build the filename the haskell code 
  # will look for
  read -r filename < <(echo "$artist $album" | sed -r -e 's/.*/\L&/g' -e 's/ /_/g' -e 's/[^a-z0-9_.-]//g')

  if [[ -n "$filename" ]]; then
    echo "$filename.jpg"
  else
    echo
  fi
}

mdir="$HOME/Music"
odir='/srv/http/static/covers'

for cover in $(find "$mdir" -type f -name 'folder.jpg'); do
  for file in $(try_files "$cover"); do
    filename="$(mkfilename "$file")"
    if [[ -n "$filename" ]]; then
      cp -v "$cover" "$odir/$filename"
      break
    fi
  done
done
