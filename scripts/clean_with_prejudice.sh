#!/usr/bin/env bash

# "strict mode" -- see
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail

# move to the root of the git repository we're in
GIT_ROOT="$(git rev-parse --show-toplevel)"
pushd $GIT_ROOT >/dev/null

# While I would *like* to use `xargs -r` to keep everything behaving
# when no matching files come out of `find`, this option does not
# appear on MacOS. So instead, we run `find` twice.
#
# An alternative choice would be to run `find`, save its stdout to a
# variable, then `echo` that variable into xargs. But I'm not feeling
# optimistic about linux and MacOS having `bash` versions that handle
# NUL-characters exactly the same, so we just run `find` twice.

# Delete all directories named 'target' in the tree below
# Sorting in reverse order suppresses error messages by deleting
#   .../target/.../target before .../target
if [[ -n "$(find . -type d -name 'target')" ]]; then
  find . -type d -name 'target' -print0 | sort -z -r | xargs -n 1 -0 rm -rv
fi

# return to original working directory
popd >/dev/null

