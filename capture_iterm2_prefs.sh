#!/usr/bin/env bash
# Capture the current iTerm2 preferences into the repo as a portable snapshot.
#
# Strips iTerm's machine-local NoSync* keys and scrubs absolute /Users paths so
# nothing machine-specific is committed. iTerm does NOT need to be quit — this
# reads live prefs via `defaults export`. Re-run after changing iTerm settings
# in the GUI, then commit the updated plist.
set -euo pipefail

repo_dir="$(cd "$(dirname "$0")" && pwd)"
dest="${repo_dir}/macos/macos_app_settings/com.googlecode.iterm2.plist"
domain="com.googlecode.iterm2"

tmp="$(mktemp)"
trap 'rm -f "${tmp}"' EXIT
defaults export "${domain}" "${tmp}"

python3 - "${tmp}" "${dest}" <<'PY'
import plistlib, sys
src, dst = sys.argv[1], sys.argv[2]
with open(src, "rb") as f:
    d = plistlib.load(f)
# Drop iTerm's machine-local settings (installation id, window-restoration
# state, dismissed tips, etc.) — iTerm namespaces these with a NoSync prefix.
for k in [k for k in d if k.startswith("NoSync")]:
    del d[k]
# Scrub absolute home paths from profiles (inert unless Custom Directory=Yes).
for b in d.get("New Bookmarks", []):
    wd = b.get("Working Directory")
    if isinstance(wd, str) and wd.startswith("/Users/"):
        b["Working Directory"] = ""
with open(dst, "wb") as f:
    plistlib.dump(d, f, fmt=plistlib.FMT_XML)
print(f"captured -> {dst}")
PY

echo "Done. Review with: git diff -- macos/macos_app_settings/com.googlecode.iterm2.plist"
