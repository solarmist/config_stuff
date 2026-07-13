#!/bin/sh

# Modern Python dev toolchain.
#  - standalone CLI apps: uv tool install (isolated, pipx-style)
#  - REPL + libraries you import: pip --user (needs break-system-packages
#    in pip.conf on externally-managed / Homebrew Python)

# Bootstrap uv if missing.
if ! command -v uv >/dev/null 2>&1; then
    curl -LsSf https://astral.sh/uv/install.sh | sh
    PATH="$HOME/.local/bin:$PATH"; export PATH   # make uv available this run
fi

# Public CLI dev tools from PyPI explicitly. A machine may set a private
# default index in ~/.config/uv/uv.toml that doesn't carry these, so pin
# --default-index for these installs:
#   ruff       lint + format (replaces flake8/isort/black)
#   mypy       static type checker
#   pyright    alternative, stricter type checker
#   pytest     test runner
#   pre-commit git hook runner
#   nox        test/lint automation across environments
#   vulture    dead-code detector
#   pip-audit  dependency CVE scanner
for tool in ruff mypy pyright pytest pre-commit nox vulture pip-audit; do
    uv tool install --default-index https://pypi.org/simple/ "$tool"
done

# REPL and importable helpers in the user site (so `import ipdb` and rich
# tracebacks work in the plain python / ipython REPL).
python3 -m pip install --user --upgrade ipython ipdb rich gnureadline
