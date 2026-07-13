# Config Portability

This repo carries a consistent user environment across machines. The central
tension it manages: most configuration should apply everywhere, but some
configuration belongs to one specific machine and must never spread to the rest.

## Language

**Shared config**:
Configuration that belongs on every machine. It lives in this repo and is
linked into `$HOME`. This is the default — config is shared unless there's a
reason it can't be.
_Avoid_: portable files, dotfiles, the broader repo

**Machine context**:
Configuration unique to one individual machine, which must never enter the
shared repo. A machine *has* its own context; the config there travels with the
machine, not with the user. This is where machine-only paths, machine-only
tools, credentials, and "work" or "personal" settings live — the personal/work
distinction is not its own concept, just examples of things a given machine's
context happens to hold.
_Avoid_: local changes, personal config, work config, private config

**OS-conditional config**:
Shared config that is applied only on machines matching a given OS (e.g. the
`macos` and `linux` packages, `.zshrc_macos`). This is still shared config — it
lives in the repo and works on *any* machine of that OS. The OS is a machine
*property* that shared config branches on; it is not machine context.
_Avoid_: platform-specific files, local OS config
