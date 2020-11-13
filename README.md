# language-zig

Template for Haskell + Nix projects.

Uses [`haskell.nix`](https://github.com/input-output-hk/haskell.nix) to get `ghc` and compliant `ormolu`, `ghcide`, and `hlint`.

`./wizard.sh` will

  1. prompt you for a package name, author name, email
  2. replace all occurrences of `language-zig`, `Jonas Carpay`, `jonascarpay@gmail.com`, and  `2020`
  4. delete `wizard.sh`
  3. reinitialize the git history

[Homepage](https://github.com/jonascarpay/template-haskell)
