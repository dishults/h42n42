# h42n42
Project that aims to make you discover the basis of web programming on the client side in OCaml thanks to Ocsigen
> Hopefully, this project will be opened to other programming languages in the future. Until then, here are all my notes that might be helpfull to get started with Eliom (OCaml).

## Mac

#### Install

```bash
brew install opam sqlite
opam init
opam install dune utop ocamlformat sqlite3 ocsipersist-sqlite eliom js_of_ocaml-lwt
```

#### Uninstall

```bash
brew uninstall --zap opam sqlite
# Remove opam switches
rm -rf ~/.opam
```

## Ubuntu at 42

#### Install

```bash
brew install opam sqlite openssl@1.1 pcre

# `disable-sandboxing` and `export` are necessary because brew
# is not installed at root level at school. Without those modifications
# `opam` won't be able to find brew's sqlite, for example
opam init --disable-sandboxing
# add to ~/.zshrc
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.brew/lib

# To prevent opam for promting user to install libsqlite3-dev with apt
# because you don't have sudo access, so it will proceed and try to use
# the one from brew
opam option depext-bypass=libsqlite3-dev

opam install dune utop ocamlformat sqlite3 ocsipersist-sqlite eliom js_of_ocaml-lwt
```

#### Uninstall

```bash
brew uninstall --zap opam sqlite openssl@1.1 pcre
# Remove opam switches
rm -rf ~/.opam
```

---

## Run

```makefile
# Compile & run. The site is accessible at http://localhost:8080/
make test.opt

# To be able to run `make test.byte` change in `Makefile`
OCSIGENSERVER := DYLD_LIBRARY_PATH=$(shell opam var lib)/stublibs ocsigenserver
```

## Notes

- Eliom is not fully supported by [vscode OCaml extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform) and `ocaml-lsp-server` (**don’t install it** to avoid errors showing up everywhere), so you’ll have only syntax highlighting in your editor.
- Eliom doesn’t automatically rebuild and relaunch when you make changes to a file. So for ease of use, I created a vscode build task (in `.vscode/tasks.json`) to rapidly do that with a single shortcut **cmd+shift+B** on Mac (**ctrl+shift+B** on Ubuntu). It automatically terminates all running tasks, autoformats all `*.eliom` files, then builds and launches the server. Feel free to use it, it helped me a lot.
- Launch `./autoformat.sh` to automatically format all `*.eliom` files if you don’t want to use the above build task.

⚠️ For **autoformat** to work make sure your project have a `.ocamlformat` file (empty or with explicite arguments)

---

```ocaml
(* Log string *)
print_endline (Js.to_string playground##.className);
(* Log anything *)
Js_of_ocaml.Firebug.console##log (Js.string "OK");
```

```bash
# Clean opam cache
opam clean --all

# Uninstall + remove dependances
opam remove <list of packages> --auto-remove
```
