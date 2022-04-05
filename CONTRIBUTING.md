# Contributing

## OCaml and opam

After following [these instructions for setting up OCaml with opam](https://v3.ocaml.org/docs/up-and-running) you should be in a position to start building this
repository. Refer to the following section for cloning the repository using git.

Once you have that set up to install the dependencies you can run 

```
opam install . --deps-only --with-test
```

This will install the main dependencies along with any we will need for running the tests. The build the project after opam has finished installing everything run:

```
dune build
```

And to run the tests (which are in the `test` directory) run:

```
dune runtest
```


After making changes to the code please also run the formatter to maintain a common style across the codebase. You can achieve this by running the following command:

```
dune build @fmt --auto
```

To run this command without any error, you might need to install the correct version of `ocamlformat`. The `.ocamlformat` file records the current version the repository uses. Install this version by running:

```
opam install ocamlformat=X.XX.X
```

where, `X.XX.X` denotes the version in the `.ocamlformat` file. *For example*, if the `0.20.1` version of `ocamlformat` has to be installed, then you must run `opam install ocamlformat=0.20.1`. To know more, kindly visit [OCamlFormat](https://github.com/ocaml-ppx/ocamlformat).

If you hit any problems please feel free to open an issue. 

## Git and GitHub workflow

The preferred workflow for contributing to a repository is to fork the main repository on GitHub, clone, and develop on a new branch.

If you aren't familiar with how to work with Github or would like to learn it, here is [a great tutorial](https://app.egghead.io/playlists/how-to-contribute-to-an-open-source-project-on-github).

Feel free to use any approach while creating a pull request. Here are a few suggestions from the dev team:

- If you are not sure whether your changes will be accepted or want to discuss the method before delving into it, please create an issue and ask it.
- Clone the repo locally (or continue editing directly in github if the change is small). Checkout
  out the branch that you created.
- Create a draft pull request with a small initial commit. Here's how you can [create a draft pull request.](https://github.blog/2019-02-14-introducing-draft-pull-requests/)
- Continue developing, feel free to ask questions in the PR, if you run into obstacles or uncertainty as you make changes
- Review your implementation according to the checks noted in the PR template
- Once you feel your branch is ready, change the PR status to "ready to review"
- Consult the tasks noted in the PR template
- When merging, consider cleaning up the commit body
- Close any issues that were addressed by this PR.
