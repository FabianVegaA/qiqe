# Qiqe

This is a project to learn how to build a monorepo for a full stack application, using [NixOs]() and [Flake]().

> **Note**: This project is a work in progress, the documentation is not complete yet, and the project is not ready to be used.

## What means Qiqe?

This is a mutated word from the spanish word "Quique", which is an animal that lives in the south of Chile. It's a little mammal that lives in the ground.

## What is the purpose of this project?

The purpose of this project is to learn how to build a full stack application using NixOs and Flake. The idea is to have a monorepo with a backend and a frontend, and to be able to deploy it in a server.

## What pretends to be this project?

This project pretends to be a Play Ground for a DSL language that I want to build. The idea is to create a lambda calculus language with a peak of syntactic sugar, to play with it just for fun.

## How to run it?

### Requirements

- [NixOs](https://nixos.org/download.html)
- [Flake](https://nixos.wiki/wiki/Flakes)

### Run

This project is a monorepo with various applications inside. To run it, you need to run the following command:

```bash
nix run .#<app-name>
```

> Apps:
>
> - auth-client: This is the frontend application
> - auth-server: This is the backend application
> - postgres: This is the database
> - postgrest: This is the API for the database
> - createdb: This is a script to create the database

> **Note**: I am working on a way to run all the applications at once.

### Development

Same as the run above, but with the following command:

```bash
nix develop .#<app-name>
```

> **Note**: Maybe you need to add the `--impure` flag to the command above.
