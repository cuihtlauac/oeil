# oeil

Follow a merged pull request until it is deployed, from a terminal or an
MCP client, by reading the web UI of an [OCurrent](https://github.com/ocurrent/ocurrent)
service such as [deploy.ci.ocaml.org](https://deploy.ci.ocaml.org).

OCurrent web UIs show their pipeline only as an SVG diagram for a browser.
oeil reads that diagram back into a graph, finds a branch's deployment chain
(build → push-manifest → pull → docker-service → notify), and follows one
commit through it. It ties each stage to the commit through the image
digests in the job logs. It needs no credentials for the OCurrent service.

```
$ oeil watch ocaml/ocaml.org#4019
ocaml/ocaml.org#4019 data-packer: give each executable its own module → main @ 823ea15fa
✓ picked up by the deployer  1.3s
✓ build linux-x86_64  823ea15fa → sha256:3f1c0e2b…
✓ push ocurrent/v3.ocaml.org-server:live  → sha256:9b7d41aa…
✓ pull for infra_www2  2026-11-16/094012-docker-pull-1c2d3e
✓ service infra_www2  2026-11-16/094031-docker-service-0a1b2c
…
✓ deployed: ocaml/ocaml.org#4019 …
```

While a job runs, its latest log lines are shown under its row, and they
fold away when it succeeds. Output goes into the scrollback, not a full-screen
UI ([console](https://github.com/samoht/ocaml-console)). On a pipe or in CI
it prints plain lines, and `--progress json` prints JSON events.

## Install

Needs OCaml ≥ 5.4.

```
opam install . --deps-only --with-test
dune build
dune install   # or: dune exec -- oeil …
```

## Commands

| Command | What it does |
|---|---|
| `oeil watch owner/repo#PR` | Waits for the PR to be merged, then follows its merge commit to the deploy target of the PR's base branch. |
| `oeil watch owner/repo@SHA [--branch B]` | Follows a commit (default branch `main`). |
| `oeil watch owner/repo:branch` | Follows the branch's latest build. |
| `oeil status [--repo owner/repo] [--all]` | Lists failed and active nodes with their errors. `--repo` also shows that repository's deployment chains. |
| `oeil log JOB [-f]` | Prints a job's log, or follows it while it runs. |
| `oeil mcp` | MCP server on stdin/stdout. |

`--instance URL` selects another OCurrent service (default
`https://deploy.ci.ocaml.org`). `watch` exits with 0 when the commit is deployed,
1 when the deploy failed, 2 on timeout (`--timeout`, default 1 h), and 3 on a
bad target or an unreachable service.

GitHub is used to resolve a PR to its merge commit, and to check whether a
newer commit that got deployed instead includes yours. A token is taken from
`GITHUB_TOKEN`, `GH_TOKEN` or `gh auth token`. Without one, unauthenticated
API limits apply.

## MCP

```
claude mcp add oeil -- oeil mcp
```

Tools:

- `deploy_status {target}`: the status of each stage and the overall verdict
  (`in_progress`, `deployed`, `failed`).
- `wait_for_deploy {target, until?: "done"|"change", timeout_s?}`: blocks until
  the deploy finishes, or until the next stage change. It sends progress
  notifications when the client passes a progress token.
- `pipeline_status {repo?}`: failed and active nodes, with error messages.
- `job_log {job, tail?}`.

## How it follows a commit

1. The build job's log names the commit it built
   (`…#refs/heads/main (<sha>)`) and the digest it pushed. If a newer commit
   was built instead, GitHub's compare API tells whether it includes yours.
   If it does, oeil follows that one and reports the target as superseded.
2. The push-manifest job's log names the built digest and the digest it
   published.
3. The pull and docker-service jobs name the published digest.
4. A stage whose matching job is older than the build is reported as
   *unchanged*: the image was already live, so nothing was redeployed.

The state of each node comes from its fill colour in the SVG. A two-colour
fill means a job is re-running while the node keeps its previous value.
A running job's page stays open and keeps sending the log, which is how
logs are followed live.

## License

ISC
